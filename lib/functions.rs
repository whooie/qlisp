use std::{
    cmp,
    fs::{
        self,
        File,
    },
    io::{
        Read,
        BufRead,
        BufReader,
    },
    mem,
    path::PathBuf,
    rc::Rc,
};
use itertools::Itertools;
use qlisp_macros::{
    builtin,
};
use crate::{
    qerr,
    qerr_fmt,
    qbool,
    qint,
    qfloat,
    qcomplex,
    qlist,
    qstr,
    qsymbol,
    qfunc,
    qlambda,
    qany,
    lang::{
        FILE_EXTENSIONS,
        PROTECTED,
        QErr,
        QResult,
        QExp,
        QExpType,
        QBuiltin,
        QLambda,
        QEnv,
        QEnvEntry,
        Indexable,
        convert_type,
        convert_type_num,
        convert_numbers_sametype,
        typecast,
        StrFmt,
    },
    repl::run_repl,
};

/*
 * special
 */

/// Assign a value to a symbol and store it in the local environment.
/// Alias: `:=`
///
/// Expected form:
/// `(def <symbol> <value expression>)`
///
/// Example:
/// ```text
/// q>> (def a (+ 1 2 3)) ; assign `6` to `a`
/// q>> a ; evaluates to 6
/// q>> (+ 5 (def b (* 10 11))) ; evaluates to 115
/// q>> b ; evaluates to 110
/// ```
#[builtin(name = "def", alias = ":=")]
pub fn fn_def(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 2 {
        return Err(qerr_fmt!(
            "def: expected 2 args but got {}", args.len()));
    }
    let symbol: String = match args.first().unwrap() {
        qsymbol!(s) => {
            if PROTECTED.contains(&s.as_ref()) {
                Err(qerr_fmt!(
                    "def: cannot overwrite protected symbol '{}'", s))
            } else if s.contains("::") {
                Err(qerr!(
                    "def: cannot overwrite values outside the local scope"))
            } else {
                Ok(s.clone())
            }
        },
        _ => Err(qerr!("def: first arg must be a symbol")),
    }?;
    let val: QExp = env.eval(args.get(1).unwrap())?;
    env.insert(symbol, QEnvEntry::Exp(val.clone()));
    return Ok(val);
}

/// Assign multiple values to multiple symbols, recursively unpacking lists if
/// necessary. The top-level value expression must be a list.
/// Alias: `*:=`
///
/// Expected form:
/// `(let (<symbols>...) (<values>...))`
///
/// Example:
/// ```text
/// q>> (let (a b (c d)) (0 1 (2 (range 3 6))))
/// q>> a ; evaluates to 0
/// q>> b ; evaluates to 1
/// q>> c ; evaluates to 2
/// q>> d ; evaluates to (3 4 5)
/// ```
#[builtin(name = "let", alias = "*:=")]
pub fn fn_let(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    fn get_assignments(lhs: &QExp, rhs: &QExp)
        -> QResult<Vec<(QExp, QExp)>>
    {
        return match (lhs, rhs) {
            (qlist!(syms), qlist!(vals)) => {
                if syms.len() != vals.len() {
                    return Err(qerr!(
                        "let: symbol structure does not match value \
                        structure"
                    ));
                }
                syms.iter().zip(vals.iter())
                    .map(|(sk, vk)| {
                        match (sk, vk) {
                            (qsymbol!(s), _) => {
                                if s.contains("::") {
                                    Err(qerr!(
                                        "let: cannot overwrite values outside \
                                        the local scope"
                                    ))
                                } else {
                                    Ok((sk.clone(), vk.clone()))
                                }
                            },
                            (qlist!(_), qlist!(_)) => {
                                get_assignments(sk, vk)
                                    .map(|subass| {
                                        let (s, v): (Vec<QExp>, Vec<QExp>)
                                            = subass.into_iter().unzip();
                                        (qlist!(s), qlist!(v))
                                    })
                            },
                            _ => Err(qerr!(
                                "let: symbol structure does not match \
                                value structure"
                            )),
                        }
                    })
                    .collect::<QResult<Vec<(QExp, QExp)>>>()
            },
            _ => Err(qerr!(
                "let: symbol structure does not match value structure"
            )),
        };
    }

    fn do_assignments(env: &mut QEnv, sym: QExp, val: &QExp)
        -> QResult<()>
    {
        return match (sym, val) {
            (qsymbol!(s), v) => {
                env.insert(s, QEnvEntry::Exp(v.clone()));
                Ok(())
            },
            (qlist!(syms), qlist!(vals)) => {
                syms.into_iter().zip(vals.iter())
                    .map(|(s, v)| do_assignments(env, s, v))
                    .collect::<QResult<Vec<()>>>()?;
                Ok(())
            },
            _ => Err(qerr!("unexpected state in let")),
        };
    }

    if args.len() != 2 {
        return Err(qerr_fmt!(
            "let: expected 2 args but got {}", args.len()));
    }
    let lhs: &QExp = args.get(0).unwrap();
    let rhs: QExp = env.eval(args.get(1).unwrap())?;
    let values: Vec<QExp>
        = get_assignments(lhs, &rhs)?
        .into_iter()
        .map(|(s, v)| {
            do_assignments(env, s, &v).map(|_| v)
        })
        .collect::<QResult<Vec<QExp>>>()?;
    return Ok(qlist!(values));
}

/// Construct an anonymous (lambda) function.
/// Alias: `@:`
///
/// Expected form:
/// `(fn (<args>...) <body expression>)`
///
/// Example:
/// ```text
/// q>> ; define a function 5 * b + a and immediately call it
/// q>> ((fn (a b) (+ a (* 5 b))) 8 2) ; evaluates to 18
/// ```
#[builtin(name = "fn", alias = "@:")]
pub fn fn_fn(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    fn subs_from_env(env: &mut QEnv, protected: &[String], body_exp: &QExp)
        -> QExp
    {
        return match body_exp {
            qsymbol!(s) => {
                if protected.contains(s) {
                    qsymbol!(s.clone())
                } else {
                    env.get_exp_cloned(s).unwrap_or_else(|| body_exp.clone())
                }
            },
            qlist!(l) => qlist!(
                l.iter().map(|qk| subs_from_env(env, protected, qk)).collect()
            ),
            q => q.clone(),
        };
    }

    fn verify_arg_pattern(exps: &[QExp], symbols: &mut Vec<String>)
        -> QResult<Vec<QExp>>
    {
        let mut pattern: Vec<QExp> = Vec::new();
        for qk in exps.iter() {
            match qk {
                qsymbol!(s) => {
                    if s.contains("::") {
                        return Err(qerr!(
                            "fn: invalid arg name: cannot contain '::'"));
                    }
                    symbols.push(s.clone());
                    pattern.push(qsymbol!(s.clone()));
                },
                qlist!(l) => {
                    pattern.push(qlist!(verify_arg_pattern(l, symbols)?));
                },
                _ => {
                    return Err(qerr!(
                        "fn: invalid arg pattern: must be a list of symbols or \
                        nested lists of symbols"
                    ));
                },
            }
        }
        return Ok(pattern);
    }

    if args.len() != 2 {
        return Err(qerr_fmt!(
            "fn: expected 2 args but got {}", args.len()));
    }
    let mut protected_symbols: Vec<String> = Vec::new();
    let arg_pattern: Vec<QExp>
        = match args.get(0).unwrap() {
            qlist!(l) => verify_arg_pattern(l, &mut protected_symbols),
            _ => Err(qerr!(
                "fn: invalid arg pattern: must be a list of symbols or nested \
                lists of symbols"
            )),
        }?;
    let body_exp: QExp
        = subs_from_env(
            env,
            &protected_symbols,
            args.get(1).ok_or_else(|| qerr!("fn: missing function body"))?
        );
    return Ok(
        qlambda!(QLambda {
            arg_pattern: Rc::new(arg_pattern),
            body_exp: Rc::new(body_exp),
        })
    );
}

/// Portmanteau of 'def' and 'fn': Construct a function and assign it to a
/// symbol in the local environment.
/// Alias: `@:=`
///
/// Expected form:
/// `(defn <symbol> (<args>...) <body expression>)`
///
/// Example:
/// ```text
/// q>> ; recursive definition of the factorial operation
/// q>> (defn factorial (n) (if (<= n 1) 1 (* (factorial (- n 1)) n)))
/// q>> (factorial 5) ; evaluates to 120
/// ```
#[builtin(name = "defn", alias = "@:=")]
pub fn fn_defn(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 3 {
        return Err(qerr_fmt!(
            "defn: expected 3 args but got {}", args.len()));
    }
    let symbol: String = match args.get(0).unwrap() {
        qsymbol!(s) => {
            if PROTECTED.contains(&s.as_ref()) {
                Err(qerr_fmt!(
                    "defn: cannot overwrite protected symbol '{}'", s))
            } else if s.contains("::") {
                Err(qerr!(
                    "defn: cannot overwrite symbols outside the local scope"))
            } else {
                Ok(s.clone())
            }
        },
        _ => Err(qerr!("defn: first arg must be a symbol")),
    }?;
    let lambda: QExp = fn_fn(env, &args[1..])?;
    env.insert(symbol, QEnvEntry::Exp(lambda.clone()));
    return Ok(lambda)
}

/// Conditional expression: Evaluates one of two expressions based on the
/// true/false value of a test expression.
///
/// Expected form:
/// `(if <test expression> <true expression> <false expression>)`
///
/// Example:
/// ```text
/// q>> (def a true)
/// q>> (if a 1 0) ; evaluates to 1
/// q>> (def b false)
/// q>> (if b 1 0) ; evaluates to 0
/// ```
#[builtin(name = "if")]
pub fn fn_if(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 3 {
        return Err(qerr_fmt!(
            "if: expected 3 args but got {}", args.len()));
    }
    let test: QExp = env.eval(args.get(0).unwrap())?;
    return match test {
        qbool!(b) => {
            let branch_idx: usize = if b { 1 } else { 2 };
            env.eval(args.get(branch_idx).unwrap())
        },
        _ => Err(qerr!("if: test arg must evaluate to a boolean")),
    };
}

/// Construct a module and assign it to a symbol in the local environment.
/// Evaluated data is returned as a list ordered by evaluation.
///
/// Expected form:
/// `(module <symbol> (<expressions...>))`
///
/// Example:
/// ```text
/// q>> ; use a namespace (module) to hold a set of constants and functions
/// q>> ; elements of a module cannot be changed from the outside
/// q>> (module my-module ((def a 10) (defn add-to-a (b) (+ a b))))
/// q>> my-module::a ; evaluates to 10
/// q>> ; functions inside a module are unaffected by outer environments
/// q>> (my-module::add-to-a 10) ; evaluates to 20
/// q>> (def a 15)
/// q>> (my-module::add-to-a 10) ; evaluates to 20
/// ```
#[builtin(name = "module")]
pub fn fn_module(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 2 {
        return Err(qerr_fmt!(
            "module: expected 2 args but got {}", args.len()));
    }
    let name: String = match args.get(0).unwrap() {
        qsymbol!(s) => {
            if PROTECTED.contains(&s.as_ref()) {
                Err(qerr_fmt!(
                    "module: cannot overwrite protected symbol '{}'", s))
            } else if s.contains("::") {
                Err(qerr!(
                    "module: cannot overwrite values outside the local scope"))
            } else {
                Ok(s.clone())
            }
        },
        _ => Err(qerr!("module: first arg must be a symbol")),
    }?;
    let mut mod_env = env.clone();
    let mod_values: Vec<QExp> = match args.get(1).unwrap() {
        qlist!(l) => mod_env.eval_multi(l),
        _ => Err(qerr!("module: second arg must be a list of expressions")),
    }?;
    env.insert(name, QEnvEntry::Mod(mod_env));
    return Ok(qlist!(mod_values));
}

/// Load an external file as a module and assign it to a symbol in the local
/// environment. External files are specified as a symbolic path, where each
/// element in the path should correspond to a directory except for the final
/// element, which should correspond to a file name. The final element should
/// not have a file extension; instead the directory corresponding to the final
/// parent in the path will be searched for files with stems equal to the final
/// element and ending with '.qlisp', or '.qlsp' (searched in that order). All
/// paths are treated as relative to the file being executed or the current
/// working directory if the REPL is being used. Use 'super' in a path to
/// specify a parent directory (equivalent to '..'). A symbol may be also be
/// specified to store the module under; the default symbol is the final path
/// element.
///
/// Expected form:
/// `(use <path> [symbol])`
///
/// Example:
/// ```text
/// q>> ; load ../mod1/mod2/child.qlisp and store it as child-mod
/// q>> (use super::mod1::mod2::child child-mod)
/// ```
#[builtin(name = "use")]
pub fn fn_use(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if !(1..=2).contains(&args.len()) {
        return Err(qerr_fmt!(
            "use: expected 1 or 2 args but got {}", args.len()));
    }
    let path: &String = match args.get(0).unwrap() {
        qsymbol!(s) => Ok(s),
        _ => Err(qerr!("use: arg must be a symbol")),
    }?;
    let filepath: PathBuf
        = env.dir.join(
            path.split("::")
            .map(|s| if s == "super" { ".." } else { s })
            .collect::<PathBuf>()
        );
    let symbol: String = match args.get(1) {
        Some(qsymbol!(s)) => {
            if PROTECTED.contains(&s.as_ref()) {
                Err(qerr_fmt!("use: cannot overwrite protected symbol '{}'", s))
            } else if s.contains("::") {
                Err(qerr!(
                    "use: cannot overwrite symbols outside the local scope"))
            } else {
                Ok(s.clone())
            }
        },
        None => Ok(
            filepath.file_stem().unwrap()
            .to_string_lossy().into_owned()
        ),
        _ => Err(qerr!("use: second arg must be a symbol")),
    }?;
    let mut mod_env
        = QEnv::default_with_dir(filepath.parent().unwrap().to_path_buf());
    for ext in FILE_EXTENSIONS.iter() {
        let file = filepath.with_extension(*ext);
        if file.is_file() {
            let display = file.display();
            let mut file = File::open(&file)
                .map_err(|e| qerr_fmt!(
                    "use: cannot read file {}: {}", display, e
                ))?;
            let mut code = String::new();
            file.read_to_string(&mut code)
                .map_err(|e| qerr_fmt!(
                    "use: cannot read file {}: {}", display, e
                ))?;
            mod_env.parse_eval(code)
                .map_err(|e| qerr_fmt!(
                    "use: cannot load module '{}': {}", path, e.message()
                ))?;
            env.insert(symbol, QEnvEntry::Mod(mod_env));
            return Ok(qint!(0));
        }
    }
    return Err(qerr_fmt!("use: cannot not load module {}", path));
}

/// Load the contents of an external file as a module and bring all of its
/// contents into the local environment.External files are specified as a
/// symbolic path, where each element in the path should correspond to a
/// directory except for the final element, which should correspond to a file
/// name. The final element should not have a file extension; instead the
/// directory corresponding to the final parent in the path will be searched for
/// files with stems equal to the final element and ending with '.qlisp', or
/// '.qlsp' (searched in that order). All paths are treated as relative to the
/// file being executed or the current working directory if the REPL is being
/// used. Use 'super' in a path to specify a parent directory (equivalent to
/// '..'). A symbol may be also be specified to store the module under; the
/// default symbol is the final path element.
/// Alias: `use*`
///
/// Expected form:
/// `(use-all <path>)`
///
/// Example:
/// ```text
/// q>> ; load the contents of ../mod1/mod2/child.qlisp into the local environment
/// q>> (use-all super::mod1::mod2::child)
/// ```
#[builtin(name = "use-all", alias = "use*")]
pub fn fn_use_all(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 1 {
        return Err(qerr_fmt!(
            "use-all: expected 1 arg but got {}", args.len()));
    }
    let path: &String = match args.get(0).unwrap() {
        qsymbol!(s) => Ok(s),
        _ => Err(qerr!("use-all: arg must be a symbol")),
    }?;
    let mut filepath: PathBuf
        = env.dir.join(
            path.split("::")
            .map(|s| if s == "super" { ".." } else { s })
            .collect::<PathBuf>()
        );
    for ext in FILE_EXTENSIONS.iter() {
        let file = filepath.with_extension(*ext);
        if file.is_file() {
            let display = file.display();
            let mut file = File::open(&file)
                .map_err(|e| qerr_fmt!(
                    "use-all: cannot read file {}: {}", display, e
                ))?;
            let mut code = String::new();
            file.read_to_string(&mut code)
                .map_err(|e| qerr_fmt!(
                    "use-all: could not interpret file {}: {}", display, e
                ))?;
            mem::swap(&mut env.dir, &mut filepath);
            env.parse_eval(code)?;
            mem::swap(&mut env.dir, &mut filepath);
            return Ok(qint!(0));
        }
    }
    return Err(qerr_fmt!("use-all: cannot not load module {}", path));
}

/// Immedately pause execution of a program and launch the REPL. Execution will
/// continue when the REPL is exited.
///
/// Expected form:
/// `(interact)`
///
/// Example:
/// ```text
/// q>> ; suppose this is in the middle of a file being executed
/// q>> (def a 10)
/// q>> (defn foo (b) (+ a b))
/// q>> ; launch the REPL here
/// q>> ; the environment will hold whatever definitions have been made up to this
/// q>> ; point in the program
/// q>> (interact)
/// q>> ; continue doing other things after the REPL exits.
/// ```
#[builtin(name = "interact")]
pub fn fn_interact(env: &mut QEnv, _args: &[QExp]) -> QResult<QExp> {
    run_repl(env);
    return Ok(qint!(0));
}

/// Check whether a set of definitions is present. Returns `true` if all symbols
/// passed as arguments exist with a value, `false` otherwise.
/// Alias: `?:=`
///
/// Expected form:
/// `(isdef <symbols>...)`
///
/// Example:
/// ```text
/// q>> (def a 10)
/// q>> (def foo (b) (+ a b))
/// q>> (module my-module ((def c 15)))
/// q>> (isdef a foo my-module::c) ; evaluates to true
/// ```
#[builtin(name = "isdef", alias = "?:=")]
pub fn fn_isdef(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    let isdef: bool
        = args.iter()
        .map(|qk| match qk {
            qsymbol!(s) => Ok(s),
            _ => Err(qerr!("isdef: args must be symbols")),
        })
        .collect::<QResult<Vec<&String>>>()?
        .into_iter()
        .all(|sym| env.get(sym).is_some());
    return Ok(qbool!(isdef));
}

/// Remove a definition from the local environment. Any value including modules
/// may be removed, but only values will be returned.
/// Alias: `!-`
///
/// Expected form:
/// `(del <symbols>...)`
///
/// Example:
/// ```text
/// q>> (def a 10)
/// q>> a ; evaluates to 10
/// q>> (del a) ; evaluates to 10
/// q>> a ; causes an error
/// ```
#[builtin(name = "def", alias = "!-")]
pub fn fn_del(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    let vals: Vec<QExp>
        = args.iter()
        .map(|qk| match qk {
            qsymbol!(s) => {
                if s.contains("::") {
                    Err(qerr!(
                        "del: cannot remove variables outside the local \
                        environment"
                    ))
                } else {
                    env.remove(s).map_err(|e| e.prepend_source("del"))
                }
            },
            _ => Err(qerr!("del: args must be symbols")),
        })
        .collect::<QResult<Vec<QEnvEntry>>>()?
        .into_iter()
        .filter_map(|q| match q {
            QEnvEntry::Exp(e) => Some(e),
            QEnvEntry::Mod(_) => None,
        })
        .collect();
    return if vals.len() == 1 {
        Ok(vals.into_iter().next().unwrap())
    } else {
        Ok(qlist!(vals))
    };
}

/*
 * systems
 */

/// Substitute values for patterns in a format string.
/// Alias: `$`
///
/// Expected form:
/// `(format <format string> <values>...)`
///
/// Example:
/// ```text
/// q>> (format \"hello, {}!\" \"John\") ; evaluates to \"hello, John!\"
/// q>> (format \"{:.5}\" 3.141592653589793238) ; evaluates to \"3.14159\"
/// ```
#[builtin(name = "format", alias = "$")]
pub fn fn_format(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.is_empty() {
        return Err(qerr!("format: missing format string"));
    }
    let fmtstr: String = match env.eval(args.get(0).unwrap())? {
        qstr!(s) => Ok(s),
        _ => Err(qerr!("format: first arg must be a format string")),
    }?;
    let vals: Vec<QExp> = env.eval_multi(&args[1..])?;
    let formatted: String
        = fmtstr.format(&vals)
        .map_err(|e| e.prepend_source("format"))?;
    return Ok(qstr!(formatted));
}

/// Substitute values for patterns in a format string and print to STDOUT. If a
/// single value is passed, the value is returned; if multiple values are
/// passed, they are all returned in a list.
/// Alias: `$-`
///
/// Expected form:
/// `(print <format string> <values>...)`
///
/// Example:
/// ```text
/// q>> (print "hello {}" "John") ; prints `hello John`; evaluates to "John"
/// q>> ; passed values are returned, so `print` can be inserted in the middle
/// q>> ; of other expressions
/// q>> (+ (print "{}" (* 2 3)) 5) ; prints `6` and evaluates to 11
/// ```
#[builtin(name = "print", alias = "$-")]
pub fn fn_print(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.is_empty() {
        return Err(qerr!("print: missing format string"));
    }
    let fmtstr: String = match env.eval(args.get(0).unwrap())? {
        qstr!(s) => Ok(s),
        _ => Err(qerr!("print: first arg must be a format string")),
    }?;
    let vals: Vec<QExp> = env.eval_multi(&args[1..])?;
    let formatted: String
        = fmtstr.format(&vals)
        .map_err(|e| e.prepend_source("print"))?;
    print!("{}", formatted);
    return if vals.len() == 1 {
        Ok(vals.into_iter().next().unwrap())
    } else {
        Ok(qlist!(vals))
    };
}

/// Substitute values for patterns in a format string and print to STDOUT with
/// newline appended. If a single value is passed, the value is returned; if
/// multiple values are passed, they are all returned in a list.
/// Alias: `$_`
///
/// Expected form:
/// `(println <format string> <values>...)`
///
/// Example:
/// ```text
/// q>> (println "hello {}" "John") ; prints `hello John\n`; evaluates to "John"
/// q>> ; passed values are returned, so `println` can be inserted in the middle
/// q>> ; of other expressions
/// q>> (+ (println "{}" (* 2 3)) 5) ; prints `6\n` and evaluates to 11
/// ```
#[builtin(name = "println", alias = "$_")]
pub fn fn_println(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.is_empty() {
        return Err(qerr!("println: missing format string"));
    }
    let fmtstr: String = match env.eval(args.get(0).unwrap())? {
        qstr!(s) => Ok(s),
        _ => Err(qerr!("println: first arg must be a format string")),
    }?;
    let vals: Vec<QExp> = env.eval_multi(&args[1..])?;
    let formatted: String
        = fmtstr.format(&vals)
        .map_err(|e| e.prepend_source("println"))?;
    println!("{}", formatted);
    return if vals.len() == 1 {
        Ok(vals.into_iter().next().unwrap())
    } else {
        Ok(qlist!(vals))
    };
}

/// Substitute values for patterns in a format string and print to STDOUT,
/// immediately flushing output after. If a single value is passed, the value is
/// returned; if multiple values are passed, they are all returned in a list.
/// Alias: `$$-`
///
/// Expected form:
/// `(print <format string> <values>...)`
///
/// Example:
/// ```text
/// q>> (print "hello {}" "John") ; prints `hello John`; evaluates to "John"
/// q>> ; passed values are returned, so `print` can be inserted in the middle
/// q>> ; of other expressions
/// q>> (+ (print "{}" (* 2 3)) 5) ; prints `6` and evaluates to 11
/// ```
#[builtin(name = "print-flush", alias = "$$-")]
pub fn fn_print_flush(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.is_empty() {
        return Err(qerr!("print: missing format string"));
    }
    let fmtstr: String = match env.eval(args.get(0).unwrap())? {
        qstr!(s) => Ok(s),
        _ => Err(qerr!("print: first arg must be a format string")),
    }?;
    let vals: Vec<QExp> = env.eval_multi(&args[1..])?;
    let formatted: String
        = fmtstr.format(&vals)
        .map_err(|e| e.prepend_source("print"))?;
    print!("{}", formatted);
    std::io::Write::flush(&mut std::io::stdout()).unwrap();
    return if vals.len() == 1 {
        Ok(vals.into_iter().next().unwrap())
    } else {
        Ok(qlist!(vals))
    };
}

/// Substitute values for patterns in a format string and print to STDOUT with
/// newline appended, immediately flushing output after. If a single value is
/// passed, the value is returned; if multiple values are passed, they are all
/// returned in a list.
/// Alias: `$$_`
///
/// Expected form:
/// `(println <format string> <values>...)`
///
/// Example:
/// ```text
/// q>> (println "hello {}" "John") ; prints `hello John\n`; evaluates to "John"
/// q>> ; passed values are returned, so `println` can be inserted in the middle
/// q>> ; of other expressions
/// q>> (+ (println "{}" (* 2 3)) 5) ; prints `6\n` and evaluates to 11
/// ```
#[builtin(name = "println-flush", alias = "$$_")]
pub fn fn_println_flush(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.is_empty() {
        return Err(qerr!("println: missing format string"));
    }
    let fmtstr: String = match env.eval(args.get(0).unwrap())? {
        qstr!(s) => Ok(s),
        _ => Err(qerr!("println: first arg must be a format string")),
    }?;
    let vals: Vec<QExp> = env.eval_multi(&args[1..])?;
    let formatted: String
        = fmtstr.format(&vals)
        .map_err(|e| e.prepend_source("println"))?;
    println!("{}", formatted);
    std::io::Write::flush(&mut std::io::stdout()).unwrap();
    return if vals.len() == 1 {
        Ok(vals.into_iter().next().unwrap())
    } else {
        Ok(qlist!(vals))
    };
}

/// Read the contents of one or more files into strs. File paths should be
/// passed as strs in a list or a single str.
/// Alias: `$<`
///
/// Expected form:
/// `(read <file paths>...)`
///
/// Example:
/// ```text
/// q>> ; suppose there are two files:
/// q>> ; /path/to/file1.txt
/// q>> ; ---
/// q>> ; some data here...
/// q>> ;
/// q>> ; /path/to/file2.txt
/// q>> ; ---
/// q>> ; some other data
/// q>> ; here...
/// q>> (read "/path/to/file1.txt") ; evaluates to "some data here...\n"
/// q>> (read ("/path/to/file1.txt" "/path/to/file2.txt"))
/// q>> ; evaluates to ("some data here...\n" "some other data\nhere...\n")
/// ```
#[builtin(name = "read", alias = "$<")]
pub fn fn_read(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    fn do_read(args: &[QExp]) -> QResult<QExp> {
        if args.len() == 1 {
            if let Some(qlist!(l)) = args.first() {
                return do_read(l);
            }
        }
        let filepaths: Vec<&String>
            = args.iter()
            .map(|q| match q {
                qstr!(s) => Ok(s),
                _ => Err(qerr!("read: file paths must be strings")),
            })
            .collect::<QResult<Vec<&String>>>()?;
        let mut bufs: Vec<String>
            = (0..filepaths.len()).map(|_| String::new()).collect();
        for (buf, path) in bufs.iter_mut().zip(filepaths.iter()) {
            fs::OpenOptions::new()
                .read(true)
                .open(path)
                .map_err(|e| qerr_fmt!(
                    "read: cannot open file {}: {}", path, e
                ))?
                .read_to_string(buf)
                .map_err(|e| qerr_fmt!(
                    "read: cannot read file {}: {}", path, e
                ))?;
        }
        return if bufs.len() == 1 {
            Ok(qstr!(bufs.into_iter().next().unwrap()))
        } else {
            Ok(qlist!(bufs.into_iter().map(|s| qstr!(s)).collect()))
        };
    }
    if args.is_empty() {
        return Err(qerr!("read: missing file paths"));
    }
    return do_read(&env.eval_multi(args)?);
}

/// Read the contents of one or more files into a list or lists of lines with
/// newline characters stripped. File paths should be passed as strs in a list
/// or a single str.
/// Alias: `$<:`
///
/// Expected form:
/// `(readlines <file paths>...)`
///
/// Example:
/// ```text
/// q>> ; suppose there are two files:
/// q>> ; /path/to/file1.txt
/// q>> ; ---
/// q>> ; some data here...
/// q>> ;
/// q>> ; /path/to/file2.txt
/// q>> ; ---
/// q>> ; some other data
/// q>> ; here...
/// q>> (readlines "/path/to/file1.txt") ; evaluates to ("some data here...")
/// q>> (readlines ("/path/to/file1.txt" "/path/to/file2.txt"))
/// q>> ; evaluates to (("some data here...") ("some other data") ("here..."))
/// ```
#[builtin(name = "readlines", alias = "$<:")]
pub fn fn_readlines(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    fn do_readlines(args: &[QExp]) -> QResult<QExp> {
        if args.len() == 1 {
            if let Some(qlist!(l)) = args.first() {
                return do_readlines(l);
            }
        }
        let filepaths: Vec<&String>
            = args.iter()
            .map(|q| match q {
                qstr!(s) => Ok(s),
                _ => Err(qerr!("readlines: file paths must be strings")),
            })
            .collect::<QResult<Vec<&String>>>()?;
        let mut file_lines: Vec<Vec<String>> = Vec::new();
        let mut file: File;
        let mut bufreader: BufReader<File>;
        for path in filepaths.iter() {
            file = fs::OpenOptions::new()
                .read(true)
                .open(path)
                .map_err(|e| qerr_fmt!(
                    "readlines: cannot open file {}: {}", path, e
                ))?;
            bufreader = BufReader::new(file);
            file_lines.push(
                bufreader.lines()
                    .collect::<std::io::Result<Vec<String>>>()
                    .map_err(|e| qerr_fmt!(
                        "readlines: cannot read file: {}", e
                    ))?
            );
        }
        return if file_lines.len() == 1 {
            Ok(qlist!(
                file_lines.into_iter().next().unwrap()
                .into_iter()
                .map(|line| qstr!(line))
                .collect()
            ))
        } else {
            Ok(qlist!(
                file_lines.into_iter()
                .map(|lines| qlist!(
                    lines.into_iter()
                    .map(|line| qstr!(line))
                    .collect()
                ))
                .collect()
            ))
        };
    }
    if args.is_empty() {
        return Err(qerr!("readlines: missing file paths"));
    }
    return do_readlines(&env.eval_multi(args)?);
}

/// Open a temporary environment holding a file for writing, erasing its
/// contents if it already exists. File paths are taken to be relative to the
/// current directory if not absolute.
/// Alias: `$|`
///
/// Expected form:
/// `(with-file <file path> (<expressions...>))`
///
/// Example:
/// ```test
/// q>> (def outer 10) ; define a variable in the outer environment
/// q>> ; write to file "path/to/file.txt"
/// q>> (with-file "path/to/file.txt" (
/// ...     (writeln "outer = {}" outer)
/// ...     (def inner 11) ; any expression is legal here
/// ...     (writeln "inner = {}" inner)
/// ... ))
/// q>> ; variables defined in the inner environment do not persist
/// q>> inner ; error!
/// ```
#[builtin(name = "with-file", alias = "$|")]
pub fn fn_with_file(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 2 {
        return Err(qerr_fmt!(
            "with-file: expected 2 args but got {}", args.len()));
    }
    let filepath: String = match env.eval(args.get(0).unwrap())? {
        qstr!(s) => Ok(s),
        _ => Err(qerr!("with-file: file paths must be strings")),
    }?;
    let res: Vec<QExp> = match args.get(1).unwrap() {
        qlist!(l) => {
            let mut writer: QEnv
                = env.sub_env()
                .with_outfile(&filepath, false)
                .map_err(|e| e.prepend_source("with-file-add"))?;
            let res: Vec<QExp> = writer.eval_multi(l)?;
            env.close_outfile()
                .map_err(|e| e.prepend_source("with-file-add"))?;
            Ok(res)
        },
        _ => Err(qerr!(
            "with-file: second arg must be a list of expressions"
        )),
    }?;
    return Ok(qlist!(res));
}

/// Open a temporary environment holding a file for writing, appending to its
/// contents if it already exists. File paths are taken to be relative to the
/// current directory if not absolute.
/// Alias: `$|+`
///
/// Expected form:
/// `(with-file <file path> (<expressions...>))`
///
/// Example:
/// ```text
/// q>> (def outer 10) ; define a variable in the outer environment
/// q>> ; write to file "path/to/file.txt"
/// q>> (with-file-add "path/to/file.txt" (
/// ...     (writeln "outer = {}" outer)
/// ...     (def inner 11) ; any expression is legal here
/// ...     (writeln "inner = {}" inner)
/// ... ))
/// q>> ; variables defined in the inner environment do not persist
/// q>> inner ; error!
/// ```
#[builtin(name = "with-file-add", alias = "$|+")]
pub fn fn_with_file_add(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 2 {
        return Err(qerr_fmt!(
            "with-file-add: expected 2 args but got {}", args.len()));
    }
    let filepath: String = match env.eval(args.get(0).unwrap())? {
        qstr!(s) => Ok(s),
        _ => Err(qerr!("with-file-add: file paths must be strings")),
    }?;
    let res: Vec<QExp> = match args.get(1).unwrap() {
        qlist!(l) => {
            let mut writer: QEnv
                = env.sub_env()
                .with_outfile(&filepath, true)
                .map_err(|e| e.prepend_source("with-file-add"))?;
            let res: Vec<QExp> = writer.eval_multi(l)?;
            env.close_outfile()
                .map_err(|e| e.prepend_source("with-file-add"))?;
            Ok(res)
        },
        _ => Err(qerr!(
            "with-file-add: second arg must be a list of expressions"
        )),
    }?;
    return Ok(qlist!(res));
}

/// Write a formatted string to the current output file held by the environment.
/// Only valid within a `with-file` or `with-file-add` environment.
/// Alias: `$>-`
///
/// Expected form:
/// `(write <format string> <values>...)`
///
/// Example:
/// ```text
/// q>> (def outer 10) ; define a variable in the outer environment
/// q>> ; write to file "path/to/file.txt"
/// q>> (with-file "path/to/file.txt" (
/// ...     (write "outer = {}" outer)
/// ...     (def inner 11) ; any expression is legal here
/// ...     (write "inner = {}" inner)
/// ... ))
/// q>> ; variables defined in the inner environment do not persist
/// q>> inner ; error!
/// ```
#[builtin(name = "write", alias = "$>-")]
pub fn fn_write(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.is_empty() {
        return Err(qerr!("write: missing format string"));
    }
    let fmtstr: String = match env.eval(args.get(0).unwrap())? {
        qstr!(s) => Ok(s),
        _ => Err(qerr!("write: first arg must be a format string")),
    }?;
    let vals: Vec<QExp> = env.eval_multi(&args[1..])?;
    let formatted: String
        = fmtstr.format(&vals)
        .map_err(|e| e.prepend_source("write"))?;
    env.write_to_file(&formatted)
        .map_err(|e| e.prepend_source("write"))?;
    return if vals.len() == 1 {
        Ok(vals.into_iter().next().unwrap())
    } else {
        Ok(qlist!(vals))
    };
}

/// Write a formatted string with newline appended to the current output file
/// held by the environment. Only valid within a `with-file` or `with-file-add`
/// environment.
/// Alias: `$>_`
///
/// Expected form:
/// `(writeln <format string> <values>...)`
///
/// Example:
/// ```text
/// q>> (def outer 10) ; define a variable in the outer environment
/// q>> ; write to file "path/to/file.txt"
/// q>> (with-file "path/to/file.txt" (
/// ...     (writeln "outer = {}" outer)
/// ...     (def inner 11) ; any expression is legal here
/// ...     (writeln "inner = {}" inner)
/// ... ))
/// q>> ; variables defined in the inner environment do not persist
/// q>> inner ; error!
/// ```
#[builtin(name = "writeln", alias = "$>_")]
pub fn fn_writeln(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.is_empty() {
        return Err(qerr!("writeln: missing format string"));
    }
    let fmtstr: String = match env.eval(args.get(0).unwrap())? {
        qstr!(s) => Ok(s),
        _ => Err(qerr!("writeln: first arg must be a format string")),
    }?;
    let vals: Vec<QExp> = env.eval_multi(&args[1..])?;
    let formatted: String
        = fmtstr.format(&vals)
        .map_err(|e| e.prepend_source("writeln"))?;
    env.writeln_to_file(&formatted)
        .map_err(|e| e.prepend_source("writeln"))?;
    return if vals.len() == 1 {
        Ok(vals.into_iter().next().unwrap())
    } else {
        Ok(qlist!(vals))
    };
}

/// Write a formatted string to the current output file held by the environment
/// and immediately flush output. Only valid within a `with-file` or
/// `with-file-add` environment.
/// Alias: `$$>-`
///
/// Expected form:
/// `(write-flush <format string> <values>...)`
///
/// Example:
/// ```text
/// q>> (def outer 10) ; define a variable in the outer environment
/// q>> ; write to file "path/to/file.txt"
/// q>> (with-file "path/to/file.txt" (
/// ...     (write-flush "outer = {}" outer)
/// ...     (def inner 11) ; any expression is legal here
/// ...     (write-flush "inner = {}" inner)
/// ... ))
/// q>> ; variables defined in the inner environment do not persist
/// q>> inner ; error!
/// ```
#[builtin(name = "write-flush", alias = "$$>-")]
pub fn fn_write_flush(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.is_empty() {
        return Err(qerr!("write: missing format string"));
    }
    let fmtstr: String = match env.eval(args.get(0).unwrap())? {
        qstr!(s) => Ok(s),
        _ => Err(qerr!("write: first arg must be a format string")),
    }?;
    let vals: Vec<QExp> = env.eval_multi(&args[1..])?;
    let formatted: String
        = fmtstr.format(&vals)
        .map_err(|e| e.prepend_source("write"))?;
    env.write_to_file(&formatted)
        .map_err(|e| e.prepend_source("write"))?;
    std::io::Write::flush(env.get_outfile_mut().unwrap())
        .map_err(|e| qerr_fmt!("write-flush: cannot flush output {}", e))?;
    return if vals.len() == 1 {
        Ok(vals.into_iter().next().unwrap())
    } else {
        Ok(qlist!(vals))
    };
}

/// Write a formatted string with newline appended to the current output file
/// held by the environment and immediately flush output. Only valid within a
/// `with-file` or `with-file-add` environment.
/// Alias: `$$>_`
///
/// Expected form:
/// `(writeln-flush <format string> <values>...)`
///
/// Example:
/// ```text
/// q>> (def outer 10) ; define a variable in the outer environment
/// q>> ; write to file "path/to/file.txt"
/// q>> (with-file "path/to/file.txt" (
/// ...     (writeln-flush "outer = {}" outer)
/// ...     (def inner 11) ; any expression is legal here
/// ...     (writeln-flush "inner = {}" inner)
/// ... ))
/// q>> ; variables defined in the inner environment do not persist
/// q>> inner ; error!
/// ```
#[builtin(name = "writeln-flush", alias = "$$>_")]
pub fn fn_writeln_flush(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.is_empty() {
        return Err(qerr!("writeln: missing format string"));
    }
    let fmtstr: String = match env.eval(args.get(0).unwrap())? {
        qstr!(s) => Ok(s),
        _ => Err(qerr!("writeln: first arg must be a format string")),
    }?;
    let vals: Vec<QExp> = env.eval_multi(&args[1..])?;
    let formatted: String
        = fmtstr.format(&vals)
        .map_err(|e| e.prepend_source("writeln"))?;
    env.writeln_to_file(&formatted)
        .map_err(|e| e.prepend_source("writeln"))?;
    std::io::Write::flush(env.get_outfile_mut().unwrap())
        .map_err(|e| qerr_fmt!("writeln-flush: cannot flush output: {}", e))?;
    return if vals.len() == 1 {
        Ok(vals.into_iter().next().unwrap())
    } else {
        Ok(qlist!(vals))
    };
}

/// Substitute values for patterns in a format string and return the result as
/// an error.
/// Alias: `!!`
///
/// Expected form:
/// `(halt <format string> <values>...)`
///
/// Example:
/// ```text
/// q>> (defn a -1)
/// q>> (if (> a 0) (* a 2) (halt "expected a positive value, but got {}" a))
/// ```
#[builtin(name = "defn", alias = "!!")]
pub fn fn_halt(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.is_empty() {
        return Err(qerr!("halt"));
    }
    let fmtstr: String = match env.eval(args.get(0).unwrap())? {
        qstr!(s) => Ok(s),
        _ => Err(qerr!("halt: first arg must be a format string")),
    }?;
    let vals: Vec<QExp> = env.eval_multi(&args[1..])?;
    let formatted: String
        = fmtstr.format(&vals)
        .map_err(|e| e.prepend_source("halt"))?;
    return Err(qerr_fmt!("halt: {}", formatted));
}

/// Returns `true` if a value is of a given type or list of types, `false`
/// otherwise. Types must be specified as strs. Available type names are:
/// `"bool"`, `"int"`, `"float"`, `"complex"`, `"list"`, `"str"`, `"function"`,
/// `"any"`.
/// Alias: `~?`
///
/// Expected form:
/// `(istype <type or list of types> <value>)`
///
/// Example:
/// ```text
/// q>> (def z 5i)
/// q>> (istype "complex" z) ; evaluates to true
/// q>> (def reals ("bool" "int" "float"))
/// q>> (if (istype reals z) 0 1) ; evaluates to 1
/// ```
#[builtin(name = "istype", alias = "~?")]
pub fn fn_istype(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 2 {
        return Err(qerr_fmt!(
            "istype: expected 2 args but got {}", args.len()));
    }
    let typespec: Vec<QExpType>
        = match env.eval(args.get(0).unwrap())? {
            qstr!(s) => {
                match s.as_str() {
                    "bool" => Ok(vec![qbool!()]),
                    "int" => Ok(vec![qint!()]),
                    "float" => Ok(vec![qfloat!()]),
                    "complex" => Ok(vec![qcomplex!()]),
                    "list" => Ok(vec![qlist!()]),
                    "str" => Ok(vec![qstr!()]),
                    "function" => Ok(vec![qfunc!(), qlambda!()]),
                    "any" => Ok(vec![qany!()]),
                    _ => Err(qerr_fmt!(
                        "istype: invalid type specification {}", s)),
                }
            },
            qlist!(l) => {
                let ty: Vec<QExpType>
                    = l.iter()
                    .map(|qk| match qk {
                        qstr!(s) => {
                            match s.as_str() {
                                "bool" => Ok(vec![qbool!()]),
                                "int" => Ok(vec![qint!()]),
                                "float" => Ok(vec![qfloat!()]),
                                "complex" => Ok(vec![qcomplex!()]),
                                "list" => Ok(vec![qlist!()]),
                                "str" => Ok(vec![qstr!()]),
                                "function"
                                    => Ok(vec![qfunc!(), qlambda!()]),
                                "any" => Ok(vec![qany!()]),
                                _ => Err(qerr_fmt!(
                                    "istype: invalid type specification {}",
                                    s
                                )),
                            }
                        },
                        _ => Err(qerr_fmt!(
                            "istype: invalid type specification {}", qk)),
                    })
                    .collect::<QResult<Vec<Vec<QExpType>>>>()?
                    .into_iter()
                    .flat_map(|v| v.into_iter())
                    .collect();
                Ok(ty)
            },
            _ => Err(qerr!(
                "istype: first arg must be a str or a list of strs")),
        }?;
    let ret: bool
        = env.eval(args.get(1).unwrap())?
        .is_type_user(&typespec);
    return Ok(qbool!(ret));
}

/// Get the type(s) of one or more values as strs. If one value is passed, the
/// result is returned as a single string, otherwise a list of strings is
/// returned. Possible return values are `"bool"`, `"int"`, `"float"`,
/// `"complex"`, `"list"`, `"str"`, `"function"`.
/// Alias: `?~`
///
/// Expected form:
/// `(type <values>...)`
///
/// Example:
/// ```text
/// q>> (type true) ; evaluates to "bool"
/// q>> (type true 1 1.0 1i) ; evaluates to ("bool" "int" "float" "complex")
/// ```
#[builtin(name = "type", alias = "?~")]
pub fn fn_type(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.is_empty() {
        return Err(qerr_fmt!(
            "type: expected at least 1 arg but got {}", args.len()));
    }
    let vals: Vec<QExp> = env.eval_multi(args)?;
    if vals.len() == 1 {
        return vals[0].exp_type_user()
            .map_err(|e| e.prepend_source("type"));
    } else {
        return Ok(qlist!(
            vals.iter().map(|qk| qk.exp_type_user())
            .collect::<QResult<Vec<QExp>>>()
            .map_err(|e| e.prepend_source("type"))?
        ));
    }
}

/*
 * type-casting
 */

/// Primitive Boolean type. Can be used as a function to cast other values to
/// this type or parse strs as such. If a single list is passed, operates on the
/// contents of the list instead of the list itself.
///
/// Expected form:
/// `(bool <values>...)`
///
/// Example:
/// ```text
/// q>> (bool 0 1.0 1i) ; evaluates to (false true true)
/// q>> (bool "false") ; evaluates to false
/// ```
#[builtin(name = "bool")]
pub fn fn_bool(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    return typecast(qbool!(), &env.eval_multi(args)?);
}

/// Signed 64-bit integer type. Can be used as a function to cast other values
/// to this type or parse strs as such. If a single list is passed, operates on
/// the contents of the list instead of the list itself.
///
/// Expected form:
/// `(int <values>...)`
///
/// Example:
/// ```text
/// q>> (int true false 5.5) ; evaluates to (1 0 5)
/// q>> (int "8") ; evaluates to 8
/// ```
#[builtin(name = "int")]
pub fn fn_int(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    return typecast(qint!(), &env.eval_multi(args)?);
}

/// 64-bit floating-point type. Can be used as a function to cast other values
/// to this type or parse strs as such. If a single list is passed, operates on
/// the contents of the list instead of the list itself.
///
/// Expected form:
/// `(float <values>...)`
///
/// Example:
/// ```text
/// q>> (float true 5) ; evaluates to (0.0 5.0)
/// q>> (float "3.14159") ; evaluates to 3.14159
/// ```
#[builtin(name = "float")]
pub fn fn_float(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    return typecast(qfloat!(), &env.eval_multi(args)?);
}

/// 128-bit complex floating-point type. Can be used as a function to cast other
/// values to this type or parse strs as such. If a single list is passed,
/// operates on the contents of the list instead of the list itself.
///
/// Expected form:
/// `(complex <values>...)`
///
/// Example:
/// ```text
/// q>> (complex true 5 5.0) ; evaluates to (1.0+0i 5.0+0i 5.0+0i)
/// q>> (complex "2+8i") ; evaluates to 2.0+8.0i
/// ```
#[builtin(name = "complex")]
pub fn fn_complex(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    return typecast(qcomplex!(), &env.eval_multi(args)?);
}

/// List type. Can be used as a function to pack all arguments into a list.
///
/// Expected form:
/// `(list <values>...)`
///
/// Example:
/// q>> (list true 5 5.0 5i "hello") ; evaluates to (true 5 5.0 5i "hello")
#[builtin(name = "list")]
pub fn fn_list(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    return Ok(qlist!(env.eval_multi(args)?));
}

/// String type. Can be used as a function to cast all arguments to strs using a
/// default formatter.
///
/// Expected form:
/// `(str <values>...)`
///
/// Example:
/// ```text
/// q>> (str 5.0) ; evaluates to "5"
/// q>> (str "hello" 1.0546 10i) ; evaluates to ("hello" "1.0546" "0+10i")
/// ```
#[builtin(name = "str")]
pub fn fn_str(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    return typecast(qstr!(), &env.eval_multi(args)?);
}

/*
 * arithmetic
 */

/// Addition operation. Supports any number of arguments, retuning the total
/// sum. If no arguments are passed, returns 0. The type of the returned value
/// is the most general of all argument types. If only a single list is passed,
/// this function is applied to its contents.
/// Alias: `+`
///
/// Expected form:
/// `(add <numbers>...)`
///
/// Example:
/// ```text
/// q>> (add 5 5 5) ; evaluates to 15
/// q>> (add 5 5 5.0) ; evaluates to 15.0
/// q>> (add 5 5.0 5.0+0i) ; evaluates to 15.0+0i
/// ```
#[builtin(name = "add", alias = "+")]
pub fn fn_add(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    fn do_add(args: &[QExp]) -> QResult<QExp> {
        if args.len() == 1 {
            if let Some(qlist!(l)) = args.first() {
                return do_add(l);
            }
        }
        let (nums, ret_type): (Vec<QExp>, QExpType)
            = convert_numbers_sametype(args)?;
        let mut acc = QExp::zero(ret_type)?;
        for x in nums.iter() {
            acc = acc.add(x)?;
        }
        return Ok(acc);
    }
    return do_add(&env.eval_multi(args)?);
}

/// Subtraction operation. Requires at least one argument, subtracting all
/// following arguments from the first. The type of the returned value is the
/// most general of all argument types. If only a single list is passed, this
/// function is applied to its contents.
/// Alias: `-`
///
/// Expected form:
/// `(sub <numbers>...)`
///
/// Example:
/// ```text
/// q>> (sub 5 5 5) ; evaluates to -10
/// q>> (sub 5 5 5.0) ; evaluates to -10.0
/// q>> (sub 5 5.0 5.0+0i) ; evaluates to -10.0+0i
/// ```
#[builtin(name = "sub", alias = "-")]
pub fn fn_sub(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    fn do_sub(args: &[QExp]) -> QResult<QExp> {
        if args.len() == 1 {
            if let Some(qlist!(l)) = args.first() {
                return do_sub(l);
            }
        }
        let (nums, _): (Vec<QExp>, _)
            = convert_numbers_sametype(args)?;
        if nums.len() == 1 {
            return nums.first().unwrap().neg();
        }
        let mut acc: QExp
            = nums.first()
            .ok_or_else(|| qerr!("expected at least one number"))?
            .clone();
        for x in nums[1..].iter() {
            acc = acc.sub(x)?;
        }
        return Ok(acc);
    }
    return do_sub(&env.eval_multi(args)?);
}

/// Multiplication operation. Supports any number of arguments, returning the
/// total product. If no arguments are passed, returns 1. The type of the
/// returned value is the most general of all argument types. If only a single
/// list is passed, this function is applied to its contents.
/// Alias: `*`
///
/// Expected form:
/// `(mul <numbers>...)`
///
/// Example:
/// ```text
/// q>> (mul 5 5 5) ; evaluates to 125
/// q>> (mul 5 5 5.0) ; evaluates to 125.0
/// q>> (mul 5 5.0 5.0+0i) ; evaluates to 125.0+0i
/// ```
#[builtin(name = "mul", alias = "*")]
pub fn fn_mul(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    fn do_mul(args: &[QExp]) -> QResult<QExp> {
        if args.len() == 1 {
            if let Some(qlist!(l)) = args.first() {
                return do_mul(l);
            }
        }
        let (nums, ret_type): (Vec<QExp>, QExpType)
            = convert_numbers_sametype(args)?;
        let mut acc = QExp::one(ret_type)?;
        for x in nums.iter() {
            acc = acc.mul(x)?;
        }
        return Ok(acc);
    }
    return do_mul(&env.eval_multi(args)?);
}

/// Division operation. Requires at least one argument, dividing the first by
/// all following arguments. The type of the returned value is either a float or
/// a complex, depending on the types of the arguments. If only a single list is
/// passed, this function is applied to its contents.
/// Alias: `/`
///
/// Expected form:
/// `(div <numbers>...)`
///
/// Example:
/// ```text
/// q>> (div 5 5 5) ; evaluates to 0.2
/// q>> (div 5 5 5.0) ; evaluates to 0.2
/// q>> (div 5 5.0 5.0+0i) ; evaluates to 0.2+0i
/// ```
#[builtin(name = "div", alias = "/")]
pub fn fn_div(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    fn do_div(args: &[QExp]) -> QResult<QExp> {
        if args.len() == 1 {
            if let Some(qlist!(l)) = args.first() {
                return do_div(l);
            }
        }
        let (nums, mut ret_type): (Vec<QExp>, QExpType)
            = convert_numbers_sametype(args)?;
        ret_type = cmp::max(ret_type, qfloat!());
        if nums.len() == 1 {
            return QExp::one(ret_type)?.div(nums.first().unwrap());
        }
        let mut acc: QExp
            = nums.first()
            .ok_or_else(|| qerr!("expected at least one number"))?
            .clone();
        for x in nums[1..].iter() {
            acc = acc.div(x)?;
        }
        return Ok(acc);
    }
    return do_div(&env.eval_multi(args)?);
}

/// Integer division operation, where a floor operator is applied after each
/// pairwise operation. Requires at least one argument, integer-dividing the
/// first by all following arguments. Does not accept complex-valued arguments.
/// The type of the returned value is always an int. If only a single list is
/// passed, this function is applied to its contents.
/// Alias: `//`
///
/// Expected form:
/// `(idiv <numbers>...)`
///
/// Example:
/// ```text
/// q>> (idiv 5 2) ; evaluates to 2
/// q>> (idiv -5 2.0) ; evaluates to -3
/// q>> (idiv 5 2.0) ; evaluates to 2
/// q>> (idiv 5 2.0 2.2) ; evaluates to 0
/// ```
#[builtin(name = "idiv", alias = "//")]
pub fn fn_idiv(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    fn do_idiv(args: &[QExp]) -> QResult<QExp> {
        if args.len() == 1 {
            if let Some(qlist!(l)) = args.first() {
                return do_idiv(l);
            }
        }
        let (nums, mut div_type): (Vec<QExp>, QExpType)
            = convert_numbers_sametype(args)?;
        div_type = cmp::max(div_type, qfloat!());
        if div_type > qfloat!() {
            return Err(qerr!(
                "cannot do floor division with complex numbers"));
        }
        if nums.len() == 1 {
            return QExp::zero(qint!());
        }
        let mut acc: QExp
            = nums.first()
            .ok_or_else(|| qerr!("expected at least one number"))?
            .clone();
        for x in nums[1..].iter() {
            acc = acc.idiv(x)?;
        }
        return Ok(acc);
    }
    return do_idiv(&env.eval_multi(args)?);
}

/*
 * boolean comparisons
 */

/// Logical AND operator, short-circuited and extended to n >= 0 inputs. Returns
/// `true` if all inputs are booleans with positive value or if there are no
/// inputs.
/// Alias: `&&`
///
/// Expected form:
/// `(and <inputs>...)`
///
/// Example:
/// ```text
/// q>> (and true true true) ; evaluates to true
/// q>> (and true true false) ; evaluates to false
/// q>> (and) ; evaluates to true
/// ```
#[builtin(name = "and", alias = "&&")]
pub fn fn_and(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    for form in args.iter() {
        match env.eval(form)? {
            qbool!(b) => {
                if !b { return Ok(qbool!(false)) } else { continue; }
            },
            _ => { return Err(qerr!("and: encountered non-boolean")); },
        }
    }
    return Ok(qbool!(true));
}

/// Logical OR operator, short-circuited and extended to n >= 0 inputs. Returns
/// `true` if all evaluated inputs are booleans and at least one of them has
/// positive value.
/// Alias: `||`
///
/// Expected form:
/// `(or <inputs>...)`
///
/// Example:
/// ```text
/// q>> (or true true true) ; evaluates to true
/// q>> (or true false false) ; evaluates to true
/// q>> (or) ; evaluates to false
/// ```
#[builtin(name = "or", alias = "||")]
pub fn fn_or(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    for form in args.iter() {
        match env.eval(form)? {
            qbool!(b) => {
                if b { return Ok(qbool!(true)) } else { continue; }
            },
            _ => { return Err(qerr!("or: encountered non-boolean")); },
        }
    }
    return Ok(qbool!(false));
}

/// Logical XOR operator, short-circuited and extended to n >= 0 inputs. Returns
/// `true` if exactly one input is `true`, `false` otherwise.
/// Alias: `^`
///
/// Expected form:
/// `(xor <inputs>...)`
///
/// Example:
/// ```text
/// q>> (xor true false false) ; evaluates to true
/// q>> (xor true true false) ; evaluates to false
/// q>> (xor) ; evaluates to false
/// ```
#[builtin(name = "xor", alias = "^")]
pub fn fn_xor(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    let mut has_true: bool = false;
    for form in args.iter() {
        match env.eval(form)? {
            qbool!(b) => {
                if has_true && b {
                    return Ok(qbool!(false));
                } else {
                    has_true = b;
                }
            },
            _ => { return Err(qerr!("xor: encountered non-boolean")); },
        }
    }
    return Ok(qbool!(has_true));
}

/// Equality comparison operator. Not to be confused with the `def`/`:=`
/// assignment keyword. Returns `false` if there exists at least 1 argument that
/// is not equal to the rest. Returns `true` if no arguments are passed. If only
/// a single list is passed, operates on the contents of the list instead of the
/// list itself. Note that two numbers of different types (e.g. 5 and 5.0) are
/// *not* considered equal, even if their numerical values are the same. To
/// perform such a comparison, cast the values to the same type, e.g.
/// `(= (float 5 5.0))`.
/// Alias: `=`
///
/// Expected form:
/// `(eq <args>...)`
///
/// Example:
/// ```text
/// q>> (def a 5)
/// q>> (eq a 5) ; evaluates to true
/// q>> (eq a 5 5.0) ; evaluates to false
/// q>> (eq (float a 5 5.0)) ; evaluates to true
/// ```
#[builtin(name = "eq", alias = "=")]
pub fn fn_eq(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    fn do_eq(args: &[QExp]) -> QResult<QExp> {
        if args.len() == 1 {
            if let Some(qlist!(l)) = args.first() {
                return do_eq(l);
            }
        }
        let first: &QExp;
        if let Some(x) = args.first() {
            first = x;
        } else {
            return Ok(qbool!(true));
        }
        for x in args[1..].iter() {
            if !first.eq(x) {
                return Ok(qbool!(false));
            }
        }
        return Ok(qbool!(true));
    }
    return do_eq(&env.eval_multi(args)?);
}

/// Non-equality comparison operator. Returns `false` if at least two arguments
/// are equal. Returns `true` if no arguments are passed. If only a single list
/// is passed, operates on the contents of the list instead of the list itself.
/// Note that two numbers of different types (e.g. 5 and 5.0) are *not*
/// considered equal, even if their numerical values are the same. To perform
/// such a comparison, cast the values to the same type, e.g.
/// `(!= (float 5 5.0))`.
/// Alias: `!=`
///
/// Expected form:
/// `(neq <args>...)`
///
/// Example:
/// ```text
/// q>> (def a 5)
/// q>> (neq a 5) ; evaluates to false
/// q>> (neq a 5.0) ; evaluates to true
/// q>> (neq (float a 5.0)) ; evaluates to false
/// ```
#[builtin(name = "neq", alias = "!=")]
pub fn fn_neq(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    fn do_neq(args: &[QExp]) -> QResult<QExp> {
        if args.len() == 1 {
            if let Some(qlist!(l)) = args.first() {
                return do_neq(l);
            }
        }
        let first: &QExp;
        if let Some(x) = args.first() {
            first = x;
        } else {
            return Ok(qbool!(true));
        }
        for x in args[1..].iter() {
            if !first.nequals(x) {
                return Ok(qbool!(false));
            }
        }
        return Ok(qbool!(true));
    }
    return do_neq(&env.eval_multi(args)?);
}

/// Greater-than comparison operator. Returns `true` if the arguments are in
/// monotonic, descending order, i.e. every element is greater than the one
/// following it. Returns `true` if no arguments are passed. If only a single
/// list is passed, operates on the contents of the list instead of the list
/// itself.
/// Alias: `>`
///
/// Expected form:
/// `(gt <args>...)`
///
/// Example:
/// ```text
/// q>> (gt 5 4) ; evaluates to true
/// q>> (gt 5 5 4) ; evaluates to false
/// q>> (gt "c" "b" "a") ; evaluates to true
/// ```
#[builtin(name = "gt", alias = ">")]
pub fn fn_gt(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    fn do_gt(args: &[QExp]) -> QResult<QExp> {
        if args.len() == 1 {
            if let Some(qlist!(l)) = args.first() {
                return do_gt(l);
            }
        }
        let mut last: &QExp;
        if let Some(x) = args.first() {
            last = x;
        } else {
            return Ok(qbool!(true));
        }
        for x in args[1..].iter() {
            if !last.gt(x)? {
                return Ok(qbool!(false));
            }
            last = x;
        }
        return Ok(qbool!(true));
    }
    return do_gt(&env.eval_multi(args)?);
}

/// Greater-than-or-equal-to comparison operator. Returns `true` if the
/// arguments are in non-increasing order, i.e. there exists no element that is
/// less than the one following it. Returns `true` if no arguments are passed.
/// If only a single list is passed, operates on the contents of the list
/// instead of the list itself.
/// Alias: `>=`
///
/// Expected form:
/// `(geq <args>...)`
///
/// Example:
/// ```text
/// q>> (geq 5 4) ; evaluates to true
/// q>> (geq 5 5 4) ; evaluates to true
/// q>> (geq "c" "b" "a") ; evaluates to true
/// ```
#[builtin(name = "geq", alias = ">=")]
pub fn fn_geq(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    fn do_geq(args: &[QExp]) -> QResult<QExp> {
        if args.len() == 1 {
            if let Some(qlist!(l)) = args.first() {
                return do_geq(l);
            }
        }
        let mut last: &QExp;
        if let Some(x) = args.first() {
            last = x;
        } else {
            return Ok(qbool!(true));
        }
        for x in args[1..].iter() {
            if !last.ge(x)? {
                return Ok(qbool!(false));
            }
            last = x;
        }
        return Ok(qbool!(true));
    }
    return do_geq(&env.eval_multi(args)?);
}

/// Less-than comparison operator. Returns `true` if the arguments are in
/// monotonic, ascending order, i.e. every element is less than the one
/// following it. Returns `true` if no arguments are passed. If only a single
/// list is passed, operates on the contents of the list instead of the list
/// itself.
/// Alias: `<`
///
/// Expected form:
/// `(lt <args>...)`
///
/// Example:
/// ```text
/// q>> (gt 5 6) ; evaluates to true
/// q>> (gt 5 5 6) ; evaluates to false
/// q>> (gt "a" "b" "c") ; evaluates to true
/// ```
#[builtin(name = "lt", alias = "<")]
pub fn fn_lt(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    fn do_lt(args: &[QExp]) -> QResult<QExp> {
        if args.len() == 1 {
            if let Some(qlist!(l)) = args.first() {
                return do_lt(l);
            }
        }
        let mut last: &QExp;
        if let Some(x) = args.first() {
            last = x;
        } else {
            return Ok(qbool!(true));
        }
        for x in args[1..].iter() {
            if !last.lt(x)? {
                return Ok(qbool!(false));
            }
            last = x;
        }
        return Ok(qbool!(true));
    }
    return do_lt(&env.eval_multi(args)?);
}

/// Less-than-or-equal-to comparison operator. Returns `true` if the arguments
/// are in non-decreasing order, i.e. there exists no element that is greater
/// than the one following it. Returns `true` if no arguments are passed. If
/// only a single list is passed, operates on the contents of the list instead
/// of the list itself.
/// Alias: `<=`
///
/// Expected form:
/// `(leq <args>...)`
///
/// Example:
/// ```text
/// q>> (leq 5 6) ; evaluates to true
/// q>> (leq 5 5 6) ; evaluates to true
/// q>> (leq "a" "b" "c") ; evaluates to true
/// ```
#[builtin(name = "leq", alias = "<=")]
pub fn fn_leq(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    fn do_leq(args: &[QExp]) -> QResult<QExp> {
        if args.len() == 1 {
            if let Some(qlist!(l)) = args.first() {
                return do_leq(l);
            }
        }
        let mut last: &QExp;
        if let Some(x) = args.first() {
            last = x;
        } else {
            return Ok(qbool!(true));
        }
        for x in args[1..].iter() {
            if !last.le(x)? {
                return Ok(qbool!(false));
            }
            last = x;
        }
        return Ok(qbool!(true));
    }
    return do_leq(&env.eval_multi(args)?);
}

/*
 * boolean accumulators
 */

/// Logical AND accumulator on a list input: Returns `true` if all items in a
/// list are `true`, `false` otherwise.
/// Alias: `&&*`
///
/// Expected form:
/// `(all (<values>...))`
///
/// Example:
/// ```text
/// q>> (def numbers (range 0 5)) ; (0 1 2 3 4)
/// q>> (all (map (fn (n) (< n 10)) numbers)) ; evaluates to true
/// q>> (all (map (fn (n) (< n 2)) numbers)) ; evaluates to false
/// ```
#[builtin(name = "all", alias = "&&*")]
pub fn fn_all(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 1 {
        return Err(qerr_fmt!(
            "all: expected 1 arg but got {}", args.len()));
    }
    return match env.eval(args.first().unwrap())? {
        qlist!(l) => {
            if l.is_empty() { return Ok(qbool!(false)); }
            for qk in l.iter() {
                if let qbool!(b) = qk {
                    if !b { return Ok(qbool!(false)); }
                } else {
                    return Err(qerr!("all: encountered non-boolean"));
                }
            }
            Ok(qbool!(true))
        },
        _ => Err(qerr!("all: arg must be a list")),
    };
}

/// Logical OR accumulator on a list input: Returns `true` if at least one item
/// in a list is `true`, `false` otherwise.
/// Alias: `||*`
///
/// Expected form:
/// `(any (<values>...))`
///
/// Example:
/// ```text
/// q>> (def numbers (range 0 5)) ; (0 1 2 3 4)
/// q>> (any (map (fn (n) (> n 10)) numbers)) ; evaluates to false
/// q>> (any (map (fn (n) (> n 2)) numbers)) ; evaluates to true
/// ```
#[builtin(name = "any", alias = "||*")]
pub fn fn_any(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 1 {
        return Err(qerr_fmt!(
            "any: expected 1 arg but got {}", args.len()));
    }
    return match env.eval(args.first().unwrap())? {
        qlist!(l) => {
            if l.is_empty() { return Ok(qbool!(false)); }
            for qk in l.iter() {
                if let qbool!(b) = qk {
                    if *b { return Ok(qbool!(true)); }
                } else {
                    return Err(qerr!("any: encountered non-boolean"));
                }
            }
            Ok(qbool!(false))
        },
        _ => Err(qerr!("any: arg must be a list")),
    };
}

/// Logical XOR accumulator on a list input: Returns `true` if exactly one item
/// in a list is `true`, `false` otherwise.
/// Alias: `^*`
///
/// Expected form:
/// `(xany (<values>...))`
///
/// Example:
/// ```text
/// q>> (def numbers (range 0 5)) ; (0 1 2 3 4)
/// q>> (xany (map (fn (n) (> n 2)) numbers)) ; evaluates to false
/// q>> (xany (map (fn (n) (= n 2)) numbers)) ; evaluates to true
/// ```
#[builtin(name = "xany", alias = "^*")]
pub fn fn_xany(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() > 1 {
        return Err(qerr_fmt!(
            "xany: expected 1 arg but got {}", args.len()));
    }
    return match env.eval(args.first().unwrap())? {
        qlist!(l) => {
            if l.is_empty() { return Ok(qbool!(false)); }
            let mut has_true: bool = false;
            for qk in l.iter() {
                if let qbool!(b) = qk {
                    if has_true && *b {
                        return Ok(qbool!(false));
                    } else {
                        has_true = *b;
                    }
                } else {
                    return Err(qerr!("xany: encountered non-boolean"));
                }
            }
            Ok(qbool!(has_true))
        },
        _ => Err(qerr!("xany: arg must be a list")),
    };
}

/*
 * iterable creation
 */

/// Construct a list of integers on a semi-open interval.
/// Alias: `..`
///
/// Expected form:
/// `(range <start> <stop>)`
///
/// Example:
/// ```text
/// q>> (range 5 15) ; evaluates to (5 6 7 8 9 10 11 12 13 14)
/// q>> (range 5 -5) ; evaluates to (5 4 3 2 1 0 -1 -2 -3 -4)
/// ```
#[builtin(name = "range", alias = "..")]
pub fn fn_range(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 2 {
        return Err(qerr_fmt!(
            "range: expected 2 args but got {}", args.len()));
    }
    let start: i64 = match env.eval(args.get(0).unwrap())? {
        qint!(i) => Ok(i),
        _ => Err(qerr!("range: start must be an int")),
    }?;
    let stop: i64 = match env.eval(args.get(1).unwrap())? {
        qint!(i) => Ok(i),
        _ => Err(qerr!("range: stop must be an int")),
    }?;
    return if stop >= start {
        Ok(qlist!((start..stop).map(|v| qint!(v)).collect()))
    } else {
        Ok(qlist!((stop + 1..start + 1).rev().map(|v| qint!(v)).collect()))
    };
}

/// Construct a list of integers on a closed interval.
/// Alias: `..=`
///
/// Expected form:
/// `(range-inc <start> <stop>)`
///
/// Example:
/// ```text
/// q>> (range-inc 5 15) ; evaluates to (5 6 7 8 9 10 11 12 13 14 15)
/// q>> (range-inc 5 -5) ; evaluates to (5 4 3 2 1 0 -1 -2 -3 -4 -5)
/// ```
#[builtin(name = "range-inc", alias = "..=")]
pub fn fn_range_inc(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 2 {
        return Err(qerr_fmt!(
            "range-inc: expected 2 args but got {}", args.len()));
    }
    let start: i64 = match env.eval(args.get(0).unwrap())? {
        qint!(i) => Ok(i),
        _ => Err(qerr!("range-inc: start must be an int")),
    }?;
    let stop: i64 = match env.eval(args.get(1).unwrap())? {
        qint!(i) => Ok(i),
        _ => Err(qerr!("range-inc: stop must be an int")),
    }?;
    return if stop >= start {
        Ok(qlist!((start..=stop).map(|v| qint!(v)).collect()))
    } else {
        Ok(qlist!((stop..=start).rev().map(|v| qint!(v)).collect()))
    };
}

/// Construct a new list or str by repeating the contents of another `n` times.
/// Alias: `#=`
///
/// Expected form:
/// `(repeat <n> <list or str>)`
///
/// Example:
/// ```text
/// q>> (repeat 3 (1 2 3)) ; evaluates to (1 2 3 1 2 3 1 2 3)
/// q>> (repeat 2 "hello") ; evaluates to "hellohello"
/// ```
#[builtin(name = "repeat", alias = "#=")]
pub fn fn_repeat(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 2 {
        return Err(qerr_fmt!(
            "repeat: expected 2 args but got {}", args.len()));
    }
    let n: usize = match env.eval(args.get(0).unwrap())? {
        qint!(i) => {
            if i >= 0 {
                Ok(i as usize)
            } else {
                Err(qerr!("repeat: number must be at least zero"))
            }
        },
        _ => Err(qerr!("repeat: first arg must be an int")),
    }?;
    let idxable
        = Indexable::from_qexp(env.eval(args.get(1).unwrap())?)
        .map_err(|_| qerr!("repeat: second arg must be a list or str"))?;
    return idxable.repeat(n);
}

/*
 * iterable accumulation
 */

/// Return the number of items in a list or characters in a str.
/// Alias: `#`
///
/// Expected form:
/// `(length <list or str>)`
///
/// Example:
/// ```text
/// q>> (length (range 0 10)) ; evaluates to 10
/// q>> (length "hello") ; evaluates to 5
/// ```
#[builtin(name = "length", alias = "#")]
pub fn fn_length(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 1 {
        return Err(qerr_fmt!(
            "length: expected 1 arg but got {}", args.len()));
    }
    return match env.eval(args.first().unwrap())? {
        qlist!(l) => Ok(qint!(l.len() as i64)),
        qstr!(s) => Ok(qint!(s.len() as i64)),
        _ => Err(qerr!("length: arg must be a list or str")),
    };
}

/// Iterate over a list or str, folding each element into an accumulator by
/// applying a function. The function should take two arguments with the first
/// being the accumulator.
/// Alias: `@.`
///
/// Expected form:
/// `(fold <start> <function> <list or str>)`
///
/// Example:
/// ```text
/// q>> (defn f (acc x) (if (= (% 2 x) 0) (+ acc x) (* acc x)))
/// q>> (fold 1 f (range 0 4)) ; evaluates to 9
/// ```
#[builtin(name = "fold", alias = "@.")]
pub fn fn_fold(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 3 {
        return Err(qerr_fmt!(
            "fold: expected 3 args but got {}", args.len()));
    }
    let start: QExp = env.eval(args.get(0).unwrap())?;
    let idxable
        = Indexable::from_qexp(env.eval(args.get(2).unwrap())?)
        .map_err(|_| qerr!("fold: third arg must be a list or str"))?;
    return match env.eval(args.get(1).unwrap())? {
        qfunc!(f) => idxable.fold(&start, |args: &[QExp]| (f.f)(env, args)),
        qlambda!(ll) => {
            let f = |args: &[QExp]| {
                let mut ll_env: QEnv = ll.env(args, env)?;
                ll_env.eval(&ll.body_exp)
            };
            idxable.fold(&start, f)
        },
        _ => Err(qerr!("fold: second arg must be a function")),
    };
}

/// Find the minimum of a list or str using the `<` function.
/// Alias: `<:`
///
/// Expected form:
/// `(min <list or str>)`
///
/// Example:
/// ```text
/// q>> (min (range 5 -5)) ; evaluates to -4
/// q>> (min "hello world") ; evaluates to " "
/// ```
#[builtin(name = "min", alias = "<:")]
pub fn fn_min(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 1 {
        return Err(qerr_fmt!(
            "min: expected 1 arg but got {}", args.len()));
    }
    let idxable
        = Indexable::from_qexp(env.eval(args.get(0).unwrap())?)
        .map_err(|e| e.prepend_source("min"))?;
    return idxable.min()
        .map_err(|e| e.prepend_source("min"));
}

/// Find the maximum of a list or str using the `>` function.
/// Alias: `:>`
///
/// Expected form:
/// `(max <list or str>)`
///
/// Example:
/// ```text
/// q>> (max (range 5 -5)) ; evaluates to 5
/// q>> (max "hello world") ; evaluates to "w"
/// ```
#[builtin(name = "max", alias = ":>")]
pub fn fn_max(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 1 {
        return Err(qerr_fmt!(
            "max: expected 1 arg but got {}", args.len()));
    }
    let idxable
        = Indexable::from_qexp(env.eval(args.get(0).unwrap())?)
        .map_err(|e| e.prepend_source("max"))?;
    return idxable.max()
        .map_err(|e| e.prepend_source("max"));
}

/// Select an extremum element through element-wise comparisons with an
/// accumulator using a comparison function. The function should take two
/// arguments, with the first being the trial element and the second being the
/// accumulator, and return `true` if the trial element is to replace the
/// accumulator, `false` otherwise. The initial value of the accumulator is set
/// to be the first element of the list or str. To set a different initial
/// value, use `fold` instead.
/// Alias: `*@.`
///
/// Expected form:
/// `(select-by <comparison function> <list or str>)`
///
/// Example:
/// ```text
/// q>> (select-by < (range 5 -5)) ; evaluates -4
/// q>> ; find the maximum number in a list divisible by 3
/// q>> (defn maxdiv3 (x acc) (and (= (mod 3 x) 0) (> x acc)))
/// q>> (select-by maxdiv3 (0 5 3 7 5 6 6 9 7 3 5 12 3 10)) ; evaluates to 12
/// ```
#[builtin(name = "select-by", alias = "*@.")]
pub fn fn_select_by(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
   if args.len() != 2 {
       return Err(qerr_fmt!(
            "select-by: expected 2 args but got {}", args.len()));
   }
   let idxable
       = Indexable::from_qexp(env.eval(args.get(1).unwrap())?)
       .map_err(|_| qerr!("select-by: second arg must be a list or str"))?;
    return match env.eval(args.get(0).unwrap())? {
        qfunc!(f) => {
            idxable.select_by(|args: &[QExp]| (f.f)(env, args))
                .map_err(|e| e.prepend_source("select-by"))
        },
        qlambda!(ll) => {
            let f = |args: &[QExp]| {
                let mut ll_env: QEnv = ll.env(args, env)?;
                ll_env.eval(&ll.body_exp)
            };
            idxable.select_by(f)
                .map_err(|e| e.prepend_source("select-by"))
        },
        _ => Err(qerr!("select-by: second arg must be a function")),
   };
}

/*
 * iterable slicing and access
 */

/// Return the item of a list or character of a str at an index. Indicies must
/// be non-negative integers.
/// Alias: `.`
///
/// Expected form:
/// `(get <index> <list or str>)`
///
/// Example:
/// ```text
/// q>> (get 3 (range-inc 1 10)) ; evaluates to 4
/// q>> (get 4 "hello world") ; evaluates to "o"
/// ```
#[builtin(name = "get", alias = ".")]
pub fn fn_get(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 2 {
        return Err(qerr_fmt!(
            "get: expected 2 args but got {}", args.len()));
    }
    let idx: i64 = match env.eval(args.get(0).unwrap())? {
        qint!(i) => Ok(i),
        _ => Err(qerr!("get: first arg must be an int")),
    }?;
    if idx < 0 {
        return Err(qerr!("get: index must be non-negative"));
    }
    let idxable
        = Indexable::from_qexp(env.eval(args.get(1).unwrap())?)
        .map_err(|_| qerr!("get: second arg must be a list or str"))?;
    return idxable.get(idx.unsigned_abs() as usize)
        .ok_or_else(|| qerr_fmt!(
            "get: index {} out of bounds for object of length {}",
            idx, idxable.len()
        ));
}

/// Return a copy of a list where values at given indices have been set to new
/// ones. Each argument following the list must be a list of length 2 where the
/// first item is the index and the second is the value.
/// Alias: `.:=`
///
/// Expected form:
/// `(set <list> (<index> <value>)...)`
///
/// Example:
/// ```text
/// q>> (set (range 0 10) (0 3) (7 10)) ; evaluates to (3 1 2 3 4 5 6 10 8 9)
/// ```
#[builtin(name = "set", alias = ".:=")]
pub fn fn_set(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() < 2 {
        return Err(qerr_fmt!(
            "set: expected at least 2 args but got {}", args.len()));
    }
    let vals: Vec<(usize, QExp)>
        = env.eval_multi(&args[1..])?
        .into_iter()
        .map(|qk| match qk {
            qlist!(l) => {
                if l.len() == 2 {
                    let mut l_ = l.into_iter();
                    match (l_.next().unwrap(), l_.next().unwrap()) {
                        (qint!(k), q) => {
                            if k < 0 {
                                Err(qerr_fmt!("set: invalid index {}", k))
                            } else {
                                Ok((k as usize, q))
                            }
                        },
                        _ => Err(qerr!("set: invalid arg form")),
                    }
                } else {
                    Err(qerr!("set: args must be lists of length 2"))
                }
            }
            _ => Err(qerr!("set: args must be lists of length 2")),
        })
        .collect::<QResult<Vec<(usize, QExp)>>>()?;
    let idxable
        = Indexable::from_qexp_list(env.eval(args.get(0).unwrap())?)
        .map_err(|_| qerr!("set: first arg must be a list"))?;
    return idxable.set(&vals)
        .map_err(|e| e.prepend_source("set"));
}

/// Return a slice of a list or substring of a str over a semi-open range of
/// indices.
/// Alias: `--`
///
/// Expected form:
/// `(slice <start> <stop> <list or str>)`
///
/// Example:
/// ```text
/// q>> (slice 2 7 (range 10 0)) ; evaluates to (8 7 6 5 4)
/// q>> (slice 2 7 "hello world") ; evaluates to "llo w"
/// ```
#[builtin(name = "slice", alias = "--")]
pub fn fn_slice(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 3 {
        return Err(qerr_fmt!(
            "slice: expected 3 args but got {}", args.len()));
    }
    let beg: i64 = match env.eval(args.get(0).unwrap())? {
        qint!(i) => Ok(i),
        _ => Err(qerr!("slice: first arg must be an int")),
    }?;
    if beg < 0 {
        return Err(qerr!("slice: start index must be non-negative"));
    }
    let end: i64 = match env.eval(args.get(1).unwrap())? {
        qint!(i) => Ok(i),
        _ => Err(qerr!("slice: second arg must be an int")),
    }?;
    if end < 0 {
        return Err(qerr!("slice: stop index must be non-negative"));
    }
    let idxable
        = Indexable::from_qexp(env.eval(args.get(2).unwrap())?)
        .map_err(|_| qerr!("slice: third arg must be a list or str"))?;
    return idxable.slice(
        beg.unsigned_abs() as usize,
        end.unsigned_abs() as usize,
        1
    )
        .map_err(|e| e.prepend_source("slice"));
}

/// Return a slice of a list or substring of a str over a closed range of
/// indices.
/// Alias: `--=`
///
/// Expected form:
/// `(slice-inc <start> <stop> <list or str>)`
///
/// Example:
/// ```text
/// q>> (slice-inc 2 7 (range 10 0)) ; evaluates to (8 7 6 5 4 3)
/// q>> (slice-inc 2 7 "hello world") ; evaluates to "llo wo"
/// ```
#[builtin(name = "slice-inc", alias = "--=")]
pub fn fn_slice_inc(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 3 {
        return Err(qerr_fmt!(
            "slice-inc: expected 3 args but got {}", args.len()));
    }
    let beg: i64 = match env.eval(args.get(0).unwrap())? {
        qint!(i) => Ok(i),
        _ => Err(qerr!("slice-inc: first arg must be an int")),
    }?;
    if beg < 0 {
        return Err(qerr!("slice-inc: start index must be non-negative"));
    }
    let end: i64 = match env.eval(args.get(1).unwrap())? {
        qint!(i) => Ok(i),
        _ => Err(qerr!("slice-inc: second arg must be an int")),
    }?;
    if end < 0 {
        return Err(qerr!("slice-inc: stop index must be non-negative"));
    }
    let idxable
        = Indexable::from_qexp(env.eval(args.get(2).unwrap())?)
        .map_err(|_| qerr!("slice-inc: third arg must be a list or str"))?;
    return idxable.slice_inc(
        beg.unsigned_abs() as usize,
        end.unsigned_abs() as usize,
        1
    )
        .map_err(|e| e.prepend_source("slice-inc"));
}

/// Portmanteau of slice and step-by: Return a slice of a list or substring of a
/// str over a semi-open range of indices with a given step size.
/// Alias: `~~`
///
/// Expected form:
/// `(slice-by <start> <stop> <step size> <list or str>)`
///
/// Example:
/// ```text
/// q>> (slice-by 2 7 2 (range 10 0)) ; evaluates to (8 6 4)
/// q>> (slice-by 2 7 2 "hello world") ; evaluates to "low"
/// ```
#[builtin(name = "slice-by", alias = "~~")]
pub fn fn_slice_by(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 4 {
        return Err(qerr_fmt!(
            "slice-by: expected 4 args but got {}", args.len()));
    }
    let beg: i64 = match env.eval(args.get(0).unwrap())? {
        qint!(i) => Ok(i),
        _ => Err(qerr!("slice-by: first arg must be an int")),
    }?;
    if beg < 0 {
        return Err(qerr!("slice-by: start index must be non-negative"));
    }
    let end: i64 = match env.eval(args.get(1).unwrap())? {
        qint!(i) => Ok(i),
        _ => Err(qerr!("slice-by: second arg must be an int")),
    }?;
    if end < 0 {
        return Err(qerr!("slice-by: stop index must be non-negative"));
    }
    let step: i64 = match env.eval(args.get(2).unwrap())? {
        qint!(i) => Ok(i),
        _ => Err(qerr!("slice-by: step must be an int")),
    }?;
    if step <= 0 {
        return Err(qerr!("slice-by: step must be positibe"));
    }
    let idxable
        = Indexable::from_qexp(env.eval(args.get(3).unwrap())?)
        .map_err(|_| qerr!("slice-by: third arg must be a list or str"))?;
    return idxable.slice(
        beg.unsigned_abs() as usize,
        end.unsigned_abs() as usize,
        step.unsigned_abs() as usize
    )
        .map_err(|e| e.prepend_source("slice-by"));
}

/// Portmanteau of slice-inc and step-by: Return a slice of a list or substring
/// of a str over a closed range of indices with a given step size.
/// Alias: `~~=`
///
/// Expected form:
/// `(slice-inc-by <start> <stop> <step size> <list or str>)`
///
/// Example:
/// ```text
/// q>> (slice-by 2 8 2 (range 10 0)) ; evaluates to (8 6 4 2)
/// q>> (slice-by 2 8 2 "hello world") ; evaluates to "lowr"
/// ```
#[builtin(name = "slice-inc-by", alias = "~~=")]
pub fn fn_slice_inc_by(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 4 {
        return Err(qerr_fmt!(
            "slice-inc-by: expected 4 args but got {}", args.len()));
    }
    let beg: i64 = match env.eval(args.get(0).unwrap())? {
        qint!(i) => Ok(i),
        _ => Err(qerr!("slice-inc-by: first arg must be an int")),
    }?;
    if beg < 0 {
        return Err(qerr!("slice-inc-by: start index must be non-negative"));
    }
    let end: i64 = match env.eval(args.get(1).unwrap())? {
        qint!(i) => Ok(i),
        _ => Err(qerr!("slice-inc-by: second arg must be an int")),
    }?;
    if end < 0 {
        return Err(qerr!("slice-inc-by: stop index must be non-negative"));
    }
    let step: i64 = match env.eval(args.get(2).unwrap())? {
        qint!(i) => Ok(i),
        _ => Err(qerr!("slice-inc-by: step must be an int")),
    }?;
    if step <= 0 {
        return Err(qerr!("slice-inc-by: step must be positive"));
    }
    let idxable
        = Indexable::from_qexp(env.eval(args.get(3).unwrap())?)
        .map_err(|_| {
            qerr!("slice-inc-by: third arg must be a list or str")
        })?;
    return idxable.slice_inc(
        beg.unsigned_abs() as usize,
        end.unsigned_abs() as usize,
        step.unsigned_abs() as usize
    )
        .map_err(|e| e.prepend_source("slice-inc-by"));
}

/// Select items from a list or characters from a str at specific indices and
/// return them in a new list or str.
/// Alias: `.*`
///
/// Expected form:
/// `(pick (<index>...) <list or str>)`
///
/// Example:
/// ```text
/// q>> (pick (0 5 3 3 6) (range 10 0)) ; evaluates to (10 5 7 7 4)
/// q>> (pick (0 5 3 3 6) "hello world") ; evaluates to "h llw"
/// ```
#[builtin(name = "pick", alias = ".*")]
pub fn fn_pick(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 2 {
        return Err(qerr_fmt!(
            "pick: expected 2 args but got {}", args.len()));
    }
    let idx: Vec<usize> = match env.eval(args.get(0).unwrap())? {
        qlist!(l) => {
            l.iter()
            .map(|qk| {
                match convert_type(qk, qint!()) {
                    Ok(qint!(i)) => {
                        if i >= 0 {
                            Ok(i as usize)
                        } else {
                            Err(qerr!(
                                "pick: first arg must be a list of \
                                non-negative ints"
                            ))
                        }
                    }
                    _ => Err(qerr!(
                        "pick: first arg must be a list of non-negative \
                        ints"
                    )),
                }
            })
            .collect::<QResult<Vec<usize>>>()
        },
        _ => Err(qerr!(
            "pick: first arg must be a list of non-negative ints"
        )),
    }?;
    let idxable
        = Indexable::from_qexp(env.eval(args.get(1).unwrap())?)
        .map_err(|_| qerr!("pick: second arg must be a list or str"))?;
    return idxable.pick(&idx)
        .map_err(|e| e.prepend_source("pick"));
}

/// Get the item in a list or character in a str at index 0. Equivalent to
/// `(get 0 ...)`.
/// Alias: `.-`
///
/// Expected form:
/// `(first <list or str>)`
///
/// Example:
/// ```text
/// q>> (first (range 0 10)) ; evaluates to 0
/// q>> (first "hello world") ; evaluates to "h"
/// ```
#[builtin(name = "first", alias = ".-")]
pub fn fn_first(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 1 {
        return Err(qerr_fmt!(
            "first: expected 1 arg but got {}", args.len()));
    }
    let idxable
        = Indexable::from_qexp(env.eval(args.get(0).unwrap())?)
        .map_err(|e| e.prepend_source("first"))?;
    return idxable.first()
        .map_err(|e| e.prepend_source("first"));
}

/// Get all items in a list or characters in a str after that at index 0.
/// Alias: `.!-`
///
/// Expected form:
/// `(rest <list or str>)`
///
/// Example:
/// ```text
/// q>> (rest (range 0 10)) ; evaluates to (1 2 3 4 5 6 7 8 9)
/// q>> (rest "hello world") ; "ello world"
/// ```
#[builtin(name = "rest", alias = ".!-")]
pub fn fn_rest(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 1 {
        return Err(qerr_fmt!(
            "rest: expected 1 arg but got {}", args.len()));
    }
    let idxable
        = Indexable::from_qexp(env.eval(args.get(0).unwrap())?)
        .map_err(|e| e.prepend_source("rest"))?;
    return idxable.rest()
        .map_err(|e| e.prepend_source("rest"));
}

/// Take the first `n` items in a list or characters in a str, discarding the
/// rest.
/// Alias: `~.`
///
/// Expected form:
/// `(take <n> <list or str>)`
///
/// Example:
/// ```text
/// q>> (take 5 (range 0 10)) ; evaluates to (0 1 2 3 4)
/// q>> (take 5 "hello world") ; evaluates to "hello"
/// ```
#[builtin(name = "take", alias = "~.")]
pub fn fn_take(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 2 {
        return Err(qerr_fmt!(
            "take: expected 2 args but got {}", args.len()));
    }
    let n: usize = match env.eval(args.get(0).unwrap())? {
        qint!(i) => {
            if i >= 0 {
                Ok(i as usize)
            } else {
                Err(qerr!("take: first arg must be a non-negative int"))
            }
        },
        _ => Err(qerr!("take: first arg must be a non-negative int")),
    }?;
    let idxable
        = Indexable::from_qexp(env.eval(args.get(1).unwrap())?)
        .map_err(|_| qerr!("take: second arg must be a list or str"))?;
    return idxable.take(n)
        .map_err(|e| e.prepend_source("take"));
}

/// Iterate from the start of a list or str, taking all items or characters for
/// which a function returns `true`, discarding all after and including the
/// first for which the function returns `false`. The function must return a
/// bool for all inputs.
/// Alias: `~.@`
///
/// Expected form:
/// `(take-while <predicate function> <list or str>)`
///
/// Example:
/// ```text
/// q>> (take-while (fn (n) (< n 3)) (0 1 3 2 2 5)) ; evaluates to (0 1)
/// q>> (take-while (fn (c) (!= c " ") "hello world") ; evaluates to "hello"
/// ```
#[builtin(name = "take-while", alias = "~.@")]
pub fn fn_take_while(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 2 {
        return Err(qerr_fmt!(
            "take-while: expected 2 args but got {}", args.len()));
    }
    let idxable
        = Indexable::from_qexp(env.eval(args.get(1).unwrap())?)
        .map_err(|_| {
            qerr!("take-while: second arg must be a list or str")
        })?;
    return match env.eval(args.get(0).unwrap())? {
        qfunc!(f) => {
            idxable.take_while(|args: &[QExp]| (f.f)(env, args))
                .map_err(|e| e.prepend_source("take-while"))
        },
        qlambda!(ll) => {
            let f = |args: &[QExp]| {
                let mut ll_env: QEnv = ll.env(args, env)?;
                ll_env.eval(&ll.body_exp)
            };
            idxable.take_while(f)
                .map_err(|e| e.prepend_source("take-while"))
        },
        _ => Err(qerr!("take-while: first arg must be a function")),
    };
}

/// Get the item in a list or character in a str at index N - 1, where N is the
/// length of the list or str. Equivalent to (get (- N 1) ...).
/// Alias: `-.`
///
/// Expected form:
/// `(last <list or str>)`
///
/// Example:
/// ```text
/// q>> (last (range 0 10)) ; evaluates to 9
/// q>> (last "hello world") ; evaluates to "d"
/// ```
#[builtin(name = "last", alias = "-.")]
pub fn fn_last(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 1 {
        return Err(qerr_fmt!(
            "last: expected 1 arg but got {}", args.len()));
    }
    let idxable
        = Indexable::from_qexp(env.eval(args.get(0).unwrap())?)
        .map_err(|e| e.prepend_source("last"))?;
    return idxable.last()
        .map_err(|e| e.prepend_source("last"));
}

/// Discard the first `n` items in a list or characters in a str, keeping the
/// rest.
/// Alias: `.~`
///
/// Expected form:
/// `(skip <n> <list or str>)`
///
/// Example:
/// ```text
/// q>> (skip 5 (range 0 10)) ; evaluates to (5 6 7 8 9)
/// q>> (skip 5 "hello world") ; evaluates to " world"
/// ```
#[builtin(name = "skip", alias = ".~")]
pub fn fn_skip(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 2 {
        return Err(qerr_fmt!(
            "skip: expected 2 args but got {}", args.len()));
    }
    let n: usize = match env.eval(args.get(0).unwrap())? {
        qint!(i) => {
            if i >= 0 {
                Ok(i as usize)
            } else {
                Err(qerr!("skip: first arg must be a non-negative int"))
            }
        },
        _ => Err(qerr!("skip: first arg must be a non-negative int")),
    }?;
    let idxable
        = Indexable::from_qexp(env.eval(args.get(1).unwrap())?)
        .map_err(|_| qerr!("skip: second arg must be a list or str"))?;
    return idxable.skip(n)
        .map_err(|e| e.prepend_source("skip"));
}

/// Iterate from the start of a list or str, skipping all items or characters
/// for which a function returns `true`, keeping all after and including the
/// first for which the function returns `false`. The function must return a
/// bool for all inputs.
/// Alias: `.~@`
///
/// Expected form:
/// `(skip-while <predicate function> <list or str>)`
///
/// Example:
/// ```text
/// q>> (skip-while (fn (n) (< n 3)) (0 1 3 2 2 5)) ; evaluates to (3 2 2 5)
/// q>> (skip-while (fn (c) (!= c " ")) "hello world") ; evaluates to " world"
/// ```
#[builtin(name = "skip-while", alias = ".~@")]
pub fn fn_skip_while(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 2 {
        return Err(qerr_fmt!(
            "skip-while: expected 2 args but got {}", args.len()));
    }
    let idxable
        = Indexable::from_qexp(env.eval(args.get(1).unwrap())?)
        .map_err(|_| {
            qerr!("skip-while: second arg must be a list or str")
        })?;
    return match env.eval(args.get(0).unwrap())? {
        qfunc!(f) => {
            idxable.skip_while(|args: &[QExp]| (f.f)(env, args))
                .map_err(|e| e.prepend_source("skip-while"))
        },
        qlambda!(ll) => {
            let f = |args: &[QExp]| {
                let mut ll_env: QEnv = ll.env(args, env)?;
                ll_env.eval(&ll.body_exp)
            };
            idxable.skip_while(f)
                .map_err(|e| e.prepend_source("skip-while"))
        },
        _ => Err(qerr!("skip-while: first arg must be a function")),
    };
}

/*
 * iterable transformation
 */

/// Step through a list or str with a given step size.
/// Alias `~`
///
/// Expected form:
/// `(step-by <step size> <list or str>)`
///
/// Example:
/// ```text
/// q>> (step-by 3 (range 0 15)) ; evaluates to (0 3 6 9 12)
/// q>> (step-by 3 "hello world") ; evaluates to "hlwl"
/// ```
#[builtin(name = "step-by", alias = "~")]
pub fn fn_step_by(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 2 {
        return Err(qerr_fmt!(
            "step-by: expected 2 args but got {}", args.len()));
    }
    let step: i64 = match env.eval(args.get(0).unwrap())? {
        qint!(i) => Ok(i),
        _ => Err(qerr!("step-by: step must be an int")),
    }?;
    if step <= 0 {
        return Err(qerr!("step-by: step must be positive"));
    }
    let idxable
        = Indexable::from_qexp(env.eval(args.get(1).unwrap())?)
        .map_err(|_| qerr!("step-by: second arg must be a list or str"))?;
    return idxable.slice(0, idxable.len(), step.unsigned_abs() as usize)
        .map_err(|e| e.prepend_source("step-by"));
}

/// Convert each item in a list or character in a str to a two-item list
/// containing the item or character and its index.
/// Alias: `##`
///
/// Expected form:
/// `(enumerate <list or str>)`
///
/// Example:
/// ```text
/// q>> (enumerate (1 1.0 1i)) ; evaluates to ((1 1) (2 1.0) (3 0.0+1.0i))
/// q>> (enumerate "abc") ; evaluates to ((0 "a") (1 "b") (2 "c"))
/// ```
#[builtin(name = "enumerate", alias = "##")]
pub fn fn_enumerate(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 1 {
        return Err(qerr_fmt!(
            "enumerate: expected 1 arg but got {}", args.len()));
    }
    let idxable
        = Indexable::from_qexp(env.eval(args.get(0).unwrap())?)
        .map_err(|e| e.prepend_source("enumerate"))?;
    return idxable.enumerate()
        .map_err(|e| e.prepend_source("enumerate"));
}

/// Reverse the order of a list or str.
/// Alias: `<>`
///
/// Expected form:
/// `(reverse <list or str>)`
///
/// Example:
/// ```text
/// q>> (reverse (range 0 10)) ; evaluates to (9 8 7 6 5 4 3 2 1 0)
/// q>> (reverse "hello world") ; evaluates to "dlrow olleh"
/// ```
#[builtin(name = "reverse", alias = "<>")]
pub fn fn_reverse(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 1 {
        return Err(qerr_fmt!(
            "reverse: expected 1 arg but got {}", args.len()));
    }
    let idxable
        = Indexable::from_qexp(env.eval(args.get(0).unwrap())?)
        .map_err(|e| e.prepend_source("reverse"))?;
    return idxable.reverse()
        .map_err(|e| e.prepend_source("reverse"));
}

/// Shift the positions of items in a list or characters in a str by a constant
/// offset n, wrapping around the ends; i.e. move the item at index k to new
/// index (k + n) % N, where N is the length of the list or str.
/// Alias: `<#>`
///
/// Expected form:
/// `(cycle <offset> <list or str>)`
///
/// Example:
/// ```text
/// q>> (cycle 2 (range 0 10)) ; evaluates to (8 9 0 1 2 3 4 5 6 7)
/// q>> (cycle 2 "hello world") ; evaluates to "ldhello wor"
/// ```
#[builtin(name = "cycle", alias = "<#>")]
pub fn fn_cycle(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 2 {
        return Err(qerr_fmt!(
            "cycle: expected 2 args but got {}", args.len()));
    }
    let shift: isize = match env.eval(args.get(0).unwrap())? {
        qint!(i) => Ok(i as isize),
        _ => Err(qerr!("cycle: first arg must be an int")),
    }?;
    let idxable
        = Indexable::from_qexp(env.eval(args.get(1).unwrap())?)
        .map_err(|_| qerr!("cycle: second arg must be a list or str"))?;
    return idxable.cycle(shift)
        .map_err(|e| e.prepend_source("cycle"));
}

/// Apply a function to each item of a list or character of a str and return the
/// results in a list.
/// Alias: `@`
///
/// Expected form:
/// `(map <function> <list or str>)`
///
/// Example:
/// ```text
/// q>> (map (fn (n) (* n n)) (range 0 8)) ; evaluates to (0 1 4 9 25 36 49 64)
/// q>> (map (fn (c) (= c "a")) "abcd") ; evaluates to (true false false false)
/// ```
#[builtin(name = "map", alias = "@")]
pub fn fn_map(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 2 {
        return Err(qerr_fmt!(
            "map: expected 2 args but got {}", args.len()));
    }
    let idxable
        = Indexable::from_qexp(env.eval(args.get(1).unwrap())?)
        .map_err(|_| qerr!("map: second arg must be a list or str"))?;
    return match env.eval(args.get(0).unwrap())? {
        qfunc!(f) => {
            idxable.map(|args: &[QExp]| (f.f)(env, args))
                .map_err(|e| e.prepend_source("map"))
        },
        qlambda!(ll) => {
            let f = |args: &[QExp]| {
                let mut ll_env: QEnv = ll.env(args, env)?;
                ll_env.eval(&ll.body_exp)
            };
            idxable.map(f)
                .map_err(|e| e.prepend_source("map"))
        },
        _ => Err(qerr!("map: first arg must be a function")),
    };
}

/// Apply a function to each item of a list or character of a str and discard
/// all for which the function returns `false`. The function must return a bool
/// for all inputs.
/// Alias: `@!`
///
/// Expected form:
/// `(filter <predicate function> <list or str>)`
///
/// Example:
/// ```text
/// q>> (filter (fn (n) (= (% 3 n) 0)) (range 0 10)) ; evaluates to (0 3 6 9)
/// q>> (filter (fn (c) (= c "l")) "hello world") ; evaluates to "lll"
/// ```
#[builtin(name = "filter", alias = "@!")]
pub fn fn_filter(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 2 {
        return Err(qerr_fmt!(
            "filter: expected 2 args but got {}", args.len()));
    }
    let idxable
        = Indexable::from_qexp(env.eval(args.get(1).unwrap())?)
        .map_err(|_| qerr!("filter: second arg must be a list or str"))?;
    return match env.eval(args.get(0).unwrap())? {
        qfunc!(f) => {
            idxable.filter(|args: &[QExp]| (f.f)(env, args))
                .map_err(|e| e.prepend_source("filter"))
        },
        qlambda!(ll) => {
            let f = |args: &[QExp]| {
                let mut ll_env: QEnv = ll.env(args, env)?;
                ll_env.eval(&ll.body_exp)
            };
            idxable.filter(f)
                .map_err(|e| e.prepend_source("filter"))
        },
        _ => Err(qerr!("filter: first arg must be a function")),
    };
}

/// Reduce a list or str down to only unique items or characters. If two
/// elements are equal, the element nearer to the start of the list or str is
/// kept; order is otherwise maintained.
/// Alias: `*!=`
///
/// Expected form:
/// `(unique <list or str>)`
///
/// Example:
/// ```text
/// q>> (unique (true 1 1 5i false true)) ; evaluates to (true 1 5i false)
/// q>> (unique "hello world") ; evaluates to "helo wrd"
/// ```
#[builtin(name = "unique", alias = "*!=")]
pub fn fn_unique(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 1 {
        return Err(qerr_fmt!(
            "unique: expected 1 arg but got {}", args.len()));
    }
    let idxable
        = Indexable::from_qexp(env.eval(args.get(0).unwrap())?)
        .map_err(|e| e.prepend_source("unique"))?;
    return idxable.unique()
        .map_err(|e| e.prepend_source("unique"));
}

/// Recursively unpack nested lists so that all items lie next to each other in
/// a single list.
/// Alias: `__`
///
/// Expected form:
/// `(flatten <list>)`
///
/// Example:
/// ```text
/// q>> (flatten ((1 2 3) (4 5 6) (7 8 9))) ; evaluates to (1 2 3 4 5 6 7 8 9)
/// ```
#[builtin(name = "flatten", alias = "__")]
pub fn fn_flatten(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 1 {
        return Err(qerr_fmt!(
            "flatten: expected 1 arg but got {}", args.len()));
    }
    let idxable
        = Indexable::from_qexp(env.eval(args.get(0).unwrap())?)
        .map_err(|e| e.prepend_source("flatten"))?;
    return idxable.flatten()
        .map_err(|e| e.prepend_source("flatten"));
}

/// Sort a list or str in ascending order using the `<` function. This sort is
/// stable and performs in O(n log(n)) time.
/// Alias: `<*`
///
/// Expected form:
/// `(sort <list or str>)`
///
/// Example:
/// ```text
/// q>> (sort (4 7 3 5 5 2 7 9 4 6 8)) ; evaluates to (2 3 4 4 5 5 6 7 7 8 9)
/// q>> (sort "hello world") ; evaluates to " dehllloorw"
/// ```
#[builtin(name = "sort", alias = "<*")]
pub fn fn_sort(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 1 {
        return Err(qerr_fmt!(
            "sort: expected 1 arg but got {}", args.len()));
    }
    let idxable
        = Indexable::from_qexp(env.eval(args.first().unwrap())?)
        .map_err(|e| e.prepend_source("sort"))?;
    return idxable.sort()
        .map_err(|e| e.prepend_source("sort"));
}

/// Sort a list or str using a given comparison function. The function should
/// take two arguments, x and y, and return `true` if x should come before y,
/// `false` otherwise.
/// Alias: `<@`
///
/// Expected form:
/// `(sort-by <comparison function> <list or str>)`
///
/// Example:
/// ```text
/// q>> (sort-by > (7 3 5 5 2 7 9 4 6 8)) ; evaluates to (9 8 7 7 6 5 5 4 3 2)
/// q>> (defn listmax (a b) (> (max a) (max b)))
/// q>> (sort-by listmax ((1 5) (6 2) (0 9))) ; evaluates to ((0 9) (6 2) (1 5))
/// ```
#[builtin(name = "sort-by", alias = "<@")]
pub fn fn_sort_by(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 2 {
        return Err(qerr_fmt!(
            "sort-by: expected 2 args but got {}", args.len()));
    }
    let idxable
        = Indexable::from_qexp(env.eval(args.get(1).unwrap())?)
        .map_err(|_| qerr!("sort-by: second arg must be a list or str"))?;
    return match env.eval(args.get(0).unwrap())? {
        qfunc!(f) => {
            idxable.sort_by(|args: &[QExp]| (f.f)(env, args))
                .map_err(|e| e.prepend_source("sort-by"))
        },
        qlambda!(ll) => {
            let f = |args: &[QExp]| {
                let mut ll_env: QEnv = ll.env(args, env)?;
                ll_env.eval(&ll.body_exp)
            };
            idxable.sort_by(f)
                .map_err(|e| e.prepend_source("sort-by"))
        },
        _ => Err(qerr!("sort-by: first arg must be a function")),
    };
}

/// Perform a series of permutations on a list X. Permutations are described
/// by n-item lists of indices (i_1 i_2 ... i_n) where the presence of an index
/// i_k indicates that the element in X at i_k is to be replaced by the element
/// at i_{k - 1}. The element at i_1 will be replaced by the element at i_n.
/// Each permutation can contain each available index at most once.
/// Alias: `.~.`
///
/// Expected form:
/// `(permute <list> <permutations>...)`
///
/// Example:
/// ```text
/// q>> ; simple swaps
/// q>> (permute (range 0 5) (0 4) (2 3)) ; evaluates to (4 1 3 2 0)
/// q>> ; cyclic permutations
/// q>> (permute ("a" "b" "c") (0 1 2)) ; evaluates to ("c" "a" "b")
/// q>> ; non-total cyclic permutations
/// q>> (permute (range 0 10) (0 2 4 6)) ; evaluates to (6 1 0 3 2 5 4 7 8 9)
/// ```
#[builtin(name = "permute", alias = ".~.")]
pub fn fn_permute(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() < 2 {
        return Err(qerr_fmt!(
            "permute: expected at least 2 args but got {}",
            args.len()
        ));
    }
    let idxable
        = Indexable::from_qexp_list(env.eval(args.get(0).unwrap())?)
        .map_err(|_| qerr!("permute: first arg must be a list"))?;
    let moves: Vec<Vec<usize>>
        = env.eval_multi(&args[1..])?
        .into_iter()
        .map(|qk| match qk {
            qlist!(l) => {
                l.into_iter()
                    .map(|lk| match lk {
                        qint!(i) => {
                            if i >= 0 {
                                Ok(i as usize)
                            } else {
                                Err(qerr_fmt!(
                                    "permute: invalid index {}", i))
                            }
                        }
                        _ => Err(qerr!(
                            "permutations must be lists of indices")),
                    })
                    .collect::<QResult<Vec<usize>>>()
            },
            _ => Err(qerr!("permutations must be lists of indices")),
        })
        .collect::<QResult<Vec<Vec<usize>>>>()?;
    return idxable.permute(&moves)
        .map_err(|e| e.prepend_source("permute"));
}

/*
 * iterable division
 */

/// Divide a list or str into two pieces with the first containing the first `n`
/// items or characters and the second containing the rest.
/// Alias: `|.`
///
/// Expected form:
/// `(split-at <n> <list or str>)`
///
/// Example:
/// ```text
/// q>> (split-at 7 (range 0 10)) ; evaluates to ((0 1 2 3 4 5 6) (7 8 9))
/// q>> (split-at 7 "hello world") ; evaluates to ("hello w" "orld")
/// ```
#[builtin(name = "split-at", alias = "|.")]
pub fn fn_split_at(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 2 {
        return Err(qerr_fmt!(
            "split-at: expected 2 args but got {}", args.len()));
    }
    let idx: usize = match env.eval(args.first().unwrap())? {
        qint!(i) => {
            if i >= 0 {
                Ok(i as usize)
            } else {
                Err(qerr!("split-at: index must be non-negative"))
            }
        },
        _ => Err(qerr!("split-at: first arg must be an int")),
    }?;
    let idxable
        = Indexable::from_qexp(env.eval(args.get(1).unwrap())?)
        .map_err(|_| {
            qerr!("split-at: second arg must be a list or str")
        })?;
    return idxable.split_at(idx)
        .map_err(|e| e.prepend_source("split-at"));
}

/// Iterate from the start of a list or str, splitting into slices or substrings
/// on each item or character for which a function returns `true`. Matched
/// elements are discarded.
/// Alias: `|@`
///
/// Expected form:
/// `(split-on <matching function> <list or str>)`
///
/// Example:
/// ```text
/// q>> (defn div3 (n) (= (mod 3 n) 0))
/// q>> (split-on div3 (range 0 5)) ; evaluates to (() (1 2) (4))
/// q>> (defn is_space (c) (= c " "))
/// q>> (split-on is_space "hello world"); evaluates to ("hello" "world")
/// ```
#[builtin(name = "split-on", alias = "|@")]
pub fn fn_split_on(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 2 {
        return Err(qerr_fmt!(
            "split-on: expected 2 args but got {}", args.len()));
    }
    let idxable
        = Indexable::from_qexp(env.eval(args.get(1).unwrap())?)
        .map_err(|_| {
            qerr!("split-on: second arg must eb a list or str")
    })?;
    return match env.eval(args.get(0).unwrap())? {
        qfunc!(f) => {
            idxable.split_on(|args: &[QExp]| (f.f)(env, args))
                .map_err(|e| e.prepend_source("split-on"))
        },
        qlambda!(ll) => {
            let f = |args: &[QExp]| {
                let mut ll_env: QEnv = ll.env(args, env)?;
                ll_env.eval(&ll.body_exp)
            };
            idxable.split_on(f)
                .map_err(|e| e.prepend_source("split-on"))
        },
        _ => Err(qerr!("split-on: first arg must be a function")),
    };
}

/// Iterate from the start of a list or str, splitting into slices or substrings
/// on each item or character for which a function returns `true`. Matched
/// elements are contained in the previous slice or substring.
/// Alias: `|@=`
///
/// Expected forms:
/// `(split-on-inc <matching function> <list or str>)`
///
/// Example:
/// ```text
/// q>> (defn div3 (n) (= (mod 3 n) 0))
/// q>> (split-on-inc div3 (range 0 5)) ; evaluates to ((0) (1 2 3) (4))
/// q>> (defn is_space (c) (= c " "))
/// q>> (split-on-inc is_space "hello world"); evaluates to ("hello " "world")
/// ```
#[builtin(name = "split-on-inc", alias = "|@=")]
pub fn fn_split_on_inc(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 2 {
        return Err(qerr_fmt!(
            "split-on-inc: expected 2 args but got {}", args.len()));
    }
    let idxable
        = Indexable::from_qexp(env.eval(args.get(1).unwrap())?)
        .map_err(|_| {
            qerr!("split-on-inc: second arg must eb a list or str")
    })?;
    return match env.eval(args.get(0).unwrap())? {
        qfunc!(f) => {
            idxable.split_on_inc(|args: &[QExp]| (f.f)(env, args))
                .map_err(|e| e.prepend_source("split-on-inc"))
        },
        qlambda!(ll) => {
            let f = |args: &[QExp]| {
                let mut ll_env: QEnv = ll.env(args, env)?;
                ll_env.eval(&ll.body_exp)
            };
            idxable.split_on_inc(f)
                .map_err(|e| e.prepend_source("split-on-inc"))
        },
        _ => Err(qerr!("split-on-inc: first arg must be a function")),
    };
}

/*
 * iterable addition
 */

/// Append one or more items, one by one, to the end of a list or strs to the
/// end of another str.
/// Alias: `+.`
///
/// Expected form:
/// `(append <list or str> <items>...)`
///
/// Example:
/// ```text
/// q>> (append (0 1 2) true 5i "abc") ; evaluates to (0 1 2 true 5i "abc")
/// q>> (append "hello " "wo" "rld") ; evaluates to "hello world"
/// ```
#[builtin(name = "append", alias = "+.")]
pub fn fn_append(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() < 2 {
        return Err(qerr_fmt!(
            "append: expected at least 2 args but got {}", args.len()));
    }
    let idxable
        = Indexable::from_qexp(env.eval(args.get(0).unwrap())?)
        .map_err(|_| qerr!("append: first arg must be a str or list"))?;
    return idxable.append(&env.eval_multi(&args[1..])?)
        .map_err(|e| e.prepend_source("append"));
}

/// Prepend one or more items, one by one, to the beginning of a list or strings
/// to the beginning of another string. Note that this process inverts the order
/// of arguments following the list or str.
/// Alias: `.+`
///
/// Expected form:
/// `(prepend <list or str> <items>...)`
///
/// Example:
/// ```text
/// q>> (prepend (0 1 2) true 5i "abc") ; evaluates to (5i true 0 1 2)
/// q>> (prepend "hello " "wo" "rld") ; evaluates to "rldwohello "
/// ```
#[builtin(name = "prepend", alias = ".+")]
pub fn fn_prepend(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() < 2 {
        return Err(qerr_fmt!(
            "prepend: expected at least 2 args but got {}", args.len()));
    }
    let idxable
        = Indexable::from_qexp(env.eval(args.get(0).unwrap())?)
        .map_err(|_| qerr!("prepend: first arg must be a str or list"))?;
    return idxable.prepend(&env.eval_multi(&args[1..])?)
        .map_err(|e| e.prepend_source("prepend"));
}

/// Insert one or more items into a list or strings into another string at a
/// given index. The order of arguments following the list or str is preserved.
/// Alias: `+.+`
///
/// Expected form:
/// `(insert <index> <list or str> <items>...)`
///
/// Example:
/// ```text
/// q>> (insert 3 (range 0 5) true false) ; evaluates to (0 1 2 true false 3 4)
/// q>> (insert 3 "hello world" "abc" "def") ; evaluates to "helabcdeflo world"
/// ```
#[builtin(name = "insert", alias = "+.+")]
pub fn fn_insert(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() < 3 {
        return Err(qerr_fmt!(
            "insert: expected at least 3 args but got {}", args.len()));
    }
    let idx: usize = match env.eval(args.get(0).unwrap())? {
        qint!(i) => {
            if i >= 0 {
                Ok(i as usize)
            } else {
                Err(qerr!("insert: index must be non-negative"))
            }
        },
        _ => Err(qerr!("insert: first arg must be an int")),
    }?;
    let idxable
        = Indexable::from_qexp(env.eval(args.get(1).unwrap())?)
        .map_err(|_| qerr!("insert: second arg must be a str or list"))?;
    return idxable.insert(idx, &env.eval_multi(&args[2..])?)
        .map_err(|e| e.prepend_source("insert"));
}

/// Concatenate two or more lists or strs in the order they are passed.
/// Arguments must be either all lists or all strs. If only a single list is
/// passed, operates on the contents of the list instead of the list itself.
/// Alias: `++`
///
/// Expected form:
/// `(join <lists or strs>...)`
///
/// Example:
/// ```text
/// q>> (join (1 2 3) (4 5 6) (7 8 9)) ; evaluates to (1 2 3 4 5 6 7 8 9)
/// q>> (join "hello" " " "world") ; evaluates to "hello world"
/// ```
#[builtin(name = "join", alias = "++")]
pub fn fn_join(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    fn do_join(args: &[QExp]) -> QResult<QExp> {
        if args.len() == 1 {
            if let Some(qlist!(l)) = args.first() {
                return do_join(l);
            }
        }
        return match args.first() {
            Some(qlist!(_)) => {
                let mut acc: Vec<QExp> = Vec::new();
                for exp in args.iter() {
                    if let qlist!(l) = exp {
                        acc.append(&mut l.to_vec());
                    } else {
                        return Err(qerr!(
                            "join: args must be all lists or all strs"));
                    }
                }
                Ok(qlist!(acc))
            },
            Some(qstr!(_)) => {
                let mut acc: String = String::new();
                for exp in args.iter() {
                    if let qstr!(s) = exp {
                        acc.push_str(s);
                    } else {
                        return Err(qerr!(
                            "join: args must be all lists or all strs"));
                    }
                }
                Ok(qstr!(acc))
            },
            None => Err(qerr!("join: expected at least one list or str")),
            _ => Err(qerr!("join: args must be all lists or all strs")),
        };
    }
    return do_join(&env.eval_multi(args)?);
}

/// Concatenate two or more lists or strs in the order they are passed,
/// inserting an extra item at each join. Arguments after the item just be
/// either all lists or all strs. If only a single list is passed, operates on
/// the contents of the list instead of the list itself.
/// Alias `+*+`
///
/// Expected form:
/// `(join-with <item> <lists or strs>...)`
///
/// Example:
/// ```text
/// q>> (join-with 0 (1 2) (3 4) (5 6 7)) ; evaluates to (1 2 0 3 4 0 5 6 7)
/// q>> (join-with " " "hello" "world") ; evaluates to "hello world"
/// ```
#[builtin(name = "join-with", alias = "+*+")]
pub fn fn_join_with(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    fn do_join_with(item: &QExp, args: &[QExp]) -> QResult<QExp> {
        if args.len() == 1 {
            if let Some(qlist!(l)) = args.first() {
                return do_join_with(item, l);
            }
        }
        let n: usize = args.len();
        return match (item, args.first()) {
            (_, Some(qlist!(_))) => {
                let mut acc: Vec<QExp> = Vec::new();
                for (k, exp) in args.iter().enumerate() {
                    if let qlist!(l) = exp {
                        acc.append(&mut l.to_vec());
                        if k < n - 1 {
                            acc.push(item.clone());
                        }
                    } else {
                        return Err(qerr!(
                            "join-with: args to join must be all lists or \
                            all strs"
                        ));
                    }
                }
                Ok(qlist!(acc))
            },
            (qstr!(s0), Some(qstr!(_))) => {
                let mut acc: String = String::new();
                for (k, exp) in args.iter().enumerate() {
                    if let qstr!(s) = exp {
                        acc.push_str(s);
                        if k < n - 1 {
                            acc.push_str(s0);
                        }
                    } else {
                        return Err(qerr!(
                            "join-with: args to join must be all lists or \
                            all strs"
                        ));
                    }
                }
                Ok(qstr!(acc))
            },
            (_, None) => Err(qerr!(
                "join-with: expected at least one list or str to join")),
            _ => Err(qerr!(
                "join-with: args to join must be all lists or all strs")),
        };
    }
    if args.len() < 2 {
        return Err(qerr_fmt!(
            "join-with: expected at least 2 args but got {}", args.len()));
    }
    let item: QExp = env.eval(args.first().unwrap())?;
    let args: Vec<QExp> = env.eval_multi(&args[1..])?;
    return do_join_with(&item, &args);
}

/// Combine lists or strs element-wise, returning a list of lists containing
/// elements drawn from each argument. The length of the returned list is
/// limited to that of the shortest argument. strs are unpacked into lists of
/// one character-long strs. If only a single list is passed, operates on the
/// contents of the list instead of the list itself.
/// Alias: `:~:`
///
/// Expected form:
/// `(zip <lists or strs>...)`
///
/// Example:
/// ```text
/// q>> (zip (1 2) (3 4) (5 6 7)) ; evaluates to ((1 3 5) (2 4 6))
/// q>> (zip ((1 3 5) (2 4 6))) ; evaluates to ((1 2) (3 4) (5 6))
/// q>> (zip (0 1 2) "hello world") ; evaluates to ((0 "h") (1 "e") (2 "l"))
/// ```
#[builtin(name = "zip", alias = ":~:")]
pub fn fn_zip(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    fn construct_zip(acc: &mut Vec<Vec<QExp>>, rest: &[QExp])
        -> QResult<()>
    {
        return match rest.first() {
            Some(qlist!(l)) => {
                acc.iter_mut().zip(l.iter())
                    .for_each(|(zk, qk)| zk.push(qk.clone()));
                construct_zip(acc, &rest[1..])
            },
            Some(qstr!(s)) => {
                acc.iter_mut().zip(s.chars())
                    .for_each(|(zk, sk)| zk.push(qstr!(sk.to_string())));
                construct_zip(acc, &rest[1..])
            },
            Some(_) => Err(qerr!("zip: args must be lists or strs")),
            None => Ok(()),
        };
    }

    fn do_zip(args: &[QExp]) -> QResult<QExp> {
        if args.len() == 1 {
            if let Some(qlist!(l)) = args.first() {
                return do_zip(l);
            }
        }
        let mut acc: Vec<Vec<QExp>> = match args.first() {
            Some(qlist!(l)) => Ok(
                l.iter().map(|qk| vec![qk.clone()]).collect()
            ),
            Some(qstr!(s)) => Ok(
                s.chars().map(|sk| vec![qstr!(sk.to_string())]).collect()
            ),
            Some(_) => Err(qerr!("zip: args must be strs or lists")),
            None => Err(qerr!("zip: expected at least one str or list")),
        }?;
        construct_zip(&mut acc, &args[1..])?;
        return Ok(qlist!(acc.into_iter().map(|zk| qlist!(zk)).collect()));
    }
    return do_zip(&env.eval_multi(args)?);
}

/// Compute the Cartesian product of two or more lists or strs. The returned
/// value is a list of lists. str arguments are unpacked into lists of one
/// character-long strs. If only a single list is passed, operates on the
/// contents of the list instead of the list itself.
/// Alias: `:*:`
///
/// Expected form:
/// `(cart <lists or strs>...)`
///
/// Example:
/// ```text
/// q>> (cart (1 2) (3 4) (5 6)) ; evaluates to ((1 3 5) (1 3 6) (1 4 5) [...])
/// q>> (cart (1 2) "ab") ; evaluates to ((1 "a") (1 "b") (2 "a") (2 "b"))
/// ```
#[builtin(name = "cart", alias = ":*:")]
pub fn fn_cart(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    fn do_cart(args: &[QExp]) -> QResult<QExp> {
        if args.len() == 1 {
            if let Some(qlist!(l)) = args.first() {
                return do_cart(l);
            }
        }
        let items: Vec<QExp> = args.iter()
            .map(|qk| {
                match qk {
                    qlist!(_) => Ok(qk.clone()),
                    qstr!(_) => Ok(qk.clone()),
                    _ => Err(qerr!("cart: args must be strs or lists")),
                }
            })
            .collect::<QResult<Vec<QExp>>>()?
            .into_iter()
            .map(|qk| {
                match qk {
                    qlist!(l) => l.into_iter(),
                    qstr!(s) => {
                        s.chars()
                            .map(|sk| qstr!(sk.to_string()))
                            .collect::<Vec<QExp>>()
                            .into_iter()
                    },
                    _ => { panic!("unexpected state in cart"); }
                }
            })
            .multi_cartesian_product()
            .map(|vk| qlist!(vk))
            .collect();
        return Ok(qlist!(items));
    }
    return do_cart(&env.eval_multi(args)?);
}

/*
 * iterable testing
 */

/// Returns `true` if a given list or str contains a given item or substring.
/// Alias: `*=`
///
/// Expected form:
/// `(contains <list or str> <item or substring>)`
///
/// Example:
/// ```text
/// q>> (contains (range 0 10) 5) ; evaluates to true
/// q>> (contains "hello world" "o w") ; evaluates to true
/// q>> (contains "hello world" "a") ; evaluates to false
/// ```
#[builtin(name = "contains", alias = "*=")]
pub fn fn_contains(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 2 {
        return Err(qerr_fmt!(
            "contains: expected 2 args but got {}", args.len()));
    }
    let idxable
        = Indexable::from_qexp(env.eval(args.get(0).unwrap())?)
        .map_err(|_| qerr!("contains: first arg must be a str or list"))?;
    return idxable.contains(&env.eval(args.get(1).unwrap())?)
        .map_err(|e| e.prepend_source("contains"));
}

/// Returns the index of the first occurrence of an item or substring in a list
/// or str. If the list or str does not contain the element, -1 is returned.
/// Alias: `#*=`
///
/// Expected form:
/// `(index-of <list or str> <item or substring>)`
///
/// Example:
/// ```text
/// q>> (index-of (range 10 0) 9) ; evaluates to 1
/// q>> (index-of "hello world" "l") ; evaluates to 2
/// q>> (index-of "hello world" "a") ; evaluates to -1
/// ```
#[builtin(name = "index-of", alias = "#*=")]
pub fn fn_index_of(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 2 {
        return Err(qerr_fmt!(
            "index-of: expected 2 args but got {}", args.len()));
    }
    let item: QExp
        = env.eval(args.get(1).unwrap())
        .map_err(|e| e.prepend_source("index-of"))?;
    let idxable
        = Indexable::from_qexp(env.eval(args.get(0).unwrap())?)
        .map_err(|_| qerr!("index-of: second arg must be a str or list"))?;
    return idxable.index_of(&item)
        .map_err(|e| e.prepend_source("index-of"));
}

/*
 * element-wise math
 */

macro_rules! elementwise_fn(
    ( $inner:ident, $method:ident ) => {
        fn $inner(args: &[QExp]) -> QResult<QExp> {
            if args.len() == 1 {
                if let Some(qlist!(l)) = args.first() {
                    return $inner(l);
                }
                return args.get(0).unwrap().$method();
            } else {
                let new: Vec<QExp>
                    = args.iter()
                    .map(|x| x.$method())
                    .collect::<QResult<Vec<QExp>>>()?;
                return Ok(qlist!(new));
            }
        }
    }
);

/// Logical and arithmetic negative: For each input, returns the logical
/// negation for booleans and additive inverse for other numerical types. If a
/// single list is passed, operates on the contents of the list instead of the
/// list itself.
/// Alias: `!`
///
/// Expected form:
/// `(neg <values>...)`
///
/// Example:
/// ```text
/// q>> (neg true) ; evaluates to false
/// q>> (neg -5) ; evaluates to 5
/// q>> (neg false 1.0546) ; evaluates to (true -1.0546)
/// ```
#[builtin(name = "neg", alias = "!")]
pub fn fn_neg(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    elementwise_fn!(do_neg,     neg     );
    return do_neg(&env.eval_multi(args)?);
}

/// Scalar multiplicative inverse: For each input x, returns the multiplicative
/// inverse 1/x. If a single list is passed, operates on the contents of the
/// list instead of the list itself. Output values are always floats, complexes,
/// or lists thereof.
/// Alias: `1/`
///
/// Expected form:
/// `(recip <values>...)`
///
/// Example:
/// ```text
/// q>> (recip 2) ; evaluates to 0.5
/// q>> (recip 1+1i) ; evaluates to 0.5+0.5i
/// ```
#[builtin(name = "recip", alias = "1/")]
pub fn fn_recip(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    elementwise_fn!(do_recip,   recip   );
    return do_recip(&env.eval_multi(args)?);
}

/// Absolute value operation for single numbers or a list of numbers. If a
/// single list is passed, operates on the contents of the list instead of the
/// list itself.
/// Alias: `|.|`
///
/// Expected form:
/// `(abs <values>...)`
///
/// Example:
/// ```text
/// q>> (abs -5) ; evaluates to 5
/// q>> (abs (-4 3.2 3+4i)) ; evaluates to (4 3.2 5.0)
/// ```
#[builtin(name = "abs", alias = "|.|")]
pub fn fn_abs(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    elementwise_fn!(do_abs,     abs     );
    return do_abs(&env.eval_multi(args)?);
}

/// Square-root operation for single numbers or a list of numbers. If a single
/// list is passed, operates on the contents of the list instead of the list
/// itself. Output values are always floats, complexes, or lists thereof.
///
/// Expected form:
/// `(sqrt <values>...)`
///
/// Example:
/// ```text
/// q>> (sqrt 2) ; evaluates to 1.4142135623720951
/// q>> (sqrt (-1 -1+0i)) ; evaluates to (NaN 0+1i)
/// ```
#[builtin(name = "sqrt")]
pub fn fn_sqrt(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    elementwise_fn!(do_sqrt,    sqrt    );
    return do_sqrt(&env.eval_multi(args)?);
}

/// Cube-root operation for single numbers or a list of numbers. If a single
/// list is passed, operates on the contents of the list instead of the list
/// itself. Output values are always floats, complexes, or lists thereof.
///
/// Expected form:
/// `(cbrt <values>...)`
///
/// Example:
/// ```text
/// q>> (cbrt 2) ; evaluates to 1.2599210498948734
/// q>> (cbrt (-1 -1+0i)) ; evaluates to (-1, 0.5+0.8660254037844386i)
/// ```
#[builtin(name = "cbrt")]
pub fn fn_cbrt(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    elementwise_fn!(do_cbrt,    cbrt    );
    return do_cbrt(&env.eval_multi(args)?);
}

/// Exponential function for single numbers or a list of numbers. If a single
/// list is passed, operates on the contents of the list instead of the list
/// itself. Output values are always floats, complexes, or lists thereof.
/// Alias: `e**`
///
/// Expected form:
/// `(exp <values>...)`
///
/// Example:
/// ```text
/// q>> (exp 1) ; evaluates to 2.718281828459045
/// q>> (exp (* 1i PI)) ; evaluates to -1+0i
/// ```
#[builtin(name = "exp", alias = "e**")]
pub fn fn_exp(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    elementwise_fn!(do_exp,     exp     );
    return do_exp(&env.eval_multi(args)?);
}

/// Floor operation for single numbers or a list of numbers. If a single list is
/// passed, operates on the contents of the list instead of the list itself.
/// Output values are always ints or lists thereof.
/// Alias: `~_`
///
/// Expected form:
/// `(floor <values>...)`
///
/// Example:
/// ```text
/// q>> (floor 5.2) ; evaluates to 5
/// q>> (floor (2.25 -5.2 -7.9)) ; evaluates to (2 -6 -8)
/// ```
#[builtin(name = "floor", alias = "~_")]
pub fn fn_floor(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    elementwise_fn!(do_floor,   floor   );
    return do_floor(&env.eval_multi(args)?);
}

/// Ceiling operation for single numbers or a list of numbers. If a single list
/// is passed, operates on the contents of the list instead of the list itself.
/// Output values are always ints or lists thereof.
/// Alias: `~^`
///
/// Expected form:
/// `(ceil <values>...)`
///
/// Example:
/// ```text
/// q>> (ceil 5.2) ; evaluates to 6
/// q>> (ceil (2.25 -5.2 -7.9)) ; evaluates to (3 -5 -7)
/// ```
#[builtin(name = "ceil", alias = "~^")]
pub fn fn_ceil(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    elementwise_fn!(do_ceil,    ceil    );
    return do_ceil(&env.eval_multi(args)?);
}

/// Round real numbers to the nearest integer. If a single list is passed,
/// operates on the contents of the list instead of the list itself. Output
/// values are always ints or lists thereof.
/// Alias: `~:`
///
/// Expected form:
/// `(round <values>...)`
///
/// Example:
/// ```text
/// q>> (round 5.5 6.5 -7.5) ; evaluates to (6 7 -8)
/// q>> (round (4.2 -7.8 1.9) ; evaluates to (4 -8 2)
/// ```
#[builtin(name = "round", alias = "~:")]
pub fn fn_round(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    elementwise_fn!(do_round,   round   );
    return do_round(&env.eval_multi(args)?);
}

/// Natural logarithm function. For complex input z, this function satisfies
///     -π ≤ arg(ln(z)) ≤ π.
/// If a single list is passed, operates on the contents of the list instead of
/// the list itself. Outputs are always floats, complexes, or lists thereof.
///
/// Expected form:
/// `(ln <values>...)`
///
/// Example:
/// ```text
/// q>> (ln 2) ; evaluates to 0.6931471805599453
/// q>> (ln 1j) ; evaluates to 0+1.5707963267948966i
/// q>> (ln (1 2j)) ; evaluates to (0 0.6931471805599453+1.5707963267948966i)
/// ```
#[builtin(name = "ln")]
pub fn fn_ln(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    elementwise_fn!(do_ln,      ln      );
    return do_ln(&env.eval_multi(args)?);
}

/// Sine function. If a single list is passed, operates on the contents of the
/// list instead of the list itself. Outputs are always floats, complexes, or
/// lists thereof.
///
/// Expected form:
/// `(sin <values>...)`
///
/// Example:
/// ```text
/// q>> (sin (0 (/ PI 2))) ; evaluates to (0 1)
/// ```
#[builtin(name = "sin")]
pub fn fn_sin(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    elementwise_fn!(do_sin,     sin     );
    return do_sin(&env.eval_multi(args)?);
}

/// Cosine function. If a single list is passed, operates on the contents of the
/// list instead of the list itself. Outputs are always floats, complexes, or
/// lists thereof.
///
/// Expected form:
/// `(cos <values>...)`
///
/// Example:
/// ```text
/// q>> (cos (0 (/ PI 2))) ; evaluates to (1 0)
/// ```
#[builtin(name = "cos")]
pub fn fn_cos(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    elementwise_fn!(do_cos,     cos     );
    return do_cos(&env.eval_multi(args)?);
}

/// Tangent function. If a single list is passed, operates on the contents of
/// the list instead of the list itself. Outputs are always floats, complexes,
/// or lists thereof.
///
/// Expected form:
/// `(tan <values>...)`
///
/// Example:
/// ```text
/// q>> (tan (0 (/ PI 4))) ; evaluates to (0 1)
/// ```
#[builtin(name = "tan")]
pub fn fn_tan(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    elementwise_fn!(do_tan,     tan     );
    return do_tan(&env.eval_multi(args)?);
}

/// Arcsine function. If a single list is passed, operates on the contents of
/// the list instead of the list itself. Outputs are always floats, complexes,
/// or lists thereof.
/// Alias: `asin`
///
/// Expected form:
/// `(arcsin <values>...)`
///
/// Example:
/// ```text
/// q>> (arcsin (0 1)) ; evaluates to (0 1.5707963267948966)
/// ```
#[builtin(name = "arcsin", alias = "asin")]
pub fn fn_arcsin(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    elementwise_fn!(do_arcsin,  arcsin  );
    return do_arcsin(&env.eval_multi(args)?);
}

/// Arccosine function. If a single list is passed, operates on the contents of
/// the list instead of the list itself. Outputs are always floats, complexes,
/// or lists thereof.
/// Alias: `acos`
///
/// Expected form:
/// `(arccos <values>...)`
///
/// Example:
/// ```text
/// q>> (arccos (0 1)) ; evaluates to (1.5707963267948966 0)
/// ```
#[builtin(name = "arccos", alias = "acos")]
pub fn fn_arccos(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    elementwise_fn!(do_arccos,  arccos  );
    return do_arccos(&env.eval_multi(args)?);
}

/// Arctangent function. If a single list is passed, operates on the contents of
/// the list instead of the list itself. Outputs are always floats, complexes,
/// or lists thereof.
/// Alias: `atan`
///
/// Expected form:
/// `(arctan <values>...)`
///
/// Example:
/// ```text
/// q>> (arctan (0 1)) ; evaluates to (0 0.7853981633974483)
/// ```
#[builtin(name = "arctan", alias = "atan")]
pub fn fn_arctan(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    elementwise_fn!(do_arctan,  arctan  );
    return do_arctan(&env.eval_multi(args)?);
}

/// Four-quadrant arctangent function. Expects arguments to be two-item lists
/// with the first being the y-coordinate. If a single list of length not equal
/// to 2 is passed, operates on the contents of the list instead of the list
/// itself. Outputs are always floats, complexes, or lists thereof.
/// Alias: `atan2`
///
/// Expected form:
/// `(arctan2 <values>...)`
///
/// Example:
/// ```text
/// q>> (arctan2 (1 1)) ; evaluates to 0.7853981633974483
/// q>> (arctan2 (-1 -1)) ; evaluates to -2.356194490192345
/// ```
#[builtin(name = "arctan2", alias = "atan2")]
pub fn fn_arctan2(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    elementwise_fn!(do_arctan2, arctan2 );
    return do_arctan2(&env.eval_multi(args)?);
}

/// Hyperbolic sine function. If a single list is passed, operates on the
/// contents of the list instead of the the list itself. Outputs are always
/// floats, complexes, or lists thereof.
///
/// Expected form:
/// `(sinh <values>...)`
///
/// Example:
/// ```text
/// q>> (sinh (0 1)) ; evaluates to (0 1.1752011936438014)
/// ```
#[builtin(name = "sinh")]
pub fn fn_sinh(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    elementwise_fn!(do_sinh,    sinh    );
    return do_sinh(&env.eval_multi(args)?);
}

/// Hyperbolic cosine function. If a single list is passed, operates on the
/// contents of the list instead of the list itself. Outputs are always floats,
/// complexes, or lists thereof.
///
/// Expected form:
/// `(cosh <values>...)`
///
/// Example:
/// ```text
/// q>> (cosh (0 1)) ; evaluates to (1 1.5430806348152437)
/// ```
#[builtin(name = "cosh")]
pub fn fn_cosh(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    elementwise_fn!(do_cosh,    cosh    );
    return do_cosh(&env.eval_multi(args)?);
}

/// Hyperbolic tangent function. If a single list is passed, operates on the
/// contents of the list instead of the list itself. Outputs are always floats,
/// complexes, or lists thereof.
///
/// Expected form:
/// `(tanh <values>...)`
///
/// Example:
/// ```text
/// q>> (tanh (0 1)) ; evaluates to (0 0.7615941559557649)
/// ```
#[builtin(name = "tanh")]
pub fn fn_tanh(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    elementwise_fn!(do_tanh,    tanh    );
    return do_tanh(&env.eval_multi(args)?);
}

/// Area hyperbolic sine function. If a single list is passed, operates on the
/// contents of the list instead of the list itself. Outputs are always floats,
/// complexes, or lists thereof.
/// Alias: `asinh`
///
/// Expected form:
/// `(arsinh <values>...)`
///
/// Example:
/// ```text
/// q>> (arsinh (0 1)) ; evaluates to (0 0.8813735870195429)
/// ```
#[builtin(name = "arsinh", alias = "asinh")]
pub fn fn_arsinh(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    elementwise_fn!(do_arsinh,  arsinh  );
    return do_arsinh(&env.eval_multi(args)?);
}

/// Area hyperbolic cosine function. If a single list is passed, operates on the
/// contents of the list instead of the list itself. Outputs are always floats,
/// complexes, or lists thereof.
/// Alias: `acosh`
///
/// Expected form:
/// `(arcosh <values>...)`
///
/// Example:
/// ```text
/// q>> (arcosh (0 1)) ; evaluates to (NaN 0)
/// ```
#[builtin(name = "arcosh", alias = "acosh")]
pub fn fn_arcosh(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    elementwise_fn!(do_arcosh,  arcosh  );
    return do_arcosh(&env.eval_multi(args)?);
}

/// Area hyperbolic tangent function. If a single list is passed, operates on
/// the contents of the list instead of the list itself. Outputs are always
/// floats, complexes, or lists thereof.
/// Alias: `atanh`
///
/// Expected form:
/// `(artanh <values>...)`
///
/// Example:
/// ```text
/// q>> (artanh (0 1)) ; evaluates to (0 inf)
/// ```
#[builtin(name = "artanh", alias = "atanh")]
pub fn fn_artanh(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    elementwise_fn!(do_artanh,  artanh  );
    return do_artanh(&env.eval_multi(args)?);
}

/// Returns the argument (polar angle) of a complex number. Returned values are
/// restricted to the range (-π, +π]. If a single list is passed, operates on
/// the contents of the list instead of the list itself. Outputs are always
/// floats or lists thereof.
///
/// Expected form:
/// `(arg <values>...)`
///
/// Example:
/// ```text
/// q>> (arg 1i) ; evaluates to 1.5707963267948966
/// q>> (arg (0 -1)) ; evaluates to (NaN 3.141592653589793)
/// ```
#[builtin(name = "arg")]
pub fn fn_arg(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    elementwise_fn!(do_arg,     arg     );
    return do_arg(&env.eval_multi(args)?);
}

/// Cis (cosine-i-sine) function. If a single list is passed, operates on the
/// contents of the list instead of the list itself. Outputs are always
/// complexes or lists thereof.
/// Alias: `e**i`
///
/// Expected form:
/// `(cis <values>...)`
///
/// Example:
/// ```text
/// q>> (cis (0 (/ PI 2) PI)) ; evaluates to (1+0i 0+1i -1+0i)
/// ```
#[builtin(name = "cis", alias = "e**i")]
pub fn fn_cis(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    elementwise_fn!(do_cis,     cis     );
    return do_cis(&env.eval_multi(args)?);
}

/// Complex conjugate operation. If a single list is passed, operates on the
/// contents of the list instead of the list itself.
/// Alias: `~z`
///
/// Expected form:
/// `(conj <values>...)`
///
/// Example:
/// ```text
/// q>> (conj (1+1i 5)) ; evaluates to (1-1i 5)
/// ```
#[builtin(name = "conj", alias = "~z")]
pub fn fn_conj(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    elementwise_fn!(do_conj,    conj    );
    return do_conj(&env.eval_multi(args)?);
}

/// Get the real part of a number. If a single list is passed, operates on the
/// contents of the list instead of the list itself.
/// Alias: `Re`
///
/// Expected form:
/// `(real <values>...)`
///
/// Example:
/// ```text
/// q>> (real (1+1i 1+2i 2+2i)) ; evaluates to (1.0 1.0 2.0)
/// ```
#[builtin(name = "read", lias = "Re")]
pub fn fn_real(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    elementwise_fn!(do_real,    real    );
    return do_real(&env.eval_multi(args)?);
}

/// Get the imaginary part of a number. If a single list is passed, operates on
/// the contents of the list instead of the list itself.
/// Alias: `Im`
///
/// Expected form:
/// `(imag <values>...)`
///
/// Example:
/// ```text
/// q>> (imag (1+1i 1+2i 2+2i)) ; evaluates to (1.0 2.0 2.0)
/// ```
#[builtin(name = "imag", alias = "Im")]
pub fn fn_imag(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    elementwise_fn!(do_imag,    imag    );
    return do_imag(&env.eval_multi(args)?);
}

/*
 * parameterized element-wise math
 */

macro_rules! param_elementwise_fn(
    (
        $inner_fn:ident,
        $err_prepend:literal,
        $elem_arg:literal,
        $param_arg:literal,
        $method:ident,
        $min_type:expr,
        $max_type:expr
    ) => {
        fn $inner_fn(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
            if args.len() != 2 {
                return Err(
                    qerr_fmt!("expected two arguments but got {}", args.len())
                    .prepend_source($err_prepend)
                );
            }
            let (elem, elem_ty): (QExp, QExpType)
                = match env.eval(args.get($elem_arg).unwrap())? {
                    qbool!(b) => Ok((qbool!(b), qbool!())),
                    qint!(i) => Ok((qint!(i), qint!())),
                    qfloat!(f) => Ok((qfloat!(f), qfloat!())),
                    qcomplex!(c) => Ok((qcomplex!(c), qcomplex!())),
                    qlist!(l) => {
                        let ty: QExpType
                            = l.iter().map(|qk| qk.exp_type()).max()
                            .ok_or(
                                qerr!("expected non-empty list")
                                .prepend_source($err_prepend)
                            )?;
                        Ok((qlist!(l), ty))
                    },
                    _ => Err(
                        qerr!("args must be numbers or lists of numbers")
                        .prepend_source($err_prepend)
                    ),
                }?;
            let (param, param_ty): (QExp, QExpType)
                = match env.eval(args.get($param_arg).unwrap())? {
                    qbool!(b) => Ok((qbool!(b), qbool!())),
                    qint!(i) => Ok((qint!(i), qint!())),
                    qfloat!(f) => Ok((qfloat!(f), qfloat!())),
                    qcomplex!(c) => Ok((qcomplex!(c), qcomplex!())),
                    qlist!(l) => {
                        let ty: QExpType
                            = l.iter().map(|qk| qk.exp_type()).max()
                            .ok_or(
                                qerr!("expected non-empty list")
                                .prepend_source($err_prepend)
                            )?;
                        Ok((qlist!(l), ty))
                    },
                    _ => Err(
                        qerr!("args must be numbers or lists of numbers")
                        .prepend_source($err_prepend)
                    ),
                }?;
            let ty: QExpType = cmp::max(elem_ty, param_ty);
            if ty < $min_type || ty > $max_type {
                return Err(
                    qerr_fmt!("cannot operate on type {}", ty)
                    .prepend_source($err_prepend)
                );
            }
            return match (elem, param) {
                (qlist!(e), qlist!(p)) => {
                    if e.len() != p.len() {
                        return Err(
                            qerr!("lists must be equal length")
                            .prepend_source($err_prepend)
                        );
                    }
                    let ret: Vec<QExp>
                        = e.iter().zip(p.iter())
                        .map(|(e, p)| e.$method(p))
                        .collect::<QResult<Vec<QExp>>>()?;
                    Ok(qlist!(ret))
                },
                (qlist!(e), p) => {
                    let ret: Vec<QExp>
                        = e.iter()
                        .map(|e| e.$method(&p))
                        .collect::<QResult<Vec<QExp>>>()?;
                    Ok(qlist!(ret))
                },
                (e, qlist!(p)) => {
                    let ret: Vec<QExp>
                        = p.iter()
                        .map(|p| e.$method(p))
                        .collect::<QResult<Vec<QExp>>>()?;
                    Ok(qlist!(ret))
                },
                (e, p) => Ok(e.$method(&p)?),
            };
        }
    }
);

/// Modulo operator for single numbers or lists of numbers. Returned values are
/// non-negative.
/// Alias: `%`
///
/// Expected form:
/// `(mod <value> <modulo>)`
///
/// Example:
/// ```text
/// q>> (mod -5.5 2.5) ; evaluates to 2.0
/// q>> (mod (range 5 15) 10) ; evaluates to (5 6 7 8 9 0 1 2 3 4)
/// q>> (mod 10 (range 5 15)) ; evaluates to (0 4 3 2 1 0 10 10 10 10)
/// q>> (mod (3 4 5) (3 4 5)) ; evluates to (0 0 0)
/// ```
#[builtin(name = "mod", alias = "%")]
pub fn fn_mod(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    param_elementwise_fn!(
        do_mod,
        "mod",
        0,
        1,
        modulo,
        qint!(),
        qfloat!()
    );
    return do_mod(env, args);
}

/// Logarithm operation for single numbers or lists of numbers. Returned values
/// are always floats, complexes, or lists thereof.
///
/// Expected form:
/// `(log <base> <value>)`
///
/// Example:
/// ```text
/// q>> (log 2 128) ; evaluates to 7.0
/// q>> (log 2 (1 2 4 8 16 32)) ; evaluates to (0.0 1.0 2.0 3.0 4.0 5.0)
/// q>> (log (2 E 10) 100) ; evaluates to (6.64385618977, 4.60517018598, 2)
/// q>> (log (4 5 6) (4 5 6)) ; evaluates to (1.0 1.0 1.0)
/// ```
#[builtin(name = "log")]
pub fn fn_log(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    param_elementwise_fn!(
        do_log,
        "log",
        1,
        0,
        log,
        qint!(),
        qcomplex!()
    );
    return do_log(env, args);
}

/// Exponentiation with arbitrary base.
/// Alias: `**`
///
/// Expected form:
/// `(pow <base> <exponent>)`
///
/// Example:
/// ```text
/// q>> (pow 2 8) ; evaluates to 256
/// q>> (pow 2 (0 1 2 3 4 5)) ; evaluates to (1 2 4 8 16 32)
/// q>> (pow (1 2 3 4 5) 2) ; evaluates to (1 4 9 16 25)
/// q>> (pow (1 2 3 4) (1 2 3 4)) ; evaluates to (1 4 27 256)
/// ```
#[builtin(name = "pow", alias = "**")]
pub fn fn_pow(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    param_elementwise_fn!(
        do_pow,
        "pow",
        0,
        1,
        pow,
        qint!(),
        qcomplex!()
    );
    return do_pow(env, args);
}

/// Bit-shift left for single numbers or lists of numbers. Overflow is prevented
/// by automatically reducing the shift size mod 64. Arguments must be bools,
/// ints, or lists thereof.
/// Alias: `<<`
///
/// Expected form:
/// `(shl <value> <shift size>)`
///
/// Example:
/// ```text
/// q>> (shl 1 7) ; evaluates to 128
/// q>> (shl 1 (1 2 3 4 5)) ; evaluates to (2 4 8 16 32)
/// q>> (shl (1 2 3 4 5) 2) ; evaluates to (4 8 12 16 20)
/// q>> (shl (1 2 3 4 5) (1 2 3 4 5)) ; evaluates to (2 8 24 64 160)
/// ```
#[builtin(name = "shl", alias = "<<")]
pub fn fn_shl(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    param_elementwise_fn!(
        do_shl,
        "shl",
        0,
        1,
        shl,
        qbool!(),
        qint!()
    );
    return do_shl(env, args);
}

/// Bit-shift right for single numbers or lists of numbers. Overflow is
/// prevented by automatically reducing the shift size mod 64. Arguments must be
/// bools, ints, or lists thereof.
/// Alias: `>>`
///
/// Expected form:
/// `(shr <value> <shift size>)`
///
/// Example:
/// ```text
/// q>> (shr 128 7) ; evaluates to 1
/// q>> (shr 128 (1 2 3 4 5)) ; evaluates to (64 32 16 8 4)
/// q>> (shr (16 32 64 128 256) 2) ; evaluates to (4 8 16 32 64)
/// q>> (shr (16 32 64 128 256) (4 5 6 7 8)) ; evaluates to (1 1 1 1 1)
/// ```
#[builtin(name = "shr", alias = ">>")]
pub fn fn_shr(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    param_elementwise_fn!(
        do_shr,
        "shr",
        0,
        1,
        shr,
        qbool!(),
        qint!()
    );
    return do_shr(env, args);
}

/*
 * list -> list math
 */

/// Discrete-valued convolution operation between two lists of numbers. Returns
/// values obtained from all points of non-zero overlap; the returned list
/// contains N + M - 1 values, where N and M are the lengths of the two argument
/// lists.
/// Alias: `<:>`
///
/// Expected form:
/// `(convolve <list1> <list2>)`
///
/// Example:
/// ```test
/// q>> (convolve (1 2 3 4 5) (6 7 8)) ; evaluates to (6 19 40 61 82 67 40)
/// ```
#[builtin(name = "convolve", alias = "<:>")]
pub fn fn_convolve(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 2 {
        return Err(qerr_fmt!(
            "convolve: expected 2 args but got {}", args.len()));
    }
    let (l1, l1_type): (Vec<QExp>, QExpType)
        = match env.eval(args.get(0).unwrap())? {
            qlist!(l) => convert_numbers_sametype(&l),
            _ => Err(qerr!("convolve: args must be lists of numbers")),
        }?;
    let N: usize = l1.len();
    let (l2, l2_type): (Vec<QExp>, QExpType)
        = match env.eval(args.get(1).unwrap())? {
            qlist!(l) => convert_numbers_sametype(&l),
            _ => Err(qerr!("convolve: args must be lists of numbers")),
        }?;
    let M: usize = l2.len();
    let mut ret: Vec<QExp> = Vec::new();
    let ret_type: QExpType = cmp::max(l1_type, l2_type);
    let mut acc: QExp = QExp::zero(ret_type)?;
    let K: usize = cmp::max(N, M);
    for t in 0..l1.len() + l2.len() - 1 {
        for x in 0..K {
            if let Some(xp) = t.checked_sub(x) {
                if let (Some(q1), Some(q2)) = (l1.get(x), l2.get(xp)) {
                    acc = acc.add(&q1.mul(q2)?)?;
                }
            }
        }
        ret.push(mem::replace(&mut acc, QExp::zero(ret_type)?));
    }
    return Ok(qlist!(ret));
}

/// Count elements of a data set in a histogram. Returns a list containing pairs
/// where, in each pair, the first element is a value and the second is the
/// number of times it occurs in the data set.
/// Alias: `|#|`
///
/// Expected form:
/// `(hist <data>)`
///
/// Example:
/// ```text
/// q>> (hist ("a" "b" "c" "a")) ; evaluates to (("a" 2) ("b" 1) ("c" 1))
/// ```
#[builtin(name = "hist", alias = "|#|")]
pub fn fn_histogram(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    fn index_of(vals: &[QExp], item: &QExp) -> Option<usize> {
        for (k, it) in vals.iter().enumerate() {
            if it == item {
                return Some(k);
            }
        }
        return None;
    }

    if args.len() != 1 {
        return Err(qerr_fmt!(
            "histogram: expected 1 arg but got {}", args.len()));
    }
    let data: Vec<QExp>
        = match env.eval(args.get(0).unwrap())? {
            qlist!(l) => Ok(l),
            _ => Err(qerr!("histogram: arg must be a list")),
        }?;
    let mut vals: Vec<QExp> = Vec::new();
    let mut counts: Vec<usize> = Vec::new();
    for item in data.iter() {
        if let Some(k) = index_of(&vals, item) {
            counts[k] += 1;
        } else {
            vals.push(item.clone());
            counts.push(1);
        }
    }
    return Ok(qlist!(
        vals.into_iter().zip(counts.into_iter())
        .map(|(v, c)| qlist!(vec![v, qint!(c as i64)]))
        .collect()
    ));
}

/// Count elements of a data set as probabilities in a histogram. Returns a list
/// containins pairs where, in each pair, the first element is a value and the
/// second is the number of times it occurs in the data set, normalized to the
/// length of the data set.
/// Alias: `|p|`
///
/// Expected form:
/// `(hist-prob <data>)`
///
/// Example:
/// ```text
/// q>> (hist-prob ("a" "b" "c" "a"))
/// q>> ; evaluates to (("a" 0.5) ("b" 0.25) ("c" 0.25))
/// ```
#[builtin(name = "hist-prob", alias = "|p|")]
pub fn fn_histogram_prob(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    fn index_of(vals: &[QExp], item: &QExp) -> Option<usize> {
        for (k, it) in vals.iter().enumerate() {
            if it == item {
                return Some(k);
            }
        }
        return None;
    }

    if args.len() != 1 {
        return Err(qerr_fmt!(
            "histogram-prob: expected 1 arg but got {}", args.len()));
    }
    let data: Vec<QExp>
        = match env.eval(args.get(0).unwrap())? {
            qlist!(l) => Ok(l),
            _ => Err(qerr!("histogram-prob: arg must be a list")),
        }?;
    let mut vals: Vec<QExp> = Vec::new();
    let mut counts: Vec<usize> = Vec::new();
    for item in data.iter() {
        if let Some(k) = index_of(&vals, item) {
            counts[k] += 1;
        } else {
            vals.push(item.clone());
            counts.push(1);
        }
    }
    let N: f64 = data.len() as f64;
    return Ok(qlist!(
        vals.into_iter().zip(counts.into_iter())
        .map(|(v, c)| qlist!(vec![v, qfloat!(c as f64 / N)]))
        .collect()
    ));
}

/// Calculates the NxN covariance matrix for N-dimensional data, passed as a
/// list of N-item lists of numbers.
/// Alias: `Cov`
///
/// Expected form:
/// `(covariance <data>)`
///
/// Example:
/// ```text
/// q>> (covariance ((1 2) (1 1) (2 5) (2 4)))
/// q>> ; evaluates to ((0.25 0.75) (0.75 2.5))
/// ```
#[builtin(name = "covariance", alias = "Cov")]
pub fn fn_covariance(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    fn mean_qexp<'a, I>(vals: I, ret_type: QExpType) -> QResult<QExp>
    where I: IntoIterator<Item = &'a QExp>
    {
        let mut N: i64 = 0;
        let mut acc = QExp::zero(ret_type)?;
        for qk in vals.into_iter() {
            acc = acc.add(qk)?;
            N += 1;
        }
        return acc.div(&qint!(N));
    }

    if args.len() != 1 {
        return Err(qerr_fmt!(
            "covariance: expected 1 arg but got {}", args.len()));
    }
    let data: Vec<QExp> = match env.eval(args.get(0).unwrap())? {
        qlist!(l) => Ok(l),
        _ => Err(qerr!("covariance: arg must be a list of n-dimensional data")),
    }?;
    let N: usize = match data.first() {
        Some(qlist!(l)) => Ok(l.len()),
        Some(_)
            => Err(qerr!("covariance: items in arg must be lists of numbers")),
        None => Err(qerr!("covariance: expected non-empty data set")),
    }?;
    let mut series: Vec<Vec<QExp>> = (0..N).map(|_| Vec::new()).collect();
    let mut ret_type: QExpType = qint!();
    for X in data.into_iter() {
        if let qlist!(l) = X {
            if l.len() == N {
                l.into_iter().zip(series.iter_mut())
                    .map(|(qk, ser)| {
                        ret_type = cmp::max(ret_type, qk.exp_type());
                        if ret_type <= qcomplex!() {
                            ser.push(qk);
                            Ok(())
                        } else {
                            Err(qerr!(
                                "covariance: encountered non-numerical value."
                            ))
                        }
                    })
                    .collect::<QResult<Vec<()>>>()?;
            } else {
                return Err(qerr!(
                    "covariance: all entries in the data set must be of the \
                    same dimension"
                ));
            }
        } else {
            return Err(qerr!(
                "covariance: all entries in the data set must be lists of \
                numbers"
            ));
        }
    }
    let mut X: &Vec<QExp>;
    let mut Y: Vec<QExp>;
    let mut XY: Vec<QExp>;
    let mut E_X: QExp;
    let mut E_Y: QExp;
    let mut E_XY: QExp;
    let mut covar: Vec<Vec<QExp>> = Vec::new();
    let mut row: Vec<QExp> = Vec::new();
    for i in 0..N {
        for j in 0..N {
            X = series.get(i).unwrap();
            Y = series.get(j).unwrap()
                .iter()
                .map(|yk| yk.conj())
                .collect::<QResult<Vec<QExp>>>()?;
            XY = X.iter().zip(Y.iter())
                .map(|(xk, yk)| xk.mul(yk))
                .collect::<QResult<Vec<QExp>>>()?;
            E_X = mean_qexp(X, ret_type)?;
            E_Y = mean_qexp(&Y, ret_type)?;
            E_XY = mean_qexp(&XY, ret_type)?;
            row.push(E_XY.sub(&E_X.mul(&E_Y)?)?);
        }
        covar.push(mem::take(&mut row));
    }
    return Ok(qlist!(covar.into_iter().map(|ci| qlist!(ci)).collect()));
}

/// Calculates the NxN Pearson correlation matrix for N-dimensional data, passed
/// as a list of N-item lists of numbers.
/// Alias: `Corr`
///
/// Expected form:
/// `(correlation <data>)`
///
/// Example:
/// ```text
/// q>> (correlation ((1 2) (1 1) (2 5) (2 4)))
/// q>> ; evaluates to ((1 0.948) (0.948 1))
/// ```
#[builtin(name = "correlation", alias = "Corr")]
pub fn fn_correlation(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    fn mean_qexp<'a, I>(vals: I, ret_type: QExpType) -> QResult<QExp>
    where I: IntoIterator<Item = &'a QExp>
    {
        let mut N: i64 = 0;
        let mut acc = QExp::zero(ret_type)?;
        for qk in vals.into_iter() {
            acc = acc.add(qk)?;
            N += 1;
        }
        return acc.div(&qint!(N));
    }

    fn stddev_qexp<'a, I>(vals: I, mean: &QExp, ret_type: QExpType)
        -> QResult<QExp>
    where I: IntoIterator<Item = &'a QExp>
    {
        let mut N: i64 = 0;
        let mut acc = QExp::zero(ret_type)?;
        for qk in vals.into_iter() {
            acc = acc.add(&qk.sub(mean)?.abs()?.pow(&qint!(2))?)?;
            N += 1;
        }
        return acc.div(&qint!(N))?.sqrt();
    }

    if args.len() != 1 {
        return Err(qerr_fmt!(
            "correlation: expected 1 arg but got {}", args.len()));
    }
    let data: Vec<QExp> = match env.eval(args.get(0).unwrap())? {
        qlist!(l) => Ok(l),
        _ => Err(qerr!(
            "correlation: arg must be a list of n-dimensional data")),
    }?;
    let N: usize = match data.first() {
        Some(qlist!(l)) => Ok(l.len()),
        Some(_)
            => Err(qerr!("correlation: items in arg must be lists of numbers")),
        None => Err(qerr!("correlation: expected non-empty data set")),
    }?;
    let mut series: Vec<Vec<QExp>> = (0..N).map(|_| Vec::new()).collect();
    let mut ret_type: QExpType = qint!();
    for X in data.into_iter() {
        if let qlist!(l) = X {
            if l.len() == N {
                l.into_iter().zip(series.iter_mut())
                    .map(|(qk, ser)| {
                        ret_type = cmp::max(ret_type, qk.exp_type());
                        if ret_type <= qcomplex!() {
                            ser.push(qk);
                            Ok(())
                        } else {
                            Err(qerr!(
                                "correlation: encountered non-numerical value."
                            ))
                        }
                    })
                    .collect::<QResult<Vec<()>>>()?;
            } else {
                return Err(qerr!(
                    "correlation: all entries in the data set must be of the \
                    same dimension"
                ));
            }
        } else {
            return Err(qerr!(
                "correlation: all entries in the data set must be lists of \
                numbers"
            ));
        }
    }
    let mut X: &Vec<QExp>;
    let mut Y: Vec<QExp>;
    let mut XY: Vec<QExp>;
    let mut E_X: QExp;
    let mut E_Y: QExp;
    let mut E_XY: QExp;
    let mut sig_X: QExp;
    let mut sig_Y: QExp;
    let mut corr: Vec<Vec<QExp>> = Vec::new();
    let mut row: Vec<QExp> = Vec::new();
    for i in 0..N {
        for j in 0..N {
            if i == j {
                row.push(QExp::one(ret_type)?);
                continue;
            }
            X = series.get(i).unwrap();
            Y = series.get(j).unwrap()
                .iter()
                .map(|yk| yk.conj())
                .collect::<QResult<Vec<QExp>>>()?;
            XY = X.iter().zip(Y.iter())
                .map(|(xk, yk)| xk.mul(yk))
                .collect::<QResult<Vec<QExp>>>()?;
            E_X = mean_qexp(X, ret_type)?;
            E_Y = mean_qexp(&Y, ret_type)?;
            E_XY = mean_qexp(&XY, ret_type)?;
            sig_X = stddev_qexp(X, &E_X, ret_type)?;
            sig_Y = stddev_qexp(&Y, &E_Y, ret_type)?;
            row.push(
                E_XY.sub(&E_X.mul(&E_Y)?)?
                    .div(&sig_X.mul(&sig_Y)?)?
            );
        }
        corr.push(mem::take(&mut row));
    }
    return Ok(qlist!(corr.into_iter().map(|ci| qlist!(ci)).collect()));
}

// pub fn eval_fft(&mut self, args: &[QExp]) -> QResult<QExp>;
// pub fn eval_ifft(&mut self, args: &[QExp]) -> QResult<QExp>;

/*
 * many/list -> value math
 */

/// Finds the mean of a (scalar) data set.
///
/// Expected form:
/// `(mean <data>)`
///
/// Example:
/// ```text
/// q>> (mean (1 6 3 5 7 9 0 4 2 6 4 3 7)) ; evaluates to 4.384615384615385
/// ```
#[builtin(name = "mean")]
pub fn fn_mean(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    fn do_mean(args: &[QExp]) -> QResult<QExp> {
        if args.len() == 1 {
            if let Some(qlist!(l)) = args.first() {
                return do_mean(l);
            }
        }
        let (nums, ret_type): (Vec<QExp>, QExpType)
            = convert_numbers_sametype(args)?;
        let N: usize = nums.len();
        let mut acc = QExp::zero(ret_type)?;
        for x in nums.into_iter() {
            acc = acc.add(&x)?;
        }
        return acc.div(&qint!(N as i64));
    }
    return do_mean(&env.eval_multi(args)?);
}

/// Finds the variance of a (scalar) data set.
/// Alias: `Var`
///
/// Expected form:
/// `(variance <data>)`
///
/// Example:
/// ```text
/// q>> (variance (1 6 3 5 7 9 0 4 2 6 4 3 7)) ; evaluates to 6.236686390532544
/// ```
#[builtin(name = "variance", alias = "Var")]
pub fn fn_variance(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    fn do_variance(args: &[QExp]) -> QResult<QExp> {
        if args.len() == 1 {
            if let Some(qlist!(l)) = args.first() {
                return do_variance(l);
            }
        }
        let (nums, ret_type): (Vec<QExp>, QExpType)
            = convert_numbers_sametype(args)?;
        let N: i64 = nums.len() as i64;
        // mean
        let mut acc = QExp::zero(ret_type)?;
        for x in nums.iter() {
            acc = acc.add(x)?;
        }
        let mean: QExp = acc.div(&qint!(N))?;
        // var
        let mut acc = QExp::zero(ret_type)?;
        for x in nums.iter() {
            acc = acc.add(&x.sub(&mean)?.abs()?.pow(&qint!(2))?)?;
        }
        return acc.div(&qint!(N));
    }
    return do_variance(&env.eval_multi(args)?);
}

/// Finds the standard deviation of a (scalar) data set.
/// Alias: `Std`
///
/// Expected form:
/// `(stddev <data>)`
///
/// Example:
/// ```text
/// q>> (stddev (1 6 3 5 7 9 0 4 2 6 4 3 7)) ; evaluates to 2.497335858576604
/// ```
#[builtin(name = "stddev", alias = "Std")]
pub fn fn_stddev(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    fn do_stddev(args: &[QExp]) -> QResult<QExp> {
        if args.len() == 1 {
            if let Some(qlist!(l)) = args.first() {
                return do_stddev(l);
            }
        }
        let (nums, ret_type): (Vec<QExp>, QExpType)
            = convert_numbers_sametype(args)?;
        let N: i64 = nums.len() as i64;
        // mean
        let mut acc = QExp::zero(ret_type)?;
        for x in nums.iter() {
            acc = acc.add(x)?;
        }
        let mean: QExp = acc.div(&qint!(N))?;
        // var
        let mut acc = QExp::zero(ret_type)?;
        for x in nums.iter() {
            acc = acc.add(&x.sub(&mean)?.abs()?.pow(&qint!(2))?)?;
        }
        let var: QExp = acc.div(&qint!(N))?;
        return var.sqrt();
    }
    return do_stddev(&env.eval_multi(args)?);
}

/*
 * parameterized list -> value math
 */

/// Finds the p-norm of an N-dimensional value (passed as a list of numbers) for
/// given p.
/// Alias: `|+|`
///
/// Expected form:
/// `(pnorm <p> <value>)`
///
/// Example:
/// ```text
/// q>> (pnorm 2 (3 4)) ; evaluates to 5.0
/// q>> (pnorm 1 (3 4)) ; evaluates to 7.0
/// ```
#[builtin(name = "pnorm", alias = "|+|")]
pub fn fn_pnorm(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 2 {
        return Err(qerr_fmt!(
            "pnorm: expected 2 args but got {}", args.len()));
    }
    let parg: QExp = env.eval(args.get(0).unwrap())?;
    let p: QExp
        = convert_type_num(&parg, cmp::max(parg.exp_type(), qint!()))
        .map_err(|e| e.prepend_source("pnorm"))?;
    if p.is_zero() {
        return Err(qerr!("pnorm: p value cannot be zero"));
    }
    let (nums, nums_type): (Vec<QExp>, QExpType)
        = match env.eval(args.get(1).unwrap())? {
            qlist!(l) => convert_numbers_sametype(&l),
            _ => Err(qerr!("second arg must be a list of numbers")),
        }
        .map_err(|e| e.prepend_source("pnorm"))?;
    let mut acc = QExp::zero(nums_type)?;
    for x in nums.iter() {
        acc = acc.add(&x.abs()?.pow(&p)?)?;
    }
    let pnorm: QExp = acc.pow(&p.recip()?)?;
    return if let qcomplex!(c) = pnorm {
        Ok(qfloat!(c.re))
    } else {
        Ok(pnorm)
    }
}

/// Finds the N-th moment of a (scalar) data set for given N.
///
/// Expected form:
/// `(moment <N> <data>)`
///
/// Example:
/// ```text
/// q>> (moment 0 (1 6 3 5 7 9 0 4 2 6 4 3 7)) ; evaluates to 1.0
/// q>> (moment 1 (1 6 3 5 7 9 0 4 2 6 4 3 7)) ; evaluates to 4.384615384615385
/// q>> (moment 2 (1 6 3 5 7 9 0 4 2 6 4 3 7)) ; evaluates to 25.46153846153846
/// ```
#[builtin(name = "moment")]
pub fn fn_moment(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 2 {
        return Err(qerr_fmt!(
            "moment: expected 2 args but got {}", args.len()));
    }
    let narg: QExp = env.eval(args.get(0).unwrap())?;
    let n: QExp
        = convert_type_num(&narg, cmp::max(narg.exp_type(), qint!()))
        .map_err(|e| e.prepend_source("moment"))?;
    let (nums, nums_type): (Vec<QExp>, QExpType)
        = match env.eval(args.get(1).unwrap())? {
            qlist!(l) => convert_numbers_sametype(&l),
            _ => Err(qerr!("second arg must be a list of numbers")),
        }
        .map_err(|e| e.prepend_source("moment"))?;
    let N: usize = nums.len();
    let mut acc = QExp::zero(nums_type)?;
    for x in nums.iter() {
        acc = acc.add(&x.pow(&n)?)?;
    }
    return acc.div(&qint!(N as i64));
}

/*
 * special-arg math
 */

// pub fn eval_sample(&mut self, args: &[QExp]) -> QResult<QExp>;

/*
 * phys module
 */

/// Convert a wavelength (in nm) to a three-element, RGB color list, with each
/// component's intensity on a [0, 255] scale.
///
/// Expected form:
/// `(phys::nmrgb <wavelengths>...)`
#[builtin(name = "nmrgb")]
pub fn phys_nmrgb(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    fn do_nmrgb(nm: f64) -> (f64, f64, f64) {
        const gamma: f64 = 0.80;
        const intensity_max: f64 = 255.0;
        let (mut r, mut g, mut b): (f64, f64, f64)
            = if (380.0_f64..440.0).contains(&nm) {
                (-(nm - 440.0) / (440.0 - 380.0), 0.0, 1.0)
            } else if (440.0_f64..490.0).contains(&nm) {
                (0.0, (nm - 440.0) / (490.0 - 440.0), 1.0)
            } else if (490.0_f64..510.0).contains(&nm) {
                (0.0, 1.0, -(nm - 510.0) / (510.0 - 490.0))
            } else if (510.0_f64..580.0).contains(&nm) {
                ((nm - 510.0) / (580.0 - 510.0), 1.0, 0.0)
            } else if (580.0_f64..645.0).contains(&nm) {
                (1.0, -(nm - 645.0) / (645.0 - 580.0), 0.0)
            } else if (645.0_f64..781.0).contains(&nm) {
                (1.0, 0.0, 0.0)
            } else {
                (0.0, 0.0, 0.0)
            };
        let factor: f64
            = if (380.0_f64..420.0).contains(&nm) {
                0.3 + 0.7 * (nm - 380.0) / (420.0 - 380.0)
            } else if (420.0_f64..701.0).contains(&nm) {
                1.0
            } else if (701.0_f64..781.0).contains(&nm) {
                0.3 + 0.7 * (781.0 - nm) / (781.0 - 701.0)
            } else {
                0.0
            };
        r = intensity_max * (factor * r).powf(gamma);
        g = intensity_max * (factor * g).powf(gamma);
        b = intensity_max * (factor * b).powf(gamma);
        return (r, g, b);
    }

    if args.is_empty() {
        return Err(qerr_fmt!(
            "nmrgb: expected at least 1 arg but got {}", args.len()));
    }
    let rgb: Vec<QExp>
        = env.eval_multi(args)?
        .into_iter()
        .map(|qk| match qk {
            qbool!(b) => Ok(if b { 1.0 } else { 0.0 }),
            qint!(i) => Ok(i as f64),
            qfloat!(f) => Ok(f),
            _ => Err(qerr!("nmrgb: args must be real numbers")),
        })
        .collect::<QResult<Vec<f64>>>()?
        .into_iter()
        .map(do_nmrgb)
        .map(|(r, g, b)| qlist!(vec![qfloat!(r), qfloat!(g), qfloat!(b)]))
        .collect();
    return if rgb.len() == 1 {
        Ok(rgb.into_iter().next().unwrap())
    } else {
        Ok(qlist!(rgb))
    };
}

/// Compute the coefficient generated by the raising operator.
///
/// Expected form:
/// `(phys::qraise <J> <mJ>)`
#[builtin(name = "qraise")]
pub fn phys_qraise(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 2 {
        return Err(qerr_fmt!(
            "qraise: expected 2 args but got {}", args.len()));
    }
    let J: f64
        = match convert_type_num(&env.eval(args.get(0).unwrap())?, qfloat!()) {
            Ok(qfloat!(f)) => Ok(f),
            _ => Err(qerr!("invalid type in qraise")),
        }?;
    let mJ: f64
        = match convert_type_num(&env.eval(args.get(1).unwrap())?, qfloat!()) {
            Ok(qfloat!(f)) => Ok(f),
            _ => Err(qerr!("invalid type in qraise")),
        }?;
    return if (-J..=J).contains(&mJ) {
        Ok(qfloat!(((J - mJ) * (J + mJ + 1.0)).sqrt()))
    } else {
        Ok(qfloat!(0.0))
    };
}

/// Compute the coefficient generated by the lowering operator.
///
/// Expected form:
/// `(phys::qlower <J> <mJ>)`
#[builtin(name = "qlower")]
pub fn phys_qlower(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 2 {
        return Err(qerr_fmt!(
            "qlower: expected 2 args but got {}", args.len()));
    }
    let J: f64
        = match convert_type_num(&env.eval(args.get(0).unwrap())?, qfloat!()) {
            Ok(qfloat!(f)) => Ok(f),
            _ => Err(qerr!("invalid type in qlower")),
        }?;
    let mJ: f64
        = match convert_type_num(&env.eval(args.get(1).unwrap())?, qfloat!()) {
            Ok(qfloat!(f)) => Ok(f),
            _ => Err(qerr!("invalid type in qlower")),
        }?;
    return if (-J..=J).contains(&mJ) {
        Ok(qfloat!(((J + mJ) * (J - mJ + 1.0)).sqrt()))
    } else {
        Ok(qfloat!(0.0))
    };
}

