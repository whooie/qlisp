use std::{
    cmp,
    fs::File,
    io::Read,
    mem,
    path::PathBuf,
    rc::Rc,
};
use itertools::Itertools;
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
        QLambda,
        QEnv,
        QEnvEntry,
        Indexable,
        convert_type,
        convert_type_num,
        convert_numbers_sametype,
        typecast,
        FormatX,
    },
    repl::run_repl,
};

/*
 * special
 */

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

pub fn fn_interact(env: &mut QEnv, _args: &[QExp]) -> QResult<QExp> {
    run_repl(env);
    return Ok(qint!(0));
}

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
        = fmtstr.formatx(&vals)
        .map_err(|e| e.prepend_source("format"))?;
    return Ok(qstr!(formatted));
}

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
        = fmtstr.formatx(&vals)
        .map_err(|e| e.prepend_source("print"))?;
    print!("{}", formatted);
    std::io::Write::flush(&mut std::io::stdout()).unwrap();
    return if vals.len() == 1 {
        Ok(vals.into_iter().next().unwrap())
    } else {
        Ok(qlist!(vals))
    };
}

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
        = fmtstr.formatx(&vals)
        .map_err(|e| e.prepend_source("println"))?;
    println!("{}", formatted);
    std::io::Write::flush(&mut std::io::stdout()).unwrap();
    return if vals.len() == 1 {
        Ok(vals.into_iter().next().unwrap())
    } else {
        Ok(qlist!(vals))
    };
}

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
        = fmtstr.formatx(&vals)
        .map_err(|e| e.prepend_source("halt"))?;
    return Err(qerr_fmt!("halt: {}", formatted));
}

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

pub fn fn_bool(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    return typecast(qbool!(), &env.eval_multi(args)?);
}

pub fn fn_int(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    return typecast(qint!(), &env.eval_multi(args)?);
}

pub fn fn_float(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    return typecast(qfloat!(), &env.eval_multi(args)?);
}

pub fn fn_complex(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    return typecast(qcomplex!(), &env.eval_multi(args)?);
}

pub fn fn_list(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    return Ok(qlist!(env.eval_multi(args)?));
}

pub fn fn_str(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    return typecast(qstr!(), &env.eval_multi(args)?);
}

/*
 * arithmetic
 */

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
        qfunc!(_, f) => idxable.fold(&start, |args: &[QExp]| f(env, args)),
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

pub fn fn_select_by(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
   if args.len() != 2 {
       return Err(qerr_fmt!(
            "select-by: expected 2 args but got {}", args.len()));
   }
   let idxable
       = Indexable::from_qexp(env.eval(args.get(1).unwrap())?)
       .map_err(|_| qerr!("select-by: second arg must be a list or str"))?;
    return match env.eval(args.get(0).unwrap())? {
        qfunc!(_, f) => {
            idxable.select_by(|args: &[QExp]| f(env, args))
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
        qfunc!(_, f) => {
            idxable.take_while(|args: &[QExp]| f(env, args))
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
        qfunc!(_, f) => {
            idxable.skip_while(|args: &[QExp]| f(env, args))
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

pub fn fn_map(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 2 {
        return Err(qerr_fmt!(
            "map: expected 2 args but got {}", args.len()));
    }
    let idxable
        = Indexable::from_qexp(env.eval(args.get(1).unwrap())?)
        .map_err(|_| qerr!("map: second arg must be a list or str"))?;
    return match env.eval(args.get(0).unwrap())? {
        qfunc!(_, f) => {
            idxable.map(|args: &[QExp]| f(env, args))
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

pub fn fn_filter(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 2 {
        return Err(qerr_fmt!(
            "filter: expected 2 args but got {}", args.len()));
    }
    let idxable
        = Indexable::from_qexp(env.eval(args.get(1).unwrap())?)
        .map_err(|_| qerr!("filter: second arg must be a list or str"))?;
    return match env.eval(args.get(0).unwrap())? {
        qfunc!(_, f) => {
            idxable.filter(|args: &[QExp]| f(env, args))
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

pub fn fn_sort_by(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 2 {
        return Err(qerr_fmt!(
            "sort-by: expected 2 args but got {}", args.len()));
    }
    let idxable
        = Indexable::from_qexp(env.eval(args.get(1).unwrap())?)
        .map_err(|_| qerr!("sort-by: second arg must be a list or str"))?;
    return match env.eval(args.get(0).unwrap())? {
        qfunc!(_, f) => {
            idxable.sort_by(|args: &[QExp]| f(env, args))
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
        qfunc!(_, f) => {
            idxable.split_on(|args: &[QExp]| f(env, args))
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
        qfunc!(_, f) => {
            idxable.split_on_inc(|args: &[QExp]| f(env, args))
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
    ( $name:ident, $inner:ident, $method:ident ) => {
        pub fn $name(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
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
            return $inner(&env.eval_multi(args)?);
        }
    }
);

elementwise_fn!(fn_neg,    do_neg,     neg  );
elementwise_fn!(fn_recip,  do_recip,   recip);
elementwise_fn!(fn_abs,    do_abs,     abs  );
elementwise_fn!(fn_sqrt,   do_sqrt,    sqrt );
elementwise_fn!(fn_cbrt,   do_cbrt,    cbrt );
elementwise_fn!(fn_exp,    do_exp,     exp  );
elementwise_fn!(fn_floor,  do_floor,   floor);
elementwise_fn!(fn_ceil,   do_ceil,    ceil );
elementwise_fn!(fn_round,  do_round,   round);
elementwise_fn!(fn_ln,     do_ln,      ln   );
elementwise_fn!(fn_sin,    do_sin,     sin  );
elementwise_fn!(fn_cos,    do_cos,     cos  );
elementwise_fn!(fn_tan,    do_tan,     tan  );
elementwise_fn!(fn_asin,   do_asin,    asin );
elementwise_fn!(fn_acos,   do_acos,    acos );
elementwise_fn!(fn_atan,   do_atan,    atan );

pub fn fn_atan2(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    fn do_atan2(args: &[QExp]) -> QResult<QExp> {
        if args.len() == 1 {
            if let Some(qlist!(l)) = args.first() {
                if l.len() == 2 && l.first().unwrap().exp_type() != qlist!() {
                    return args.first().unwrap().atan2();
                } else {
                    return do_atan2(l);
                }
            }
            return args.get(0).unwrap().atan2();
        } else {
            let new: Vec<QExp>
                = args.iter()
                .map(|x| x.atan2())
                .collect::<QResult<Vec<QExp>>>()?;
            return Ok(qlist!(new));
        }
    }
    return do_atan2(&env.eval_multi(args)?);
}

elementwise_fn!(fn_sinh,   do_sinh,    sinh );
elementwise_fn!(fn_cosh,   do_cosh,    cosh );
elementwise_fn!(fn_tanh,   do_tanh,    tanh );
elementwise_fn!(fn_asinh,  do_asinh,   asinh);
elementwise_fn!(fn_acosh,  do_acosh,   acosh);
elementwise_fn!(fn_atanh,  do_atanh,   atanh);
elementwise_fn!(fn_arg,    do_arg,     arg  );
elementwise_fn!(fn_cis,    do_cis,     cis  );
elementwise_fn!(fn_conj,   do_conj,    conj );
elementwise_fn!(fn_real,   do_real,    real );
elementwise_fn!(fn_imag,   do_imag,    imag );

/*
 * parameterized element-wise math
 */

macro_rules! param_elementwise_fn(
    (
        $name:ident,
        $err_prepend:literal,
        $elem_arg:literal,
        $param_arg:literal,
        $method:ident,
        $min_type:expr,
        $max_type:expr
    ) => {
        pub fn $name(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
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

param_elementwise_fn!(fn_mod,  "mod",  0,  1,  modulo, qint!(),    qfloat!()  );
param_elementwise_fn!(fn_log,  "log",  1,  0,  log,    qint!(),    qcomplex!());
param_elementwise_fn!(fn_pow,  "pow",  0,  1,  pow,    qint!(),    qcomplex!());

/*
 * list -> list math
 */

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

