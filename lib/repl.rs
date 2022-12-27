use std::{
    borrow::Cow,
    mem,
    str::FromStr,
};
use crossterm::event::{
    Event as CTEvent,
    KeyEvent,
    KeyCode,
    KeyModifiers as KeyMod,
    // MouseEvent,
    // MouseEventKind,
};
use nu_ansi_term::{
    Style,
    Color,
};
use num_complex::Complex64 as C64;
use phf::phf_map;
use reedline::{
    self as reed,
    ReedlineEvent as RLEvent,
};
use crate::{
    qerr,
    qerr_fmt,
    qbool,
    qlist,
    qstr,
    qsymbol,
    lang::{
        CONSTS,
        KEYWORDS,
        OPERATORS,
        TYPES,
        SPECIAL,
        PROTECTED,
        QResult,
        QErr,
        QExp,
        QEnv,
        QEnvEntry,
        tokenize,
        TokenState,
        parse,
    },
};

macro_rules! println_flush {
    ( $fmt:literal, $( $val:expr ),* ) => {
        println!($fmt, $( $val, )*);
        std::io::Write::flush(&mut std::io::stdout()).unwrap();
    }
}

static HELP_TEXT: phf::Map<&'static str, &'static str>
    = phf_map! {
/*
 * special functions
 */
        "def"
            => "Assign a value to a symbol.",
        "let"
            => "Assign multiple values to multiple symbols.",
        "fn"
            => "Construct an anonymous function.",
        "defn"
            => "Construct a function and assign it to a symbol.",
        "if"
            => "Condition evaluation.",
        "module"
            => "Construct a module and assign it to a symbol.",
        "use"
            => "Load an external file as a module.",
        "use-all"
            => "Load the contents of an external file.",
        "interact"
            => "Pause execution and launch the REPL.",
        "isdef"
            => "Check whether a symbol is defined.",
        "del"
            => "Remove a definition.",
/*
 * systems
 */
        "format"
            => "Substitute values for patterns in a str.",
        "print"
            => "Print a formatted str to STDOUT.",
        "println"
            => "Print a formatted str to STDOUT with a newline appended.",
        "halt"
            => "Halt execution of a program with an error message.",
        "istype"
            => "Test the type of a value.",
        "type"
            => "Get the type(s) of one or more values.",
/*
 * type-casting
 */
        "bool"
            => "Boolean type.",
        "int"
            => "Integer type.",
        "float"
            => "Floating-point type.",
        "complex"
            => "Complex floating-point type.",
        "list"
            => "List type.",
        "str"
            => "String type.",
/*
 * arithmetic
 */
        "add"
            => "Addition operation.",
        "sub"
            => "subtraction operation.",
        "mul"
            => "Multiplication operation.",
        "div"
            => "Division operation.",
        "idiv"
            => "Integer division operation.",
/*
 * boolean comparisons
 */
        "and"
            => "Logical AND operator.",
        "or"
            => "Logical OR operator.",
        "xor"
            => "Logical XOR operator.",
        "eq"
            => "Equality comparison operator.",
        "neq"
            => "Non-equality comparison operator.",
        "gt"
            => "Greater-than comparison operator.",
        "geq"
            => "Greater-than-or-equal-to comparison operator.",
        "lt"
            => "Less-than comparison operator.",
        "leq"
            => "Less-than-or-equal-to comparison operator.",
/*
 * boolean accumulators
 */
        "all"
            => "Logical AND accumulator.",
        "any"
            => "Logical OR accumulator.",
        "xany"
            => "Logical XOR accumulator",
/*
 * iterable creation
 */
        "range"
            => "Construct a list of integers on a semi-open interval.",
        "range-inc"
            => "Construct a list of integers on a closed interval.",
        "repeat"
            => "Repeat a list or str `n` times.",
/*
 * iterable accumulation
 */
        "length"
            => "Find the length of a list or str.",
        "fold"
            => "Fold elements of a list or str into an accumulator.",
        "min"
            => "Find the minimum of a list or str.",
        "max"
            => "Find the maximum of a list or str.",
        "select-by"
            => "Select an element of a list or str by a comparison function.",
/*
 * iterable slicing and access
 */
        "get"
            => "Get the element of a list or str at an index.",
        "set"
            => "Change the value of an item in a list.",
        "slice"
            => "Slice a list or str over a semi-open range of indices.",
        "slice-inc"
            => "Slice a list or str over a closed range of indices.",
        "slice-by"
            => "Step through a semi-open slice.",
        "slice-inc-by"
            => "Step through a closed slice.",
        "pick"
            => "Select multiple elements from a list or str",
        "first"
            => "Get the first element in a list or str.",
        "rest"
            => "Get everything except the first element in a list or str.",
        "take"
            => "Take the first `n` element of a list or str.",
        "take-while"
            => "Take elements of a list or str satisfying a predicate.",
        "last"
            => "Get the last element of a list or str.",
        "skip"
            => "Discard the first `n` elements of a list or str.",
        "skip-while"
            => "Skip elements of a list or str satisfying a predicate.",
/*
 * iterable transformations
 */
        "step-by"
            => "Step through a list or str.",
        "enumerate"
            => "Convert elements in a list or str to (index, element) pairs.",
        "reverse"
            => "Reverse the order of a list or str.",
        "cycle"
            => "Shift elements in a list or str, wrapping at the ends.",
        "map"
            => "Apply a function to each element of a list or str.",
        "filter"
            => "Take only elements of a list or str satisfying a predicate.",
        "unique"
            => "Reduce a list or str to only its unique elements.",
        "flatten"
            => "Recursively unpack nested lists.",
        "sort"
            => "Sort the elements of a list or str.",
        "sort-by"
            => "Sort the elements of a list or str by a comparison function.",
        "permute"
            => "Perform a series of cyclic permutations on a list or str.",
/*
 * iterable division
 */
        "split-at"
            => "Divide a list or str into two pieces.",
        "split-on"
            => "Divide a list or str into pieces.",
        "split-on-inc"
            => "Divide a list or str into pieces, keeping split elements.",
/*
 * iterable addition
 */
        "append"
            => "Append elements to the end of a list or str.",
        "prepend"
            => "Prepend element to the beginning of a list or str.",
        "insert"
            => "Insert one or more elements into a list or str.",
        "join"
            => "Concatenate lists or strs.",
        "join-with"
            => "Concatenate lists or strs with an item placed at each join.",
        "zip"
            => "Combine lists or strs element-wise.",
        "cart"
            => "Cartesian product.",
/*
 * iterable testing
 */
        "contains"
            => "Determine whether a list or str contains a particular element.",
        "index-of"
            => "Find the first occurrence of an element in a list or str",
/*
 * element-wise math
 */
        "neg"
            => "Logical and arithmetic negative.",
        "recip"
            => "Scalar multiplicative inverse.",
        "abs"
            => "Absolute value operation.",
        "sqrt"
            => "Square-root operation.",
        "cbrt"
            => "Cube-root operation.",
        "exp"
            => "Exponential function.",
        "floor"
            => "Floor operation.",
        "ceil"
            => "Ceiling operation.",
        "round"
            => "Round to the nearest int.",
        "ln"
            => "Natural logarithm function.",
        "sin"
            => "Sine function.",
        "cos"
            => "Cosine function.",
        "tan"
            => "Tangent function.",
        "arcsin"
            => "Arcsine function.",
        "arccos"
            => "Arccosine function.",
        "arctan"
            => "Arctangent function.",
        "arctan2"
            => "Four-quadrant arctangent function.",
        "sinh"
            => "Hyperbolic sine function.",
        "cosh"
            => "Hyperbolic cosine function.",
        "tanh"
            => "Hyperbolic tangent function.",
        "arsinh"
            => "Area hyperbolic sine function.",
        "arcosh"
            => "Area hyperbolic cosine function.",
        "artanh"
            => "Area hyperbolilc tangent function.",
        "arg"
            => "Complex number argument.",
        "cis"
            => "Cis function.",
        "conj"
            => "Complex conjugate operation.",
        "real"
            => "Real part of a number.",
        "imag"
            => "Imaginary part of a number.",
/*
 * parameterized element-wise math
 */
        "mod"
            => "Modulo operator.",
        "log"
            => "Logarithm operation.",
        "pow"
            => "Exponentiation with arbitrary base.",
        "shl"
            => "Bit-shift left.",
        "shr"
            => "Bit-shift right.",
/*
 * list -> list math
 */
        "convolve"
            => "Discrete-valued convolution.",
        "hist"
            => "Count elements of a data set in a histogram.",
        "hist-prob"
            => "Count elements of a data set as probabilities in a histogram.",
        "covariance"
            => "Covariance matrix for N-dimensional data.",
        "correlation"
            => "Pearson correlation matrix for N-dimensional data.",
        // fft
        // ifft
/*
 * list -> value math
 */
        "mean"
            => "Mean of a data set.",
        "variance"
            => "Variance of a data set.",
        "stddev"
            => "Standard deviation of a data set.",
/*
 * list+1 -> value math
 */
        "pnorm"
            => "P-norm of a N-dimensional value.",
        "moment"
            => "N-th moment of a data set.",
/*
 * special-arg math
 */
        // sample
/*
 * REPL-only
 */

//         "vars" => (
//             "List all defined symbols in an environment.",
// "
// List all defined symbols as strings within the local environment or any loaded
// modules. REPL-only.
//
// Expected form:
// (vars [<modules>...])
//
// Example:
// q>> (vars) ; list all symbols in the local environment
// q>> (module my-module (...))
// q>> (vars my-module) ; list all symbols in my-module
// ",
//         ),
//
//         "defs" => (
//             "List all non-default definitions in an environment.",
// "
// List all user-defined symbols as strings within the local environment or any
// loaded modules. REPL-only.
//
// Expected form:
// (defs [<modules>...])
//
// Example:
// q>> (defs) ; list all user-defined symbols in the local environment
// q>> (module my-module (...))
// q>> (defs my-module) ; list all user-defined symbols in my-module
// ",
//         ),

    };

static FUNCTION_SYMBOLS: phf::Map<&'static str, &'static str> = phf_map! {
    // special functions -- keyword-like
    ":="    => "def",
    "*:="   => "let",
    "@:"    => "fn",
    "@:="   => "defn",
    "use*"  => "use-all",
    "?:="   => "isdef",
    "!-"    => "del",
    // systems
    "$"     => "format",
    "$-"    => "print",
    "$_"    => "println",
    "$<"    => "read",
    "$<:"   => "readlines",
    "$|"    => "with-file",
    "$|+"   => "with-file-add",
    "$>-"   => "write",
    "$>_"   => "writeln",
    "$>>-"  => "write-flush",
    "$>>_"  => "writeln-flush",
    "!!"    => "halt",
    "~?"    => "istype",
    "?~"    => "type",
    // arithmetic
    "+"     => "add",
    "-"     => "sub",
    "*"     => "mul",
    "/"     => "div",
    "//"    => "idiv",
    // boolean comparisons
    "&&"    => "and",
    "||"    => "or",
    "^"     => "xor",
    "="     => "eq",
    "!="    => "neq",
    ">"     => "gt",
    ">="    => "geq",
    "<"     => "lt",
    "<="    => "leq",
    // boolean accumulators
    "&&*"   => "all",
    "||*"   => "any",
    "^*"    => "xany",
    // iterable creation
    ".."    => "range",
    "..="   => "range-inc",
    "#="    => "repeat",
    // iterable accumulation
    "#"     => "length",
    "@."    => "fold",
    "<:"    => "min",
    ":>"    => "max",
    "*@."   => "select-by",
    // iterable slicing and access
    "."     => "get",
    ".:="   => "set",
    "--"    => "slice",
    "--="   => "slice-inc",
    "~~"    => "slice-by",
    "~~="   => "slice-inc-by",
    ".*"    => "pick",
    ".-"    => "first",
    ".!-"   => "rest",
    "~."    => "take",
    "~.@"   => "take-while",
    "-."    => "last",
    ".~"    => "skip",
    ".~@"   => "skip-while",
    // iterable transformation
    "~"     => "step-by",
    "##"    => "enumerate",
    "<>"    => "reverse",
    "<#>"   => "cycle",
    "@"     => "map",
    "@!"    => "filter",
    "*!="   => "unique",
    "__"    => "flatten",
    "<*"    => "sort",
    "<@"    => "sort-by",
    ".~."   => "permute",
    // iterable division
    "|."    => "split-at",
    "|@"    => "split-on",
    "|@="   => "split-on-inc",
    // iterable addition
    "+."    => "append",
    ".+"    => "prepend",
    "+.+"   => "insert",
    "++"    => "join",
    "+*+"   => "join-with",
    ":~:"    => "zip",
    ":*:"   => "cart",
    // iterable testing
    "*="    => "contains",
    "#*="   => "index-of",
    // element-wise math
    "!"     => "neg",
    "1/"    => "recip",
    "|.|"   => "abs",
    "e**"   => "exp",
    "~_"    => "floor",
    "~^"    => "ceil",
    "~:"    => "round",
    "asin"  => "arcsin",
    "acos"  => "arccos",
    "atan"  => "arctan",
    "asinh" => "arsinh",
    "acosh" => "arcosh",
    "atanh" => "artanh",
    "e**i"  => "cis",
    "~z"    => "conj",
    "Re"    => "real",
    "Im"    => "imag",
    // parameterized element-wise math
    "%"     => "mod",
    "**"    => "pow",
    "<<"    => "shl",
    ">>"    => "shr",
    // list -> list math
    "<:>"   => "convolve",
    "|#|"   => "histogram",
    "|p|"   => "histogram-prob",
    "Cov"   => "covariance",
    "Corr"  => "correlation",
    // "{F}"   => "fft",
    // "{iF}"  => "ifft",
    // list -> value math
    "Var"   => "variance",
    "Std"   => "stddev",
    // list+1 -> value math
    "|+|"   => "pnorm",
    // special-arg math
    // "?."    => "sample",
};

enum ReplOut {
    Print(QExp),
    NoPrint(QExp),
}

impl ReplOut {
    fn as_print(exp: QExp) -> Self { Self::Print(exp) }

    fn as_noprint(exp: QExp) -> Self { Self::NoPrint(exp) }

    fn unpack(self) -> QExp {
        return match self {
            ReplOut::Print(exp) => {
                if let qstr!(s) = &exp {
                    println_flush!("\"{}\"", s);
                } else {
                    println_flush!("{}", exp);
                }
                exp
            },
            ReplOut::NoPrint(exp) => exp,
        };
    }

    fn unpack_unchecked(self) -> QExp {
        return match self {
            ReplOut::Print(exp) | ReplOut::NoPrint(exp) => exp
        };
    }

    fn print(self) -> ReplOut {
        return match self {
            ReplOut::Print(_) => self,
            ReplOut::NoPrint(exp) => ReplOut::Print(exp),
        };
    }

    fn noprint(self) -> ReplOut {
        return match self {
            ReplOut::Print(exp) => ReplOut::NoPrint(exp),
            ReplOut::NoPrint(_) => self,
        };
    }
}

trait ReplEnv {
    const NOPRINT: &'static [&'static str];

    fn repl_eval(&mut self, exp: &QExp) -> QResult<ReplOut>;

    fn repl_eval_forms(&mut self, args: &[QExp]) -> QResult<Vec<ReplOut>>;

    fn repl_parse_eval(&mut self, expr: String) -> QResult<Vec<ReplOut>>;

    fn repl_eval_help(&mut self, args: &[QExp]) -> QResult<QExp>;

    fn repl_eval_vars(&mut self, args: &[QExp]) -> QResult<QExp>;

    fn repl_eval_defs(&mut self, args: &[QExp]) -> QResult<QExp>;

    fn repl_func(&mut self, exp: &QExp, args: &[QExp])
        -> Option<QResult<ReplOut>>;
}

impl<'a> ReplEnv for QEnv<'a> {
    const NOPRINT: &'static [&'static str] = &[
        "help",             "?",
        "def",              ":=",
        "let",              "*:=",
        "defn",             "@:=",
        "print",            "$-",
        "println",          "$_",
        "print-flush",      "$$-",
        "println-flush",    "$$_",
        "halt",             "!!",
        "use",
        "use-all",          "use*",
        "interact",
    ];

    fn repl_eval(&mut self, exp: &QExp) -> QResult<ReplOut> {
        return match exp {
            qlist!(list) => {
                if let Some(first_form) = list.first() {
                    match self.repl_func(first_form, &list[1..]) {
                        Some(res) => res,
                        None => {
                            match first_form {
                                qsymbol!(s) => {
                                    if Self::NOPRINT.contains(&s.as_ref()) {
                                        self.eval(exp)
                                            .map(ReplOut::as_noprint)
                                    } else {
                                        self.eval(exp)
                                            .map(ReplOut::as_print)
                                    }
                                },
                                _ => self.eval(exp).map(ReplOut::as_print),
                            }
                        },
                    }
                } else {
                    self.eval(exp).map(ReplOut::as_print)
                }
            },
            _ => self.eval(exp).map(ReplOut::as_print),
        };
    }

    fn repl_parse_eval(&mut self, expr: String) -> QResult<Vec<ReplOut>> {
        let exps: Vec<QExp> = parse(&tokenize(expr)?)?;
        let evaled: Vec<ReplOut> = self.repl_eval_forms(&exps)?;
        return Ok(evaled);
    }

    fn repl_eval_forms(&mut self, args: &[QExp]) -> QResult<Vec<ReplOut>> {
        return args.iter().map(|x| self.repl_eval(x)).collect();
    }

    fn repl_eval_help(&mut self, args: &[QExp]) -> QResult<QExp> {
        if args.is_empty() {
            let mut topics: Vec<(&'static str, &'static str)>
                = (&HELP_TEXT).into_iter()
                .map(|(t, s)| (*t, *s))
                .collect();
            topics.sort_by(|l, r| l.0.cmp(r.0));
            println_flush!("{}", "-".repeat(80));
            println!(
"Help: Print information on various topics.\n\
Type `(help <topics>...)` for more information on one or more specific topic.\n\
Available topics:"
            );
            for (topic, text) in topics.into_iter() {
                println_flush!("{:>14}    {}", topic, text);
            }
            println_flush!("{}", "-".repeat(80));
            return Ok(qbool!(true));
        }
        for arg in args.iter() {
            println_flush!("{}", "-".repeat(80));
            match arg {
                qstr!(s) | qsymbol!(s) => {
                    if let Some(QEnvEntry::Exp(QExp::Func(f))) = self.get(s) {
                        println_flush!("Help for {}:\n{}", f.name, f.help_text);
                    } else if let Some(unalias) = FUNCTION_SYMBOLS.get(s) {
                        if
                            let Some(QEnvEntry::Exp(QExp::Func(f)))
                                = self.get(unalias)
                        {
                            println_flush!("Help for {} ({}):\n{}",
                                f.alias.unwrap_or(f.name),
                                f.name,
                                f.help_text
                            );
                        } else {
                            panic!("missing alias");
                        }
                    } else {
                        return Err(qerr_fmt!(
                            "help: no help text for symbol '{}'", s));
                    }
                },
                _ => {
                    return Err(qerr!("help: args must be symbols or strs"));
                },
            }
        }
        println_flush!("{}", "-".repeat(80));
        return Ok(qbool!(true));
    }

    fn repl_eval_vars(&mut self, args: &[QExp]) -> QResult<QExp> {
        return if args.is_empty() {
            Ok(qlist!(
                self.symbols().into_iter().map(|s| qstr!(s)).collect()
            ))
        } else {
            let res: Vec<QExp>
                = args.iter()
                .map(|qk| match qk {
                    qsymbol!(s) => match self.get_ok(s) {
                        Ok(QEnvEntry::Mod(m)) => Ok(m.symbols()),
                        Ok(QEnvEntry::Exp(_))
                            => Err(qerr!("vars: args must be loaded modules")),
                        Err(e) => Err(e.prepend_source("vars")),
                    },
                    _ => Err(qerr!("vars: args must be symbols")),
                })
                .collect::<QResult<Vec<Vec<String>>>>()?
                .into_iter()
                .map(|ss| qlist!(ss.into_iter().map(|s| qstr!(s)).collect()))
                .collect();
            if res.len() == 1 {
                Ok(res.into_iter().next().unwrap())
            } else {
                Ok(qlist!(res))
            }
        };
    }

    fn repl_eval_defs(&mut self, args: &[QExp]) -> QResult<QExp> {
        return if args.is_empty() {
            Ok(qlist!(
                self.symbols()
                    .into_iter()
                    .filter_map(|s| {
                        (!PROTECTED.contains(&s.as_ref()))
                            .then_some(qstr!(s))
                    })
                    .collect()
            ))
        } else {
            let res: Vec<QExp>
                = args.iter()
                .map(|qk| match qk {
                    qsymbol!(s) => match self.get_ok(s) {
                        Ok(QEnvEntry::Mod(m)) => {
                            Ok(
                                m.symbols()
                                .into_iter()
                                .filter_map(|s| {
                                    (!PROTECTED.contains(&s.as_ref()))
                                        .then_some(s)
                                })
                                .collect()
                            )
                        },
                        Ok(QEnvEntry::Exp(_))
                            => Err(qerr!("defs: args must be loaded modules")),
                        Err(e) => Err(e.prepend_source("vars")),
                    },
                    _ => Err(qerr!("defs: args must be symbols")),
                })
                .collect::<QResult<Vec<Vec<String>>>>()?
                .into_iter()
                .map(|ss| qlist!(ss.into_iter().map(|s| qstr!(s)).collect()))
                .collect();
            if res.len() == 1 {
                Ok(res.into_iter().next().unwrap())
            } else {
                Ok(qlist!(res))
            }
        };
    }

    fn repl_func(&mut self, exp: &QExp, args: &[QExp])
        -> Option<QResult<ReplOut>>
    {
        return if let qsymbol!(s) = exp {
            match s.as_ref() {
                "help" | "?"
                    => Some(self.repl_eval_help(args).map(ReplOut::as_noprint)),
                "vars"
                    => Some(self.repl_eval_vars(args).map(ReplOut::as_print)),
                "defs"
                    => Some(self.repl_eval_defs(args).map(ReplOut::as_print)),
                _ => None,
            }
        } else {
            None
        }
    }
}

#[derive(Clone)]
pub struct QPrompt { }

impl reed::Prompt for QPrompt {
    fn render_prompt_left(&self) -> Cow<str> { Cow::from("") }

    fn render_prompt_right(&self) -> Cow<str> { Cow::from("") }

    fn render_prompt_indicator(&self, prompt_mode: reed::PromptEditMode)
        -> Cow<str>
    {
        return match prompt_mode {
            reed::PromptEditMode::Default => Cow::from("q>> "),
            reed::PromptEditMode::Emacs => Cow::from("q>> "),
            reed::PromptEditMode::Vi(vimode) => match vimode {
                    reed::PromptViMode::Normal => Cow::from("q|> "),
                    reed::PromptViMode::Insert => Cow::from("q>> "),
            },
            reed::PromptEditMode::Custom(_) => Cow::from("q>> "),
        };
    }

    fn render_prompt_multiline_indicator(&self) -> Cow<str> {
        return Cow::from("... ");
    }

    fn render_prompt_history_search_indicator(
        &self,
        _history_search: reed::PromptHistorySearch,
    ) -> Cow<str>
    {
        return Cow::from("history: ");
    }
}

pub struct QHighlighter { }

impl QHighlighter {
    pub fn new() -> Self { Self { } }
}

macro_rules! hi_style {
    ( $fg:expr, $bg:expr ) => {
        Style {
            foreground: $fg,
            background: $bg,
            is_bold: false,
            is_dimmed: false,
            is_italic: false,
            is_underline: false,
            is_blink: false,
            is_reverse: false,
            is_hidden: false,
            is_strikethrough: false,
        }
    };
    ( $fg:expr, $bg:expr, bold ) => {
        Style {
            foreground: $fg,
            background: $bg,
            is_bold: true,
            is_dimmed: false,
            is_italic: false,
            is_underline: false,
            is_blink: false,
            is_reverse: false,
            is_hidden: false,
            is_strikethrough: false,
        }
    };
    ( $fg:expr, $bg:expr, dimmed ) => {
        Style {
            foreground: $fg,
            background: $bg,
            is_bold: false,
            is_dimmed: true,
            is_italic: false,
            is_underline: false,
            is_blink: false,
            is_reverse: false,
            is_hidden: false,
            is_strikethrough: false,
        }
    };
}
const HI_KEYWORD: Style = hi_style!(Some(Color::LightRed), None);
const HI_OPERATOR: Style = hi_style!(Some(Color::LightRed), None);
const HI_TYPE: Style = hi_style!(Some(Color::White), None, bold);
const HI_SPECIAL: Style = hi_style!(Some(Color::LightMagenta), None);
const HI_CONST: Style = hi_style!(Some(Color::White), None, bold);
const HI_BUILTIN: Style = hi_style!(Some(Color::LightCyan), None);
const HI_STRING: Style = hi_style!(Some(Color::LightYellow), None);
const HI_STRING_ESCAPE: Style = hi_style!(Some(Color::LightCyan), None);
const HI_NUMBER: Style = hi_style!(Some(Color::LightBlue), None);
const HI_COMMA: Style = hi_style!(Some(Color::White), None, dimmed);
const HI_COMMENT: Style = hi_style!(Some(Color::White), None, dimmed);
const HI_PATHSEP: Style = hi_style!(Some(Color::Cyan), None, bold);
const HI_INVALID: Style = hi_style!(Some(Color::White), Some(Color::LightRed), bold);
macro_rules! hi_paren_style {
    ( $r:expr, $g:expr, $b:expr ) => {
        Style {
            foreground: Some(Color::Rgb($r, $g, $b)),
            background: None,
            is_bold: true,
            is_dimmed: false,
            is_italic: false,
            is_underline: false,
            is_blink: false,
            is_reverse: false,
            is_hidden: false,
            is_strikethrough: false,
        }
    }
}
const HI_PAREN: [Style; 8] = [
    hi_paren_style!(255, 255, 255),
    hi_paren_style!(255, 236, 214),
    hi_paren_style!(255, 215, 170),
    hi_paren_style!(255, 181, 112),
    hi_paren_style!(226, 145,  90),
    hi_paren_style!(175, 117, 105),
    hi_paren_style!(120,  95, 115),
    hi_paren_style!( 79,  89, 124),
];
const PLEVEL_MAX: usize = 8;

pub fn push_normal(
    acc: &mut Vec<(Style, String)>,
    term_style: &mut Style,
    term: &mut String,
) {
    if !term.is_empty() {
        match term.as_ref() {
            "i" | "-i" => {
                acc.push((mem::take(term_style), mem::take(term)));
            },
            _ => {
                if CONSTS.contains(&term.as_ref()) {
                    acc.push((HI_CONST, mem::take(term)));
                } else if KEYWORDS.contains(&term.as_ref()) {
                    acc.push((HI_KEYWORD, mem::take(term)));
                } else if OPERATORS.contains(&term.as_ref()) {
                    acc.push((HI_OPERATOR, mem::take(term)));
                } else if TYPES.contains(&term.as_ref()) {
                    acc.push((HI_TYPE, mem::take(term)));
                } else if SPECIAL.contains(&term.as_ref()) {
                    acc.push((HI_SPECIAL, mem::take(term)));
                } else if PROTECTED.contains(&term.as_ref()) {
                    acc.push((HI_BUILTIN, mem::take(term)));
                } else if let Ok(_) = bool::from_str(term) {
                    acc.push((HI_NUMBER, mem::take(term)));
                } else if let Ok(_) = i64::from_str(term) {
                    acc.push((HI_NUMBER, mem::take(term)));
                } else if let Ok(_) = f64::from_str(term) {
                    acc.push((HI_NUMBER, mem::take(term)));
                } else if let Ok(_) = C64::from_str(term) {
                    acc.push((HI_NUMBER, mem::take(term)));
                } else {
                    if term.contains("::") {
                        let style: Style = mem::take(term_style);
                        let pathelems: Vec<String>
                            = mem::take(term).split("::")
                            .map(|s| s.to_string())
                            .collect();
                        let n: usize = pathelems.len();
                        pathelems.into_iter()
                            .enumerate()
                            .for_each(|(k, s)| {
                                if CONSTS.contains(&s.as_ref()) {
                                    acc.push((HI_CONST, s));
                                } else if KEYWORDS.contains(&s.as_ref()) {
                                    acc.push((HI_KEYWORD, s));
                                } else if OPERATORS.contains(&s.as_ref()) {
                                    acc.push((HI_OPERATOR, s));
                                } else if TYPES.contains(&s.as_ref()) {
                                    acc.push((HI_TYPE, s));
                                } else if SPECIAL.contains(&s.as_ref()) {
                                    acc.push((HI_SPECIAL, s));
                                } else if PROTECTED.contains(&s.as_ref()) {
                                    acc.push((HI_BUILTIN, s));
                                } else {
                                    acc.push((style, s));
                                }
                                if k < n - 1 {
                                    acc.push((HI_PATHSEP, "::".into()));
                                }
                            });
                    } else {
                        acc.push((mem::take(term_style), mem::take(term)));
                    }
                }
            }
        }
    }
    *term_style = Style::default();
}

impl reed::Highlighter for QHighlighter {
    fn highlight(&self, line: &str, cursor: usize) -> reed::StyledText {
        if line.is_empty() {
            return reed::StyledText { buffer: vec![] };
        }
        let mut state = TokenState::Normal;
        let mut plevel: usize = 0;
        let mut ret: Vec<(Style, String)> = Vec::new();
        let mut term: String = String::new();
        let mut term_style = Style::default();
        let L: String = line.to_string() + " ";
        let n: usize = L.len();

        let mut paren_score: isize = 0;
        let cursor_paren_l: Option<usize> = {
            let mut k: usize = cursor;
            if &L[k..k + 1] == ")" { paren_score = 1; }
            loop {
                match &L[k..k + 1] {
                    ")" => { paren_score -= 1; },
                    "(" => { paren_score += 1; },
                    _ => { },
                }
                if paren_score == 1 {
                    break Some(k);
                }
                if k == 0 {
                    break None;
                }
                k -= 1;
            }
        };
        paren_score = 0;
        let cursor_paren_r: Option<usize> = {
            let mut k: usize = cursor;
            if &L[k..k + 1] == "(" { paren_score = 1; }
            loop {
                match &L[k..k + 1] {
                    "(" => { paren_score -= 1; },
                    ")" => { paren_score += 1; },
                    _ => { },
                }
                if paren_score == 1 {
                    break Some(k);
                }
                if k == n - 1 {
                    break None;
                }
                k += 1;
            }
        };

        for (k, x) in L.chars().enumerate() {
            match state {
                TokenState::Normal => match x {
                    ';' => {
                        push_normal(&mut ret, &mut term_style, &mut term);
                        state = TokenState::InComment;
                        term_style = HI_COMMENT;
                        term.push(x);
                    },
                    '"' => {
                        push_normal(&mut ret, &mut term_style, &mut term);
                        state = TokenState::InString;
                        term_style = HI_STRING;
                        term.push(x);
                    },
                    '(' | ')' => {
                        push_normal(&mut ret, &mut term_style, &mut term);
                        term_style
                            = if x == ')' {
                                if plevel == 0 {
                                    HI_INVALID
                                } else {
                                    HI_PAREN[(plevel - 1) % PLEVEL_MAX]
                                }
                            } else {
                                HI_PAREN[plevel % PLEVEL_MAX]
                            };
                        if Some(k) == cursor_paren_l
                            || Some(k) == cursor_paren_r
                        {
                            term_style.foreground = Some(Color::LightCyan);
                        }
                        ret.push((
                            mem::take(&mut term_style),
                            x.to_string(),
                        ));
                        plevel = if x == '(' {
                            plevel.checked_add(1).unwrap_or(usize::MAX)
                        } else {
                            plevel.checked_sub(1).unwrap_or(0)
                        }
                    },
                    ' ' | ',' | '\n' | '\t' => {
                        push_normal(&mut ret, &mut term_style, &mut term);
                        if x == ',' {
                            term_style = HI_COMMA;
                        } else {
                            term_style = Style::default();
                        }
                        ret.push((
                            mem::take(&mut term_style),
                            x.to_string(),
                        ));
                    },
                    _ => {
                        term.push(x);
                    },
                },
                TokenState::InComment => match x {
                    '\n' => {
                        push_normal(&mut ret, &mut term_style, &mut term);
                        ret.push((Style::default(), x.to_string()));
                        state = TokenState::Normal;
                    },
                    _ => { term.push(x); },
                },
                TokenState::InString => match x {
                    '"' => {
                        term.push(x);
                        ret.push((
                            mem::take(&mut term_style),
                            mem::take(&mut term),
                        ));
                        state = TokenState::Normal;
                    },
                    '\\' => {
                        ret.push((
                            mem::take(&mut term_style),
                            mem::take(&mut term),
                        ));
                        state = TokenState::StringEscape;
                        term_style = HI_STRING_ESCAPE;
                        term.push(x);
                    },
                    _ => { term.push(x); },
                },
                TokenState::StringEscape => {
                    match x {
                        '"' | '\\' | 'n' | 'r' | 't' => { term.push(x); },
                        '\n' => { term.push(' '); },
                        _ => {
                            term.push(x);
                            term_style = HI_INVALID;
                        },
                    }
                    push_normal(&mut ret, &mut term_style, &mut term);
                    state = TokenState::InString;
                    term_style = HI_STRING;
                },
            }
        }
        push_normal(&mut ret, &mut term_style, &mut term);
        return reed::StyledText { buffer: ret };
    }
}

pub struct QEditMode { }

impl QEditMode {
    pub fn new() -> Self { Self { } }
}

impl reed::EditMode for QEditMode {
    fn parse_event(&mut self, event: CTEvent) -> RLEvent {
        return match event {
            CTEvent::Key(
                KeyEvent { code, modifiers }
            ) => match (code, modifiers) {
                (KeyCode::Char(c), KeyMod::NONE) => {
                    RLEvent::Edit(vec![
                        reed::EditCommand::InsertChar(c.to_ascii_lowercase())
                    ])
                },
                (KeyCode::Char(c), KeyMod::SHIFT) => {
                    RLEvent::Edit(vec![
                        reed::EditCommand::InsertChar(c.to_ascii_uppercase())
                    ])
                },
                (KeyCode::Char('c'), KeyMod::CONTROL) => {
                    RLEvent::CtrlC
                },
                (KeyCode::Char('d'), KeyMod::CONTROL) => {
                    RLEvent::CtrlD
                },
                (KeyCode::Char('a'), KeyMod::CONTROL) => {
                    RLEvent::Edit(vec![
                        reed::EditCommand::MoveToStart
                    ])
                },
                (KeyCode::Char('e'), KeyMod::CONTROL) => {
                    RLEvent::Edit(vec![
                        reed::EditCommand::MoveToEnd
                    ])
                },
                (KeyCode::Char('r'), KeyMod::CONTROL) => {
                    RLEvent::SearchHistory
                },
                (KeyCode::Char('l'), KeyMod::CONTROL) => {
                    RLEvent::ClearScreen
                },
                (KeyCode::Char('l'), keymod) => {
                    if keymod == KeyMod::CONTROL | KeyMod::SHIFT {
                        RLEvent::ClearScrollback
                    } else {
                        RLEvent::None
                    }
                },
                (KeyCode::Left, KeyMod::NONE) => {
                    RLEvent::Left
                },
                (KeyCode::Right, KeyMod::NONE) => {
                    RLEvent::Right
                },
                (KeyCode::Char('b'), keymod) => {
                    if keymod == KeyMod::ALT | KeyMod::SHIFT {
                        RLEvent::Edit(vec![
                            reed::EditCommand::MoveBigWordLeft
                        ])
                    } else if keymod == KeyMod::ALT {
                        RLEvent::Edit(vec![
                            reed::EditCommand::MoveWordLeft
                        ])
                    } else {
                        RLEvent::None
                    }
                },
                (KeyCode::Char('w'), keymod) => {
                    if keymod == KeyMod::ALT | KeyMod::SHIFT {
                        RLEvent::Edit(vec![
                            reed::EditCommand::MoveBigWordRightStart
                        ])
                    } else if keymod == KeyMod::ALT {
                        RLEvent::Edit(vec![
                            reed::EditCommand::MoveWordRightStart
                        ])
                    } else {
                        RLEvent::None
                    }
                },
                (KeyCode::Char('e'), keymod) => {
                    if keymod == KeyMod::ALT | KeyMod::SHIFT {
                        RLEvent::Edit(vec![
                            reed::EditCommand::MoveBigWordRightEnd
                        ])
                    } else if keymod == KeyMod::ALT {
                        RLEvent::Edit(vec![
                            reed::EditCommand::MoveWordRightEnd
                        ])
                    } else {
                        RLEvent::None
                    }
                },
                (KeyCode::Up, KeyMod::NONE) => {
                    RLEvent::Up
                },
                (KeyCode::Down, KeyMod::NONE) => {
                    RLEvent::Down
                },
                (KeyCode::Enter, KeyMod::SHIFT) => {
                    RLEvent::Edit(vec![
                        reed::EditCommand::InsertChar('\n')
                    ])
                },
                (KeyCode::Enter, KeyMod::CONTROL) => {
                    RLEvent::Submit
                },
                (KeyCode::Enter, KeyMod::NONE) => {
                    RLEvent::Enter
                },
                (KeyCode::Backspace, KeyMod::CONTROL) => {
                    RLEvent::Edit(vec![
                        reed::EditCommand::BackspaceWord
                    ])
                },
                (KeyCode::Backspace, KeyMod::NONE) => {
                    RLEvent::Edit(vec![
                        reed::EditCommand::Backspace
                    ])
                },
                (KeyCode::Delete, KeyMod::CONTROL) => {
                    RLEvent::Edit(vec![
                        reed::EditCommand::DeleteWord
                    ])
                },
                (KeyCode::Delete, KeyMod::NONE) => {
                    RLEvent::Edit(vec![
                        reed::EditCommand::Delete
                    ])
                },
                (KeyCode::Home, KeyMod::NONE) => {
                    RLEvent::Edit(vec![
                        reed::EditCommand::MoveToLineStart
                    ])
                },
                (KeyCode::End, KeyMod::NONE) => {
                    RLEvent::Edit(vec![
                        reed::EditCommand::MoveToLineEnd
                    ])
                },
                (KeyCode::Esc, KeyMod::NONE) => {
                    RLEvent::Esc
                },
                (KeyCode::Tab, KeyMod::NONE) => {
                    RLEvent::Edit(vec![
                        reed::EditCommand::InsertChar('\t'),
                    ])
                },
                _ => RLEvent::None,
            },
            // Event::Mouse(
            //     MouseEvent { kind, column, row, modifiers }
            // ) => match (kind, column, row, modifiers) {
            //     _ => RLEvent::None,
            // },
            CTEvent::Mouse(_) => RLEvent::None,
            CTEvent::Resize(n, m) => RLEvent::Resize(n, m),
        };
    }

    fn edit_mode(&self) -> reed::PromptEditMode {
        return reed::PromptEditMode::Custom("QEM".into());
    }
}

pub fn run_repl(env: &mut QEnv) {
    let mut line_editor
        = reed::Reedline::create()
        .with_validator(Box::new(reed::DefaultValidator))
        .with_highlighter(Box::new(QHighlighter::new()))
        .with_edit_mode(Box::new(QEditMode::new()));
    let prompt = QPrompt { };
    println!(
        "Welcome to the QLisp interpreter REPL. Type `(help)` for more \
        information."
    );
    loop {
        match line_editor.read_line(&prompt) {
            Ok(reed::Signal::Success(s)) => {
                let (expr, _comment): (String, String)
                    = match s.split_once(';') {
                        Some((e, c)) => (e.to_string(), c.to_string()),
                        None => (s, "".to_string()),
                    };
                if ["quit", "exit"].contains(&expr.trim()) {
                    break;
                }
                match env.repl_parse_eval(expr) {
                    Ok(res) => {
                        res.into_iter().for_each(|r| { r.unpack(); });
                    },
                    Err(e) => match e {
                        QErr::Reason(msg)
                            => { println_flush!("Error: {}", msg); },
                    },
                }
            },
            Ok(reed::Signal::CtrlD) => {
                break;
            },
            Ok(reed::Signal::CtrlC) => {
                continue;
            },
            x => { println_flush!("Error: {:?}", x); },
        }
    }
}

