use std::{
    cmp,
    collections::HashMap,
    fmt::{
        self,
        Write as FmtWrite,
    },
    fs::{
        self,
        File,
    },
    io::Write as IOWrite,
    mem,
    path::{
        Path,
        PathBuf,
    },
    rc::Rc,
    str::FromStr,
};
use num_complex::{
    Complex64 as C64,
    ComplexFloat,
};
use num_traits::{
    One,
    Zero,
};
use strfmt::{
    self,
    strfmt as strformat,
};
use crate::functions as fns;

#[derive(Debug)]
pub enum QErr {
    Reason(String),
}

impl QErr {
    pub fn prepend_source(self, source: &str) -> Self {
        let QErr::Reason(reason) = self;
        return QErr::Reason(format!("{}: {}", source, reason));
    }

    pub fn message(&self) -> &String {
        let QErr::Reason(msg) = self;
        msg
    }
}

#[macro_export]
macro_rules! qerr(
    ( $reason:literal ) => {
        QErr::Reason($reason.into())
    }
);

#[macro_export]
macro_rules! qerr_fmt(
    ( $($arg:tt)* ) => {
        QErr::Reason(format!($($arg)*))
    }
);

pub const FILE_EXTENSIONS: &[&str] = &[
    "qlisp",
    "qlsp",
];

pub const CONSTS: &[&str] = &[
    "PI", "2PI", "iPI", "i2PI",
    "E",
    "SQRT2", "1/SQRT2", "i/SQRT2",
];

pub const KEYWORDS: &[&str] = &[
    "def",              ":=",
    "let",              "*:=",
    "fn",               "@:",
    "defn",             "@:=",
    "if",
    "module",
    "use",
    "use-all",          "use*",
    "isdef",            "?:=",
    "del",              "!-",
];

pub const OPERATORS: &[&str] = &[
    "add",              "+",
    "sub",              "-",
    "mul",              "*",
    "div",              "/",
    "idiv",             "//",
    "and",              "&&",
    "or",               "||",
    "xor",              "^",
    "eq",               "=",
    "neq",              "!=",
    "gt",               ">",
    "geq",              ">=",
    "lt",               "<",
    "leq",              "<=",
    "mod",              "%",
    "pow",              "**",
    "shl",              "<<",
    "shr",              ">>",
];

pub const TYPES: &[&str] = &[
    "bool",
    "int",
    "float",
    "complex",
    "list",
    "str",
];

pub const SPECIAL: &[&str] = &[
    "interact",
];

pub const PROTECTED: &[&str] = &[
    // constants
    "PI", "2PI", "iPI", "i2PI",
    "E",
    "SQRT2", "1/SQRT2", "i/SQRT2",
    // "inf", "inF", "iNf", "iNF", "Inf", "InF", "INf", "INF",
    // "nan", "naN", "nAn", "nAN", "Nan", "NaN", "NAn", "NAN",
    // special -- keyword-like
    "def",              ":=",   // done
    "let",              "*:=",  // done
    "fn",               "@:",   // done
    "defn",             "@:=",  // done
    "if",                       // done
    "module",                   // done
    "use",                      // done
    "use-all",          "use*", // done
    "interact",                 // done
    "isdef",            "?:=",  // done
    "del",              "!-",   // done
    // systems
    "format",           "$",    // done
    "print",            "$-",   // done
    "println",          "$_",   // done
    "print-flush",      "$$-",  // done
    "println-flush",    "$$_",  // done
    "read",             "$<",   //
    "readlines",        "$<:",  //
    "with-file",        "$|",   //
    "with-file-add",    "$|+",  //
    "write",            "$>-",  //
    "writeln",          "$>_",  //
    "write-flush",      "$>>-", //
    "writeln-flush",    "$>>_", //
    "halt",             "!!",   // done
    "istype",           "~?",   // done
    "type",             "?~",   // done
    // type-casting
    "bool",                     // done
    "int",                      // done
    "float",                    // done
    "complex",                  // done
    "list",                     // done
    "str",                      // done
    // arithmetic
    "add",              "+",    // done
    "sub",              "-",    // done
    "mul",              "*",    // done
    "div",              "/",    // done
    "idiv",             "//",   // done
    // boolean comparisons
    "and",              "&&",   // done
    "or",               "||",   // done
    "xor",              "^",    // done
    "eq",               "=",    // done
    "neq",              "!=",   // done
    "gt",               ">",    // done
    "geq",              ">=",   // done
    "lt",               "<",    // done
    "leq",              "<=",   // done
    // boolean accumulators
    "all",              "&&*",  // done
    "any",              "||*",  // done
    "xany",             "^*",   // done
    // iterable creation
    "range",            "..",   // done
    "range-inc",        "..=",  // done
    "repeat",           "#=",   // done
    // iterable accumulation
    "length",           "#",    // done
    "fold",             "@.",   // done
    "min",              "<:",   // done
    "max",              ":>",   // done
    "select-by",        "*@.",  // done
    // iterable slicing and access
    "get",              ".",    // done
    "set",              ".:=",  // done
    "slice",            "--",   // done
    "slice-inc",        "--=",  // done
    "slice-by",         "~~",   // done
    "slice-inc-by",     "~~=",  // done
    "pick",             ".*",   // done
    "first",            ".-",   // done
    "rest",             ".!-",  // done
    "take",             "~.",   // done
    "take-while",       "~.@",  // done
    "last",             "-.",   // done
    "skip",             ".~",   // done
    "skip-while",       ".~@",  // done
    // iterable transformation
    "step-by",          "~",    // done
    "enumerate",        "##",   // done
    "reverse",          "<>",   // done
    "cycle",            "<#>",  // done
    "map",              "@",    // done
    "filter",           "@!",   // done
    "unique",           "*!=",  // done
    "flatten",          "__",   // done
    "sort",             "<*",   // done
    "sort-by",          "<@",   // done
    "permute",          ".~.",  // done
    // iterable division
    "split-at",         "|.",   // done
    "split-on",         "|@",   // done
    "split-on-inc",     "|@=",  // done
    // iterable addition
    "append",           "+.",   // done
    "prepend",          ".+",   // done
    "insert",           "+.+",  // done
    "join",             "++",   // done
    "join-with",        "+*+",  // done
    "zip",              ":~:",  // done
    "cart",             ":*:",  // done
    // iterable testing
    "contains",         "*=",   // done
    "index-of",         "#*=",  // done

    // element-wise math
    "neg",              "!",    // done
    "recip",            "1/",   // done
    "abs",              "|.|",  // done
    "sqrt",                     // done
    "cbrt",                     // done
    "exp",              "e**",  // done
    "floor",            "~_",   // done
    "ceil",             "~^",   // done
    "round",            "~:",   // done
    "ln",                       // done
    "sin",                      // done
    "cos",                      // done
    "tan",                      // done
    "arcsin",           "asin", // done
    "arccos",           "acos", // done
    "arctan",           "atan", // done
    "arctan2",          "atan2",// done
    "sinh",                     // done
    "cosh",                     // done
    "tanh",                     // done
    "arsinh",           "asinh",// done
    "arcosh",           "acosh",// done
    "artanh",           "atanh",// done
    "arg",                      // done
    "cis",              "e**i", // done
    "conj",             "~z",   // done
    "real",             "Re",   // done
    "imag",             "Im",   // done
    // parameterized element-wise math
    "mod",              "%",    // done
    "log",                      // done
    "pow",              "**",   // done
    "shl",              "<<",   // done
    "shr",              ">>",   // done
    // list -> list math
    "convolve",         "<:>",  // done
    "hist",             "|#|",  // done
    "hist-prob",        "|p|",  // done
    "covariance",       "Cov",  // done
    "correlation",      "Corr", // done
    // "fft",              "{F}",  //
    // "ifft",             "{iF}", //
    // list -> value math
    "mean",                     // done
    "variance",         "Var",  // done
    "stddev",           "Std",  // done
    // list+1 -> value math
    "pnorm",            "|+|",  // done
    "moment",                   // done
    // special-arg math
    "sample",           "?.",   //
    // default modules
    "phys",
];

pub type QResult<T> = Result<T, QErr>;

#[derive(Clone)]
pub enum QExp {
    Bool(bool),
    Int(i64),
    Float(f64),
    Complex(C64),
    List(Vec<QExp>),
    // Array(QArray),
    Str(String),
    Symbol(String),
    Func(QBuiltin),
    // Func(String, fn(&mut QEnv, &[QExp]) -> Result<QExp, QErr>),
    Lambda(QLambda),
    // Module(QEnv<'a>)
}

// #[derive(Clone)]
// pub enum QExp {
//     Number(QNum),
//     Indexable(QIdxable),
//     Symbol(String),
//     Func(QFunc),
// }
//
// #[derive(Clone)]
// pub enum QNum {
//     Bool(bool),
//     Int(i64),
//     Float(f64),
//     Complex(C64),
// }
//
// #[derive(Clone)]
// pub enum QIdxable {
//     List(Vec<QExp>),
//     Str(String),
// }
//
// #[derive(Clone)]
// pub enum QFunc {
//     Func(String, fn(&mut QEnv, &[QExp]) -> Result<QExp, QErr>),
//     Lambda(QLambda),
// }

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum QExpType {
    Bool = 0,
    Int = 1,
    Float = 2,
    Complex = 3,
    List = 4,
    Str = 5,
    Symbol = 6,
    Func = 7,
    Lambda = 8,
    Any = 9,
}

#[macro_export]
macro_rules! qbool(
    ( $id:ident ) => {
        QExp::Bool($id)
    };
    ( $val:expr ) => {
        QExp::Bool($val)
    };
    ( _ ) => {
        QExp::Bool(_)
    };
    ( ) => {
        QExpType::Bool
    };
);

#[macro_export]
macro_rules! qint(
    ( $id:ident ) => {
        QExp::Int($id)
    };
    ( $val:expr ) => {
        QExp::Int($val)
    };
    ( _ ) => {
        QExp::Int(_)
    };
    ( ) => {
        QExpType::Int
    };
);

#[macro_export]
macro_rules! qfloat(
    ( $id:ident ) => {
        QExp::Float($id)
    };
    ( $val:expr ) => {
        QExp::Float($val)
    };
    ( _ ) => {
        QExp::Float(_)
    };
    ( ) => {
        QExpType::Float
    };
);

#[macro_export]
macro_rules! qcomplex(
    ( $id:ident ) => {
        QExp::Complex($id)
    };
    ( $val:expr ) => {
        QExp::Complex($val)
    };
    ( _ ) => {
        QExp::Complex(_)
    };
    ( ) => {
        QExpType::Complex
    };
);

#[macro_export]
macro_rules! qlist(
    ( $id:ident ) => {
        QExp::List($id)
    };
    ( $val:expr ) => {
        QExp::List($val)
    };
    ( _ ) => {
        QExp::List(_)
    };
    ( ) => {
        QExpType::List
    };
);

#[macro_export]
macro_rules! qstr(
    ( $id:ident ) => {
        QExp::Str($id)
    };
    ( $val:expr ) => {
        QExp::Str($val)
    };
    ( _ ) => {
        QExp::Str(_)
    };
    ( ) => {
        QExpType::Str
    };
);

#[macro_export]
macro_rules! qsymbol(
    ( $id:ident ) => {
        QExp::Symbol($id)
    };
    ( $val:expr ) => {
        QExp::Symbol($val)
    };
    ( _ ) => {
        QExp::Symbol(_)
    };
    ( ) => {
        QExpType::Symbol
    };
);

#[macro_export]
macro_rules! qfunc(
    ( $id1:ident, $id2:ident ) => {
        QExp::Func($id1, $id2)
    };
    ( $id1:ident, _ ) => {
        QExp::Func($id1, _)
    };
    ( _, $id2:ident ) => {
        QExp::Func(_, $id2)
    };
    ( $name:expr, $val:expr ) => {
        QExp::Func($name.to_string(), $val)
    };
    ( _, _ ) => {
        QExp::Func(_, _)
    };
    ( $id:ident ) => {
        QExp::Func($id)
    };
    ( $val:expr ) => {
        QExp::Func($val)
    };
    ( _ ) => {
        QExp::Func(_)
    };
    ( ) => {
        QExpType::Func
    };
);

#[macro_export]
macro_rules! qlambda(
    ( $id:ident ) => {
        QExp::Lambda($id)
    };
    ( $val:expr ) => {
        QExp::Lambda($val)
    };
    ( _ ) => {
        QExp::Lambda(_)
    };
    ( ) => {
        QExpType::Lambda
    };
);

#[macro_export]
macro_rules! qany(
    ( ) => {
        QExpType::Any
    };
);

impl fmt::Display for QExpType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return match self {
            qbool!() => "bool",
            qint!() => "int",
            qfloat!() => "float",
            qcomplex!() => "complex",
            qlist!() => "list",
            qstr!() => "str",
            qsymbol!() => "symbol",
            qfunc!() => "func",
            qlambda!() => "lambda",
            qany!() => "any",
        }.fmt(f);
    }
}

impl From<usize> for QExpType {
    fn from(u: usize) -> QExpType {
        return match u {
            0 => qbool!(),
            1 => qint!(),
            2 => qfloat!(),
            3 => qcomplex!(),
            4 => qlist!(),
            5 => qstr!(),
            6 => qsymbol!(),
            7 => qfunc!(),
            8 => qlambda!(),
            9 => qany!(),
            _ => qlist!(),
        };
    }
}

impl QExpType {
    pub fn equals_user(&self, other: &Self) -> bool {
        return match self {
            qfunc!() => [qfunc!(), qlambda!()].contains(other),
            qlambda!() => [qfunc!(), qlambda!()].contains(other),
            qany!() => true,
            ty => ty == other,
        };
    }
}

impl QExp {
    pub fn exp_type(&self) -> QExpType {
        return match self {
            qbool!(_) => qbool!(),
            qint!(_) => qint!(),
            qfloat!(_) => qfloat!(),
            qcomplex!(_) => qcomplex!(),
            qlist!(_) => qlist!(),
            qstr!(_) => qstr!(),
            qsymbol!(_) => qsymbol!(),
            // qfunc!(_, _) => qfunc!(),
            qfunc!(_) => qfunc!(),
            qlambda!(_) => qlambda!(),
        };
    }

    pub fn exp_type_user(&self) -> QResult<QExp> {
        let ty: String = match self.exp_type() {
            qbool!() => "bool",
            qint!() => "int",
            qfloat!() => "float",
            qcomplex!() => "complex",
            qlist!() => "list",
            qstr!() => "str",
            qsymbol!() => "symbol",
            qfunc!() => "function",
            qlambda!() => "function",
            qany!() => "any",
        }.to_string();
        return Ok(qstr!(ty));
    }

    pub fn is_type_user(&self, types: &[QExpType]) -> bool {
        return types.iter().any(|ty| ty.equals_user(&self.exp_type()));
    }

    pub fn zero(ty: QExpType) -> QResult<QExp> {
        return match ty {
            qbool!() => Ok(qbool!(false)),
            qint!() => Ok(qint!(0)),
            qfloat!() => Ok(qfloat!(0.0)),
            qcomplex!() => Ok(qcomplex!(C64::zero())),
            qlist!() => Ok(qlist!(vec![])),
            qstr!() => Ok(qstr!("".to_string())),
            qsymbol!() => Err(qerr!("zero value for symbols does not exist")),
            qfunc!() => Err(qerr!("zero value for functions does not exist")),
            qlambda!() => Err(qerr!("zero value for lambdas does not exist")),
            qany!() => Err(qerr!("zero value for anytype does not exist")),
        };
    }

    pub fn is_zero(&self) -> bool {
        return match self {
            qbool!(b) => *b,
            qint!(i) => *i == 0,
            qfloat!(f) => *f == 0.0,
            qcomplex!(c) => *c == C64::zero(),
            qlist!(l) => l.is_empty(),
            qstr!(s) => s.is_empty(),
            _ => false,
        };
    }

    pub fn one(ty: QExpType) -> QResult<QExp> {
        return match ty {
            qbool!() => Ok(qbool!(true)),
            qint!() => Ok(qint!(1)),
            qfloat!() => Ok(qfloat!(1.0)),
            qcomplex!() => Ok(qcomplex!(C64::one())),
            qlist!() => Err(qerr!("unitary value for lists does not exist")),
            qstr!() => Err(qerr!("unitary value for strings does not exist")),
            qsymbol!() => Err(qerr!("unitary value for symbols does not exist")),
            qfunc!() => Err(qerr!("unitary value for functions does not exist")),
            qlambda!() => Err(qerr!("unitary value for lambdas does not exist")),
            qany!() => Err(qerr!("unitary value for anytype does not exist")),
        };
    }

    pub fn not(&self) -> QResult<QExp> {
        return match self {
            qbool!(b) => Ok(qbool!(!b)),
            _ => Err(qerr!("non-boolean in not")),
        };
    }

    pub fn and(&self, rhs: &QExp) -> QResult<QExp> {
        return match self {
            qbool!(l) => match rhs {
                qbool!(r) => Ok(qbool!(*l && *r)),
                _ => Err(qerr!("non-boolean in and")),
            },
            _ => Err(qerr!("non-boolean in and")),
        };
    }

    pub fn or(&self, rhs: &QExp) -> QResult<QExp> {
        return match self {
            qbool!(l) => match rhs {
                qbool!(r) => Ok(qbool!(*l || *r)),
                _ => Err(qerr!("non-boolean in or")),
            },
            _ => Err(qerr!("non-boolean in or")),
        };
    }

    pub fn neg(&self) -> QResult<QExp> {
        return match self {
            qbool!(b) => Ok(qbool!(!b)),
            qint!(i) => Ok(qint!(-i)),
            qfloat!(f) => Ok(qfloat!(-f)),
            qcomplex!(c) => Ok(qcomplex!(-c)),
            _ => Err(qerr!("invalid type in neg")),
        };
    }

    pub fn add(&self, rhs: &QExp) -> QResult<QExp> {
        let ret_type: QExpType
            = cmp::max(
                cmp::max(self.exp_type(), rhs.exp_type()),
                qint!(),
            );
        let lhs_c: QExp = convert_type_num(self, ret_type)?;
        let rhs_c: QExp = convert_type_num(rhs,  ret_type)?;
        return match (lhs_c, rhs_c) {
            (qint!(l), qint!(r)) => Ok(qint!(l + r)),
            (qfloat!(l), qfloat!(r)) => Ok(qfloat!(l + r)),
            (qcomplex!(l), qcomplex!(r)) => Ok(qcomplex!(l + r)),
            _ => Err(qerr!("invalid type in add")),
        };
    }

    pub fn sub(&self, rhs: &QExp) -> QResult<QExp> {
        let ret_type: QExpType
            = cmp::max(
                cmp::max(self.exp_type(), rhs.exp_type()),
                qint!(),
            );
        let lhs_c: QExp = convert_type_num(self, ret_type)?;
        let rhs_c: QExp = convert_type_num(rhs,  ret_type)?;
        return match (lhs_c, rhs_c) {
            (qint!(l), qint!(r)) => Ok(qint!(l - r)),
            (qfloat!(l), qfloat!(r)) => Ok(qfloat!(l - r)),
            (qcomplex!(l), qcomplex!(r)) => Ok(qcomplex!(l - r)),
            _ => Err(qerr!("invalid type in sub")),
        };
    }

    pub fn mul(&self, rhs: &QExp) -> QResult<QExp> {
        let ret_type: QExpType
            = cmp::max(
                cmp::max(self.exp_type(), rhs.exp_type()),
                qint!(),
            );
        let lhs_c: QExp = convert_type_num(self, ret_type)?;
        let rhs_c: QExp = convert_type_num(rhs,  ret_type)?;
        return match (lhs_c, rhs_c) {
            (qint!(l), qint!(r)) => Ok(qint!(l * r)),
            (qfloat!(l), qfloat!(r)) => Ok(qfloat!(l * r)),
            (qcomplex!(l), qcomplex!(r)) => Ok(qcomplex!(l * r)),
            _ => Err(qerr!("invalid type in mul")),
        };
    }

    pub fn div(&self, rhs: &QExp) -> QResult<QExp> {
        (!rhs.is_zero())
            .then(|| ())
            .ok_or_else(|| qerr!("encountered zero in div"))?;
        let ret_type: QExpType
            = cmp::max(
                cmp::max(self.exp_type(), rhs.exp_type()),
                qfloat!(),
            );
        let lhs_c: QExp = convert_type_num(self, ret_type)?;
        let rhs_c: QExp = convert_type_num(rhs,  ret_type)?;
        return match (lhs_c, rhs_c) {
            (qfloat!(l), qfloat!(r)) => Ok(qfloat!(l / r)),
            (qcomplex!(l), qcomplex!(r)) => Ok(qcomplex!(l / r)),
            _ => Err(qerr!("invalid type in div")),
        };
    }

    pub fn idiv(&self, rhs: &QExp) -> QResult<QExp> {
        (!rhs.is_zero())
            .then(|| ())
            .ok_or_else(|| qerr!("encountered zero in idiv"))?;
        let lhs_c: QExp = convert_type_num(self, qfloat!())?;
        let rhs_c: QExp = convert_type_num(rhs,  qfloat!())?;
        return match (lhs_c, rhs_c) {
            (qfloat!(l), qfloat!(r)) => Ok(qint!(l.div_euclid(r) as i64)),
            _ => Err(qerr!("invalid type in idiv")),
        };
    }

    pub fn modulo(&self, M: &QExp) -> QResult<QExp> {
        (!M.is_zero())
            .then(|| ())
            .ok_or_else(|| qerr!("encountered zero in mod"))?;
        let ret_type: QExpType
            = cmp::max(
                cmp::max(self.exp_type(), M.exp_type()),
                qint!(),
            );
        (ret_type < qcomplex!())
            .then(|| ())
            .ok_or_else(|| qerr!("cannot modulo by a complex value"))?;
        let N_c: QExp = convert_type_num(self, ret_type)?;
        let M_c: QExp = convert_type_num(M,    ret_type)?;
        return match (N_c, M_c) {
            (qint!(n), qint!(m)) => Ok(qint!(n.rem_euclid(m))),
            (qfloat!(n), qfloat!(m)) => {
                Ok(qfloat!(n.rem_euclid(m)))
            },
            _ => Err(qerr!("invalid type in mod")),
        };
    }

    pub fn log(&self, base: &QExp) -> QResult<QExp> {
        (!base.is_zero())
            .then(|| ())
            .ok_or_else(|| qerr!("encountered zero base in log"))?;
        let X_c: QExp
            = convert_type_num(self, cmp::max(self.exp_type(), qfloat!()))?;
        let b_c: QExp = convert_type_num(base, qfloat!())?;
        return match (X_c, b_c) {
            (qfloat!(x), qfloat!(b)) => Ok(qfloat!(x.log(b))),
            (qcomplex!(x), qfloat!(b)) => Ok(qcomplex!(x.log(b))),
            _ => Err(qerr!("invalid type in log")),
        };
    }

    pub fn pow(&self, exponent: &QExp) -> QResult<QExp> {
        let mut ret_type: QExpType
            = cmp::max(
                cmp::max(self.exp_type(), exponent.exp_type()),
                qint!(),
            );
        let e_c: QExp
            = match convert_type_num(exponent, ret_type)? {
                qint!(i) => {
                    if i < 0 {
                        ret_type = qfloat!();
                        convert_type_num(exponent, ret_type)?
                    } else {
                        qint!(i)
                    }
                },
                q => q,
            };
        let b_c: QExp = convert_type_num(self, ret_type)?;
        return match (b_c, e_c) {
            (qint!(b), qint!(e)) => Ok(qint!(b.pow(e.unsigned_abs() as u32))),
            (qfloat!(b), qfloat!(e)) => Ok(qfloat!(b.powf(e))),
            (qcomplex!(b), qcomplex!(e)) => Ok(qcomplex!(b.powc(e))),
            _ => Err(qerr!("invalid type in pow")),
        };
    }

    pub fn shl(&self, sh: &QExp) -> QResult<QExp> {
        let n_c: i64 = match convert_type_num(self, qint!()) {
            Ok(qint!(i)) => Ok(i),
            _ => Err(qerr!("invalid type in shl")),
        }?;
        let sh_c: u32
            = match convert_type_num(sh, qint!()) {
                Ok(qint!(i)) => {
                    if i < 0 {
                        Err(qerr!("shift size cannot be negative"))
                    } else {
                        Ok(i as u32)
                    }
                },
                _ => Err(qerr!("invalid type in shl")),
            }?;
        return Ok(qint!(n_c.overflowing_shl(sh_c).0));
    }

    pub fn shr(&self, sh: &QExp) -> QResult<QExp> {
        let n_c: i64 = match convert_type_num(self, qint!()) {
            Ok(qint!(i)) => Ok(i),
            _ => Err(qerr!("invalid type in shr")),
        }?;
        let sh_c: u32
            = match convert_type_num(sh, qint!()) {
                Ok(qint!(i)) => {
                    if i < 0 {
                        Err(qerr!("shift size cannot be negative"))
                    } else {
                        Ok(i as u32)
                    }
                },
                _ => Err(qerr!("invalid type in shr")),
            }?;
        return Ok(qint!(n_c.overflowing_shr(sh_c).0));
    }

    pub fn equals(&self, rhs: &QExp) -> bool {
        // let ty: QExpType = cmp::max(self.exp_type(), rhs.exp_type());
        // let lhs_c: QExp = match convert_type(self, ty) {
        //     Ok(l) => l,
        //     Err(_) => return false,
        // };
        // let rhs_c: QExp = match convert_type(rhs,  ty) {
        //     Ok(r) => r,
        //     Err(_) => return false,
        // };
        return match (self, rhs) {
            (qbool!(l), qbool!(r)) => l == r,
            (qint!(l), qint!(r)) => l == r,
            (qfloat!(l), qfloat!(r)) => l == r,
            (qcomplex!(l), qcomplex!(r)) => l == r,
            (qlist!(l), qlist!(r))
                => l.iter().zip(r.iter()).all(|(lk, rk)| lk.equals(rk)),
            (qstr!(l), qstr!(r)) => l == r,
            _ => false,
        };
    }

    pub fn nequals(&self, rhs: &QExp) -> bool { !self.equals(rhs) }

    pub fn lt(&self, rhs: &QExp) -> QResult<bool> {
        let ty: QExpType = cmp::max(self.exp_type(), rhs.exp_type());
        let lhs_c: QExp = convert_type(self, ty)?;
        let rhs_c: QExp = convert_type(rhs,  ty)?;
        return match (lhs_c, rhs_c) {
            (qint!(l), qint!(r)) => Ok(l < r),
            (qfloat!(l), qfloat!(r)) => Ok(l < r),
            (qstr!(l), qstr!(r)) => Ok(l < r),
            _ => Err(qerr!("incompatible types in lt")),
        };
    }

    pub fn gt(&self, rhs: &QExp) -> QResult<bool> {
        let ty: QExpType = cmp::max(self.exp_type(), rhs.exp_type());
        let lhs_c: QExp = convert_type(self, ty)?;
        let rhs_c: QExp = convert_type(rhs,  ty)?;
        return match (lhs_c, rhs_c) {
            (qint!(l), qint!(r)) => Ok(l > r),
            (qfloat!(l), qfloat!(r)) => Ok(l > r),
            (qstr!(l), qstr!(r)) => Ok(l > r),
            _ => Err(qerr!("incompatible types in gt")),
        };
    }

    pub fn le(&self, rhs: &QExp) -> QResult<bool> { Ok(!self.gt(rhs)?) }

    pub fn ge(&self, rhs: &QExp) -> QResult<bool> { Ok(!self.lt(rhs)?) }

    pub fn recip(&self) -> QResult<QExp> {
        (!self.is_zero())
            .then(|| ())
            .ok_or_else(|| qerr!("encountered zero in recip"))?;
        return match self {
            qbool!(b) => Ok(qfloat!((*b as u8 as f64).recip())),
            qint!(i) => Ok(qfloat!((*i as f64).recip())),
            qfloat!(f) => Ok(qfloat!(f.recip())),
            qcomplex!(c) => Ok(qcomplex!(c.recip())),
            _ => Err(qerr!("invalid type in recip")),
        };
    }

    pub fn abs(&self) -> QResult<QExp> {
        return match self {
            qbool!(b) => Ok(qbool!(*b)),
            qint!(i) => Ok(qint!(i.abs())),
            qfloat!(f) => Ok(qfloat!(f.abs())),
            qcomplex!(c) => Ok(qfloat!(c.norm())),
            _ => Err(qerr!("invalid type in abs")),
        };
    }

    pub fn sqrt(&self) -> QResult<QExp> {
        return match self {
            qbool!(b) => Ok(qfloat!(if *b { 1.0 } else { 0.0 })),
            qint!(i) => Ok(qfloat!((*i as f64).sqrt())),
            qfloat!(f) => Ok(qfloat!(f.sqrt())),
            qcomplex!(c) => Ok(qcomplex!(c.sqrt())),
            _ => Err(qerr!("invalid type in sqrt")),
        };
    }

    pub fn cbrt(&self) -> QResult<QExp> {
        return match self {
            qbool!(b) => Ok(qfloat!(if *b { 1.0 } else { 0.0 })),
            qint!(i) => Ok(qfloat!((*i as f64).cbrt())),
            qfloat!(f) => Ok(qfloat!(f.cbrt())),
            qcomplex!(c) => Ok(qcomplex!(c.cbrt())),
            _ => Err(qerr!("invalid type in cbrt")),
        };
    }

    pub fn exp(&self) -> QResult<QExp> {
        return match self {
            qbool!(b)
                => Ok(qfloat!(if *b { std::f64::consts::E } else { 1.0 })),
            qint!(i) => Ok(qfloat!((*i as f64).exp())),
            qfloat!(f) => Ok(qfloat!(f.exp())),
            qcomplex!(c) => Ok(qcomplex!(c.exp())),
            _ => Err(qerr!("invalid type in exp")),
        };
    }

    pub fn floor(&self) -> QResult<QExp> {
        return match self {
            qbool!(b) => Ok(qint!(if *b { 1 } else { 0 })),
            qint!(i) => Ok(qint!(*i)),
            qfloat!(f) => Ok(qint!(f.floor() as i64)),
            _ => Err(qerr!("invalid type in floor")),
        };
    }

    pub fn ceil(&self) -> QResult<QExp> {
        return match self {
            qbool!(b) => Ok(qint!(if *b { 1 } else { 0 })),
            qint!(i) => Ok(qint!(*i)),
            qfloat!(f) => Ok(qint!(f.ceil() as i64)),
            _ => Err(qerr!("invalid type in ceil")),
        };
    }

    pub fn round(&self) -> QResult<QExp> {
        return match self {
            qbool!(b) => Ok(qint!(if *b { 1 } else { 0 })),
            qint!(i) => Ok(qint!(*i)),
            qfloat!(f) => Ok(qint!(f.round() as i64)),
            _ => Err(qerr!("invalid type in round")),
        };
    }

    pub fn ln(&self) -> QResult<QExp> {
        return match self {
            qbool!(b) => Ok(qfloat!(if *b { 0.0 } else { f64::NEG_INFINITY })),
            qint!(i) => Ok(qfloat!((*i as f64).ln())),
            qfloat!(f) => Ok(qfloat!(f.ln())),
            qcomplex!(c) => Ok(qcomplex!(c.ln())),
            _ => Err(qerr!("invalid type in ln")),
        };
    }

    pub fn sin(&self) -> QResult<QExp> {
        return match self {
            qbool!(b) => Ok(qfloat!(if *b { 1.0_f64.sin() } else { 0.0 })),
            qint!(i) => Ok(qfloat!((*i as f64).sin())),
            qfloat!(f) => Ok(qfloat!(f.sin())),
            qcomplex!(c) => Ok(qcomplex!(c.sin())),
            _ => Err(qerr!("invalid type in sin")),
        };
    }

    pub fn cos(&self) -> QResult<QExp> {
        return match self {
            qbool!(b) => Ok(qfloat!(if *b { 1.0_f64.cos() } else { 1.0 })),
            qint!(i) => Ok(qfloat!((*i as f64).cos())),
            qfloat!(f) => Ok(qfloat!(f.cos())),
            qcomplex!(c) => Ok(qcomplex!(c.cos())),
            _ => Err(qerr!("invalid type in cos")),
        };
    }

    pub fn tan(&self) -> QResult<QExp> {
        return match self {
            qbool!(b) => Ok(qfloat!(if *b { 1.0_f64.tan() } else { 0.0 })),
            qint!(i) => Ok(qfloat!((*i as f64).tan())),
            qfloat!(f) => Ok(qfloat!(f.tan())),
            qcomplex!(c) => Ok(qcomplex!(c.tan())),
            _ => Err(qerr!("invalid type in tan")),
        };
    }

    pub fn arcsin(&self) -> QResult<QExp> {
        return match self {
            qbool!(b) => Ok(qfloat!(
                if *b { std::f64::consts::FRAC_PI_2 } else { 0.0 }
            )),
            qint!(i) => Ok(qfloat!((*i as f64).asin())),
            qfloat!(f) => Ok(qfloat!(f.asin())),
            qcomplex!(c) => Ok(qcomplex!(c.asin())),
            _ => Err(qerr!("invalid type in asin")),
        };
    }

    pub fn arccos(&self) -> QResult<QExp> {
        return match self {
            qbool!(b) => Ok(qfloat!(
                if *b { 0.0 } else { std::f64::consts::FRAC_PI_2 }
            )),
            qint!(i) => Ok(qfloat!((*i as f64).acos())),
            qfloat!(f) => Ok(qfloat!(f.acos())),
            qcomplex!(c) => Ok(qcomplex!(c.acos())),
            _ => Err(qerr!("invalid type in acos")),
        };
    }

    pub fn arctan(&self) -> QResult<QExp> {
        return match self {
            qbool!(b) => Ok(qfloat!(if *b { 1.0_f64.atan() } else { 0.0 })),
            qint!(i) => Ok(qfloat!((*i as f64).atan())),
            qfloat!(f) => Ok(qfloat!(f.atan())),
            qcomplex!(c) => Ok(qcomplex!(c.atan())),
            _ => Err(qerr!("invalid type in atan")),
        };
    }

    pub fn arctan2(&self) -> QResult<QExp> {
        return match self {
            qlist!(l) => {
                if l.len() == 2 {
                    match (
                        convert_type_num(&l[0], qfloat!()),
                        convert_type_num(&l[1], qfloat!()),
                    ) {
                        (Ok(qfloat!(y)), Ok(qfloat!(x)))
                            => Ok(qfloat!(y.atan2(x))),
                        _ => Err(qerr!("invalid type in atan2")),
                    }
                } else {
                    Err(qerr!("atan2: args must be 2-item lists"))
                }
            },
            _ => Err(qerr!("invalid type in atan2")),
        };
    }

    pub fn sinh(&self) -> QResult<QExp> {
        return match self {
            qbool!(b) => Ok(qfloat!(if *b { 1.0_f64.sinh() } else { 0.0 })),
            qint!(i) => Ok(qfloat!((*i as f64).sinh())),
            qfloat!(f) => Ok(qfloat!(f.sinh())),
            qcomplex!(c) => Ok(qcomplex!(c.sinh())),
            _ => Err(qerr!("invalid type in sinh")),
        }
    }

    pub fn cosh(&self) -> QResult<QExp> {
        return match self {
            qbool!(b) => Ok(qfloat!(if *b { 1.0_f64.cosh() } else { 1.0 })),
            qint!(i) => Ok(qfloat!((*i as f64).cosh())),
            qfloat!(f) => Ok(qfloat!(f.cosh())),
            qcomplex!(c) => Ok(qcomplex!(c.cosh())),
            _ => Err(qerr!("invalid type in cosh")),
        };
    }

    pub fn tanh(&self) -> QResult<QExp> {
        return match self {
            qbool!(b) => Ok(qfloat!(if *b { 1.0_f64.tanh() } else { 0.0 })),
            qint!(i) => Ok(qfloat!((*i as f64).tanh())),
            qfloat!(f) => Ok(qfloat!(f.tanh())),
            qcomplex!(c) => Ok(qcomplex!(c.tanh())),
            _ => Err(qerr!("invalid type in tanh")),
        }
    }

    pub fn arsinh(&self) -> QResult<QExp> {
        return match self {
            qbool!(b) => Ok(qfloat!(if *b { 1.0_f64.asinh() } else { 0.0 })),
            qint!(i) => Ok(qfloat!((*i as f64).asinh())),
            qfloat!(f) => Ok(qfloat!(f.asinh())),
            qcomplex!(c) => Ok(qcomplex!(c.asinh())),
            _ => Err(qerr!("invalid type in asinh")),
        };
    }

    pub fn arcosh(&self) -> QResult<QExp> {
        return match self {
            qbool!(b) => Ok(qfloat!(if *b { 0.0 } else { f64::NAN })),
            qint!(i) => Ok(qfloat!((*i as f64).acosh())),
            qfloat!(f) => Ok(qfloat!(f.acosh())),
            qcomplex!(c) => Ok(qcomplex!(c.acosh())),
            _ => Err(qerr!("invalid type in acosh")),
        };
    }

    pub fn artanh(&self) -> QResult<QExp> {
        return match self {
            qbool!(b) => Ok(qfloat!(if *b { f64::INFINITY } else { 0.0 })),
            qint!(i) => Ok(qfloat!((*i as f64).atanh())),
            qfloat!(f) => Ok(qfloat!(f.atanh())),
            qcomplex!(c) => Ok(qcomplex!(c.atanh())),
            _ => Err(qerr!("invalid type in atanh")),
        };
    }

    pub fn arg(&self) -> QResult<QExp> {
        return match self {
            qbool!(_) => Ok(qfloat!(0.0)),
            qint!(i) => Ok(qfloat!(
                match i.cmp(&0) {
                    cmp::Ordering::Greater => 0.0,
                    cmp::Ordering::Less => std::f64::consts::PI,
                    cmp::Ordering::Equal => f64::NAN,
                }
            )),
            qfloat!(f) => Ok(qfloat!(
                match f.partial_cmp(&0.0) {
                    Some(cmp::Ordering::Greater) => 0.0,
                    Some(cmp::Ordering::Less) => std::f64::consts::PI,
                    Some(cmp::Ordering::Equal) => f64::NAN,
                    None => f64::NAN,
                }
            )),
            qcomplex!(c) => Ok(qfloat!(c.arg())),
            _ => Err(qerr!("invalid type in arg")),
        };
    }

    pub fn cis(&self) -> QResult<QExp> {
        return match self {
            qbool!(b) => Ok(qcomplex!(
                if *b { C64::cis(1.0) } else { C64::cis(0.0) }
            )),
            qint!(i) => Ok(qcomplex!(C64::cis(*i as f64))),
            qfloat!(f) => Ok(qcomplex!(C64::cis(*f))),
            _ => Err(qerr!("invalid type in cis")),
        };
    }

    pub fn conj(&self) -> QResult<QExp> {
        return match self {
            qbool!(b) => Ok(qbool!(*b)),
            qint!(i) => Ok(qint!(*i)),
            qfloat!(f) => Ok(qfloat!(*f)),
            qcomplex!(c) => Ok(qcomplex!(c.conj())),
            _ => Err(qerr!("invalid type in conj")),
        };
    }

    pub fn real(&self) -> QResult<QExp> {
        return match self {
            qbool!(b) => Ok(qfloat!(if *b { 1.0 } else { 0.0 })),
            qint!(i) => Ok(qfloat!(*i as f64)),
            qfloat!(f) => Ok(qfloat!(*f)),
            qcomplex!(c) => Ok(qfloat!(c.re)),
            _ => Err(qerr!("invalid type in real")),
        };
    }

    pub fn imag(&self) -> QResult<QExp> {
        return match self {
            qbool!(_) => Ok(qfloat!(0.0)),
            qint!(_) => Ok(qfloat!(0.0)),
            qfloat!(_) => Ok(qfloat!(0.0)),
            qcomplex!(c) => Ok(qfloat!(c.im)),
            _ => Err(qerr!("invalid type in imag")),
        };
    }
}

impl PartialEq for QExp {
    fn eq(&self, other: &QExp) -> bool { self.equals(other) }
}

impl fmt::Debug for QExp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return match self {
            qbool!(x) => write!(f, "bool({:?})", x),
            qint!(x) => write!(f, "int({:?})", x),
            qfloat!(x) => write!(f, "float({:?})", x),
            qcomplex!(x) => write!(f, "complex({:?})", x),
            qlist!(x) => {
                let formatted: Vec<String>
                    = x.iter().map(|xk| format!("{:?}", xk)).collect();
                write!(f, "list([{}])", formatted.join(", "))
            },
            qstr!(x) => write!(f, "str({:?})", x),
            qsymbol!(x) => write!(f, "symbol({:?})", x),
            // qfunc!(name, _) => write!(f, "func({} {{ ... }})", name),
            qfunc!(x) => write!(f, "func({} {{ ... }})", x.name),
            qlambda!(x)
                => write!(f, "lambda({:?} {{ ... }})", x.arg_pattern.as_ref()),
        }
    }
}

impl fmt::Display for QExp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return match self {
            qbool!(x) => x.fmt(f),
            qint!(x) => x.fmt(f),
            qfloat!(x) => x.fmt(f),
            qcomplex!(x) => x.fmt(f),
            qlist!(x) => {
                let n: usize = x.len();
                write!(f, "(")?;
                for (k, xk) in x.iter().enumerate() {
                    match xk {
                        qstr!(s) => {
                            write!(f, "\"")?;
                            s.fmt(f)?;
                            write!(f, "\"")?;
                        },
                        _ => { xk.fmt(f)?; },
                    }
                    if k < n - 1 { write!(f, ", ")?; }
                }
                write!(f, ")")
            },
            qstr!(x) => x.fmt(f),
            qsymbol!(x) => write!(f, "{}", x),
            // qfunc!(name, _) => write!(f, "func <{}> {{ ... }}", name),
            qfunc!(x) => write!(f, "func <{}> {{ ... }}", x.name),
            qlambda!(x) => {
                let n: usize = x.arg_pattern.as_ref().len();
                write!(f, "lambda (")?;
                for (k, sk) in x.arg_pattern.as_ref().iter().enumerate() {
                    sk.fmt(f)?;
                    if k < n - 1 { write!(f, ", ")?; }
                }
                write!(f, ") {{ ... }}")
            },
        };
    }
}

impl strfmt::DisplayStr for QExp {
    fn display_str(&self, f: &mut strfmt::Formatter<'_, '_>)
        -> strfmt::Result<()>
    {
        return (&self).display_str(f)
    }
}

impl strfmt::DisplayStr for &QExp {
    fn display_str(&self, f: &mut strfmt::Formatter<'_, '_>)
        -> strfmt::Result<()>
    {
        return match self {
            qbool!(x) => {
                if *x {
                    write!(f, "true")
                        .map_err(|_| strfmt::FmtError::Invalid(
                            "invalid write".to_string()
                        ))
                } else {
                    write!(f, "false")
                        .map_err(|_| strfmt::FmtError::Invalid(
                            "invalid write".to_string()
                        ))
                }
            },
            qint!(x) => (*x).display_str(f),
            qfloat!(x) => (*x).display_str(f),
            qcomplex!(x) => {
                x.re.display_str(f)?;
                match f.sign() {
                    strfmt::Sign::Unspecified
                    | strfmt::Sign::Minus
                    | strfmt::Sign::Space
                        => {
                            if x.im >= 0.0 {
                                write!(f, "+")
                                    .map_err(|_| strfmt::FmtError::Invalid(
                                        "invalid write".to_string()
                                    ))?;
                            }
                            x.im.display_str(f)?;
                        },
                    strfmt::Sign::Plus => { x.im.display_str(f)?; },
                }
                write!(f, "i")
                    .map_err(|_| strfmt::FmtError::Invalid(
                        "invalid write".to_string()
                    ))
            },
            qlist!(x) => {
                let n: usize = x.len();
                write!(f, "(")
                    .map_err(|_| strfmt::FmtError::Invalid(
                            "invalid write".to_string()
                    ))?;
                for (k, xk) in x.iter().enumerate() {
                    match xk {
                        qstr!(s) => {
                            write!(f, "\"")
                                .map_err(|_| strfmt::FmtError::Invalid(
                                    "invalid write".to_string()
                                ))?;
                            s.display_str(f)?;
                            write!(f, "\"")
                                .map_err(|_| strfmt::FmtError::Invalid(
                                    "invalid write".to_string()
                                ))?;
                        },
                        _ => { xk.display_str(f)?; },
                    }
                    if k < n - 1 {
                        write!(f, ", ")
                            .map_err(|_| strfmt::FmtError::Invalid(
                                "invalid write".to_string()
                            ))?;
                    }
                }
                write!(f, ")")
                    .map_err(|_| strfmt::FmtError::Invalid(
                        "invalid write".to_string()
                    ))
            },
            qstr!(x) => x.display_str(f),
            qsymbol!(x) => write!(f, "{}", x)
                .map_err(|_| strfmt::FmtError::Invalid(
                    "invalid write".to_string()
                )),
            qfunc!(x) => write!(f, "func <{}> {{ ... }}", x.name)
                .map_err(|_| strfmt::FmtError::Invalid(
                    "invalid write".to_string()
                )),
            // qfunc!(name, _) => write!(f, "func <{}> {{ ... }}", name)
            //     .map_err(|_| strfmt::FmtError::Invalid(
            //         "invalid write".to_string()
            //     )),
            qlambda!(x) => {
                let n: usize = x.arg_pattern.as_ref().len();
                write!(f, "lambda (")
                    .map_err(|_| strfmt::FmtError::Invalid(
                        "invalid write".to_string()
                    ))?;
                for (k, sk) in x.arg_pattern.as_ref().iter().enumerate() {
                    sk.display_str(f)?;
                    if k < n - 1 {
                        write!(f, ", ")
                            .map_err(|_| strfmt::FmtError::Invalid(
                                "invalid write".to_string()
                            ))?;
                    }
                }
                write!(f, ") {{ ... }}")
                    .map_err(|_| strfmt::FmtError::Invalid(
                        "invalid write".to_string()
                    ))
            },
        };
    }
}

#[derive(Copy, Clone)]
pub struct QBuiltin {
    pub f: fn(&mut QEnv, &[QExp]) -> QResult<QExp>,
    pub name: &'static str,
    pub help_text: &'static str,
    pub alias: Option<&'static str>,
}

#[derive(Clone)]
pub struct QLambda {
    pub arg_pattern: Rc<Vec<QExp>>,
    pub body_exp: Rc<QExp>,
}

impl QLambda {
    pub fn get_arg_pattern(&self) -> &Rc<Vec<QExp>> { &self.arg_pattern }

    pub fn get_body_exp(&self) -> &Rc<QExp> { &self.body_exp }

    pub fn env<'a>(&self, args: &[QExp], outer_env: &'a mut QEnv)
        -> QResult<QEnv<'a>>
    {
        fn let_assignment(
            lhs: &[QExp],
            rhs: &[QExp],
            data: &mut HashMap<String, QEnvEntry>
        ) -> QResult<()>
        {
            if lhs.len() != rhs.len() {
                return Err(qerr!(
                    "fn eval: could not match values to arg pattern"
                ));
            }
            for (lk, rk) in lhs.iter().zip(rhs) {
                match (lk, rk) {
                    (qsymbol!(s), q) => {
                        data.insert(s.clone(), QEnvEntry::Exp(q.clone()));
                    },
                    (qlist!(l), qlist!(r)) => {
                        let_assignment(l, r, data)?;
                    },
                    _ => {
                        return Err(qerr!(
                            "fn eval: could not match values to arg pattern"
                        ))
                    },
                }
            }
            return Ok(());
        }

        let values: Vec<QExp> = outer_env.eval_multi(args)?;
        let mut data: HashMap<String, QEnvEntry> = HashMap::new();
        let_assignment(self.arg_pattern.as_ref(), &values, &mut data)?;
        return Ok(QEnv {
            data,
            outer: Some(outer_env),
            dir: outer_env.dir.clone(),
            outfile: None,
        });
    }

    pub fn eval(&self, outer_env: &mut QEnv, args: &[QExp])
        -> QResult<QExp>
    {
        let mut env: QEnv = self.env(args, outer_env)?;
        return env.eval(&self.body_exp);
    }
}

// #[derive(Clone)]
// pub enum QArray {
//     Bool(nd::ArrayD<bool>),
//     Int(nd::ArrayD<i64>),
//     Float(nd::ArrayD<f64>),
//     Complex(nd::ArrayD<C64>),
// }

pub enum Indexable {
    List(Vec<QExp>),
    Str(String),
}

impl Indexable {
    pub fn from_qexp(exp: QExp) -> QResult<Self> {
        return match exp {
            qlist!(l) => Ok(Indexable::List(l)),
            qstr!(s) => Ok(Indexable::Str(s)),
            _ => Err(qerr!("arg must be a str or list")),
        };
    }

    pub fn from_qexp_list(exp: QExp) -> QResult<Self> {
        return match exp {
            qlist!(l) => Ok(Indexable::List(l)),
            _ => Err(qerr!("arg must be a list")),
        };
    }

    pub fn from_qexp_str(exp: QExp) -> QResult<Self> {
        return match exp {
            qstr!(s) => Ok(Indexable::Str(s)),
            _ => Err(qerr!("arg must be a str")),
        };
    }

    pub fn is_list(&self) -> bool {
        return match self {
            Indexable::List(_) => true,
            Indexable::Str(_) => false,
        };
    }

    pub fn is_str(&self) -> bool {
        return match self {
            Indexable::List(_) => false,
            Indexable::Str(_) => true,
        };
    }

    pub fn repeat(&self, n: usize) -> QResult<QExp> {
        return match self {
            Indexable::List(l) => {
                let mut acc: Vec<QExp> = Vec::new();
                for _ in 0..n {
                    acc.append(&mut l.clone());
                }
                Ok(qlist!(acc))
            },
            Indexable::Str(s) => {
                let mut acc: String = String::new();
                for _ in 0..n {
                    acc.push_str(s);
                }
                Ok(qstr!(acc))
            },
        };
    }

    pub fn len(&self) -> usize {
        return match self {
            Indexable::List(l) => l.len(),
            Indexable::Str(s) => s.len(),
        };
    }

    pub fn is_empty(&self) -> bool { self.len() == 0 }

    pub fn get(&self, idx: usize) -> Option<QExp> {
        return match self {
            Indexable::List(l) => l.get(idx).cloned(),
            Indexable::Str(s)
                => s.get(idx..idx + 1).map(|s| qstr!(s.to_string())),
        };
    }

    pub fn set(&self, vals: &[(usize, QExp)]) -> QResult<QExp> {
        return match self {
            Indexable::List(l) => {
                let mut ret: Vec<QExp> = l.clone();
                let n: usize = ret.len();
                vals.iter()
                    .map(|(k, qk)| {
                        if *k < n {
                            ret[*k] = qk.clone();
                            Ok(())
                        } else {
                            Err(qerr_fmt!(
                                "index {} out of bounds for an object of \
                                length {}",
                                k, n
                            ))
                        }
                    })
                    .collect::<QResult<Vec<()>>>()
                    .map(|_| qlist!(ret))
            },
            Indexable::Str(_) => Err(qerr!("cannot operate on a str")),
        };
    }

    pub fn slice(&self, beg: usize, end: usize, step: usize) -> QResult<QExp> {
        if step == 0 {
            return Err(qerr!("step must not be 0"));
        }
        if beg >= self.len() {
            return Err(qerr_fmt!(
                "start index {} out of bounds for object of length {}",
                beg, self.len()
            ));
        }
        if end > self.len() {
            return Err(qerr_fmt!(
                "stop index {} out of bounds for object of length {}",
                end, self.len()
            ));
        }
        if beg > end {
            return match self {
                Indexable::List(_) => Ok(qlist!(vec![])),
                Indexable::Str(_) => Ok(qstr!("".to_string())),
            };
        }
        return match self {
            Indexable::List(l) => {
                Ok(qlist!(
                    l[beg..end].iter()
                    .step_by(step)
                    .cloned()
                    .collect()
                ))
            },
            Indexable::Str(s) => {
                Ok(qstr!(
                    s[beg..end].chars()
                    .step_by(step)
                    .collect()
                ))
            },
        };
    }

    pub fn slice_inc(&self, beg: usize, end: usize, step: usize)
        -> QResult<QExp>
    {
        if step == 0 {
            return Err(qerr!("step must not be 0"));
        }
        if beg >= self.len() {
            return Err(qerr_fmt!(
                "start index {} out of bounds for object of length {}",
                beg, self.len()
            ));
        }
        if end >= self.len() {
            return Err(qerr_fmt!(
                "stop index {} out of bounds for object of length {}",
                end, self.len()
            ));
        }
        if beg > end {
            return match self {
                Indexable::List(_) => Ok(qlist!(vec![])),
                Indexable::Str(_) => Ok(qstr!("".to_string())),
            };
        }
        return match self {
            Indexable::List(l) => {
                Ok(qlist!(
                    l[beg..=end].iter()
                    .step_by(step)
                    .cloned()
                    .collect()
                ))
            },
            Indexable::Str(s) => {
                Ok(qstr!(
                    s[beg..=end].chars()
                    .step_by(step)
                    .collect()
                ))
            },
        };
    }

    pub fn enumerate(&self) -> QResult<QExp> {
        return match self {
            Indexable::List(l) => {
                Ok(qlist!(
                    l.iter().cloned().enumerate()
                    .map(|(k, qk)| qlist!(vec![qint!(k as i64), qk]))
                    .collect()
                ))
            },
            Indexable::Str(s) => {
                Ok(qlist!(
                    s.chars().enumerate()
                    .map(|(k, sk)| qlist!(
                        vec![qint!(k as i64), qstr!(sk.to_string())]
                    ))
                    .collect()
                ))
            },
        };
    }

    pub fn pick(&self, idx: &[usize]) -> QResult<QExp> {
        return if let Indexable::List(l) = self {
            let exps: Vec<QExp> = idx.iter()
                .map(|k| {
                    l.get(*k)
                        .cloned()
                        .ok_or_else(|| qerr_fmt!(
                            "index {} out of bounds for object of length {}",
                            k, l.len()
                        ))
                })
                .collect::<QResult<Vec<QExp>>>()?;
            Ok(qlist!(exps))
        } else if let Indexable::Str(s) = self {
            let ss: String = idx.iter()
                .map(|k| {
                    s.get(*k..*k + 1)
                        .ok_or_else(|| qerr_fmt!(
                            "index {} out of bounds for object of length {}",
                            k, s.len()
                        ))
                })
                .collect::<QResult<String>>()?;
            Ok(qstr!(ss))
        } else {
            Err(qerr!("unexpected state"))
        };
    }

    pub fn reverse(&self) -> QResult<QExp> {
        return match self {
            Indexable::List(l) => Ok(qlist!(l.iter().rev().cloned().collect())),
            Indexable::Str(s) => Ok(qstr!(s.chars().rev().collect())),
        };
    }

    pub fn cycle(&self, shift: isize) -> QResult<QExp> {
        return match self {
            Indexable::List(l) => {
                let mut new = l.clone();
                match shift.cmp(&0) {
                    cmp::Ordering::Greater => {
                        new.rotate_right(shift as usize);
                    },
                    cmp::Ordering::Less => {
                        new.rotate_left(shift.unsigned_abs() as usize);
                    },
                    cmp::Ordering::Equal => { },
                }
                Ok(qlist!(new))
            },
            Indexable::Str(s) => {
                let n: usize = s.len();
                let d: usize = shift.rem_euclid(n as isize) as usize;
                Ok(qstr!(s[d..n].to_string() + &s[0..n]))
            },
        };
    }

    pub fn first(&self) -> QResult<QExp> {
        return match self {
            Indexable::List(l)
                => l.first().cloned()
                .ok_or_else(|| qerr!("list must have length at least 1")),
            Indexable::Str(s)
                => {
                    let S: String = s.chars().take(1).collect();
                    if !S.is_empty() {
                        Ok(qstr!(S))
                    } else {
                        Err(qerr!("str must have length at least 1"))
                    }
                },
        };
    }

    pub fn rest(&self) -> QResult<QExp> {
        return match self {
            Indexable::List(l)
                => Ok(qlist!(l.iter().skip(1).cloned().collect())),
            Indexable::Str(s)
                => Ok(qstr!(s.chars().skip(1).collect())),
        };
    }

    pub fn take(&self, n: usize) -> QResult<QExp> {
        return match self {
            Indexable::List(l)
                => Ok(qlist!(l.iter().take(n).cloned().collect())),
            Indexable::Str(s)
                => Ok(qstr!(s.chars().take(n).collect())),
        };
    }

    pub fn take_while<F>(&self, mut f: F) -> QResult<QExp>
    where F: FnMut(&[QExp]) -> QResult<QExp>
    {
        return match self {
            Indexable::List(l) => {
                let mut acc: Vec<QExp> = Vec::new();
                for qk in l.iter() {
                    match f(&[qk.clone()])? {
                        qbool!(b) => {
                            if b { acc.push(qk.clone()); } else { break; }
                        },
                        _ => {
                            return Err(qerr!("predicate must return a bool"));
                        },
                    }
                }
                Ok(qlist!(acc))
            },
            Indexable::Str(s) => {
                let mut acc: String = String::new();
                for sk in s.chars() {
                    match f(&[qstr!(sk.to_string())])? {
                        qbool!(b) => {
                            if b { acc.push(sk); } else { break; }
                        },
                        _ => {
                            return Err(qerr!("predicate must return a bool"));
                        },
                    }
                }
                Ok(qstr!(acc))
            },
        };
    }

    pub fn last(&self) -> QResult<QExp> {
        return match self {
            Indexable::List(l)
                => l.last().cloned()
                .ok_or_else(|| qerr!("list must have length at least 1")),
            Indexable::Str(s)
                => {
                    Ok(qstr!(
                        s.chars().last()
                        .ok_or_else(
                            || qerr!("str must have length at least 1")
                        )?
                        .to_string()
                    ))
                },
        };
    }

    pub fn skip(&self, n: usize) -> QResult<QExp> {
        return match self {
            Indexable::List(l)
                => Ok(qlist!(l.iter().skip(n).cloned().collect())),
            Indexable::Str(s)
                => Ok(qstr!(s.chars().skip(n).collect())),
        };
    }

    pub fn skip_while<F>(&self, mut f: F) -> QResult<QExp>
    where F: FnMut(&[QExp]) -> QResult<QExp>
    {
        return match self {
            Indexable::List(l) => {
                let mut acc: Vec<QExp> = Vec::new();
                let mut take: bool = false;
                for qk in l.iter() {
                    if !take {
                        match f(&[qk.clone()])? {
                            qbool!(b) => {
                                if b {
                                    continue;
                                } else {
                                    acc.push(qk.clone());
                                    take = true;
                                }
                            },
                            _ => {
                                return Err(qerr!(
                                    "predicate must return a bool"));
                            }
                        }
                    } else {
                        acc.push(qk.clone());
                    }
                }
                Ok(qlist!(acc))
            },
            Indexable::Str(s) => {
                let mut acc: String = String::new();
                let mut take: bool = false;
                for sk in s.chars() {
                    if !take {
                        match f(&[qstr!(sk.to_string())])? {
                            qbool!(b) => {
                                if b {
                                    continue;
                                } else {
                                    acc.push(sk);
                                    take = true;
                                }
                            },
                            _ => {
                                return Err(qerr!(
                                    "predicate must return a bool"));
                            }
                        }
                    } else {
                        acc.push(sk);
                    }
                }
                Ok(qstr!(acc))
            },
        };
    }

    pub fn split_at(&self, idx: usize) -> QResult<QExp> {
        return match self {
            Indexable::List(l) => {
                let (ll, lr): (&[QExp], &[QExp]) = l.split_at(idx);
                Ok(qlist!(vec![qlist!(ll.to_vec()), qlist!(lr.to_vec())]))
            },
            Indexable::Str(s) => {
                let (sl, sr): (&str, &str) = s.split_at(idx);
                Ok(qlist!(vec![qstr!(sl.to_string()), qstr!(sr.to_string())]))
            },
        };
    }

    pub fn split_on<F>(&self, mut f: F) -> QResult<QExp>
    where F: FnMut(&[QExp]) -> QResult<QExp>
    {
        return match self {
            Indexable::List(l) => {
                let mut acc: Vec<QExp> = Vec::new();
                let mut cur: Vec<QExp> = Vec::new();
                for qk in l.iter() {
                    match f(&[qk.clone()])? {
                        qbool!(b) => {
                            if b {
                                acc.push(qlist!(mem::take(&mut cur)));
                            } else {
                                cur.push(qk.clone());
                            }
                        }
                        _ => {
                            return Err(qerr!("predicate must return a bool"));
                        },
                    }
                }
                if !cur.is_empty() {
                    acc.push(qlist!(cur));
                }
                Ok(qlist!(acc))
            },
            Indexable::Str(s) => {
                let mut acc: Vec<QExp> = Vec::new();
                let mut cur: String = String::new();
                for sk in s.chars() {
                    match f(&[qstr!(sk.to_string())])? {
                        qbool!(b) => {
                            if b {
                                acc.push(qstr!(mem::take(&mut cur)));
                            } else {
                                cur.push(sk);
                            }
                        }
                        _ => {
                            return Err(qerr!("predicate must return a bool"));
                        },
                    }
                }
                if !cur.is_empty() {
                    acc.push(qstr!(cur));
                }
                Ok(qlist!(acc))
            },
        };
    }

    pub fn split_on_inc<F>(&self, mut f: F) -> QResult<QExp>
    where F: FnMut(&[QExp]) -> QResult<QExp>
    {
        return match self {
            Indexable::List(l) => {
                let mut acc: Vec<QExp> = Vec::new();
                let mut cur: Vec<QExp> = Vec::new();
                for qk in l.iter() {
                    match f(&[qk.clone()])? {
                        qbool!(b) => {
                            if b {
                                cur.push(qk.clone());
                                acc.push(qlist!(mem::take(&mut cur)));
                            } else {
                                cur.push(qk.clone());
                            }
                        }
                        _ => {
                            return Err(qerr!("predicate must return a bool"));
                        },
                    }
                }
                if !cur.is_empty() {
                    acc.push(qlist!(cur));
                }
                Ok(qlist!(acc))
            },
            Indexable::Str(s) => {
                let mut acc: Vec<QExp> = Vec::new();
                let mut cur: String = String::new();
                for sk in s.chars() {
                    match f(&[qstr!(sk.to_string())])? {
                        qbool!(b) => {
                            if b {
                                cur.push(sk);
                                acc.push(qstr!(mem::take(&mut cur)));
                            } else {
                                cur.push(sk);
                            }
                        }
                        _ => {
                            return Err(qerr!("predicate must return a bool"));
                        },
                    }
                }
                if !cur.is_empty() {
                    acc.push(qstr!(cur));
                }
                Ok(qlist!(acc))
            },
        };
    }

    pub fn zip(&self, other: &Indexable) -> QResult<QExp> {
        return match (self, other) {
            (Indexable::List(ll), Indexable::List(lr)) => {
                Ok(qlist!(
                    ll.iter().cloned().zip(lr.iter().cloned())
                    .map(|(llk, lrk)| qlist!(vec![llk, lrk]))
                    .collect()
                ))
            },
            (Indexable::List(ll), Indexable::Str(sr)) => {
                Ok(qlist!(
                    ll.iter().cloned().zip(sr.chars())
                    .map(|(llk, srk)| {
                        qlist!(vec![llk, qstr!(srk.to_string())])
                    })
                    .collect()
                ))
            },
            (Indexable::Str(sl), Indexable::List(lr)) => {
                Ok(qlist!(
                    sl.chars().zip(lr.iter().cloned())
                    .map(|(slk, lrk)| {
                        qlist!(vec![qstr!(slk.to_string()), lrk])
                    })
                    .collect()
                ))
            },
            (Indexable::Str(sl), Indexable::Str(sr)) => {
                Ok(qlist!(
                    sl.chars().zip(sr.chars())
                    .map(|(slk, srk)| {
                        qlist!(vec![
                            qstr!(slk.to_string()),
                            qstr!(srk.to_string())
                        ])
                    })
                    .collect()
                ))
            },
        };
    }

    pub fn join(&self, other: &Indexable) -> QResult<QExp> {
        return match (self, other) {
            (Indexable::List(ll), Indexable::List(lr)) => {
                Ok(qlist!(
                    std::iter::Iterator::chain(
                        ll.iter().cloned(),
                        lr.iter().cloned(),
                    )
                    .collect()
                ))
            },
            (Indexable::List(ll), Indexable::Str(sr)) => {
                Ok(qlist!(
                    std::iter::Iterator::chain(
                        ll.iter().cloned(),
                        sr.chars().map(|srk| qstr!(srk.to_string())),
                    )
                    .collect()
                ))
            },
            (Indexable::Str(sl), Indexable::List(lr)) => {
                Ok(qlist!(
                    std::iter::Iterator::chain(
                        sl.chars().map(|slk| qstr!(slk.to_string())),
                        lr.iter().cloned(),
                    )
                    .collect()
                ))
            },
            (Indexable::Str(sl), Indexable::Str(sr)) => {
                Ok(qlist!(
                    std::iter::Iterator::chain(
                        sl.chars().map(|slk| qstr!(slk.to_string())),
                        sr.chars().map(|srk| qstr!(srk.to_string())),
                    )
                    .collect()
                ))
            },
        };
    }

    pub fn append(&self, items: &[QExp]) -> QResult<QExp> {
        return match self {
            Indexable::List(l) => {
                let mut ll: Vec<QExp> = l.clone();
                ll.append(&mut items.to_vec());
                Ok(qlist!(ll))
            },
            Indexable::Str(s) => {
                let addl: String
                    = items.iter()
                    .map(|qk| match qk {
                        qstr!(sk) => Ok(sk.clone()),
                        _ => Err(qerr!("can only append strs to a str")),
                    })
                    .collect::<QResult<Vec<String>>>()?
                    .join("");
                Ok(qstr!(s.clone() + &addl))
            },
        };
    }

    pub fn prepend(&self, items: &[QExp]) -> QResult<QExp> {
        return match self {
            Indexable::List(l) => {
                let mut ll: Vec<QExp> = items.iter().rev().cloned().collect();
                ll.append(&mut l.clone());
                Ok(qlist!(ll))
            },
            Indexable::Str(s) => {
                let addl: String
                    = items.iter()
                    .map(|qk| match qk {
                        qstr!(sk) => Ok(sk.clone()),
                        _ => Err(qerr!("can only prepend strs to a str")),
                    })
                    .collect::<QResult<Vec<String>>>()?
                    .join("");
                Ok(qstr!(addl + s))
            },
        };
    }

    pub fn insert(&self, idx: usize, items: &[QExp]) -> QResult<QExp> {
        if idx >= self.len() {
            return Err(qerr_fmt!(
                "index {} out of bounds for object of length {}",
                idx, self.len()
            ));
        }
        return match self {
            Indexable::List(l) => {
                let mut ll: Vec<QExp> = l.clone();
                items.iter().rev()
                    .for_each(|qk| { ll.insert(idx, qk.clone()) });
                Ok(qlist!(ll))
            },
            Indexable::Str(s) => {
                let mut ss: String = s.clone();
                for qk in items.iter().rev() {
                    if let qstr!(ins) = qk {
                        ss.insert_str(idx, ins);
                    } else {
                        return Err(qerr!("can only insert a str into a str"));
                    }
                }
                Ok(qstr!(ss))
            },
        };
    }

    pub fn map<F>(&self, mut f: F) -> QResult<QExp>
    where F: FnMut(&[QExp]) -> QResult<QExp>
    {
        return match self {
            Indexable::List(l) => {
                Ok(qlist!(
                    l.iter()
                    .map(|qk| f(&[qk.clone()]))
                    .collect::<QResult<Vec<QExp>>>()?
                ))
            },
            Indexable::Str(s) => {
                Ok(qlist!(
                    s.chars()
                    .map(|sk| f(&[qstr!(sk.to_string())]))
                    .collect::<QResult<Vec<QExp>>>()?
                ))
            },
        };
    }

    pub fn filter<F>(&self, mut f: F) -> QResult<QExp>
    where F: FnMut(&[QExp]) -> QResult<QExp>
    {
        return match self {
            Indexable::List(l) => {
                let mut acc: Vec<QExp> = Vec::new();
                for qk in l.iter() {
                    match f(&[qk.clone()])? {
                        qbool!(true) => { acc.push(qk.clone()); },
                        qbool!(false) => { continue; },
                        _ => {
                            return Err(qerr!("predicate must return a bool"));
                        },
                    }
                }
                Ok(qlist!(acc))
            },
            Indexable::Str(s) => {
                let mut acc: String = String::new();
                for sk in s.chars() {
                    match f(&[qstr!(sk.to_string())])? {
                        qbool!(true) => { acc.push(sk); },
                        qbool!(false) => { continue; },
                        _ => {
                            return Err(qerr!("predicate must return a bool"));
                        },
                    }
                }
                Ok(qstr!(acc))
            },
        };
    }

    pub fn unique(&self) -> QResult<QExp> {
        return match self {
            Indexable::List(l) => {
                let mut acc: Vec<QExp> = Vec::new();
                l.iter()
                    .for_each(|qk| {
                        if !acc.contains(qk) { acc.push(qk.clone()); }
                    });
                Ok(qlist!(acc))
            },
            Indexable::Str(s) => {
                let mut acc: String = String::new();
                s.chars()
                    .for_each(|ck| {
                        if !acc.contains(ck) { acc.push(ck); }
                    });
                Ok(qstr!(acc))
            },
        };
    }

    pub fn flatten_qexp(exps: &[QExp]) -> Vec<QExp> {
        let mut ret: Vec<QExp> = Vec::new();
        for q in exps.iter() {
            match q {
                qlist!(l) => {
                    ret.append(&mut Indexable::flatten_qexp(l));
                },
                qexp => {
                    ret.push(qexp.clone());
                },
            }
        }
        return ret;
    }

    pub fn flatten(&self) -> QResult<QExp> {
        return match self {
            Indexable::List(l) => Ok(qlist!(Indexable::flatten_qexp(l))),
            Indexable::Str(_) => Err(qerr!("can only flatten lists")),
        };
    }

    pub fn contains(&self, exp: &QExp) -> QResult<QExp> {
        return match self {
            Indexable::List(l) => Ok(qbool!(l.contains(exp))),
            Indexable::Str(s) => {
                match exp {
                    qstr!(sp) => Ok(qbool!(s.contains(sp))),
                    _ => Err(qerr!("strs can only contain other strs")),
                }
            },
        };
    }

    pub fn index_of(&self, exp: &QExp) -> QResult<QExp> {
        return match self {
            Indexable::List(l) => {
                for (k, qk) in l.iter().enumerate() {
                    if exp.equals(qk) { return Ok(qint!(k as i64)); }
                }
                Ok(qint!(-1))
            },
            Indexable::Str(s) => {
                match exp {
                    qstr!(sp) => {
                        match sp.len().cmp(&s.len()) {
                            cmp::Ordering::Greater
                                => Ok(qint!(-1)),
                            cmp::Ordering::Equal
                                => Ok(qint!(if sp == s { -1 } else { 0 })),
                            cmp::Ordering::Less => {
                                let n: usize = sp.len();
                                for k in 0..s.len() - sp.len() {
                                    if sp == s.get(k..k + n).unwrap() {
                                        return Ok(qint!(k as i64))
                                    }
                                }
                                Ok(qint!(-1))
                            },
                        }
                    },
                    _ => Err(qerr!("strs can only contain other strs")),
                }
            },
        };
    }

    pub fn fold<F>(&self, start: &QExp, mut f: F) -> QResult<QExp>
    where F: FnMut(&[QExp]) -> QResult<QExp>
    {
        return match self {
            Indexable::List(l) => {
                let mut acc: QExp = start.clone();
                for qk in l.iter() {
                    acc = f(&[acc.clone(), qk.clone()])?;
                }
                Ok(acc)
            },
            Indexable::Str(_) => Err(qerr!("can only fold lists")),
        };
    }

    pub fn min(&self) -> QResult<QExp> {
        return match self {
            Indexable::List(l) => {
                let mut m: &QExp
                    = l.first()
                    .ok_or_else(|| qerr!("expected non-empty list or str"))?;
                for qk in l.iter() {
                    if qk.lt(m)? {
                        m = qk;
                    }
                }
                Ok(m.clone())
            },
            Indexable::Str(s) => {
                let mut m: QExp
                    = s.get(0..1)
                    .map(|s0| qstr!(s0.to_string()))
                    .ok_or_else(|| qerr!("expected non-empty list or str"))?;
                let mut sk: QExp;
                for ck in s.chars() {
                    sk = qstr!(ck.to_string());
                    if sk.lt(&m)? {
                        m = sk;
                    }
                }
                Ok(m)
            },
        };
    }

    pub fn max(&self) -> QResult<QExp> {
        return match self {
            Indexable::List(l) => {
                let mut m: &QExp
                    = l.first()
                    .ok_or_else(|| qerr!("expected non-empty list or str"))?;
                for qk in l.iter() {
                    if qk.gt(m)? {
                        m = qk;
                    }
                }
                Ok(m.clone())
            },
            Indexable::Str(s) => {
                let mut m: QExp
                    = s.get(0..1)
                    .map(|s0| qstr!(s0.to_string()))
                    .ok_or_else(|| qerr!("expected non-empty list or str"))?;
                let mut sk: QExp;
                for ck in s.chars() {
                    sk = qstr!(ck.to_string());
                    if sk.gt(&m)? {
                        m = sk;
                    }
                }
                Ok(m)
            },
        };
    }

    pub fn select_by<F>(&self, mut f: F) -> QResult<QExp>
    where F: FnMut(&[QExp]) -> QResult<QExp>
    {
        return match self {
            Indexable::List(l) => {
                let mut m: &QExp
                    = l.first()
                    .ok_or_else(|| qerr!("expected non-empty list or str"))?;
                for qk in l.iter().skip(1) {
                    match f(&[qk.clone(), m.clone()])? {
                        qbool!(b) => { if b { m = qk; } },
                        _ => {
                            return Err(qerr!(
                                "comparison function must return a bool"));
                        },
                    }
                }
                Ok(m.clone())
            },
            Indexable::Str(s) => {
                let mut m: QExp
                    = s.get(0..1)
                    .map(|s0| qstr!(s0.to_string()))
                    .ok_or_else(|| qerr!("expected non-empty list or str"))?;
                let mut sk: QExp;
                for ck in s.chars().skip(1) {
                    sk = qstr!(ck.to_string());
                    match f(&[sk.clone(), m.clone()])? {
                        qbool!(b) => { if b { m = sk; } },
                        _ => {
                            return Err(qerr!(
                                "comparison function must return a bool"));
                        },
                    }
                }
                Ok(m)
            },
        };
    }

    pub fn merge_sort_list(items: &[QExp]) -> QResult<Vec<&QExp>> {
        return if items.len() <= 1 {
            Ok(items.iter().collect())
        } else {
            let n: usize = items.len();
            let L: Vec<&QExp> = Indexable::merge_sort_list(&items[0..n / 2])?;
            let mut L_iter = L.iter();
            let R: Vec<&QExp> = Indexable::merge_sort_list(&items[n / 2..n])?;
            let mut R_iter = R.iter();
            let mut acc: Vec<&QExp> = Vec::with_capacity(n);
            let mut l: Option<&&QExp> = L_iter.next();
            let mut r: Option<&&QExp> = R_iter.next();
            loop {
                if let (Some(&lk), Some(&rk)) = (l, r) {
                    if lk.le(rk)? {
                        acc.push(lk);
                        l = L_iter.next();
                    } else {
                        acc.push(rk);
                        r = R_iter.next();
                    }
                } else if let (Some(&lk), None) = (l, r) {
                    acc.push(lk);
                    l = L_iter.next();
                } else if let (None, Some(&rk)) = (l, r) {
                    acc.push(rk);
                    r = R_iter.next();
                } else {
                    break;
                }
            }
            Ok(acc)
        };
    }

    pub fn merge_sort_str(chars: &str) -> QResult<String> {
        return if chars.len() <= 1 {
            Ok(chars.chars().collect())
        } else {
            let n: usize = chars.len();
            let L: String
                = Indexable::merge_sort_str(chars.get(0..n / 2).unwrap())?;
            let mut L_iter = L.chars();
            let R: String
                = Indexable::merge_sort_str(chars.get(n / 2..n).unwrap())?;
            let mut R_iter = R.chars();
            let mut acc: String = String::with_capacity(n);
            let mut l: Option<char> = L_iter.next();
            let mut r: Option<char> = R_iter.next();
            loop {
                if let (Some(lk), Some(rk)) = (l, r) {
                    if lk <= rk {
                        acc.push(lk);
                        l = L_iter.next();
                    } else {
                        acc.push(rk);
                        r = R_iter.next();
                    }
                } else if let (Some(lk), None) = (l, r) {
                    acc.push(lk);
                    l = L_iter.next();
                } else if let (None, Some(rk)) = (l, r) {
                    acc.push(rk);
                    r = R_iter.next();
                } else {
                    break;
                }
            }
            Ok(acc)
        };
    }

    pub fn sort(&self) -> QResult<QExp> {
        return match self {
            Indexable::List(l) => Ok(qlist!(
                Indexable::merge_sort_list(l)?.into_iter().cloned().collect()
            )),
            Indexable::Str(s) => Ok(qstr!(
                Indexable::merge_sort_str(s)?
            )),
        };
    }

    pub fn merge_sort_list_by<'a, F>(items: &'a [QExp], f: &mut F)
        -> QResult<Vec<&'a QExp>>
    where F: FnMut(&[QExp]) -> QResult<QExp>
    {
        return if items.len() <= 1 {
            Ok(items.iter().collect())
        } else {
            let n: usize = items.len();
            let L: Vec<&QExp>
                = Indexable::merge_sort_list_by(&items[0..n / 2], f)?;
            let mut L_iter = L.iter();
            let R: Vec<&QExp>
                = Indexable::merge_sort_list_by(&items[n / 2..n], f)?;
            let mut R_iter = R.iter();
            let mut acc: Vec<&QExp> = Vec::with_capacity(n);
            let mut l: Option<&&QExp> = L_iter.next();
            let mut r: Option<&&QExp> = R_iter.next();
            loop {
                if let (Some(&lk), Some(&rk)) = (l, r) {
                    match f(&[lk.clone(), rk.clone()])? {
                        qbool!(b) => {
                            if !b {
                                acc.push(rk);
                                r = R_iter.next();
                            } else {
                                acc.push(lk);
                                l = L_iter.next();
                            }
                        },
                        _ => {
                            return Err(qerr!(
                                "comparison function must return a bool"))
                        },
                    }
                } else if let (Some(&lk), None) = (l, r) {
                    acc.push(lk);
                    l = L_iter.next();
                } else if let (None, Some(&rk)) = (l, r) {
                    acc.push(rk);
                    r = R_iter.next();
                } else {
                    break;
                }
            }
            Ok(acc)
        };
    }

    pub fn merge_sort_str_by<F>(chars: &str, f: &mut F) -> QResult<String>
    where F: FnMut(&[QExp]) -> QResult<QExp>
    {
        return if chars.len() <= 1 {
            Ok(chars.chars().collect())
        } else {
            let n: usize = chars.len();
            let L: String
                = Indexable::merge_sort_str_by(
                    chars.get(0..n / 2).unwrap(), f)?;
            let mut L_iter = L.chars();
            let R: String
                = Indexable::merge_sort_str_by(
                    chars.get(n / 2..n).unwrap(), f)?;
            let mut R_iter = R.chars();
            let mut acc: String = String::with_capacity(n);
            let mut l: Option<char> = L_iter.next();
            let mut r: Option<char> = R_iter.next();
            loop {
                if let (Some(lk), Some(rk)) = (l, r) {
                    match f(&[qstr!(lk.to_string()), qstr!(rk.to_string())])? {
                        qbool!(b) => {
                            if !b {
                                acc.push(rk);
                                r = R_iter.next();
                            } else {
                                acc.push(lk);
                                l = L_iter.next();
                            }
                        },
                        _ => {
                            return Err(qerr!(
                                "comparison function must return a bool"))
                        },
                    }
                } else if let (Some(lk), None) = (l, r) {
                    acc.push(lk);
                    l = L_iter.next();
                } else if let (None, Some(rk)) = (l, r) {
                    acc.push(rk);
                    r = R_iter.next();
                } else {
                    break;
                }
            }
            Ok(acc)
        };
    }

    pub fn sort_by<F>(&self, mut f: F) -> QResult<QExp>
    where F: FnMut(&[QExp]) -> QResult<QExp>
    {
        return match self {
            Indexable::List(l) => Ok(qlist!(
                Indexable::merge_sort_list_by(l, &mut f)?
                    .into_iter().cloned().collect()
            )),
            Indexable::Str(s) => Ok(qstr!(
                Indexable::merge_sort_str_by(s, &mut f)?
            )),
        };
    }

    pub fn permute(&self, moves: &[Vec<usize>]) -> QResult<QExp> {
        return if let Indexable::List(l) = self {
            let n: usize = self.len();
            let mut to: Vec<usize>;
            let mut ret: Vec<QExp> = l.to_vec();
            for p in moves.iter() {
                if p.len() < 2 {
                    return Err(qerr!(
                        "permutation spec must be at least 2 items"));
                }
                to = Vec::with_capacity(p.len());
                for pk in p.iter() {
                    if to.contains(pk) {
                        return Err(qerr!(
                            "permutations can contain each index at most once"
                        ));
                    } else if *pk >= n {
                        return Err(qerr_fmt!(
                            "invalid index {} for object of length {}", pk, n
                        ));
                    } else {
                        to.push(*pk);
                    }
                }
                to.rotate_left(1);
                p.iter().zip(to.iter()).rev().take(p.len() - 1)
                    .for_each(|(&a, &b)| ret.swap(a, b));
            }
            Ok(qlist!(ret))
        } else {
            Err(qerr!("can only permute lists"))
        }
    }
}

pub fn convert_type(exp: &QExp, ty: QExpType) -> QResult<QExp> {
    return match ty {
        qbool!() => {
            if let qbool!(b) = exp {
                Ok(qbool!(*b))
            } else if let qint!(i) = exp {
                Ok(qbool!(*i != 0))
            } else if let qfloat!(f) = exp {
                Ok(qbool!(*f != 0.0))
            } else if let qcomplex!(c) = exp {
                Ok(qbool!(*c != C64::zero()))
            } else if let qstr!(s) = exp {
                s.parse::<bool>()
                    .map(|b| qbool!(b))
                    .map_err(|_| qerr!("could not parse str as bool"))
            } else {
                Err(qerr!("could not convert type to bool"))
            }
        },
        qint!() => {
            if let qbool!(b) = exp {
                Ok(qint!(if *b { 1 } else { 0 }))
            } else if let qint!(i) = exp {
                Ok(qint!(*i))
            } else if let qfloat!(f) = exp {
                Ok(qint!(*f as i64))
            } else if let qstr!(s) = exp {
                s.parse::<i64>()
                    .map(|i| qint!(i))
                    .map_err(|_| qerr!("could not parse str as int"))
            } else {
                Err(qerr!("could not convert type to int"))
            }
        },
        qfloat!() => {
            if let qbool!(b) = exp {
                Ok(qfloat!(if *b { 1.0 } else { 0.0 }))
            } else if let qint!(i) = exp {
                Ok(qfloat!(*i as f64))
            } else if let qfloat!(f) = exp {
                Ok(qfloat!(*f))
            } else if let qstr!(s) = exp {
                s.parse::<f64>()
                    .map(|f| qfloat!(f))
                    .map_err(|_| qerr!("could not parse str as float"))
            } else {
                Err(qerr!("could not convert type to float"))
            }
        },
        qcomplex!() => {
            if let qbool!(b) = exp {
                Ok(qcomplex!(if *b { C64::one() } else { C64::zero() }))
            } else if let qint!(i) = exp {
                Ok(qcomplex!(C64::new(*i as f64, 0.0)))
            } else if let qfloat!(f) = exp {
                Ok(qcomplex!(C64::new(*f, 0.0)))
            } else if let qcomplex!(c) = exp {
                Ok(qcomplex!(*c))
            } else if let qstr!(s) = exp {
                s.parse::<C64>()
                    .map(|c| qcomplex!(c))
                    .map_err(|_| qerr!("could not parse str as complex"))
            } else {
                Err(qerr!("could not convert type to complex"))
            }
        },
        qlist!() => {
            if let qlist!(l) = exp {
                Ok(qlist!(l.clone()))
            } else {
                Err(qerr!("could not convert type to list"))
            }
        },
        qstr!() => Ok(qstr!(format!("{}", exp))),
        qsymbol!() => {
            if let qsymbol!(s) = exp {
                Ok(qsymbol!(s.clone()))
            } else {
                Err(qerr!("could not convert type to symbol"))
            }
        },
        qfunc!() => {
            // if let qfunc!(name, f) = exp {
            //     Ok(qfunc!(name.clone(), *f))
            // } else {
            //     Err(qerr!("could not convert type to func"))
            // }
            if let qfunc!(f) = exp {
                Ok(qfunc!(*f))
            } else {
                Err(qerr!("could not convert type to func"))
            }
        },
        qlambda!() => {
            if let qlambda!(ll) = exp {
                Ok(qlambda!(ll.clone()))
            } else {
                Err(qerr!("could not convert type to lambda"))
            }
        },
        qany!() => Ok(exp.clone()),
    };
}

pub fn convert_type_num(exp: &QExp, ty: QExpType) -> QResult<QExp> {
    return match ty {
        qbool!() | qint!() | qfloat!() | qcomplex!() => convert_type(exp, ty),
        _ => Err(qerr!("encountered non-numerical value")),
    };
}

pub fn convert_numbers_sametype(args: &[QExp])
    -> QResult<(Vec<QExp>, QExpType)>
{
    let exp_type: QExpType
        = cmp::max(
            args.iter().map(|qexp| qexp.exp_type()).max()
                .ok_or_else(|| qerr!("expected at least one item"))?,
            qint!(),
        );
    if exp_type > qcomplex!() {
        return Err(qerr!("encountered non-numerical value"));
    }
    return
        args.iter()
        .map(|x| convert_type_num(x, exp_type))
        .collect::<QResult<Vec<QExp>>>()
        .map(|v| (v, exp_type));
}

// args should be evaluated
pub fn typecast(ty: QExpType, args: &[QExp]) -> QResult<QExp> {
    if args.len() == 1 {
        if let Some(qlist!(l)) = args.first() {
            return typecast(ty, l);
        }
        return convert_type(&args[0], ty);
    } else {
        let new: Vec<QExp>
            = args.iter()
            .map(|x| convert_type(x, ty))
            .collect::<QResult<Vec<QExp>>>()?;
        return Ok(qlist!(new));
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TokenState {
    Normal = 0,
    InComment = 1,
    InString = 2,
    StringEscape = 3,
}

pub fn tokenize(input: String) -> QResult<Vec<String>> {
    let mut state = TokenState::Normal;
    let mut ret: Vec<String> = Vec::new();
    let mut term: String = String::new();
    for x in input.chars() {
        match state {
            TokenState::Normal => match x {
                ';' => {
                    if !term.is_empty() {
                        ret.push(mem::take(&mut term));
                    }
                    state = TokenState::InComment;
                },
                '"' => {
                    if !term.is_empty() {
                        return Err(qerr!("illegal insertion of '\"'"));
                    }
                    term.push(x);
                    state = TokenState::InString;
                },
                '(' | ')' => {
                    if !term.is_empty() {
                        ret.push(mem::take(&mut term));
                    }
                    ret.push(x.to_string());
                },
                ' ' | ',' | '\n' | '\t' => {
                    if !term.is_empty() {
                        ret.push(mem::take(&mut term));
                    }
                },
                _ => {
                    term.push(x);
                }
            },
            TokenState::InComment => match x {
                '\n' => { state = TokenState::Normal; },
                _ => { continue; },
            },
            TokenState::InString => match x {
                '"' => {
                    term.push(x);
                    ret.push(mem::take(&mut term));
                    state = TokenState::Normal;
                },
                '\\' => {
                    state = TokenState::StringEscape;
                },
                _ => {
                    term.push(x);
                },
            },
            TokenState::StringEscape => {
                match x {
                    '"' => { term.push('"'); },
                    '\\' => { term.push('\\'); },
                    'n' => { term.push('\n'); },
                    'r' => { term.push('\r'); },
                    't' => { term.push('\t'); },
                    '\n' => { term.push(' '); },
                    _ => {
                        return Err(qerr_fmt!(
                            "unknown escape sequence \\{}", x
                        ));
                    },
                }
                state = TokenState::InString;
            },
        }
    }
    if !term.is_empty() {
        ret.push(term);
        // ret.push(mem::take(&mut term));
    }
    return Ok(ret);
}

pub fn parse(tokens: &[String]) -> QResult<Vec<QExp>> {
    if tokens.is_empty() {
        return Ok(vec![]);
    }
    let mut exps: Vec<QExp> = Vec::new();
    let mut rest: &[String] = tokens;
    loop {
        let (exp, rest_new): (QExp, &[String]) = parse_single(rest)?;
        exps.push(exp);
        rest = rest_new;
        if rest.is_empty() { break; }
    }
    return Ok(exps);
}

fn parse_single(tokens: &[String]) -> QResult<(QExp, &[String])> {
    let (token, rest)
        = tokens.split_first()
        .ok_or_else(|| qerr!("missing closing ')'"))?;
    return match &token[..] {
        "(" => read_sequence(rest),
        ")" => Err(qerr!("unexpected ')'")),
        _ => Ok((parse_atom(token)?, rest)),
    };
}

fn read_sequence(tokens: &[String]) -> QResult<(QExp, &[String])> {
    let mut ret: Vec<QExp> = Vec::new();
    let mut xs = tokens;
    loop {
        let (next_token, rest)
            = xs.split_first()
            .ok_or_else(|| qerr!("missing closing ')'"))?;
        if next_token == ")" {
            return Ok((qlist!(ret), rest));
        }
        let (exp, new_xs) = parse_single(xs)?;
        ret.push(exp);
        xs = new_xs;
    }
}

pub fn parse_atom(token: &str) -> QResult<QExp> {
    return match token {
        "i" | "-i" => Ok(qsymbol!(token.to_string())),
        "PI" => Ok(qfloat!(std::f64::consts::PI)),
        "2PI" => Ok(qfloat!(2.0 * std::f64::consts::PI)),
        "iPI" => Ok(qcomplex!(C64 { re: 0.0, im: std::f64::consts::PI })),
        "i2PI" => Ok(qcomplex!(
            C64 { re: 0.0, im: 2.0 * std::f64::consts::PI }
        )),
        "E" => Ok(qfloat!(std::f64::consts::E)),
        "SQRT2" => Ok(qfloat!(std::f64::consts::SQRT_2)),
        "1/SQRT2" => Ok(qfloat!(std::f64::consts::FRAC_1_SQRT_2)),
        "i/SQRT2" => Ok(qcomplex!(
            C64 { re: 0.0, im: std::f64::consts::FRAC_1_SQRT_2 }
        )),
        _ => {
            if let Ok(b) = bool::from_str(token) {
                Ok(qbool!(b))
            } else if let Ok(i) = i64::from_str(token) {
                Ok(qint!(i))
            } else if let Ok(f) = f64::from_str(token) {
                Ok(qfloat!(f))
            } else if let Ok(c) = C64::from_str(token) {
                Ok(qcomplex!(c))
            } else if token.starts_with('"') {
                if token.ends_with('"') {
                    Ok(qstr!(token[1..token.len() - 1].to_string()))
                } else {
                    Err(qerr!("missing closing '\"'"))
                }
            } else {
                Ok(qsymbol!(token.to_string()))
            }
        },
    };
}

#[derive(Debug)]
pub struct QEnv<'a> {
    data: HashMap<String, QEnvEntry<'a>>,
    outer: Option<&'a QEnv<'a>>,
    pub dir: PathBuf,
    outfile: Option<File>,
}

/// All fields are cloned except for `outfile`, which is `None` in the new
/// object.
impl<'a> Clone for QEnv<'a> {
    fn clone(&self) -> Self {
        return QEnv {
            data: self.data.clone(),
            outer: self.outer.clone(),
            dir: self.dir.clone(),
            outfile: None,
        };
    }
}

#[derive(Clone, Debug)]
pub enum QEnvEntry<'a> {
    Exp(QExp),
    Mod(QEnv<'a>),
}

macro_rules! add_mod(
    ( $env:ident, $name:literal, $mod:ident ) => {
        $env.insert(
            $name.to_string(),
            QEnvEntry::Mod(QEnv {
                data: $mod,
                outer: None,
                dir: PathBuf::from("."),
                outfile: None,
            })
        );
    }
);

macro_rules! add_fn(
    ( $env:ident, $builtin:path ) => {
        $env.insert(
            $builtin.name.to_string(),
            QEnvEntry::Exp(QExp::Func($builtin))
        );
        if $builtin.alias.is_some() {
            $env.insert(
                $builtin.alias.unwrap().to_string(),
                QEnvEntry::Exp(QExp::Func($builtin))
            );
        }
    };
    ( $env:ident, $name:literal, $alias:literal, $fn:ident ) => {
        $env.insert(
            $name.to_string(),
            QEnvEntry::Exp(QExp::Func($name.to_string(), $fn))
        );
        $env.insert(
            $alias.to_string(),
            QEnvEntry::Exp(QExp::Func($name.to_string(), $fn))
        );
    };

    ( $env:ident, $name:literal, $alias:literal, $fn:path ) => {
        $env.insert(
            $name.to_string(),
            QEnvEntry::Exp(QExp::Func($name.to_string(), $fn))
        );
        $env.insert(
            $alias.to_string(),
            QEnvEntry::Exp(QExp::Func($name.to_string(), $fn))
        );
    };

    ( $env:ident, $name:literal, $fn:ident ) => {
        $env.insert(
            $name.to_string(),
            QEnvEntry::Exp(QExp::Func($name.to_string(), $fn))
        );
    };

    ( $env:ident, $name:literal, $fn:path ) => {
        $env.insert(
            $name.to_string(),
            QEnvEntry::Exp(QExp::Func($name.to_string(), $fn))
        );
    };
);

macro_rules! add_float(
    ( $env: ident, $name:literal, $f:expr ) => {
        $env.insert($name.to_string(), QEnvEntry::Exp(QExp::Float($f)));
    }
);

impl<'a> Default for QEnv<'a> {
    fn default() -> QEnv<'a> {
        let mut env: HashMap<String, QEnvEntry> = HashMap::new();
        // special -- keyword-like
        add_fn!(env,    fns::FnDef          );
        add_fn!(env, fns::FnLet             );
        add_fn!(env, fns::FnFn              );
        add_fn!(env, fns::FnDefn            );
        add_fn!(env, fns::FnIf              );
        add_fn!(env, fns::FnModule          );
        add_fn!(env, fns::FnUse             );
        add_fn!(env, fns::FnUseAll          );
        add_fn!(env, fns::FnInteract        );
        add_fn!(env, fns::FnIsdef           );
        add_fn!(env, fns::FnDel             );
        // systems
        add_fn!(env, fns::FnFormat          );
        add_fn!(env, fns::FnPrint           );
        add_fn!(env, fns::FnPrintln         );
        add_fn!(env, fns::FnPrintFlush      );
        add_fn!(env, fns::FnPrintlnFlush    );
        add_fn!(env, fns::FnRead            );
        add_fn!(env, fns::FnReadlines       );
        add_fn!(env, fns::FnWithFile        );
        add_fn!(env, fns::FnWithFileAdd     );
        add_fn!(env, fns::FnWrite           );
        add_fn!(env, fns::FnWriteln         );
        add_fn!(env, fns::FnWriteFlush      );
        add_fn!(env, fns::FnWritelnFlush    );
        add_fn!(env, fns::FnHalt            );
        add_fn!(env, fns::FnIstype          );
        add_fn!(env, fns::FnType            );
        // type-casting
        add_fn!(env, fns::FnBool            );
        add_fn!(env, fns::FnInt             );
        add_fn!(env, fns::FnFloat           );
        add_fn!(env, fns::FnComplex         );
        add_fn!(env, fns::FnList            );
        add_fn!(env, fns::FnStr             );
        // arithmetic
        add_fn!(env, fns::FnAdd             );
        add_fn!(env, fns::FnSub             );
        add_fn!(env, fns::FnMul             );
        add_fn!(env, fns::FnDiv             );
        add_fn!(env, fns::FnIdiv            );
        // boolean comparisons
        add_fn!(env, fns::FnAnd             );
        add_fn!(env, fns::FnOr              );
        add_fn!(env, fns::FnXor             );
        add_fn!(env, fns::FnEq              );
        add_fn!(env, fns::FnNeq             );
        add_fn!(env, fns::FnGt              );
        add_fn!(env, fns::FnGeq             );
        add_fn!(env, fns::FnLt              );
        add_fn!(env, fns::FnLeq             );
        // boolean accumulators
        add_fn!(env, fns::FnAll             );
        add_fn!(env, fns::FnAny             );
        add_fn!(env, fns::FnXany            );
        // iterable creation
        add_fn!(env, fns::FnRange           );
        add_fn!(env, fns::FnRangeInc        );
        add_fn!(env, fns::FnRepeat          );
        // iterable accumulation
        add_fn!(env, fns::FnLength          );
        add_fn!(env, fns::FnFold            );
        add_fn!(env, fns::FnMin             );
        add_fn!(env, fns::FnMax             );
        add_fn!(env, fns::FnSelectBy        );
        // iterable slicing and access
        add_fn!(env, fns::FnGet             );
        add_fn!(env, fns::FnSet             );
        add_fn!(env, fns::FnSlice           );
        add_fn!(env, fns::FnSliceInc        );
        add_fn!(env, fns::FnSliceBy         );
        add_fn!(env, fns::FnSliceIncBy      );
        add_fn!(env, fns::FnPick            );
        add_fn!(env, fns::FnFirst           );
        add_fn!(env, fns::FnRest            );
        add_fn!(env, fns::FnTake            );
        add_fn!(env, fns::FnTakeWhile       );
        add_fn!(env, fns::FnLast            );
        add_fn!(env, fns::FnSkip            );
        add_fn!(env, fns::FnSkipWhile       );
        // iterable transformation
        add_fn!(env, fns::FnStepBy          );
        add_fn!(env, fns::FnEnumerate       );
        add_fn!(env, fns::FnReverse         );
        add_fn!(env, fns::FnCycle           );
        add_fn!(env, fns::FnMap             );
        add_fn!(env, fns::FnFilter          );
        add_fn!(env, fns::FnUnique          );
        add_fn!(env, fns::FnFlatten         );
        add_fn!(env, fns::FnSort            );
        add_fn!(env, fns::FnSortBy          );
        add_fn!(env, fns::FnPermute         );
        // iterable division
        add_fn!(env, fns::FnSplitAt         );
        add_fn!(env, fns::FnSplitOn         );
        add_fn!(env, fns::FnSplitOnInc      );
        // iterable addition
        add_fn!(env, fns::FnAppend          );
        add_fn!(env, fns::FnPrepend         );
        add_fn!(env, fns::FnInsert          );
        add_fn!(env, fns::FnJoin            );
        add_fn!(env, fns::FnJoinWith        );
        add_fn!(env, fns::FnZip             );
        add_fn!(env, fns::FnCart            );
        // iterable testing
        add_fn!(env, fns::FnContains        );
        add_fn!(env, fns::FnIndexOf         );

        // element-wise math
        add_fn!(env, fns::FnNeg             );
        add_fn!(env, fns::FnRecip           );
        add_fn!(env, fns::FnAbs             );
        add_fn!(env, fns::FnSqrt            );
        add_fn!(env, fns::FnCbrt            );
        add_fn!(env, fns::FnExp             );
        add_fn!(env, fns::FnFloor           );
        add_fn!(env, fns::FnCeil            );
        add_fn!(env, fns::FnRound           );
        add_fn!(env, fns::FnLn              );
        add_fn!(env, fns::FnSin             );
        add_fn!(env, fns::FnCos             );
        add_fn!(env, fns::FnTan             );
        add_fn!(env, fns::FnArcsin          );
        add_fn!(env, fns::FnArccos          );
        add_fn!(env, fns::FnArctan          );
        add_fn!(env, fns::FnArctan2         );
        add_fn!(env, fns::FnSinh            );
        add_fn!(env, fns::FnCosh            );
        add_fn!(env, fns::FnTanh            );
        add_fn!(env, fns::FnArsinh          );
        add_fn!(env, fns::FnArcosh          );
        add_fn!(env, fns::FnArtanh          );
        add_fn!(env, fns::FnArg             );
        add_fn!(env, fns::FnCis             );
        add_fn!(env, fns::FnConj            );
        add_fn!(env, fns::FnReal            );
        add_fn!(env, fns::FnImag            );
        // parameterized element-wise math
        add_fn!(env, fns::FnMod             );
        add_fn!(env, fns::FnLog             );
        add_fn!(env, fns::FnPow             );
        add_fn!(env, fns::FnShl             );
        add_fn!(env, fns::FnShr             );
        // list -> list math
        add_fn!(env, fns::FnConvolve        );
        add_fn!(env, fns::FnHistogram       );
        add_fn!(env, fns::FnHistogramProb   );
        add_fn!(env, fns::FnCovariance      );
        add_fn!(env, fns::FnCorrelation     );
        // add_fn!(env, "fft",             "{F}",  fns::fn_fft             );
        // add_fn!(env, "ifft",            "{iF}", fns::fn_ifft            );
        // list -> value math
        add_fn!(env, fns::FnMean            );
        add_fn!(env, fns::FnVariance        );
        add_fn!(env, fns::FnStddev          );
        // list+1 -> value math
        add_fn!(env, fns::FnPnorm           );
        add_fn!(env, fns::FnMoment          );
        // special-arg math
        // add_fn!(env, "sample",          "?.",   fns::fn_sample          );

        let mut phys: HashMap<String, QEnvEntry> = HashMap::new();
        add_float!(phys,    "h",        6.626070040e-34         );
        add_float!(phys,    "hbar",     1.0545718001391127e-34  );
        add_float!(phys,    "c",        2.99792458e+8           );
        add_float!(phys,    "NA",       6.022140857e+23         );
        add_float!(phys,    "kB",       1.38064852e-23          );
        add_float!(phys,    "e0",       8.8541878128e-12        );
        add_float!(phys,    "u0",       1.25663706212e-6        );
        add_float!(phys,    "G",        6.67430e-11             );
        add_float!(phys,    "g",        9.80665                 );
        add_float!(phys,    "e",        1.602176634e-19         );
        add_float!(phys,    "me",       9.1093837015e-31        );
        add_float!(phys,    "mp",       1.67262192369e-27       );
        add_float!(phys,    "mn",       1.67492749804e-27       );
        add_float!(phys,    "mu",       1.66053906660e-27       );
        add_float!(phys,    "Rinf",     10973731.568160         );
        add_float!(phys,    "alpha",    7.2973525693e-3         );
        add_float!(phys,    "R",        8.314462618             );
        add_float!(phys,    "SB",       5.670366816083268e-08   );
        add_float!(phys,    "a0",       5.29177210903e-11       );
        add_float!(phys,    "uB",       9.2740100783e-24        );
        add_float!(phys,    "uN",       5.050783699e-27         );
        add_float!(phys,    "Eh",       4.3597447222071e-18     );
        add_float!(phys,    "amu",      1.66053906660e-27       );
        add_fn!(phys, fns::PhysNmrgb );
        add_fn!(phys, fns::PhysQraise);
        add_fn!(phys, fns::PhysQlower);
        add_mod!(env, "phys", phys);

        return QEnv {
            data: env,
            outer: None,
            dir: PathBuf::from("."),
            outfile: None,
        };
    }
}

impl<'a> QEnv<'a> {
    pub fn new(outer_env: Option<&'a QEnv<'a>>, dir: PathBuf) -> Self {
        return QEnv {
            data: HashMap::new(),
            outer: outer_env,
            dir,
            outfile: None,
        };
    }

    pub fn default_with_dir(dir: PathBuf) -> Self {
        return QEnv { dir, ..Self::default() };
    }

    pub fn default_with_outfile(outfile: File) -> Self {
        return QEnv { outfile: Some(outfile), ..Self::default() };
    }

    /// Use this as a way to evaluate arbitrary expressions without changing the
    /// local environment or for nested `with-file` evaluations. Mutability
    /// rules prevent the use of this as a way to cheaply construct modules,
    /// however.
    pub fn sub_env<'b>(&'b mut self) -> QEnv<'b>
    where 'a: 'b
    {
        return QEnv {
            data: HashMap::new(),
            outer: Some(self),
            dir: self.dir.clone(),
            outfile: None,
        };
    }

    pub fn has_outfile(&self) -> bool { self.outfile.is_some() }

    pub fn open_outfile<P>(&mut self, path: P, append: bool) -> QResult<()>
    where P: AsRef<Path>
    {
        return if !self.has_outfile() {
            let file = fs::OpenOptions::new()
                .create(true)
                .write(true)
                .append(append)
                .open(path)
                .map_err(|e| qerr_fmt!("cannot open file: {}", e))?;
            self.outfile = Some(file);
            Ok(())
        } else {
            Err(qerr!("output file handle already present"))
        };
    }

    pub fn close_outfile(&mut self) -> QResult<()> {
        return match mem::replace(&mut self.outfile, None) {
            Some(mut f) => {
                f.flush()
                    .map_err(|e| qerr_fmt!(
                        "failed to flush output before closing: {}", e
                    ))?;
                Ok(())
            },
            None => { Ok(()) },
        }
    }

    pub fn with_dir(mut self, dir: PathBuf) -> Self {
        self.dir = dir;
        return self;
    }

    pub fn with_outfile<P>(mut self, path: P, append: bool) -> QResult<Self>
    where P: AsRef<Path>
    {
        self.open_outfile(path, append)?;
        return Ok(self);
    }

    pub fn clone_with_outfile<P>(&self, path: P, append: bool) -> QResult<Self>
    where P: AsRef<Path>
    {
        let mut new = self.clone();
        new.open_outfile(path, append)?;
        return Ok(new);
    }

    pub fn get_outfile(&self) -> Option<&File> {
        return self.outfile.as_ref();
    }

    pub fn get_outfile_mut(&mut self) -> Option<&mut File> {
        return self.outfile.as_mut();
    }

    pub fn write_to_file(&mut self, out: &str) -> QResult<()> {
        return match self.get_outfile_mut() {
            Some(f) => {
                write!(f, "{}", out)
                    .map_err(|e| qerr_fmt!("cannot write to file: {}", e))
            },
            None => Err(qerr!("missing output file handle")),
        };
    }

    pub fn writeln_to_file(&mut self, out: &str) -> QResult<()> {
        return match self.get_outfile_mut() {
            Some(f) => {
                writeln!(f, "{}", out)
                    .map_err(|e| qerr_fmt!("cannot write to file: {}", e))
            },
            None => Err(qerr!("missing output file handle")),
        };
    }

    pub fn symbols(&self) -> Vec<String> { self.data.keys().cloned().collect() }

    pub fn len(&self) -> usize { self.data.len() }

    pub fn is_empty(&self) -> bool { self.data.is_empty() }

    pub fn get_data(&self) -> &HashMap<String, QEnvEntry<'a>> { &self.data }

    pub fn get_outer(&'a self) -> Option<&'a QEnv<'a>> {
        return (&self.outer).as_deref()
    }

    fn get_path(&self, path: &[String]) -> Option<&QEnvEntry> {
        return if let Some(sym) = path.first() {
            match (self.data.get(sym), path.len() > 1) {
                (Some(x), false) => Some(x),
                (Some(QEnvEntry::Mod(m)), true) => m.get_path(&path[1..]),
                (Some(QEnvEntry::Exp(_)), true) => None,
                (None, _) => None,
            }
        } else {
            None
        };
    }

    fn get_path_ok(&self, path: &[String]) -> QResult<Option<&QEnvEntry>> {
        return if let Some(sym) = path.first() {
            match (self.data.get(sym), path.len() > 1) {
                (Some(x), false) => Ok(Some(x)),
                (Some(QEnvEntry::Mod(m)), true) => m.get_path_ok(&path[1..]),
                (Some(QEnvEntry::Exp(_)), true) => {
                    Err(qerr_fmt!("encountered non-module '{}' in path", sym))
                },
                (None, _) => Ok(None),
            }
        } else {
            Err(qerr!("unexpected empty path"))
        };
    }

    pub fn get(&self, k: &str) -> Option<&QEnvEntry> {
        let path: Vec<String> = k.split("::").map(|s| s.to_string()).collect();
        return if let Some(x) = self.get_path(&path) {
            Some(x)
        } else if let Some(outer) = &self.outer {
            outer.get_path(&path)
        } else {
            None
        };
    }

    pub fn get_ok(&self, k: &str) -> QResult<&QEnvEntry> {
        let path: Vec<String> = k.split("::").map(|s| s.to_string()).collect();
        return if let Some(x) = self.get_path_ok(&path)? {
            Ok(x)
        } else if let Some(outer) = &self.outer {
            outer.get_ok(k)
        } else {
            Err(qerr_fmt!("symbol '{}' is undefined", k))
        };
    }

    fn get_exp_path(&self, path: &[String]) -> Option<&QExp> {
        return if let Some(sym) = path.first() {
            match (self.data.get(sym), path.len() > 1) {
                (Some(QEnvEntry::Exp(q)), false) => Some(q),
                (Some(QEnvEntry::Mod(_)), false) => None,
                (Some(QEnvEntry::Exp(_)), true) => None,
                (Some(QEnvEntry::Mod(m)), true) => m.get_exp_path(&path[1..]),
                (None, _) => None,
            }
        } else {
            None
        };
    }

    fn get_exp_path_ok(&self, path: &[String]) -> QResult<Option<&QExp>> {
        return if let Some(sym) = path.first() {
            match (self.data.get(sym), path.len() > 1) {
                (Some(QEnvEntry::Exp(q)), false) => Ok(Some(q)),
                (Some(QEnvEntry::Mod(_)), false) => {
                    Err(qerr_fmt!(
                        "module '{}' cannot be referenced directly", sym
                    ))
                },
                (Some(QEnvEntry::Exp(_)), true) => {
                    Err(qerr_fmt!("encountered non-module '{}' in path", sym))
                },
                (Some(QEnvEntry::Mod(m)), true)
                    => m.get_exp_path_ok(&path[1..]),
                (None, _) => Ok(None),
                // (None, _) => Err(qerr_fmt!("symbol '{}' is undefined", sym)),
            }
        } else {
            Err(qerr!("unexpected empty path"))
        };
    }

    pub fn get_exp(&self, k: &str) -> Option<&QExp> {
        let path: Vec<String> = k.split("::").map(|s| s.to_string()).collect();
        return if let Some(exp) = self.get_exp_path(&path) {
            Some(exp)
        } else if let Some(outer) = &self.outer {
            outer.get_exp_path(&path)
        } else {
            None
        };
    }

    pub fn get_exp_cloned(&self, k: &str) -> Option<QExp> {
        let path: Vec<String> = k.split("::").map(|s| s.to_string()).collect();
        return if let Some(exp) = self.get_exp_path(&path) {
            Some(exp.clone())
        } else if let Some(outer) = &self.outer {
            outer.get_exp_path(&path).cloned()
        } else {
            None
        };
    }

    pub fn get_exp_ok(&self, k: &str) -> QResult<&QExp> {
        let path: Vec<String> = k.split("::").map(|s| s.to_string()).collect();
        return if let Some(exp) = self.get_exp_path_ok(&path)? {
            Ok(exp)
        } else if let Some(outer) = &self.outer {
            outer.get_exp_ok(k)
        } else {
            Err(qerr_fmt!("symbol '{}' is undefined", k))
        };
    }

    pub fn get_exp_ok_cloned(&self, k: &str) -> QResult<QExp> {
        let path: Vec<String> = k.split("::").map(|s| s.to_string()).collect();
        return if let Some(exp) = self.get_exp_path_ok(&path)? {
            Ok(exp.clone())
        } else if let Some(outer) = &self.outer {
            outer.get_exp_ok_cloned(k)
        } else {
            Err(qerr_fmt!("symbol '{}' is undefined", k))
        };
    }

    fn get_exp_env_of(&mut self, k: &str) -> QResult<QEnv> {
        let mut path: std::collections::VecDeque<String>
            = k.split("::").map(|s| s.to_string()).collect();
        let mut env: QEnv = self.clone();
        while let Some(sym) = path.pop_front() {
            match (env.data.get(&sym), path.is_empty()) {
                (Some(QEnvEntry::Mod(m)), false) => { env = m.clone(); },
                (Some(QEnvEntry::Exp(_)), true) => { return Ok(env) },
                _ => { return Err(qerr!("unexpected state in get_exp_env_of")); },
            }
        }
        return Err(qerr!("unexpected state in get_exp_env_of"));
    }

    pub fn insert(&mut self, k: String, val: QEnvEntry<'a>) {
        if k != "_".to_string() {
            self.data.insert(k, val);
        }
    }

    pub fn remove(&mut self, k: &str) -> QResult<QEnvEntry<'a>> {
        return self.data.remove(k)
            .ok_or_else(|| qerr_fmt!("symbol '{}' is undefined", k));
    }

    pub fn eval(&mut self, exp: &QExp) -> QResult<QExp> {
        return match exp {
            qbool!(_) | qint!(_) | qfloat!(_) | qcomplex!(_) | qstr!(_)
                => Ok(exp.clone()),
            qlist!(list) => {
                if let Some(first) = list.first() {
                    let evalfirst: QExp = self.eval(first)?;
                    match evalfirst {
                        // qfunc!(_, f) => f(self, &list[1..]),
                        qfunc!(f) => (f.f)(self, &list[1..]),
                        qlambda!(ll) => {
                            if let qsymbol!(s) = first {
                                if s.contains("::") {
                                    let mut evalenv: QEnv
                                        = self.get_exp_env_of(s)?;
                                    ll.eval(&mut evalenv, &list[1..])
                                } else {
                                    ll.eval(self, &list[1..])
                                }
                            } else {
                                ll.eval(self, &list[1..])
                            }
                        },
                        // qlambda!(ll) => ll.eval(self, &list[1..]),
                        _ => {
                            let mut with_evalfirst: Vec<QExp> = vec![evalfirst];
                            with_evalfirst.append(
                                &mut self.eval_multi(&list[1..])?
                            );
                            Ok(qlist!(with_evalfirst))
                            // Ok(qlist!(self.eval_multi(&with_evalfirst)?))
                        },
                    }
                } else {
                    Ok(exp.clone())
                }
            },
            qsymbol!(k) => self.get_exp_ok_cloned(k),
            qfunc!(_) => Ok(exp.clone()),
            qlambda!(_) => Ok(exp.clone()),
        };
    }
    
    pub fn eval_verify(&mut self, ty: QExpType, exp: &QExp) -> QResult<QExp> {
        if exp.exp_type() == qsymbol!() && ty == qsymbol!() {
            return Ok(exp.clone());
        }
        let v: QExp = self.eval(exp)?;
        if v.exp_type() != ty && ty != qany!() {
            return Err(qerr_fmt!(
                "expected type {} but got {}", ty, v.exp_type()));
        }
        return Ok(v);
    }

    pub fn verify_args(&mut self, types: &[QExpType], args: &[QExp])
        -> QResult<Vec<QExp>>
    {
        if types.len() != args.len() {
            return Err(qerr_fmt!(
                "expected {} args but got {}", types.len(), args.len()));
        }
        return types.iter().zip(args.iter())
            .map(|(ty, ar)| self.eval_verify(*ty, ar))
            .collect::<QResult<Vec<QExp>>>();
    }

    pub fn eval_multi(&mut self, args: &[QExp]) -> QResult<Vec<QExp>> {
        return args.iter().map(|x| self.eval(x)).collect();
    }

    pub fn parse_eval(&mut self, expr: String) -> QResult<Vec<QExp>> {
        let exps: Vec<QExp> = parse(&tokenize(expr)?)?;
        let evaled: Vec<QExp> = self.eval_multi(&exps)?;
        return Ok(evaled);
    }
}

pub trait StrFmt {
    fn format(&self, vals: &[QExp]) -> QResult<String>;
}

impl StrFmt for String {
    fn format(&self, vals: &[QExp]) -> QResult<String> {
        let mut chars = self.chars().peekable();
        let mut fmtstr = String::new();
        let mut counter: usize = 0;
        while let Some(chr) = chars.next() {
            match chr {
                '{' => {
                    if let Some(next_chr) = chars.peek() {
                        if next_chr == &'{' {
                            fmtstr.push(chr);
                            fmtstr.push(chars.next().unwrap());
                        } else {
                            fmtstr.push(chr);
                            fmtstr.push_str(&format!("{}", counter));
                            counter += 1;
                        }
                    } else {
                        fmtstr.push(chr);
                    }
                },
                _ => { fmtstr.push(chr); },
            }
        }
        if vals.len() < counter {
            return Err(qerr_fmt!(
                "invalid format string: {} position{} but only {} value{}",
                counter,
                if counter > 1 { "s" } else { "" },
                vals.len(),
                if vals.len() > 1 { "s" } else { "" }
            ));
        }
        let vals_map: HashMap<String, &QExp>
            = vals.iter().enumerate()
            .map(|(k, q)| (format!("{}", k), q))
            .collect();
        return strformat(&fmtstr, &vals_map)
            .map_err(|e| qerr_fmt!("invalid format string: {}", e));
    }
}

