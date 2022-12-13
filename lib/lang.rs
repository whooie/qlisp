use std::{
    cmp,
    collections::HashMap,
    fmt,
    mem,
    path::PathBuf,
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
use formatx::{
    self as fmtx,
    formatx,
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
        QErr::Reason($reason.to_string())
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

pub const PROTECTED: &[&str] = &[
    // constants
    "PI", "2PI", "iPI", "i2PI",
    "E",
    "inf", "inF", "iNf", "iNF", "Inf", "InF", "INf", "INF",
    "nan", "naN", "nAn", "nAN", "Nan", "NaN", "NAn", "NAN",
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
    "min",              "<<",   // done
    "max",              ">>",   // done
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
    Func(String, fn(&mut QEnv, &[QExp]) -> Result<QExp, QErr>),
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
            qfunc!(_, _) => qfunc!(),
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

    pub fn asin(&self) -> QResult<QExp> {
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

    pub fn acos(&self) -> QResult<QExp> {
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

    pub fn atan(&self) -> QResult<QExp> {
        return match self {
            qbool!(b) => Ok(qfloat!(if *b { 1.0_f64.atan() } else { 0.0 })),
            qint!(i) => Ok(qfloat!((*i as f64).atan())),
            qfloat!(f) => Ok(qfloat!(f.atan())),
            qcomplex!(c) => Ok(qcomplex!(c.atan())),
            _ => Err(qerr!("invalid type in atan")),
        };
    }

    pub fn atan2(&self) -> QResult<QExp> {
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

    pub fn asinh(&self) -> QResult<QExp> {
        return match self {
            qbool!(b) => Ok(qfloat!(if *b { 1.0_f64.asinh() } else { 0.0 })),
            qint!(i) => Ok(qfloat!((*i as f64).asinh())),
            qfloat!(f) => Ok(qfloat!(f.asinh())),
            qcomplex!(c) => Ok(qcomplex!(c.asinh())),
            _ => Err(qerr!("invalid type in asinh")),
        };
    }

    pub fn acosh(&self) -> QResult<QExp> {
        return match self {
            qbool!(b) => Ok(qfloat!(if *b { 0.0 } else { f64::NAN })),
            qint!(i) => Ok(qfloat!((*i as f64).acosh())),
            qfloat!(f) => Ok(qfloat!(f.acosh())),
            qcomplex!(c) => Ok(qcomplex!(c.acosh())),
            _ => Err(qerr!("invalid type in acosh")),
        };
    }

    pub fn atanh(&self) -> QResult<QExp> {
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
            qfunc!(name, _) => write!(f, "func({} {{ ... }})", name),
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
            qfunc!(name, _) => write!(f, "func <{}> {{ ... }}", name),
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
            dir: outer_env.dir.clone()
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
            if let qfunc!(name, f) = exp {
                Ok(qfunc!(name.clone(), *f))
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
enum TokenState {
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
                ' ' | ',' | '\n' => {
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
                    _ => { term.push(x); },
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

#[derive(Clone, Debug)]
pub struct QEnv<'a> {
    data: HashMap<String, QEnvEntry<'a>>,
    outer: Option<&'a QEnv<'a>>,
    pub dir: PathBuf,
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
            })
        );
    }
);

macro_rules! add_fn(
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
        add_fn!(env, "def",             ":=",   fns::fn_def             );
        add_fn!(env, "let",             "*:=",  fns::fn_let             );
        add_fn!(env, "fn",              "@:",   fns::fn_fn              );
        add_fn!(env, "defn",            "@:=",  fns::fn_defn            );
        add_fn!(env, "if",                      fns::fn_if              );
        add_fn!(env, "module",                  fns::fn_module          );
        add_fn!(env, "use",                     fns::fn_use             );
        add_fn!(env, "use-all",         "use*", fns::fn_use_all         );
        add_fn!(env, "interact",                fns::fn_interact        );
        add_fn!(env, "isdef",           "?:=",  fns::fn_isdef           );
        add_fn!(env, "del",             "!-",   fns::fn_del             );
        // systems
        add_fn!(env, "format",          "$",    fns::fn_format          );
        add_fn!(env, "print",           "$-",   fns::fn_print           );
        add_fn!(env, "println",         "$_",   fns::fn_println         );
        add_fn!(env, "halt",            "!!",   fns::fn_halt            );
        add_fn!(env, "istype",          "~?",   fns::fn_istype          );
        add_fn!(env, "type",            "?~",   fns::fn_type            );
        // add_fn!(env, "with-file",       "$|",   fns::fn_with_file       );
        // type-casting
        add_fn!(env, "bool",                    fns::fn_bool            );
        add_fn!(env, "int",                     fns::fn_int             );
        add_fn!(env, "float",                   fns::fn_float           );
        add_fn!(env, "complex",                 fns::fn_complex         );
        add_fn!(env, "list",                    fns::fn_list            );
        add_fn!(env, "str",                     fns::fn_str             );
        // arithmetic
        add_fn!(env, "add",             "+",    fns::fn_add             );
        add_fn!(env, "sub",             "-",    fns::fn_sub             );
        add_fn!(env, "mul",             "*",    fns::fn_mul             );
        add_fn!(env, "div",             "/",    fns::fn_div             );
        add_fn!(env, "idiv",            "//",   fns::fn_idiv            );
        // boolean comparisons
        add_fn!(env, "and",             "&&",   fns::fn_and             );
        add_fn!(env, "or",              "||",   fns::fn_or              );
        add_fn!(env, "xor",             "^",    fns::fn_xor             );
        add_fn!(env, "eq",              "=",    fns::fn_eq              );
        add_fn!(env, "neq",             "!=",   fns::fn_neq             );
        add_fn!(env, "gt",              ">",    fns::fn_gt              );
        add_fn!(env, "geq",             ">=",   fns::fn_geq             );
        add_fn!(env, "lt",              "<",    fns::fn_lt              );
        add_fn!(env, "leq",             "<=",   fns::fn_leq             );
        // boolean accumulators
        add_fn!(env, "all",             "&&*",  fns::fn_all             );
        add_fn!(env, "any",             "||*",  fns::fn_any             );
        add_fn!(env, "xany",            "^*",   fns::fn_xany            );
        // iterable creation
        add_fn!(env, "range",           "..",   fns::fn_range           );
        add_fn!(env, "range-inc",       "..=",  fns::fn_range_inc       );
        add_fn!(env, "repeat",          "#=",   fns::fn_repeat          );
        // iterable accumulation
        add_fn!(env, "length",          "#",    fns::fn_length          );
        add_fn!(env, "fold",            "@.",   fns::fn_fold            );
        add_fn!(env, "min",             "<<",   fns::fn_min             );
        add_fn!(env, "max",             ">>",   fns::fn_max             );
        add_fn!(env, "select-by",       "*@.",  fns::fn_select_by       );
        // iterable slicing and access
        add_fn!(env, "get",             ".",    fns::fn_get             );
        add_fn!(env, "set",             ".:=",  fns::fn_set             );
        add_fn!(env, "slice",           "--",   fns::fn_slice           );
        add_fn!(env, "slice-inc",       "--=",  fns::fn_slice_inc       );
        add_fn!(env, "slice-by",        "~~",   fns::fn_slice_by        );
        add_fn!(env, "slice-inc-by",    "~~=",  fns::fn_slice_inc_by    );
        add_fn!(env, "pick",            ".*",   fns::fn_pick            );
        add_fn!(env, "first",           ".-",   fns::fn_first           );
        add_fn!(env, "take",            "~.",   fns::fn_take            );
        add_fn!(env, "take-while",      "~.@",  fns::fn_take_while      );
        add_fn!(env, "last",            "-.",   fns::fn_last            );
        add_fn!(env, "skip",            ".~",   fns::fn_skip            );
        add_fn!(env, "skip-while",      ".~@",  fns::fn_skip_while      );
        // iterable transformation
        add_fn!(env, "step-by",         "~",    fns::fn_step_by         );
        add_fn!(env, "enumerate",       "##",   fns::fn_enumerate       );
        add_fn!(env, "reverse",         "<>",   fns::fn_reverse         );
        add_fn!(env, "cycle",           "<#>",  fns::fn_cycle           );
        add_fn!(env, "map",             "@",    fns::fn_map             );
        add_fn!(env, "filter",          "@!",   fns::fn_filter          );
        add_fn!(env, "unique",          "*!=",  fns::fn_unique          );
        add_fn!(env, "flatten",         "__",   fns::fn_flatten         );
        add_fn!(env, "sort",            "<*",   fns::fn_sort            );
        add_fn!(env, "sort-by",         "<@",   fns::fn_sort_by         );
        add_fn!(env, "permute",         ".~.",  fns::fn_permute         );
        // iterable division
        add_fn!(env, "split-at",        "|.",   fns::fn_split_at        );
        add_fn!(env, "split-on",        "|@",   fns::fn_split_on        );
        add_fn!(env, "split-on-inc",    "|@=",  fns::fn_split_on_inc    );
        // iterable addition
        add_fn!(env, "append",          "+.",   fns::fn_append          );
        add_fn!(env, "prepend",         ".+",   fns::fn_prepend         );
        add_fn!(env, "insert",          "+.+",  fns::fn_insert          );
        add_fn!(env, "join",            "++",   fns::fn_join            );
        add_fn!(env, "join-with",       "+*+",  fns::fn_join_with       );
        add_fn!(env, "zip",             ":~:",  fns::fn_zip             );
        add_fn!(env, "cart",            ":*:",  fns::fn_cart            );
        // iterable testing
        add_fn!(env, "contains",        "*=",   fns::fn_contains        );
        add_fn!(env, "index-of",        "#*=",  fns::fn_index_of        );

        // element-wise math
        add_fn!(env, "neg",             "!",    fns::fn_neg             );
        add_fn!(env, "recip",           "1/",   fns::fn_recip           );
        add_fn!(env, "abs",             "|.|",  fns::fn_abs             );
        add_fn!(env, "sqrt",                    fns::fn_sqrt            );
        add_fn!(env, "cbrt",                    fns::fn_cbrt            );
        add_fn!(env, "exp",             "e**",  fns::fn_exp             );
        add_fn!(env, "floor",           "~_",   fns::fn_floor           );
        add_fn!(env, "ceil",            "~^",   fns::fn_ceil            );
        add_fn!(env, "round",           "~:",   fns::fn_round           );
        add_fn!(env, "ln",                      fns::fn_ln              );
        add_fn!(env, "sin",                     fns::fn_sin             );
        add_fn!(env, "cos",                     fns::fn_cos             );
        add_fn!(env, "tan",                     fns::fn_tan             );
        add_fn!(env, "arcsin",          "asin", fns::fn_asin            );
        add_fn!(env, "arccos",          "acos", fns::fn_acos            );
        add_fn!(env, "arctan",          "atan", fns::fn_atan            );
        add_fn!(env, "arctan2",         "atan2",fns::fn_atan2           );
        add_fn!(env, "sinh",                    fns::fn_sinh            );
        add_fn!(env, "cosh",                    fns::fn_cosh            );
        add_fn!(env, "tanh",                    fns::fn_tanh            );
        add_fn!(env, "arsinh",          "asinh",fns::fn_asinh           );
        add_fn!(env, "arcosh",          "acosh",fns::fn_acosh           );
        add_fn!(env, "artanh",          "atanh",fns::fn_atanh           );
        add_fn!(env, "arg",                     fns::fn_arg             );
        add_fn!(env, "cis",             "e**i", fns::fn_cis             );
        add_fn!(env, "conj",            "~z",   fns::fn_conj            );
        add_fn!(env, "real",            "Re",   fns::fn_real            );
        add_fn!(env, "imag",            "Im",   fns::fn_imag            );
        // parameterized element-wise math
        add_fn!(env, "mod",             "%",    fns::fn_mod             );
        add_fn!(env, "log",                     fns::fn_log             );
        add_fn!(env, "pow",             "**",   fns::fn_pow             );
        // list -> list math
        add_fn!(env, "convolve",        "<:>",  fns::fn_convolve        );
        add_fn!(env, "hist",            "|#|",  fns::fn_histogram       );
        add_fn!(env, "hist-prob",       "|p|",  fns::fn_histogram_prob  );
        add_fn!(env, "covariance",      "Cov",  fns::fn_covariance      );
        add_fn!(env, "correlation",     "Corr", fns::fn_correlation     );
        // add_fn!(env, "fft",             "{F}",  fns::fn_fft             );
        // add_fn!(env, "ifft",            "{iF}", fns::fn_ifft            );
        // list -> value math
        add_fn!(env, "mean",                    fns::fn_mean            );
        add_fn!(env, "variance",        "Var",  fns::fn_variance        );
        add_fn!(env, "stddev",          "Std",  fns::fn_stddev          );
        // list+1 -> value math
        add_fn!(env, "pnorm",           "|+|",  fns::fn_pnorm           );
        add_fn!(env, "moment",                  fns::fn_moment          );
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
        add_fn!(phys, "nmrgb", fns::phys_nmrgb);
        add_mod!(env, "phys", phys);

        return QEnv { data: env, outer: None, dir: PathBuf::from(".") };
    }
}

impl<'a> QEnv<'a> {
    pub fn new(outer_env: Option<&'a QEnv<'a>>, dir: PathBuf) -> Self {
        return QEnv { data: HashMap::new(), outer: outer_env, dir };
    }

    pub fn default_with_dir(dir: PathBuf) -> Self {
        return QEnv { dir, ..Self::default() };
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
        self.data.insert(k, val);
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
                        qfunc!(_, f) => f(self, &list[1..]),
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
            qfunc!(_, _) => Ok(exp.clone()),
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

pub trait FormatX {
    fn formatx(&self, vals: &[QExp]) -> QResult<String>;
}

impl FormatX for String {
    fn formatx(&self, vals: &[QExp]) -> QResult<String> {
        let mut fmt: fmtx::Template
            = formatx!(self)
            .map_err(|e| qerr_fmt!("invalid format string: {}", e.message()))?;
        vals.iter()
            .for_each(|qk| fmt.replace_positional(qk));
        return fmt.text()
            .map_err(|e| qerr_fmt!("could not format string: {}", e.message()));
    }
}

