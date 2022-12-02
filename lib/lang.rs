use std::{
    cmp,
    collections::HashMap,
    fmt,
    mem,
    rc::Rc,
    str::FromStr,
};
use num_complex::Complex64 as C64;
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

pub const PROTECTED: &'static [&'static str] = &[
    // special -- keyword-like
    "def",              ":=",   // done
    "let",              ":=*",  // done
    "fn",               "@:",   // done
    "defn",             "@:=",  // done
    "if",               "=>",   // done
    "and",              "&&",   // done
    "or",               "||",   // done
    "xor",              "^",    // done

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
    "zip",              "::",   // done
    "cart",             ":*:",  // done
    // iterable testing
    "contains",         "*=",   // done
    "index-of",         "#*=",  // done

    // element-wise math
    "neg",              "!",    // done
    "recip",            "1/",   //
    "abs",              "|.|",  //
    "sqrt",                     //
    "cbrt",                     //
    "exp",              "e**",  //
    "floor",            "~_",   //
    "ceil",             "~^",   //
    "round",            "~:",   //
    "ln",                       //
    "sin",                      //
    "cos",                      //
    "tan",                      //
    "asin",                     //
    "acos",                     //
    "atan",                     //
    "atan2",                    //
    "sinh",                     //
    "cosh",                     //
    "tanh",                     //
    "asinh",                    //
    "acosh",                    //
    "atanh",                    //
    "arg",                      //
    "cis",              "e**i", //
    "conj",             "~z",   //
    "real",             "Re",   //
    "imag",             "Im",   //
    // parameterized element-wise math
    "mod",              "%",    // done
    "log",                      //
    "pow",              "**",   //
    // list -> list math
    "convolve",         "{*}",  //
    "histogram",        "|#|",  //
    "histogram-prob",   "|p|",  //
    "fft",              "{F}",  //
    "ifft",             "{iF}", //
    "findpeaks",        "^?",   //
    "covariance",       "Cov",  //
    "correlation",      "Corr", //
    // list -> value math
    "mean",             "{E}",  //
    "variance",         "Var",  //
    "stddev",           "Std",  //
    // list+1 -> value math
    "pnorm",            "|+|",  //
    "moment",           "{En}", //
    // special-arg math
    "sample",           "?.",   //
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
    // Func(fn(&[QExp]) -> Result<QExp, QErr>),
    Lambda(QLambda),
    // Module(QEnv<'a>)
}

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
    pub fn eq_user(&self, other: &Self) -> bool {
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
        return types.iter().any(|ty| ty.eq_user(&self.exp_type()));
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
            qlist!(l) => l.len() == 0,
            qstr!(s) => s.len() == 0,
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
        (!rhs.is_zero()).then(|| ()).ok_or(qerr!("encountered zero in div"))?;
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
        (!rhs.is_zero()).then(|| ()).ok_or(qerr!("encountered zero in idiv"))?;
        let lhs_c: QExp = convert_type_num(self, qfloat!())?;
        let rhs_c: QExp = convert_type_num(rhs,  qfloat!())?;
        return match (lhs_c, rhs_c) {
            (qfloat!(l), qfloat!(r)) => Ok(qint!(l.div_euclid(r) as i64)),
            _ => Err(qerr!("invalid type in idiv")),
        };
    }

    pub fn modulo(&self, M: &QExp) -> QResult<QExp> {
        (!M.is_zero()).then(|| ()).ok_or(qerr!("encountered zero in mod"))?;
        let ret_type: QExpType
            = cmp::max(
                cmp::max(self.exp_type(), M.exp_type()),
                qint!(),
            );
        (!(ret_type > qcomplex!()))
            .then(|| ()).ok_or(qerr!("cannot modulo by a complex value"))?;
        let N_c: QExp = convert_type_num(self, ret_type)?;
        let M_c: QExp = convert_type_num(M,    ret_type)?;
        println!("{:?} {:?}", N_c, M_c);
        return match (N_c, M_c) {
            (qint!(n), qint!(m)) => Ok(qint!(n.rem_euclid(m))),
            (qfloat!(n), qfloat!(m)) => {
                println!("{:?}", qfloat!(n.rem_euclid(m)));
                Ok(qfloat!(n.rem_euclid(m)))
            },
            _ => Err(qerr!("invalid type in mod")),
        };
    }

    pub fn eq(&self, rhs: &QExp) -> bool {
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
                => l.iter().zip(r.iter()).all(|(lk, rk)| lk.eq(rk)),
            (qstr!(l), qstr!(r)) => l == r,
            _ => false,
        };
    }

    pub fn neq(&self, rhs: &QExp) -> bool { !self.eq(rhs) }

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
}

impl PartialEq for QExp {
    fn eq(&self, other: &QExp) -> bool { self.eq(other) }
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
                => write!(f, "lambda({:?} {{ ... }})", x.params_exp.as_ref()),
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
            qfunc!(name, _) => write!(f, "func {} {{ ... }}", name),
            qlambda!(x)
                => write!(f, "lambda {} {{ ... }}", x.params_exp.as_ref()),
        };
    }
}

#[derive(Clone)]
pub struct QLambda {
    pub params_exp: Rc<QExp>,
    pub body_exp: Rc<QExp>,
}

impl QLambda {
    pub fn get_params_exp(&self) -> &Rc<QExp> { &self.params_exp }

    pub fn get_body_exp(&self) -> &Rc<QExp> { &self.body_exp }

    pub fn parse_symbols(form: Rc<QExp>) -> QResult<Vec<String>> {
        let list: Vec<QExp> = match form.as_ref() {
            qlist!(s) => Ok(s.clone()),
            _ => Err(qerr!("expected args form to be a list")),
        }?;
        return list.iter()
            .map(|x| {
                match x {
                    qsymbol!(s) => Ok(s.clone()),
                    _ => Err(qerr!("arguments must be a list of symbols")),
                }
            })
            .collect();
    }

    pub fn env<'a>(&self, args: &[QExp], outer_env: &'a mut QEnv)
        -> QResult<QEnv<'a>>
    {
        let symbols: Vec<String>
            = Self::parse_symbols(self.params_exp.clone())?;
        if symbols.len() != args.len() {
            return Err(qerr_fmt!(
                "expected {} arguments but got {}",
                symbols.len(),
                args.len()
            ));
        }
        let values: Vec<QExp> = outer_env.eval_multi(args)?;
        let mut data: HashMap<String, QExp> = HashMap::new();
        for (k, v) in symbols.into_iter().zip(values.into_iter()) {
            data.insert(k, v);
        }
        return Ok(QEnv { data, outer: Some(outer_env) });
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

    pub fn slice_inc(&self, beg: usize, end: usize, step: usize) -> QResult<QExp> {
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
                        .ok_or(qerr_fmt!(
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
                        .ok_or(qerr_fmt!(
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
                if shift > 0 {
                    new.rotate_right(shift as usize);
                } else if shift < 0 {
                    new.rotate_left(shift.abs() as usize);
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
                .ok_or(qerr!("list must have length at least 1")),
            Indexable::Str(s)
                => {
                    let S: String = s.chars().take(1).collect();
                    if S.len() > 0 {
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
                .ok_or(qerr!("list must have length at least 1")),
            Indexable::Str(s)
                => {
                    Ok(qstr!(
                        s.chars().last()
                        .ok_or(qerr!("str must have length at least 1"))?
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
                Ok(qlist!(
                    vec![
                        qlist!(ll.iter().cloned().collect()),
                        qlist!(lr.iter().cloned().collect()),
                    ]
                ))
            },
            Indexable::Str(s) => {
                let (sl, sr): (&str, &str) = s.split_at(idx);
                Ok(qlist!(vec![
                    qstr!(sl.to_string()),
                    qstr!(sr.to_string()),
                ]))
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
                                acc.push(
                                    qlist!(mem::replace(&mut cur, Vec::new()))
                                );
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
                                acc.push(
                                    qstr!(
                                        mem::replace(&mut cur, String::new())
                                    )
                                );
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
                                acc.push(
                                    qlist!(mem::replace(&mut cur, Vec::new()))
                                );
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
                                acc.push(
                                    qstr!(
                                        mem::replace(&mut cur, String::new())
                                    )
                                );
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
                ll.append(&mut items.iter().cloned().collect());
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
                    if exp.eq(qk) { return Ok(qint!(k as i64)); }
                }
                Ok(qint!(-1))
            },
            Indexable::Str(s) => {
                match exp {
                    qstr!(sp) => {
                        if sp.len() > s.len() {
                            Ok(qint!(-1))
                        } else if sp.len() == s.len() {
                            Ok(qint!(if sp == s { -1 } else { 0 }))
                        } else {
                            let n: usize = sp.len();
                            for k in 0..s.len() - sp.len() {
                                if sp == s.get(k..k + n).unwrap() {
                                    return Ok(qint!(k as i64));
                                }
                            }
                            Ok(qint!(-1))
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
                    .ok_or(qerr!("expected non-empty list or str"))?;
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
                    .ok_or(qerr!("expected non-empty list or str"))?;
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
                    .ok_or(qerr!("expected non-empty list or str"))?;
                for qk in l.iter() {
                    if qk.gt(&m)? {
                        m = qk;
                    }
                }
                Ok(m.clone())
            },
            Indexable::Str(s) => {
                let mut m: QExp
                    = s.get(0..1)
                    .map(|s0| qstr!(s0.to_string()))
                    .ok_or(qerr!("expected non-empty list or str"))?;
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
                    .ok_or(qerr!("expected non-empty list or str"))?;
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
                    .ok_or(qerr!("expected non-empty list or str"))?;
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
            let mut ret: Vec<QExp> = l.iter().cloned().collect();
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
                .ok_or(qerr!("expected at least one item"))?,
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
                        ret.push(mem::replace(&mut term, String::new()));
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
                        ret.push(mem::replace(&mut term, String::new()));
                    }
                    ret.push(x.to_string());
                },
                ' ' | ',' | '\n' => {
                    if !term.is_empty() {
                        ret.push(mem::replace(&mut term, String::new()));
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
                    ret.push(mem::replace(&mut term, String::new()));
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
        ret.push(mem::replace(&mut term, String::new()));
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
        .ok_or(qerr!("missing closing ')'"))?;
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
            .ok_or(qerr!("missing closing ')'"))?;
        if next_token == ")" {
            return Ok((qlist!(ret), rest));
        }
        let (exp, new_xs) = parse_single(xs)?;
        ret.push(exp);
        xs = new_xs;
    }
}

pub fn parse_atom(token: &str) -> QResult<QExp> {
    return if let Ok(b) = bool::from_str(token) {
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
    };
}

#[derive(Clone)]
pub struct QEnv<'a> {
    data: HashMap<String, QExp>,
    outer: Option<&'a QEnv<'a>>,
}

macro_rules! add_fn(
    ( $env:ident, $name:literal, $alias:literal, $fn:ident ) => {
        $env.insert($name.to_string(),  QExp::Func($name.to_string(), $fn));
        $env.insert($alias.to_string(), QExp::Func($name.to_string(), $fn));
    };
    ( $env:ident, $name:literal, $alias:literal, $fn:path ) => {
        $env.insert($name.to_string(),  QExp::Func($name.to_string(), $fn));
        $env.insert($alias.to_string(), QExp::Func($name.to_string(), $fn));
    };
    ( $env:ident, $name:literal, $fn:ident ) => {
        $env.insert($name.to_string(), QExp::Func($name.to_string(), $fn));
    };
    ( $env:ident, $name:literal, $fn:path ) => {
        $env.insert($name.to_string(), QExp::Func($name.to_string(), $fn));
    };
);

impl<'a> Default for QEnv<'a> {
    fn default() -> QEnv<'a> {
        let mut env: HashMap<String, QExp> = HashMap::new();
            // special -- keyword-like
            add_fn!(env, "def",             ":=",   fns::fn_def);
            add_fn!(env, "let",             "*:=",  fns::fn_let);
            add_fn!(env, "fn",              "@:",   fns::fn_fn);
            add_fn!(env, "defn",            "@:=",  fns::fn_defn);
            add_fn!(env, "if",              "=>",   fns::fn_if);
            // systems
            add_fn!(env, "format",          "$",    fns::fn_format);
            add_fn!(env, "print",           "$-",   fns::fn_print);
            add_fn!(env, "println",         "$_",   fns::fn_println);
            add_fn!(env, "halt",            "!!",   fns::fn_halt);
            add_fn!(env, "istype",          "~?",   fns::fn_istype);
            add_fn!(env, "type",            "?~",   fns::fn_type);
            // type-casting
            add_fn!(env, "bool",                    fns::fn_bool);
            add_fn!(env, "int",                     fns::fn_int);
            add_fn!(env, "float",                   fns::fn_float);
            add_fn!(env, "complex",                 fns::fn_complex);
            add_fn!(env, "list",                    fns::fn_list);
            add_fn!(env, "str",                     fns::fn_str);
            // arithmetic
            add_fn!(env, "add",             "+",    fns::fn_add);
            add_fn!(env, "sub",             "-",    fns::fn_sub);
            add_fn!(env, "mul",             "*",    fns::fn_mul);
            add_fn!(env, "div",             "/",    fns::fn_div);
            add_fn!(env, "idiv",            "//",   fns::fn_idiv);
            // boolean comparisons
            add_fn!(env, "and",             "&&",   fns::fn_and);
            add_fn!(env, "or",              "||",   fns::fn_or);
            add_fn!(env, "xor",             "^",    fns::fn_xor);
            add_fn!(env, "eq",              "=",    fns::fn_eq);
            add_fn!(env, "neq",             "!=",   fns::fn_neq);
            add_fn!(env, "gt",              ">",    fns::fn_gt);
            add_fn!(env, "geq",             ">=",   fns::fn_geq);
            add_fn!(env, "lt",              "<",    fns::fn_lt);
            add_fn!(env, "leq",             "<=",   fns::fn_leq);
            // boolean accumulators
            add_fn!(env, "all",             "&&*",  fns::fn_all);
            add_fn!(env, "any",             "||*",  fns::fn_any);
            add_fn!(env, "xany",            "^*",   fns::fn_xany);
            // iterable creation
            add_fn!(env, "range",           "..",   fns::fn_range);
            add_fn!(env, "range-inc",       "..=",  fns::fn_range_inc);
            add_fn!(env, "repeat",          "#=",   fns::fn_repeat);
            // iterable accumulation
            add_fn!(env, "length",          "#",    fns::fn_length);
            add_fn!(env, "fold",            "@.",   fns::fn_fold);
            add_fn!(env, "min",             "<<",   fns::fn_min);
            add_fn!(env, "max",             ">>",   fns::fn_max);
            add_fn!(env, "select-by",       "*@.",  fns::fn_select_by);
            // iterable slicing and access
            add_fn!(env, "get",             ".",    fns::fn_get);
            add_fn!(env, "set",             ".:=",  fns::fn_set);
            add_fn!(env, "slice",           "--",   fns::fn_slice);
            add_fn!(env, "slice-inc",       "--=",  fns::fn_slice_inc);
            add_fn!(env, "slice-by",        "~~",   fns::fn_slice_by);
            add_fn!(env, "slice-inc-by",    "~~=",  fns::fn_slice_inc_by);
            add_fn!(env, "pick",            ".*",   fns::fn_pick);
            add_fn!(env, "first",           ".-",   fns::fn_first);
            add_fn!(env, "take",            "~.",   fns::fn_take);
            add_fn!(env, "take-while",      "~.@",  fns::fn_take_while);
            add_fn!(env, "last",            "-.",   fns::fn_last);
            add_fn!(env, "skip",            ".~",   fns::fn_skip);
            add_fn!(env, "skip-while",      ".~@",  fns::fn_skip_while);
            // iterable transformation
            add_fn!(env, "step-by",         "~",    fns::fn_step_by);
            add_fn!(env, "enumerate",       "##",   fns::fn_enumerate);
            add_fn!(env, "reverse",         "<>",   fns::fn_reverse);
            add_fn!(env, "cycle",           "<#>",  fns::fn_cycle);
            add_fn!(env, "map",             "@",    fns::fn_map);
            add_fn!(env, "filter",          "@!",   fns::fn_filter);
            add_fn!(env, "unique",          "*!=",  fns::fn_unique);
            add_fn!(env, "flatten",         "__",   fns::fn_flatten);
            add_fn!(env, "sort",            "<*",   fns::fn_sort);
            add_fn!(env, "sort-by",         "<@",   fns::fn_sort_by);
            add_fn!(env, "permute",         ".~.",  fns::fn_permute);
            // iterable division
            add_fn!(env, "split-at",        "|.",   fns::fn_split_at);
            add_fn!(env, "split-on",        "|@",   fns::fn_split_on);
            add_fn!(env, "split-on-inc",    "|@=",  fns::fn_split_on_inc);
            // iterable addition
            add_fn!(env, "append",          "+.",   fns::fn_append);
            add_fn!(env, "prepend",         ".+",   fns::fn_prepend);
            add_fn!(env, "insert",          "+.+",  fns::fn_insert);
            add_fn!(env, "join",            "++",   fns::fn_join);
            add_fn!(env, "join-with",       "+*+",  fns::fn_join_with);
            add_fn!(env, "zip",             "::",   fns::fn_zip);
            add_fn!(env, "cart",            ":*:",  fns::fn_cart);
            // iterable testing
            add_fn!(env, "contains",        "*=",   fns::fn_contains);
            add_fn!(env, "index-of",        "#*=",  fns::fn_index_of);

            // element-wise math
            // add_fn!(env, "neg",             "!",    fns::fn_neg);
            // add_fn!(env, "recip",           "1/",   fns::fn_recip);
            // add_fn!(env, "abs",             "|.|",  fns::fn_abs);
            // add_fn!(env, "sqrt",                    fns::fn_sqrt);
            // add_fn!(env, "cbrt",                    fns::fn_cbrt);
            // add_fn!(env, "exp",             "e**",  fns::fn_exp);
            // add_fn!(env, "floor",           "~_",   fns::fn_floor);
            // add_fn!(env, "ceil",            "~^",   fns::fn_ceil);
            // add_fn!(env, "round",           "~:",   fns::fn_round);
            // add_fn!(env, "ln",                      fns::fn_ln);
            // add_fn!(env, "sin",                     fns::fn_sin);
            // add_fn!(env, "cos",                     fns::fn_cos);
            // add_fn!(env, "tan",                     fns::fn_tan);
            // add_fn!(env, "asin",                    fns::fn_asin);
            // add_fn!(env, "acos",                    fns::fn_acos);
            // add_fn!(env, "atan",                    fns::fn_atan);
            // add_fn!(env, "atan2",                   fns::fn_atan2);
            // add_fn!(env, "sinh",                    fns::fn_sinh);
            // add_fn!(env, "cosh",                    fns::fn_cosh);
            // add_fn!(env, "tanh",                    fns::fn_tanh);
            // add_fn!(env, "asinh",                   fns::fn_asinh);
            // add_fn!(env, "acosh",                   fns::fn_acosh);
            // add_fn!(env, "atanh",                   fns::fn_atanh);
            // add_fn!(env, "arg",                     fns::fn_arg);
            // add_fn!(env, "cis",             "e**i", fns::fn_cis);
            // add_fn!(env, "conj",            "~_",   fns::fn_conj);
            // add_fn!(env, "real",            "Re",   fns::fn_real);
            // add_fn!(env, "imag",            "Im",   fns::fn_imag);
            // parameterized element-wise math
            // add_fn!(env, "mod",             "%",    fns::fn_mod);
            // add_fn!(env, "log",                     fns::fn_log);
            // add_fn!(env, "pow",             "**",   fns::fn_pow);
            // list -> list math
            // add_fn!(env, "convolve",        "{*}",  fns::fn_convolve);
            // add_fn!(env, "histogram",       "|#|",  fns::fn_histogram);
            // add_fn!(env, "histogram-prob",  "|p|",  fns::fn_histogram_prob);
            // add_fn!(env, "fft",             "{F}",  fns::fn_fft);
            // add_fn!(env, "ifft",            "{iF}", fns::fn_ifft);
            // add_fn!(env, "findpeaks",       "^?",   fns::fn_findpeaks);
            // add_fn!(env, "covariance",      "Cov",  fns::fn_covariance);
            // add_fn!(env, "correlation",     "Corr", fns::fn_correlation);
            // list -> value math
            // add_fn!(env, "mean",            "{E}",  fns::fn_mean);
            // add_fn!(env, "variance",        "Var",  fns::fn_variance);
            // add_fn!(env, "stddev",          "Std",  fns::fn_stddev);
            // list+1 -> value math
            // add_fn!(env, "pnorm",           "|+|",  fns::fn_pnorm);
            // add_fn!(env, "moment",          "{En}", fns::fn_moment);
            // special-arg math
            // add_fn!(env, "sample",          "?.",   fns::fn_sample);
        return QEnv { data: env, outer: None };
    }
}

impl<'a> QEnv<'a> {
    pub fn get_data(&self) -> &HashMap<String, QExp> { &self.data }

    pub fn get_outer(&'a self) -> Option<&'a QEnv<'a>> { (&self.outer).as_deref() }

    pub fn get(&self, k: &str) -> Option<&QExp> {
        return if let Some(exp) = self.data.get(k) {
            Some(exp)
        } else if let Some(outer) = &self.outer {
            outer.get(k)
        } else {
            None
        }
    }

    pub fn get_cloned(&self, k: &str) -> Option<QExp> {
        return if let Some(exp) = self.data.get(k) {
            Some(exp.clone())
        } else if let Some(outer) = &self.outer {
            outer.get_cloned(k)
        } else {
            None
        };
    }

    pub fn get_ok(&self, k: &str) -> QResult<&QExp> {
        return if let Some(exp) = self.data.get(k) {
            Ok(exp)
        } else if let Some(outer) = &self.outer {
            outer.get_ok(k)
        } else {
            Err(qerr_fmt!("symbol '{}' is undefined", k))
        };
    }

    pub fn get_ok_cloned(&self, k: &str) -> QResult<QExp> {
        return if let Some(exp) = self.data.get(k) {
            Ok(exp.clone())
        } else if let Some(outer) = &self.outer {
            outer.get_ok_cloned(k)
        } else {
            Err(qerr_fmt!("symbol '{}' is undefined", k))
        };
    }

    pub fn insert(&mut self, k: String, val: QExp) {
        self.data.insert(k, val);
    }

    pub fn eval(&mut self, exp: &QExp) -> QResult<QExp> {
        return match exp {
            qbool!(_) | qint!(_) | qfloat!(_) | qcomplex!(_) | qstr!(_)
                => Ok(exp.clone()),
            qlist!(list) => {
                if let Some(first) = list.first() {
                    match self.eval(first)? {
                        qfunc!(_, f) => f(self, &list[1..]),
                        qlambda!(ll) => {
                            let argvals: Vec<QExp>
                                = self.eval_multi(&list[1..])?;
                            ll.eval(self, &argvals)
                        },
                        qsymbol!(s) => Err(
                            qerr_fmt!("could not evaluate symbol {}", s)
                        ),
                        _ => Ok(qlist!(self.eval_multi(list)?)),
                    }
                } else {
                    Ok(exp.clone())
                }
            },
            qsymbol!(k)
                => self.get_cloned(k)
                .ok_or(qerr_fmt!("symbol '{}' is undefined", k)),
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

// impl<'a> MathBuiltins for QEnv<'a> {
//     /*
//      * element-wise math
//      */
//
//     fn eval_neg(&mut self, args: &[QExp]) -> QResult<QExp> {
//         fn fn_neg(args: &[QExp]) -> QResult<QExp> {
//             if args.len() == 1 {
//                 if let Some(qlist!(l)) = args.first() {
//                     return fn_neg(l);
//                 }
//                 return args.get(0).unwrap().neg();
//             } else {
//                 let new: Vec<QExp>
//                     = args.iter()
//                     .map(|x| x.neg())
//                     .collect::<QResult<Vec<QExp>>>()?;
//                 return Ok(qlist!(new));
//             }
//         }
//         return fn_neg(&self.eval_multi(args)?);
//     }
//
//     // fn eval_recip(&mut self, args: &[QExp]) -> QResult<QExp>;
//     // fn eval_abs(&mut self, args: &[QExp]) -> QResult<QExp>;
//     // fn eval_sqrt(&mut self, args: &[QExp]) -> QResult<QExp>;
//     // fn eval_cbrt(&mut self, args: &[QExp]) -> QResult<QExp>;
//     // fn eval_exp(&mut self, args: &[QExp]) -> QResult<QExp>;
//     // fn eval_floor(&mut self, args: &[QExp]) -> QResult<QExp>;
//     // fn eval_ceil(&mut self, args: &[QExp]) -> QResult<QExp>;
//     // fn eval_round(&mut self, args: &[QExp]) -> QResult<QExp>;
//     // fn eval_ln(&mut self, args: &[QExp]) -> QResult<QExp>;
//     // fn eval_sin(&mut self, args: &[QExp]) -> QResult<QExp>;
//     // fn eval_cos(&mut self, args: &[QExp]) -> QResult<QExp>;
//     // fn eval_tan(&mut self, args: &[QExp]) -> QResult<QExp>;
//     // fn eval_asin(&mut self, args: &[QExp]) -> QResult<QExp>;
//     // fn eval_acos(&mut self, args: &[QExp]) -> QResult<QExp>;
//     // fn eval_atan(&mut self, args: &[QExp]) -> QResult<QExp>;
//     // fn eval_atan2(&mut self, args: &[QExp]) -> QResult<QExp>;
//     // fn eval_sinh(&mut self, args: &[QExp]) -> QResult<QExp>;
//     // fn eval_cosh(&mut self, args: &[QExp]) -> QResult<QExp>;
//     // fn eval_tanh(&mut self, args: &[QExp]) -> QResult<QExp>;
//     // fn eval_asinh(&mut self, args: &[QExp]) -> QResult<QExp>;
//     // fn eval_acosh(&mut self, args: &[QExp]) -> QResult<QExp>;
//     // fn eval_atanh(&mut self, args: &[QExp]) -> QResult<QExp>;
//     // fn eval_arg(&mut self, args: &[QExp]) -> QResult<QExp>;
//     // fn eval_cis(&mut self, args: &[QExp]) -> QResult<QExp>;
//     // fn eval_conj(&mut self, args: &[QExp]) -> QResult<QExp>;
//     // fn eval_real(&mut self, args: &[QExp]) -> QResult<QExp>;
//     // fn eval_imag(&mut self, args: &[QExp]) -> QResult<QExp>;
//
//     /*
//      * parameterized element-wise math
//      */
//
//     fn eval_mod(&mut self, args: &[QExp]) -> QResult<QExp> {
//         if args.len() != 2 {
//             return Err(qerr!("mod must have exactly two arguments"));
//         }
//         let mut M: QExp = self.eval(args.get(0).unwrap())?;
//         let (nums, mut ty): (QExp, QExpType)
//             = match self.eval(args.get(1).unwrap())? {
//                 qbool!(b) => Ok((qbool!(b), qbool!())),
//                 qint!(i) => Ok((qint!(i), qint!())),
//                 qfloat!(f) => Ok((qfloat!(f), qfloat!())),
//                 qlist!(l) => {
//                     convert_numbers_sametype(&l)
//                         .map(|(n, t)| (qlist!(n), t))
//                         .map_err(|e| e.prepend_source("mod"))
//                 },
//                 _ => Err(qerr!(
//                     "mod: second argument must be a number or a list of numbers"
//                 )),
//             }?;
//         ty = cmp::max(M.exp_type(), ty);
//         if ty > qfloat!() {
//             return Err(qerr!(
//                 "mod: cannot modulo with complex or non-numerical values"));
//         }
//         M = convert_type_num(&M, ty)
//             .map_err(|e| e.prepend_source("mod"))?;
//         return match &nums {
//             qbool!(_) | qint!(_) | qfloat!(_) => {
//                 convert_type_num(&nums, ty)?.modulo(&M)
//                     .map_err(|e| e.prepend_source("mod"))
//             },
//             qlist!(l) => {
//                 Ok(qlist!(
//                     l.iter()
//                     .map(|qk| convert_type_num(qk, ty))
//                     .collect::<QResult<Vec<QExp>>>()
//                     .map_err(|e| e.prepend_source("mod"))?
//                     .iter()
//                     .map(|qk| qk.modulo(&M))
//                     .collect::<QResult<Vec<QExp>>>()
//                     .map_err(|e| e.prepend_source("mod"))?
//                 ))
//             },
//             _ => Err(qerr!("unexpected state")),
//         };
//     }
//
//     // fn eval_log(&mut self, args: &[QExp]) -> QResult<QExp>;
//     // fn eval_pow(&mut self, args: &[QExp]) -> QResult<QExp>;
//
//     /*
//      * list -> list math
//      */
//
//     // fn eval_convolve(&mut self, args: &[QExp]) -> QResult<QExp>;
//     // fn eval_histogram(&mut self, args: &[QExp]) -> QResult<QExp>;
//     // fn eval_histogram_prob(&mut self, args: &[QExp]) -> QResult<QExp>;
//     // fn eval_fft(&mut self, args: &[QExp]) -> QResult<QExp>;
//     // fn eval_ifft(&mut self, args: &[QExp]) -> QResult<QExp>;
//     // fn eval_findpeaks(&mut self, args: &[QExp]) -> QResult<QExp>;
//     // fn eval_covariance(&mut self, args: &[QExp]) -> QResult<QExp>;
//     // fn eval_correlation(&mut self, args: &[QExp]) -> QResult<QExp>;
//
//     /*
//      * list -> value math
//      */
//
//     // fn eval_mean(&mut self, args: &[QExp]) -> QResult<QExp>;
//     // fn eval_variance(&mut self, args: &[QExp]) -> QResult<QExp>;
//     // fn eval_stddev(&mut self, args: &[QExp]) -> QResult<QExp>;
//
//     /*
//      * parameterized list -> value math
//      */
//
//     // fn eval_pnorm(&mut self, args: &[QExp]) -> QResult<QExp>;
//     // fn eval_moment(&mut self, args: &[QExp]) -> QResult<QExp>;
//
//     /*
//      * special-arg math
//      */
//
//     // fn eval_sample(&mut self, args: &[QExp]) -> QResult<QExp>;
// }
//
