use std::{
    cmp,
    collections::HashMap,
    fmt,
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

const BUILTINS: &'static [&'static str] = &[
    "def",              ":=", //
    "let",              ":=*", //
    "fn",               "`", //
    "defn",             ":`", //
    "if",               "=>", //
    "and",              "&&", //
    "all",              "&&*", //
    "or",               "||", //
    "any",              "||*", //
    "xor",              "^", //
    "xany",             "^*", //
    "neg",              "!", //
    "mod",              "%", //
    "range",            "..", //
    "range-inc",        "..=", //
    "length",           "#", //
    "get",              ".", //
    "slice",            "--", //
    "slice-inc",        "--=", //
    "slice-by",         "~~", //
    "slice-inc-by",     "~~=", //
    "step-by",          "~", //
    "enumerate",        "##", //
    "pick",             ".*", //
    "reverse",          "<>", //
    "first",            ".-", //
    "take",             "~.", //
    "take-while",       "~.@", //
    "last",             "-.", //
    "skip", "           .~", //
    "skip-while",       ".~@", //
    "append",           "+.", //
    "prepend",          ".+", //
    "insert",           "+.+", //
    "map",              "@", //
    "filter",           "@!", //
    "flatten",          "__",
    "contains",         "*=",
    "fold",             "@.",
    "min",              "<<",
    "max",              ">>",
    "format",           "$", //
    "print",            "$-", //
    "println",          "$_", //
    "halt",             "!!", //
    // below are functions implemented in qlang::functions
    "add",              "+", //
    "sub",              "-", //
    "mul",              "*", //
    "div",              "/", //
    "eq",               "=", //
    "neq",              "!=", //
    "gt",               ">", //
    "geq",              ">=", //
    "lt",               "<", //
    "leq",              "<=", //
    "join",             "++", //
    "zip",              "::", //
    "bool",
    "int",
    "float",
    "complex",
    "list",
    "str",
];

pub type QResult<T> = Result<T, QErr>;

#[derive(Clone)]
pub enum QExp {
    Bool(bool),
    Int(i64),
    Float(f64),
    Complex(C64),
    List(Vec<QExp>),
    Str(String),
    Symbol(String),
    Func(fn(&[QExp]) -> Result<QExp, QErr>),
    Lambda(QLambda),
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
            qfunc!(_) => qfunc!(),
            qlambda!(_) => qlambda!(),
        };
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
            (qint!(l), qint!(r)) => Ok(qint!(l / r)),
            (qfloat!(l), qfloat!(r)) => Ok(qfloat!(l / r)),
            (qcomplex!(l), qcomplex!(r)) => Ok(qcomplex!(l / r)),
            _ => Err(qerr!("invalid type in div")),
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
        return match (N_c, M_c) {
            (qint!(n), qint!(m)) => Ok(qint!(n % m)),
            (qfloat!(n), qfloat!(m)) => Ok(qfloat!(n % m)),
            _ => Err(qerr!("invalid type in mod")),
        };
    }

    pub fn eq(&self, rhs: &QExp) -> bool {
        let ty: QExpType = cmp::max(self.exp_type(), rhs.exp_type());
        let lhs_c: QExp = match convert_type(self, ty) {
            Ok(l) => l,
            Err(_) => return false,
        };
        let rhs_c: QExp = match convert_type(rhs,  ty) {
            Ok(r) => r,
            Err(_) => return false,
        };
        return match (lhs_c, rhs_c) {
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
            qbool!(x) => write!(f, "QBool({:?})", x),
            qint!(x) => write!(f, "QInt({:?})", x),
            qfloat!(x) => write!(f, "QFloat({:?})", x),
            qcomplex!(x) => write!(f, "QComplex({:?})", x),
            qlist!(x) => {
                let formatted: Vec<String>
                    = x.iter().map(|xk| format!("{:?}", xk)).collect();
                write!(f, "QList([{}])", formatted.join(", "))
            },
            qstr!(x) => write!(f, "QStr({:?})", x),
            qsymbol!(x) => write!(f, "QSymbol({:?})", x),
            qfunc!(_) => write!(f, "QFunc({{ ... }})"),
            qlambda!(x)
                => write!(f, "QLambda({:?} {{ ... }})", x.params_exp.as_ref()),
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
                    xk.fmt(f)?;
                    if k < n - 1 { write!(f, ", ")?; }
                }
                write!(f, ")")
            },
            qstr!(x) => x.fmt(f),
            qsymbol!(x) => write!(f, "{}", x),
            qfunc!(_) => write!(f, "function {{ ... }}"),
            qlambda!(x)
                => write!(f, "lambda {} {{ ... }}", x.params_exp.as_ref()),
        };
    }
}

#[derive(Clone)]
pub struct QLambda {
    params_exp: Rc<QExp>,
    body_exp: Rc<QExp>,
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

    pub fn env<'a>(
        &self,
        arg_forms: &[QExp],
        outer_env: &'a mut QEnv,
    ) -> QResult<QEnv<'a>> {
        let symbols: Vec<String>
            = Self::parse_symbols(self.params_exp.clone())?;
        if symbols.len() != arg_forms.len() {
            return Err(qerr_fmt!(
                "expected {} arguments but got {}",
                symbols.len(),
                arg_forms.len()
            ));
        }
        let values: Vec<QExp> = outer_env.eval_forms(arg_forms)?;
        let mut data: HashMap<String, QExp> = HashMap::new();
        for (k, v) in symbols.into_iter().zip(values.into_iter()) {
            data.insert(k, v);
        }
        return Ok(QEnv { data, outer: Some(outer_env) });
    }
}

enum Indexable {
    List(Vec<QExp>),
    Str(String),
}
impl Indexable {
    fn from_qexp(exp: QExp) -> QResult<Self> {
        return match exp {
            qlist!(l) => Ok(Indexable::List(l)),
            qstr!(s) => Ok(Indexable::Str(s)),
            _ => Err(qerr!("arg must be a str or list")),
        };
    }

    fn from_qexp_list(exp: QExp) -> QResult<Self> {
        return match exp {
            qlist!(l) => Ok(Indexable::List(l)),
            _ => Err(qerr!("arg must be a list")),
        };
    }

    fn from_qexp_str(exp: QExp) -> QResult<Self> {
        return match exp {
            qstr!(s) => Ok(Indexable::Str(s)),
            _ => Err(qerr!("arg must be a str")),
        };
    }

    fn is_list(&self) -> bool {
        return match self {
            Indexable::List(_) => true,
            Indexable::Str(_) => false,
        };
    }

    fn is_str(&self) -> bool {
        return match self {
            Indexable::List(_) => false,
            Indexable::Str(_) => true,
        };
    }

    fn len(&self) -> usize {
        return match self {
            Indexable::List(l) => l.len(),
            Indexable::Str(s) => s.len(),
        };
    }

    fn get(&self, idx: usize) -> Option<QExp> {
        return match self {
            Indexable::List(l) => l.get(idx).cloned(),
            Indexable::Str(s)
                => s.get(idx..idx + 1).map(|s| qstr!(s.to_string())),
        };
    }

    fn slice(&self, beg: usize, end: usize, step: usize) -> QResult<QExp> {
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

    fn slice_inc(&self, beg: usize, end: usize, step: usize) -> QResult<QExp> {
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

    fn enumerate(&self) -> QResult<QExp> {
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

    fn pick(&self, idx: &[usize]) -> QResult<QExp> {
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

    fn reverse(&self) -> QResult<QExp> {
        return match self {
            Indexable::List(l) => Ok(qlist!(l.iter().rev().cloned().collect())),
            Indexable::Str(s) => Ok(qstr!(s.chars().rev().collect())),
        };
    }

    fn first(&self) -> QResult<QExp> {
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

    fn take(&self, n: usize) -> QResult<QExp> {
        return match self {
            Indexable::List(l)
                => Ok(qlist!(l.iter().take(n).cloned().collect())),
            Indexable::Str(s)
                => Ok(qstr!(s.chars().take(n).collect())),
        };
    }

    fn take_while<F>(&self, mut f: F) -> QResult<QExp>
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

    fn last(&self) -> QResult<QExp> {
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

    fn skip(&self, n: usize) -> QResult<QExp> {
        return match self {
            Indexable::List(l)
                => Ok(qlist!(l.iter().skip(n).cloned().collect())),
            Indexable::Str(s)
                => Ok(qstr!(s.chars().skip(n).collect())),
        };
    }

    fn skip_while<F>(&self, mut f: F) -> QResult<QExp>
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

    fn zip(&self, other: &Indexable) -> QResult<QExp> {
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

    fn join(&self, other: &Indexable) -> QResult<QExp> {
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

    fn append(&self, items: &[QExp]) -> QResult<QExp> {
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

    fn prepend(&self, items: &[QExp]) -> QResult<QExp> {
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

    fn insert(&self, idx: usize, items: &[QExp]) -> QResult<QExp> {
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

    fn map<F>(&self, mut f: F) -> QResult<QExp>
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

    fn filter<F>(&self, mut f: F) -> QResult<QExp>
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

    fn flatten_qexp(exps: &[QExp]) -> Vec<QExp> {
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

    fn flatten(&self) -> QResult<QExp> {
        return match self {
            Indexable::List(l) => Ok(qlist!(Indexable::flatten_qexp(l))),
            Indexable::Str(_) => Err(qerr!("can only flatten lists")),
        };
    }

    fn contains(&self, exp: &QExp) -> QResult<QExp> {
        return match self {
            Indexable::List(l) => {
                match exp {
                    qlist!(ll) => Ok(qlist!(
                        ll.iter()
                        .map(|qk| qbool!(l.contains(qk)))
                        .collect()
                    )),
                    q => Ok(qbool!(l.contains(q))),
                }
            },
            Indexable::Str(s) => {
                match exp {
                    qlist!(ll) => {
                        ll.iter()
                        .map(|qk| {
                            match qk {
                                qstr!(sp) => Ok(qbool!(s.contains(sp))),
                                _ => Err(qerr!(
                                    "strs can only contain other strs")),
                            }
                        })
                        .collect::<QResult<Vec<QExp>>>()
                        .map(|v| qlist!(v))
                    },
                    qstr!(sp) => Ok(qbool!(s.contains(sp))),
                    _ => Err(qerr!("strs can only contain other strs")),
                }
            },
        };
    }

    fn fold<F>(&self, start: &QExp, mut f: F) -> QResult<QExp>
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

    fn min(&self) -> QResult<QExp> {
        return match self {
            Indexable::List(l) => {
                let mut m: QExp
                    = l.first()
                    .ok_or(qerr!("expected non-empty list or str"))?
                    .clone();
                for qk in l.iter() {
                    if qk.lt(&m)? {
                        m = qk.clone();
                    }
                }
                Ok(m)
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

    fn max(&self) -> QResult<QExp> {
        return match self {
            Indexable::List(l) => {
                let mut m: QExp
                    = l.first()
                    .ok_or(qerr!("expected non-empty list or str"))?
                    .clone();
                for qk in l.iter() {
                    if qk.gt(&m)? {
                        m = qk.clone();
                    }
                }
                Ok(m)
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

// pub fn parse_exprs(input: String) -> QResult<Vec<QExp>> {
//     let mut list_level: usize = 0;
//     let mut in_str: bool = false;
//     let mut in_comment: bool = false;
//     let mut exprs: Vec<QExp> = Vec::new();
//     let mut list: Vec<QExp> = Vec::new();
//     let mut term: String = String::new();
//     for x in input.chars() {
//
//

pub fn tokenize(expr: String) -> Vec<String> {
    return expr
        .replace('(', " ( ")
        .replace(')', " ) ")
        .split_whitespace()
        .map(|x| x.to_string())
        .collect();
}

pub fn parse(tokens: &[String]) -> QResult<(QExp, &[String])> {
    let (token, rest)
        = tokens.split_first()
        .ok_or(qerr!("missing closing ')'"))?;
    return match &token[..] {
        "(" => read_seq(rest),
        ")" => Err(qerr!("unexpected ')'")),
        _ => Ok((parse_atom(token), rest)),
    };
}

pub fn read_seq(tokens: &[String]) -> QResult<(QExp, &[String])> {
    let mut res: Vec<QExp> = Vec::new();
    let mut xs = tokens;
    loop {
        let (next_token, rest)
            = xs.split_first()
            .ok_or(qerr!("missing closing ')'"))?;
        if next_token == ")" {
            // println!("{:?} | {:?}", res, rest);
            return Ok((QExp::List(res), rest));
        }
        let (exp, new_xs) = parse(xs)?;
        res.push(exp);
        xs = new_xs;
    }
}

pub fn parse_atom(token: &str) -> QExp {
    return if let Ok(b) = bool::from_str(token) {
        qbool!(b)
    } else if let Ok(i) = i64::from_str(token) {
        qint!(i)
    } else if let Ok(f) = f64::from_str(token) {
        qfloat!(f)
    } else if let Ok(c) = C64::from_str(token) {
        qcomplex!(c)
    } else {
        QExp::Symbol(token.to_string())
    };
}

#[derive(Clone)]
pub struct QEnv<'a> {
    data: HashMap<String, QExp>,
    outer: Option<&'a QEnv<'a>>,
}

macro_rules! add_fn(
    ( $env:ident, $name:literal, $fn:ident ) => {
        $env.insert($name.to_string(), QExp::Func($fn));
    };
    ( $env:ident, $name:literal, $fn:path ) => {
        $env.insert($name.to_string(), QExp::Func($fn));
    };
);

impl<'a> Default for QEnv<'a> {
    fn default() -> QEnv<'a> {
        let mut env: HashMap<String, QExp> = HashMap::new();
        add_fn!( env,   "add",      fns::fn_add     );
        add_fn!( env,   "+",        fns::fn_add     );
        add_fn!( env,   "sub",      fns::fn_sub     );
        add_fn!( env,   "-",        fns::fn_sub     );
        add_fn!( env,   "mul",      fns::fn_mul     );
        add_fn!( env,   "*",        fns::fn_mul     );
        add_fn!( env,   "div",      fns::fn_div     );
        add_fn!( env,   "/",        fns::fn_div     );
        add_fn!( env,   "eq",       fns::fn_eq      );
        add_fn!( env,   "=",        fns::fn_eq      );
        add_fn!( env,   "neq",      fns::fn_neq     );
        add_fn!( env,   "!=",       fns::fn_neq     );
        add_fn!( env,   "lt",       fns::fn_lt      );
        add_fn!( env,   "<",        fns::fn_lt      );
        add_fn!( env,   "leq",      fns::fn_leq     );
        add_fn!( env,   "<=",       fns::fn_leq     );
        add_fn!( env,   "gt",       fns::fn_gt      );
        add_fn!( env,   ">",        fns::fn_gt      );
        add_fn!( env,   "geq",      fns::fn_geq     );
        add_fn!( env,   ">=",       fns::fn_geq     );
        add_fn!( env,   "join",     fns::fn_join    );
        add_fn!( env,   "++",       fns::fn_join    );
        add_fn!( env,   "zip",      fns::fn_zip     );
        add_fn!( env,   "::",       fns::fn_zip     );
        add_fn!( env,   "bool",     fns::fn_bool    );
        add_fn!( env,   "int",      fns::fn_int     );
        add_fn!( env,   "float",    fns::fn_float   );
        add_fn!( env,   "complex",  fns::fn_complex );
        add_fn!( env,   "list",     fns::fn_list    );
        add_fn!( env,   "str",      fns::fn_str     );
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
                let first_form: &QExp = match list.first() {
                    Some(exp) => exp,
                    None => { return Ok(exp.clone()); }
                };
                let arg_forms: &[QExp] = &list[1..];
                match self.eval_builtin(first_form, arg_forms) {
                    Some(res) => res,
                    None => {
                        let first_eval: QExp = self.eval(first_form)?;
                        match first_eval {
                            qfunc!(f) => f(&self.eval_forms(arg_forms)?),
                            qlambda!(ll) => {
                                let mut ll_env: QEnv
                                    = ll.env(arg_forms, self)?;
                                ll_env.eval(&ll.body_exp)
                            },
                            qsymbol!(s) => Err(
                                qerr_fmt!("could not eval symbol {}", s)
                            ),
                            _ => Ok(qlist!(self.eval_forms(list)?)),
                        }
                    }
                }
            },
            qsymbol!(k)
                => self.get_cloned(k)
                .ok_or(qerr_fmt!("symbol '{}' is undefined", k)),
            qfunc!(_) => Ok(exp.clone()),
            // qfunc!(_) => Err(qerr!("encountered unexpected function")),
            qlambda!(_) => Ok(exp.clone()),
            // qlambda!(_) => Err(qerr!("encountered unexpected lambda")),
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

    pub fn eval_forms(&mut self, arg_forms: &[QExp]) -> QResult<Vec<QExp>> {
        return arg_forms.iter().map(|x| self.eval(x)).collect();
    }

    pub fn eval_def(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        let args: Vec<QExp>
            = self.verify_args(&[qsymbol!(), qany!()], arg_forms)
            .map_err(|e| e.prepend_source("def"))?;
        let symbol: String = match args.first().unwrap() {
            qsymbol!(s) => {
                if BUILTINS.contains(&s.as_ref()) {
                    Err(qerr_fmt!(
                        "def: cannot overwrite protected symbol '{}'", s))
                } else {
                    Ok(s.clone())
                }
            },
            _ => Err(qerr!("def: first arg must be a symbol")),
        }?;
        self.insert(symbol, args[1].clone());
        return Ok(args[1].clone());
    }

    fn eval_let_assignments(&mut self, lhs: &QExp, rhs_eval: &QExp)
        -> QResult<Vec<(QExp, QExp)>>
    {
        return match (lhs, rhs_eval) {
            (qlist!(syms), qlist!(vals)) => {
                if syms.len() != vals.len() {
                    return Err(qerr!(
                        "let: symbol structure does not match value structure"
                    ));
                }
                syms.iter().zip(vals.iter())
                    .map(|(sk, vk)| {
                        match (sk, vk) {
                            (qsymbol!(_), _) => Ok((sk.clone(), vk.clone())),
                            (qlist!(_), qlist!(_)) => {
                                self.eval_let_assignments(sk, vk)
                                    .map(|subassigns| {
                                        let (s, v): (Vec<QExp>, Vec<QExp>)
                                            = subassigns.iter().cloned().unzip();
                                        (qlist!(s), qlist!(v))
                                    })
                            },
                            _ => Err(qerr!(
                                "let: symbol structure does not match value \
                                structure"
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

    fn do_let_assignment(&mut self, sym: &QExp, val: &QExp) -> QResult<()> {
        return match (sym, val) {
            (qsymbol!(s), v) => {
                self.insert(s.clone(), v.clone());
                Ok(())
            },
            (qlist!(syms), qlist!(vals)) => {
                syms.iter().zip(vals.iter())
                    .map(|(s, v)| self.do_let_assignment(s, v))
                    .collect::<QResult<Vec<()>>>()?;
                Ok(())
            },
            _ => Err(qerr!("unexpected state")),
        };
    }

    pub fn eval_let(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() != 2 {
            return Err(qerr_fmt!(
                "let: expected 2 args but got {}", arg_forms.len()));
        }
        let lhs: &QExp = arg_forms.get(0).unwrap();
        let rhs: &QExp = arg_forms.get(1).unwrap();
        let rhs_eval: QExp = self.eval(&rhs)?;
        let values: Vec<QExp>
            = self.eval_let_assignments(lhs, &rhs_eval)?
            .into_iter()
            .map(|(s, v)| {
                self.do_let_assignment(&s, &v).map(|_| v)
            })
            .collect::<QResult<Vec<QExp>>>()?;
        return Ok(qlist!(values));
    }

    pub fn eval_fn(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() != 2 {
            return Err(qerr_fmt!(
                "fn: expected 2 args but got {}", arg_forms.len()));
        }
        let params_exp: &QExp
            = arg_forms.first().ok_or(qerr!("fn: missing function args"))?;
        let body_exp: &QExp
            = arg_forms.get(1).ok_or(qerr!("fn: missing function body"))?;
        return Ok(
            qlambda!(QLambda {
                params_exp: Rc::new(params_exp.clone()),
                body_exp: Rc::new(body_exp.clone()),
            })
        );
    }

    pub fn eval_defn(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() != 3 {
            return Err(qerr_fmt!(
                "defn: expected 3 args but got {}", arg_forms.len()));
        }
        let first_form: &QExp
            = arg_forms.first().ok_or(qerr!("defn: missing function name"))?;
        let symbol: String = match first_form {
            qsymbol!(s) => {
                if BUILTINS.contains(&s.as_ref()) {
                    Err(qerr_fmt!(
                        "defn: cannot overwrite protected symbol '{}'", s))
                } else {
                    Ok(s.clone())
                }
            },
            _ => Err(qerr!("defn: first arg must be a symbol")),
        }?;
        let params_exp: &QExp
            = arg_forms.get(1).ok_or(qerr!("defn: missing function args"))?;
        let body_exp: &QExp
            = arg_forms.get(2).ok_or(qerr!("defn: missing function body"))?;
        let function = QLambda {
            params_exp: Rc::new(params_exp.clone()),
            body_exp: Rc::new(body_exp.clone()),
        };
        self.insert(symbol, qlambda!(function.clone()));
        return Ok(qlambda!(function));
    }

    pub fn eval_if(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        let test_form: &QExp
            = arg_forms.first().ok_or(qerr!("if: expected test arg"))?;
        let test_eval: QExp = self.eval(test_form)?;
        return match test_eval {
            qbool!(b) => {
                let form_idx: usize = if b { 1 } else { 2 };
                let res_form: &QExp
                    = arg_forms.get(form_idx)
                    .ok_or(
                        qerr_fmt!("if: missing condition case {}", form_idx))?;
                self.eval(res_form)
            },
            _ => Err(qerr!("if: invalid test arg")),
        };
    }

    // `and` is a built-in here because we want it to short-circuit
    pub fn eval_and(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        for form in arg_forms.iter() {
            match self.eval(form)? {
                qbool!(b) => {
                    if !b { return Ok(qbool!(false)) } else { continue; }
                },
                _ => { return Err(qerr!("and: encountered non-boolean")); },
            }
        }
        return Ok(qbool!(true));
    }

    pub fn eval_all(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() > 1 {
            return Err(qerr!("all must have exactly one argument"));
        }
        return match arg_forms.first().unwrap() {
            qlist!(exps) => self.eval_and(exps),
            qsymbol!(s) => match self.get_ok_cloned(s)? {
                qlist!(exp) => self.eval_and(&exp),
                _ => Err(qerr!("all argument must be a list")),
            },
            _ => Err(qerr!("all argument must be a list")),
        };
    }

    // `or` is a built-in here because we want it to short-circuit
    pub fn eval_or(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        for form in arg_forms.iter() {
            match self.eval(form)? {
                qbool!(b) => {
                    if b { return Ok(qbool!(true)) } else { continue; }
                },
                _ => { return Err(qerr!("or: encountered non-boolean")); },
            }
        }
        return Ok(qbool!(false));
    }

    pub fn eval_any(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() > 1 {
            return Err(qerr!("any must have exactly one argument"));
        }
        return match arg_forms.first().unwrap() {
            qlist!(exps) => self.eval_or(exps),
            qsymbol!(s) => match self.get_ok_cloned(s)? {
                qlist!(exp) => self.eval_or(&exp),
                _ => Err(qerr!("any argument must be a list")),
            },
            _ => Err(qerr!("any argument must be a list")),
        };
    }

    // `xor` is a built-in here because we want it to short-circuit
    pub fn eval_xor(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        let mut has_true: bool = false;
        for form in arg_forms.iter() {
            match self.eval(form)? {
                qbool!(b) => {
                    if has_true && b {
                        return Ok(qbool!(false))
                    } else {
                        has_true = b;
                    }
                },
                _ => { return Err(qerr!("xor: encountered non-boolean")); },
            }
        }
        return Ok(qbool!(has_true));
    }

    pub fn eval_xany(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() > 1 {
            return Err(qerr!("xany must have exactly one argument"));
        }
        return match arg_forms.first().unwrap() {
            qlist!(exps) => self.eval_xor(exps),
            qsymbol!(s) => match self.get_ok_cloned(s)? {
                qlist!(exp) => self.eval_xor(&exp),
                _ => Err(qerr!("any argument must be a list")),
            },
            _ => Err(qerr!("xany argument must be a list")),
        };
    }

    pub fn eval_neg(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() < 1 {
            return Err(qerr_fmt!(
                "neg: expected at least 1 argument but got {}",
                arg_forms.len()
            ));
        }
        return if arg_forms.len() == 1 {
            self.eval(arg_forms.get(0).unwrap())?.neg()
        } else {
            let evaled: Vec<QExp> = self.eval_forms(arg_forms)?;
            return evaled.into_iter()
                .map(|qk| qk.neg())
                .collect::<QResult<Vec<QExp>>>()
                .map(|v| qlist!(v))
                .map_err(|e| e.prepend_source("neg"));
        };
    }

    pub fn eval_mod(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() != 2 {
            return Err(qerr!("mod must have exactly two arguments"));
        }
        let mut M: QExp = self.eval(arg_forms.get(0).unwrap())?;
        let (nums, mut ty): (QExp, QExpType)
            = match self.eval(arg_forms.get(1).unwrap())? {
                qbool!(b) => Ok((qbool!(b), qbool!())),
                qint!(i) => Ok((qint!(i), qint!())),
                qfloat!(f) => Ok((qfloat!(f), qfloat!())),
                qlist!(l) => {
                    convert_numbers_sametype(&l)
                        .map(|(n, t)| (qlist!(n), t))
                },
                _ => Err(qerr!(
                    "mod: second argument must be a number or a list of numbers"
                )),
            }?;
        ty = cmp::max(M.exp_type(), ty);
        if ty > qfloat!() {
            return Err(qerr!(
                "mod: cannot modulo with complex or non-numerical values"));
        }
        M = convert_type_num(&M, ty)
            .map_err(|e| e.prepend_source("mod"))?;
        return match &nums {
            qbool!(_) | qint!(_) | qfloat!(_) => {
                convert_type_num(&nums, ty)?.modulo(&M)
            },
            qlist!(l) => {
                Ok(qlist!(
                    l.iter()
                    .map(|qk| convert_type_num(qk, ty))
                    .collect::<QResult<Vec<QExp>>>()?
                    .iter()
                    .map(|qk| qk.modulo(&M))
                    .collect::<QResult<Vec<QExp>>>()?
                ))
            },
            _ => Err(qerr!("unexpected state")),
        };
    }

    pub fn eval_range(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() > 2 {
            return Err(qerr!("range must have exactly two arguments"));
        }
        let form0: &QExp
            = arg_forms.get(0).ok_or(qerr!("range: missing start"))?;
        let start: i64 = match self.eval(form0)? {
            qint!(i) => Ok(i),
            _ => Err(qerr!("range: start must be an int")),
        }?;
        let form1: &QExp
            = arg_forms.get(1).ok_or(qerr!("range: missing stop"))?;
        let stop: i64 = match self.eval(form1)? {
            qint!(i) => Ok(i),
            _ => Err(qerr!("range: stop must be an int")),
        }?;
        return if stop >= start {
            Ok(qlist!((start..stop).map(|v| qint!(v)).collect()))
        } else {
            Ok(qlist!((stop + 1..start + 1).rev().map(|v| qint!(v)).collect()))
        };
    }

    pub fn eval_range_inc(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() > 2 {
            return Err(qerr!("range-inc must have exactly two arguments"));
        }
        let form0: &QExp
            = arg_forms.get(0).ok_or(qerr!("range: missing start"))?;
        let start: i64 = match self.eval(form0)? {
            qint!(i) => Ok(i),
            _ => Err(qerr!("range: start must be an int")),
        }?;
        let form1: &QExp
            = arg_forms.get(1).ok_or(qerr!("range: missing stop"))?;
        let stop: i64 = match self.eval(form1)? {
            qint!(i) => Ok(i),
            _ => Err(qerr!("range: stop must be an int")),
        }?;
        return if stop >= start {
            Ok(qlist!((start..=stop).map(|v| qint!(v)).collect()))
        } else {
            Ok(qlist!((stop..=start).rev().map(|v| qint!(v)).collect()))
        };
    }

    pub fn eval_length(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() != 1 {
            return Err(qerr_fmt!(
                "length: expected 1 arg but got {}", arg_forms.len()));
        }
        return match self.eval(arg_forms.first().unwrap())? {
            qlist!(l) => Ok(qint!(l.len() as i64)),
            qstr!(s) => Ok(qint!(s.len() as i64)),
            _ => Err(qerr!("length: arg must be a list or str")),
        };
    }

    pub fn eval_get(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() != 2 {
            return Err(qerr_fmt!(
                "get: expected 2 args but got {}", arg_forms.len()));
        }
        let idx: i64 = match self.eval(arg_forms.get(0).unwrap())? {
            qint!(i) => Ok(i),
            _ => Err(qerr!("get: first arg must be an int")),
        }?;
        if idx < 0 {
            return Err(qerr!("get: index must be non-negative"));
        }
        let idxable
            = Indexable::from_qexp(self.eval(arg_forms.get(1).unwrap())?)
            .map_err(|_| qerr!("get: second arg must be a list or str"))?;
        return idxable.get(idx.abs() as usize)
            .ok_or(qerr_fmt!(
                "get: index {} out of bounds for object of length {}",
                idx, idxable.len()
            ));
    }

    pub fn eval_slice(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() != 3 {
            return Err(qerr_fmt!(
                "slice: expected 3 args but got {}", arg_forms.len()));
        }
        let beg: i64 = match self.eval(arg_forms.get(0).unwrap())? {
            qint!(i) => Ok(i),
            _ => Err(qerr!("slice: first arg must be an int")),
        }?;
        if beg < 0 {
            return Err(qerr!("slice: start index must be non-negative"));
        }
        let end: i64 = match self.eval(arg_forms.get(1).unwrap())? {
            qint!(i) => Ok(i),
            _ => Err(qerr!("slice: second arg must be an int")),
        }?;
        if end < 0 {
            return Err(qerr!("slice: stop index must be non-negative"));
        }
        let idxable
            = Indexable::from_qexp(self.eval(arg_forms.get(2).unwrap())?)
            .map_err(|_| qerr!("slice: third arg must be a list or str"))?;
        return idxable.slice(beg.abs() as usize, end.abs() as usize, 1);
    }

    pub fn eval_slice_inc(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() != 3 {
            return Err(qerr_fmt!(
                "slice-inc: expected 3 args but got {}", arg_forms.len()));
        }
        let beg: i64 = match self.eval(arg_forms.get(0).unwrap())? {
            qint!(i) => Ok(i),
            _ => Err(qerr!("slice-inc: first arg must be an int")),
        }?;
        if beg < 0 {
            return Err(qerr!("slice-inc: start index must be non-negative"));
        }
        let end: i64 = match self.eval(arg_forms.get(1).unwrap())? {
            qint!(i) => Ok(i),
            _ => Err(qerr!("slice-inc: second arg must be an int")),
        }?;
        if end < 0 {
            return Err(qerr!("slice-inc: stop index must be non-negative"));
        }
        let idxable
            = Indexable::from_qexp(self.eval(arg_forms.get(2).unwrap())?)
            .map_err(|_| qerr!("slice-inc: third arg must be a list or str"))?;
        return idxable.slice_inc(beg.abs() as usize, end.abs() as usize, 1);
    }

    pub fn eval_slice_by(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() != 4 {
            return Err(qerr_fmt!(
                "slice-by: expected 4 args but got {}", arg_forms.len()));
        }
        let beg: i64 = match self.eval(arg_forms.get(0).unwrap())? {
            qint!(i) => Ok(i),
            _ => Err(qerr!("slice-by: first arg must be an int")),
        }?;
        if beg < 0 {
            return Err(qerr!("slice-by: start index must be non-negative"));
        }
        let end: i64 = match self.eval(arg_forms.get(1).unwrap())? {
            qint!(i) => Ok(i),
            _ => Err(qerr!("slice-by: second arg must be an int")),
        }?;
        if end < 0 {
            return Err(qerr!("slice-by: stop index must be non-negative"));
        }
        let step: i64 = match self.eval(arg_forms.get(2).unwrap())? {
            qint!(i) => Ok(i),
            _ => Err(qerr!("slice-by: step must be an int")),
        }?;
        if step <= 0 {
            return Err(qerr!("slice-by: step must be positibe"));
        }
        let idxable
            = Indexable::from_qexp(self.eval(arg_forms.get(2).unwrap())?)
            .map_err(|_| qerr!("slice-by: third arg must be a list or str"))?;
        return idxable.slice(
            beg.abs() as usize, end.abs() as usize, step.abs() as usize);
    }

    pub fn eval_slice_inc_by(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() != 4 {
            return Err(qerr_fmt!(
                "slice-inc-by: expected 4 args but got {}", arg_forms.len()));
        }
        let beg: i64 = match self.eval(arg_forms.get(0).unwrap())? {
            qint!(i) => Ok(i),
            _ => Err(qerr!("slice-inc-by: first arg must be an int")),
        }?;
        if beg < 0 {
            return Err(qerr!("slice-inc-by: start index must be non-negative"));
        }
        let end: i64 = match self.eval(arg_forms.get(1).unwrap())? {
            qint!(i) => Ok(i),
            _ => Err(qerr!("slice-inc-by: second arg must be an int")),
        }?;
        if end < 0 {
            return Err(qerr!("slice-inc-by: stop index must be non-negative"));
        }
        let step: i64 = match self.eval(arg_forms.get(2).unwrap())? {
            qint!(i) => Ok(i),
            _ => Err(qerr!("slice-inc-by: step must be an int")),
        }?;
        if step <= 0 {
            return Err(qerr!("slice-inc-by: step must be positive"));
        }
        let idxable
            = Indexable::from_qexp(self.eval(arg_forms.get(2).unwrap())?)
            .map_err(|_| {
                qerr!("slice-inc-by: third arg must be a list or str")
            })?;
        return idxable.slice_inc(
            beg.abs() as usize, end.abs() as usize, step.abs() as usize);
    }

    pub fn eval_step_by(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() != 2 {
            return Err(qerr_fmt!(
                "step-by: expected 2 args but got {}", arg_forms.len()));
        }
        let step: i64 = match self.eval(arg_forms.get(0).unwrap())? {
            qint!(i) => Ok(i),
            _ => Err(qerr!("step-by: step must be an int")),
        }?;
        if step <= 0 {
            return Err(qerr!("step-by: step must be positive"));
        }
        let idxable
            = Indexable::from_qexp(self.eval(arg_forms.get(1).unwrap())?)
            .map_err(|_| qerr!("step-by: second arg must be a list or str"))?;
        return idxable.slice(0, idxable.len(), step.abs() as usize);
    }

    pub fn eval_enumerate(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() != 1 {
            return Err(qerr_fmt!(
                "enumerate: expected 1 arg but got {}", arg_forms.len()));
        }
        let idxable
            = Indexable::from_qexp(self.eval(arg_forms.get(0).unwrap())?)
            .map_err(|e| e.prepend_source("enumerate"))?;
        return idxable.enumerate();
    }

    pub fn eval_pick(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() != 2 {
            return Err(qerr_fmt!(
                "pick: expected 2 args but got {}", arg_forms.len()));
        }
        let idx: Vec<usize> = match self.eval(arg_forms.get(0).unwrap())? {
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
            = Indexable::from_qexp(self.eval(arg_forms.get(1).unwrap())?)
            .map_err(|_| qerr!("pick: second arg must be a list or str"))?;
        return idxable.pick(&idx);
    }

    pub fn eval_reverse(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() != 1 {
            return Err(qerr_fmt!(
                "reverse: expected 1 arg but got {}", arg_forms.len()));
        }
        let idxable
            = Indexable::from_qexp(self.eval(arg_forms.get(0).unwrap())?)
            .map_err(|e| e.prepend_source("reverse"))?;
        return idxable.reverse();
    }

    pub fn eval_first(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() != 1 {
            return Err(qerr_fmt!(
                "first: expected 1 arg but got {}", arg_forms.len()));
        }
        let idxable
            = Indexable::from_qexp(self.eval(arg_forms.get(0).unwrap())?)
            .map_err(|e| e.prepend_source("first"))?;
        return idxable.first();
    }

    pub fn eval_take(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() != 2 {
            return Err(qerr_fmt!(
                "take: expected 2 args but got {}", arg_forms.len()));
        }
        let n: usize = match self.eval(arg_forms.get(0).unwrap())? {
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
            = Indexable::from_qexp(self.eval(arg_forms.get(1).unwrap())?)
            .map_err(|_| qerr!("take: second arg must be a list or str"))?;
        return idxable.take(n);
    }

    pub fn eval_take_while(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() != 2 {
            return Err(qerr_fmt!(
                "take-while: expected 2 args but got {}", arg_forms.len()));
        }
        let idxable
            = Indexable::from_qexp(self.eval(arg_forms.get(1).unwrap())?)
            .map_err(|_| {
                qerr!("take-while: second arg must be a list or str")
            })?;
        return match self.eval(arg_forms.get(0).unwrap())? {
            qfunc!(f) => idxable.take_while(f),
            qlambda!(ll) => {
                let f = |args: &[QExp]| {
                    let mut ll_env: QEnv = ll.env(args, self)?;
                    ll_env.eval(&ll.body_exp)
                };
                idxable.take_while(f)
            },
            _ => Err(qerr!("take-while: first arg must be a function")),
        };
    }

    pub fn eval_last(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() != 1 {
            return Err(qerr_fmt!(
                "last: expected 1 arg but got {}", arg_forms.len()));
        }
        let idxable
            = Indexable::from_qexp(self.eval(arg_forms.get(0).unwrap())?)
            .map_err(|e| e.prepend_source("last"))?;
        return idxable.last();
    }

    pub fn eval_skip(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() != 2 {
            return Err(qerr_fmt!(
                "skip: expected 2 args but got {}", arg_forms.len()));
        }
        let n: usize = match self.eval(arg_forms.get(0).unwrap())? {
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
            = Indexable::from_qexp(self.eval(arg_forms.get(1).unwrap())?)
            .map_err(|_| qerr!("skip: second arg must be a list or str"))?;
        return idxable.skip(n);
    }

    pub fn eval_skip_while(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() != 2 {
            return Err(qerr_fmt!(
                "skip-while: expected 2 args but got {}", arg_forms.len()));
        }
        let idxable
            = Indexable::from_qexp(self.eval(arg_forms.get(1).unwrap())?)
            .map_err(|_| {
                qerr!("skip-while: second arg must be a list or str")
            })?;
        return match self.eval(arg_forms.get(0).unwrap())? {
            qfunc!(f) => idxable.skip_while(f),
            qlambda!(ll) => {
                let f = |args: &[QExp]| {
                    let mut ll_env: QEnv = ll.env(args, self)?;
                    ll_env.eval(&ll.body_exp)
                };
                idxable.skip_while(f)
            },
            _ => Err(qerr!("skip-while: first arg must be a function")),
        };
    }

    pub fn eval_append(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() < 2 {
            return Err(qerr!("append: expected at least 2 args"));
        }
        let idxable
            = Indexable::from_qexp(self.eval(arg_forms.get(0).unwrap())?)
            .map_err(|_| qerr!("append: first arg must be a str or list"))?;
        return idxable.append(&self.eval_forms(&arg_forms[1..])?);
    }

    pub fn eval_prepend(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() < 2 {
            return Err(qerr!("prepend: expected at least 2 args"));
        }
        let idxable
            = Indexable::from_qexp(self.eval(arg_forms.get(0).unwrap())?)
            .map_err(|_| qerr!("prepend: first arg must be a str or list"))?;
        return idxable.prepend(&self.eval_forms(&arg_forms[1..])?);
    }

    pub fn eval_insert(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() < 3 {
            return Err(qerr_fmt!(
                "insert: expected at least 3 args but got {}",
                arg_forms.len()
            ));
        }
        let idx: usize = match self.eval(arg_forms.get(0).unwrap())? {
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
            = Indexable::from_qexp(self.eval(arg_forms.get(1).unwrap())?)
            .map_err(|_| qerr!("insert: second arg must be a str or list"))?;
        return idxable.insert(idx, &self.eval_forms(&arg_forms[2..])?);
    }

    pub fn eval_map(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() != 2 {
            return Err(qerr_fmt!(
                "map: expected 2 args but got {}", arg_forms.len()));
        }
        let idxable
            = Indexable::from_qexp(self.eval(arg_forms.get(1).unwrap())?)
            .map_err(|_| qerr!("map: second arg must be a list or str"))?;
        return match self.eval(arg_forms.get(0).unwrap())? {
            qfunc!(f) => idxable.map(f),
            qlambda!(ll) => {
                let f = |args: &[QExp]| {
                    let mut ll_env: QEnv = ll.env(args, self)?;
                    ll_env.eval(&ll.body_exp)
                };
                idxable.map(f)
            },
            _ => Err(qerr!("map: first arg must be a function")),
        };
    }

    pub fn eval_filter(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() != 2 {
            return Err(qerr_fmt!(
                "filter: expected 2 args but got {}", arg_forms.len()));
        }
        let idxable
            = Indexable::from_qexp(self.eval(arg_forms.get(1).unwrap())?)
            .map_err(|_| qerr!("filter: second arg must be a list or str"))?;
        return match self.eval(arg_forms.get(0).unwrap())? {
            qfunc!(f) => idxable.filter(f),
            qlambda!(ll) => {
                let f = |args: &[QExp]| {
                    let mut ll_env: QEnv = ll.env(args, self)?;
                    ll_env.eval(&ll.body_exp)
                };
                idxable.filter(f)
            },
            _ => Err(qerr!("filter: first arg must be a function")),
        };
    }

    pub fn eval_flatten(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() != 1 {
            return Err(qerr_fmt!(
                "flatten: expected 1 arg but got {}", arg_forms.len()));
        }
        let idxable
            = Indexable::from_qexp(self.eval(arg_forms.get(0).unwrap())?)
            .map_err(|e| e.prepend_source("flatten"))?;
        return idxable.flatten();
    }

    pub fn eval_contains(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() < 2 {
            return Err(qerr!("contains: expected at least 2 args"));
        }
        let idxable
            = Indexable::from_qexp(self.eval(arg_forms.get(0).unwrap())?)
            .map_err(|_| qerr!("contains: first arg must be a str or list"))?;
        return if arg_forms.len() > 2 {
            idxable.contains(&qlist!(self.eval_forms(&arg_forms[1..])?))
        } else {
            idxable.contains(&self.eval(arg_forms.get(1).unwrap())?)
        }
    }

    pub fn eval_fold(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() != 3 {
            return Err(qerr_fmt!(
                "fold: expected 3 args but got {}", arg_forms.len()));
        }
        let start: QExp = self.eval(arg_forms.get(0).unwrap())?;
        let idxable
            = Indexable::from_qexp(self.eval(arg_forms.get(2).unwrap())?)
            .map_err(|_| qerr!("fold: third arg must be a list or str"))?;
        return match self.eval(arg_forms.get(1).unwrap())? {
            qfunc!(f) => idxable.fold(&start, f),
            qlambda!(ll) => {
                let f = |args: &[QExp]| {
                    let mut ll_env: QEnv = ll.env(args, self)?;
                    ll_env.eval(&ll.body_exp)
                };
                idxable.fold(&start, f)
            },
            _ => Err(qerr!("fold: second arg must be a function")),
        };
    }

    pub fn eval_min(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() != 1 {
            return Err(qerr_fmt!(
                "min: expected 1 arg but got {}", arg_forms.len()));
        }
        let idxable
            = Indexable::from_qexp(self.eval(arg_forms.get(0).unwrap())?)
            .map_err(|e| e.prepend_source("min"))?;
        return idxable.min();
    }

    pub fn eval_max(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() != 1 {
            return Err(qerr_fmt!(
                "max: expected 1 arg but got {}", arg_forms.len()));
        }
        let idxable
            = Indexable::from_qexp(self.eval(arg_forms.get(0).unwrap())?)
            .map_err(|e| e.prepend_source("max"))?;
        return idxable.max();
    }

    pub fn eval_format(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() < 1 {
            return Err(qerr!("format: missing format str"));
        }
        let mut fmt: fmtx::Template
            = match self.eval(arg_forms.first().unwrap())? {
                qstr!(s) => {
                    formatx!(s)
                        .map_err(|e| {
                            qerr_fmt!(
                                "format: invalid format string: {}",
                                e.message()
                            )
                        })
                },
                _ => Err(qerr!("format: first arg must be a format string")),
            }?;
        arg_forms.iter().skip(1)
            .for_each(|qk| fmt.replace_positional(qk));
        return fmt.text()
            .map(|s| qstr!(s))
            .map_err(|e| {
                qerr_fmt!("format: could not format string: {}", e.message())
            });
    }

    pub fn eval_print(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        return self.eval_format(arg_forms)
            .map(|fmtstr| {
                print!("{}", fmtstr);
                std::io::Write::flush(&mut std::io::stdout()).unwrap();
                fmtstr
            })
            .map_err(|e| e.prepend_source("print"));
    }

    pub fn eval_println(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        return self.eval_format(arg_forms)
            .map(|fmtstr| {
                println!("{}", fmtstr);
                std::io::Write::flush(&mut std::io::stdout()).unwrap();
                fmtstr
            })
            .map_err(|e| e.prepend_source("println"));
    }

    pub fn eval_halt(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        return match self.eval_format(arg_forms) {
            Ok(msg) => Err(qerr_fmt!("halt: {}", msg)),
            Err(e) => Err(e.prepend_source("halt")),
        };
    }

    pub fn eval_builtin(&mut self, exp: &QExp, arg_forms: &[QExp])
        -> Option<QResult<QExp>>
    {
        return match exp {
            qsymbol!(s) => match s.as_ref() {
                "def"           => Some(self.eval_def(arg_forms)),
                ":="            => Some(self.eval_def(arg_forms)),
                "let"           => Some(self.eval_let(arg_forms)),
                ":=*"           => Some(self.eval_let(arg_forms)),
                "fn"            => Some(self.eval_fn(arg_forms)),
                "`"             => Some(self.eval_fn(arg_forms)),
                "defn"          => Some(self.eval_defn(arg_forms)),
                ":`"            => Some(self.eval_defn(arg_forms)),
                "if"            => Some(self.eval_if(arg_forms)),
                "=>"            => Some(self.eval_if(arg_forms)),
                "and"           => Some(self.eval_and(arg_forms)),
                "&&"            => Some(self.eval_and(arg_forms)),
                "all"           => Some(self.eval_all(arg_forms)),
                "&&*"           => Some(self.eval_all(arg_forms)),
                "or"            => Some(self.eval_or(arg_forms)),
                "||"            => Some(self.eval_or(arg_forms)),
                "any"           => Some(self.eval_any(arg_forms)),
                "||*"           => Some(self.eval_any(arg_forms)),
                "xor"           => Some(self.eval_xor(arg_forms)),
                "^"             => Some(self.eval_xor(arg_forms)),
                "xany"          => Some(self.eval_xany(arg_forms)),
                "^*"            => Some(self.eval_xany(arg_forms)),
                "neg"           => Some(self.eval_neg(arg_forms)),
                "!"             => Some(self.eval_neg(arg_forms)),
                "mod"           => Some(self.eval_mod(arg_forms)),
                "%"             => Some(self.eval_mod(arg_forms)),
                "range"         => Some(self.eval_range(arg_forms)),
                ".."            => Some(self.eval_range(arg_forms)),
                "range-inc"     => Some(self.eval_range_inc(arg_forms)),
                "..="           => Some(self.eval_range_inc(arg_forms)),
                "length"        => Some(self.eval_length(arg_forms)),
                "#"             => Some(self.eval_length(arg_forms)),
                "get"           => Some(self.eval_get(arg_forms)),
                "."             => Some(self.eval_get(arg_forms)),
                "slice"         => Some(self.eval_slice(arg_forms)),
                "--"            => Some(self.eval_slice(arg_forms)),
                "slice-inc"     => Some(self.eval_slice_inc(arg_forms)),
                "--="           => Some(self.eval_slice_inc(arg_forms)),
                "slice-by"      => Some(self.eval_slice_by(arg_forms)),
                "~~"            => Some(self.eval_slice_by(arg_forms)),
                "slice-inc-by"  => Some(self.eval_slice_inc_by(arg_forms)),
                "~~="           => Some(self.eval_slice_inc_by(arg_forms)),
                "step-by"       => Some(self.eval_step_by(arg_forms)),
                "~"             => Some(self.eval_step_by(arg_forms)),
                "enumerate"     => Some(self.eval_enumerate(arg_forms)),
                "##"            => Some(self.eval_enumerate(arg_forms)),
                "pick"          => Some(self.eval_pick(arg_forms)),
                ".*"            => Some(self.eval_pick(arg_forms)),
                "reverse"       => Some(self.eval_reverse(arg_forms)),
                "<>"            => Some(self.eval_reverse(arg_forms)),
                "first"         => Some(self.eval_first(arg_forms)),
                ".-"            => Some(self.eval_first(arg_forms)),
                "take"          => Some(self.eval_take(arg_forms)),
                "~."            => Some(self.eval_take(arg_forms)),
                "take-while"    => Some(self.eval_take_while(arg_forms)),
                "~.@"           => Some(self.eval_take_while(arg_forms)),
                "last"          => Some(self.eval_last(arg_forms)),
                "-."            => Some(self.eval_last(arg_forms)),
                "skip"          => Some(self.eval_skip(arg_forms)),
                ".~"            => Some(self.eval_skip(arg_forms)),
                "skip-while"    => Some(self.eval_skip_while(arg_forms)),
                ".~@"           => Some(self.eval_skip_while(arg_forms)),
                "append"        => Some(self.eval_append(arg_forms)),
                "+."            => Some(self.eval_append(arg_forms)),
                "prepend"       => Some(self.eval_prepend(arg_forms)),
                ".+"            => Some(self.eval_prepend(arg_forms)),
                "insert"        => Some(self.eval_insert(arg_forms)),
                "+.+"           => Some(self.eval_insert(arg_forms)),
                "map"           => Some(self.eval_map(arg_forms)),
                "@"             => Some(self.eval_map(arg_forms)),
                "filter"        => Some(self.eval_filter(arg_forms)),
                "@!"            => Some(self.eval_filter(arg_forms)),
                "flatten"       => Some(self.eval_flatten(arg_forms)),
                "__"            => Some(self.eval_flatten(arg_forms)),
                "contains"      => Some(self.eval_contains(arg_forms)),
                "=*"            => Some(self.eval_contains(arg_forms)),
                "fold"          => Some(self.eval_fold(arg_forms)),
                "@."            => Some(self.eval_fold(arg_forms)),
                "min"           => Some(self.eval_min(arg_forms)),
                "<<"            => Some(self.eval_min(arg_forms)),
                "max"           => Some(self.eval_max(arg_forms)),
                ">>"            => Some(self.eval_max(arg_forms)),
                "format"        => Some(self.eval_format(arg_forms)),
                "$"             => Some(self.eval_format(arg_forms)),
                "print"         => Some(self.eval_print(arg_forms)),
                "$-"            => Some(self.eval_print(arg_forms)),
                "println"       => Some(self.eval_println(arg_forms)),
                "$_"            => Some(self.eval_println(arg_forms)),
                "halt"          => Some(self.eval_halt(arg_forms)),
                "!!"            => Some(self.eval_halt(arg_forms)),
                _ => None,
            },
            _ => None,
        };
    }

    pub fn parse_eval(&mut self, expr: String) -> QResult<QExp> {
        let (parsed_exp, _): (QExp, _) = parse(&tokenize(expr))?;
        let evaled: QExp = self.eval(&parsed_exp)?;
        return Ok(evaled);
    }
}

