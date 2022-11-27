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
    "def",              ":=",   // done
    "let",              ":=*",  // done
    "fn",               "@:",   // done
    "defn",             "@:=",  // done
    "if",               "=>",   // done
    "and",              "&&",   // done
    "all",              "&&*",  // done
    "or",               "||",   // done
    "any",              "||*",  // done
    "xor",              "^",    // done
    "xany",             "^*",   // done
    "neg",              "!",    // done
    "range",            "..",   // done
    "range-inc",        "..=",  // done
    "repeat",           "#=",   // done
    "length",           "#",    // done
    "get",              ".",    // done
    "set",              ".:=",  // done
    "slice",            "--",   // done
    "slice-inc",        "--=",  // done
    "slice-by",         "~~",   // done
    "slice-inc-by",     "~~=",  // done
    "step-by",          "~",    // done
    "enumerate",        "##",   // done
    "pick",             ".*",   // done
    "reverse",          "<>",   // done
    "cycle",            "<#>",  // done
    "first",            ".-",   // done
    "take",             "~.",   // done
    "take-while",       "~.@",  // done
    "last",             "-.",   // done
    "skip", "           .~",    // done
    "skip-while",       ".~@",  // done
    "split-at",         "|.",   // done
    "split-on",         "|@",   // done
    "split-on-inc",     "|@=",  // done
    "append",           "+.",   // done
    "prepend",          ".+",   // done
    "insert",           "+.+",  // done
    "map",              "@",    // done
    "filter",           "@!",   // done
    "unique",           "*!=",  // done
    "flatten",          "__",   // done
    "contains",         "*=",   // done
    "index-of",         "#*=",  // done
    "fold",             "@.",   // done
    "min",              "<<",   // done
    "max",              ">>",   // done
    "select-by",        "*@.",  // done
    "sort",             "<*",   // done
    "sort-by",          "<@",   // done
    "permute",          ".~.",  // done
    "format",           "$",    // done
    "print",            "$-",   // done
    "println",          "$_",   // done
    "halt",             "!!",   // done
    "istype",           "~?",   // done
    "type",             "?~",   // done
    // math
    "mod",              "%",    // done
    "abs",              "|.|",  
    "recip",            "1/",   
    "sqrt",                     
    "cbrt",                     
    "exp",              "e**",  
    "floor",            "~_",   
    "ceil",             "~^",   
    "round",            "~.",   
    "padd",             "p+",   
    "log",                      
    "ln",                       
    "pow",              "**",   
    "sin",                      
    "cos",                      
    "tan",                      
    "asin",                     
    "acos",                     
    "atan",                     
    "atan2",                    
    "sinh",                     
    "cosh",                     
    "tanh",                     
    "asinh",                    
    "acosh",                    
    "atanh",                    
    "arg",                      
    "cis",              "e**i", 
    "conj",             "~z",   
    "real",             "Re",   
    "imag",             "Im",   
    "sum",              "{S}",  
    "product",          "{P}",  
    "mean",             "{E}",  
    "variance",         "Var",  
    "stddev",           "Std",  
    "moment",           "{En}", 
    "covariance",       "Cov",  
    "correlation",      "Corr", 
    "convolve",         "{*}",  
    "histogram",        "|#|",  
    "histogram-prob",   "|p|",  
    "fft",              "{F}",  
    "ifft",             "{iF}", 
    "findpeaks",        "^?",   
    "sample",           "?.",   
    // implemented in qlisp::functions
    "add",              "+",    // done
    "sub",              "-",    // done
    "mul",              "*",    // done
    "div",              "/",    // done
    "idiv",             "//",   // done
    "eq",               "=",    // done
    "neq",              "!=",   // done
    "gt",               ">",    // done
    "geq",              ">=",   // done
    "lt",               "<",    // done
    "leq",              "<=",   // done
    "join",             "++",   // done
    "zip",              "::",   // done
    "cart",             ":*:",  // done
    "bool",                     // done
    "int",                      // done
    "float",                    // done
    "complex",                  // done
    "list",                     // done
    "str",                      // done
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

    fn repeat(&self, n: usize) -> QResult<QExp> {
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

    fn set(&self, vals: &[(usize, QExp)]) -> QResult<QExp> {
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

    fn cycle(&self, shift: isize) -> QResult<QExp> {
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

    fn split_at(&self, idx: usize) -> QResult<QExp> {
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

    fn split_on<F>(&self, mut f: F) -> QResult<QExp>
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

    fn split_on_inc<F>(&self, mut f: F) -> QResult<QExp>
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

    fn unique(&self) -> QResult<QExp> {
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

    fn index_of(&self, exp: &QExp) -> QResult<QExp> {
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

    fn max(&self) -> QResult<QExp> {
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

    fn select_by<F>(&self, mut f: F) -> QResult<QExp>
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

    fn merge_sort_list(items: &[QExp]) -> QResult<Vec<&QExp>> {
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

    fn merge_sort_str(chars: &str) -> QResult<String> {
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

    fn sort(&self) -> QResult<QExp> {
        return match self {
            Indexable::List(l) => Ok(qlist!(
                Indexable::merge_sort_list(l)?.into_iter().cloned().collect()
            )),
            Indexable::Str(s) => Ok(qstr!(
                Indexable::merge_sort_str(s)?
            )),
        };
    }

    fn merge_sort_list_by<'a, F>(items: &'a [QExp], f: &mut F)
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

    fn merge_sort_str_by<F>(chars: &str, f: &mut F) -> QResult<String>
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

    fn sort_by<F>(&self, mut f: F) -> QResult<QExp>
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

    fn permute(&self, moves: &[Vec<usize>]) -> QResult<QExp> {
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

// pub fn tokenize(expr: String) -> Vec<String> {
//     return expr
//         .replace('(', " ( ")
//         .replace(')', " ) ")
//         .split_whitespace()
//         .map(|x| x.to_string())
//         .collect();
// }

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum TokenState {
    Normal = 0,
    InComment = 1,
    InString = 2,
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
                ' ' => {
                    if !term.is_empty() {
                        ret.push(mem::replace(&mut term, String::new()));
                    }
                },
                '\n' => {
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
                _ => {
                    term.push(x);
                },
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

// pub fn parse(tokens: &[String]) -> QResult<(QExp, &[String])> {
//     let (token, rest)
//         = tokens.split_first()
//         .ok_or(qerr!("missing closing ')'"))?;
//     return match &token[..] {
//         "(" => read_seq(rest),
//         ")" => Err(qerr!("unexpected ')'")),
//         _ => Ok((parse_atom(token), rest)),
//     };
// }
//
// pub fn read_seq(tokens: &[String]) -> QResult<(QExp, &[String])> {
//     let mut res: Vec<QExp> = Vec::new();
//     let mut xs = tokens;
//     loop {
//         let (next_token, rest)
//             = xs.split_first()
//             .ok_or(qerr!("missing closing ')'"))?;
//         if next_token == ")" {
//             // println!("{:?} | {:?}", res, rest);
//             return Ok((QExp::List(res), rest));
//         }
//         let (exp, new_xs) = parse(xs)?;
//         res.push(exp);
//         xs = new_xs;
//     }
// }
//
// pub fn parse_atom(token: &str) -> QExp {
//     return if let Ok(b) = bool::from_str(token) {
//         qbool!(b)
//     } else if let Ok(i) = i64::from_str(token) {
//         qint!(i)
//     } else if let Ok(f) = f64::from_str(token) {
//         qfloat!(f)
//     } else if let Ok(c) = C64::from_str(token) {
//         qcomplex!(c)
//     } else if token.starts_with('"') && token.ends_with('"') {
//         qstr!(token.to_string())
//     } else {
//         qsymbol!(token.to_string())
//     };
// }

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
        add_fn!( env,   "idiv",     fns::fn_idiv    );
        add_fn!( env,   "//",       fns::fn_idiv    );
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
        add_fn!( env,   "cart",     fns::fn_cart    );
        add_fn!( env,   ":*:",      fns::fn_cart    );
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
        return match self.eval(arg_forms.first().unwrap())? {
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
        return match self.eval(arg_forms.first().unwrap())? {
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
            match self.eval(arg_forms.first().unwrap())? {
                qlist!(l) => self.eval_neg(&l),
                q => q.neg(),
            }
        } else {
            let evaled: Vec<QExp> = self.eval_forms(arg_forms)?;
            return evaled.into_iter()
                .map(|qk| match qk {
                    qlist!(l) => self.eval_neg(&l),
                    qexp => qexp.neg(),
                })
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

    pub fn eval_repeat(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() != 2 {
            return Err(qerr_fmt!(
                "repeat: expected 2 args but got {}", arg_forms.len()));
        }
        let n: usize = match self.eval(arg_forms.get(0).unwrap())? {
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
            = Indexable::from_qexp(self.eval(arg_forms.get(1).unwrap())?)
            .map_err(|_| qerr!("repeat: second arg must be a list or str"))?;
        return idxable.repeat(n);
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

    pub fn eval_set(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() < 2 {
            return Err(qerr_fmt!(
                "set: expected at least 2 args but got {}", arg_forms.len()));
        }
        let vals: Vec<(usize, QExp)>
            = self.eval_forms(&arg_forms[1..])?
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
            = Indexable::from_qexp_list(self.eval(arg_forms.get(0).unwrap())?)
            .map_err(|_| qerr!("set: first arg must be a list"))?;
        return idxable.set(&vals)
            .map_err(|e| e.prepend_source("set"));
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
            = Indexable::from_qexp(self.eval(arg_forms.get(3).unwrap())?)
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
            = Indexable::from_qexp(self.eval(arg_forms.get(3).unwrap())?)
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

    pub fn eval_cycle(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() != 2 {
            return Err(qerr_fmt!(
                "cycle: expected 2 args but got {}", arg_forms.len()));
        }
        let shift: isize = match self.eval(arg_forms.get(0).unwrap())? {
            qint!(i) => Ok(i as isize),
            _ => Err(qerr!("cycle: first arg must be an int")),
        }?;
        let idxable
            = Indexable::from_qexp(self.eval(arg_forms.get(1).unwrap())?)
            .map_err(|_| qerr!("cycle: second arg must be a list or str"))?;
        return idxable.cycle(shift)
            .map_err(|e| e.prepend_source("cycle"));
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

    pub fn eval_split_at(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() != 2 {
            return Err(qerr_fmt!(
                "split-at: expected 2 args but got {}", arg_forms.len()));
        }
        let idx: usize = match self.eval(arg_forms.first().unwrap())? {
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
            = Indexable::from_qexp(self.eval(arg_forms.get(1).unwrap())?)
            .map_err(|_| {
                qerr!("split-at: second arg must be a list or str")
            })?;
        return idxable.split_at(idx);
    }

    pub fn eval_split_on(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() != 2 {
            return Err(qerr_fmt!(
                "split-on: expected 2 args but got {}", arg_forms.len()));
        }
        let idxable
            = Indexable::from_qexp(self.eval(arg_forms.get(1).unwrap())?)
            .map_err(|_| {
                qerr!("split-on: second arg must eb a list or str")
        })?;
        return match self.eval(arg_forms.get(0).unwrap())? {
            qfunc!(f) => idxable.split_on(f),
            qlambda!(ll) => {
                let f = |args: &[QExp]| {
                    let mut ll_env: QEnv = ll.env(args, self)?;
                    ll_env.eval(&ll.body_exp)
                };
                idxable.split_on(f)
            },
            _ => Err(qerr!("split-on: first arg must be a function")),
        };
    }

    pub fn eval_split_on_inc(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() != 2 {
            return Err(qerr_fmt!(
                "split-on-inc: expected 2 args but got {}", arg_forms.len()));
        }
        let idxable
            = Indexable::from_qexp(self.eval(arg_forms.get(1).unwrap())?)
            .map_err(|_| {
                qerr!("split-on-inc: second arg must eb a list or str")
        })?;
        return match self.eval(arg_forms.get(0).unwrap())? {
            qfunc!(f) => idxable.split_on_inc(f),
            qlambda!(ll) => {
                let f = |args: &[QExp]| {
                    let mut ll_env: QEnv = ll.env(args, self)?;
                    ll_env.eval(&ll.body_exp)
                };
                idxable.split_on_inc(f)
            },
            _ => Err(qerr!("split-on-inc: first arg must be a function")),
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

    pub fn eval_unique(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() != 1 {
            return Err(qerr_fmt!(
                "unique: expected 1 arg but got {}", arg_forms.len()));
        }
        let idxable
            = Indexable::from_qexp(self.eval(arg_forms.get(0).unwrap())?)
            .map_err(|e| e.prepend_source("unique"))?;
        return idxable.unique()
            .map_err(|e| e.prepend_source("unique"));
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

    pub fn eval_index_of(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() != 2 {
            return Err(qerr_fmt!(
                "index-of: expected 2 args but got {}", arg_forms.len()));
        }
        let item: QExp
            = self.eval(arg_forms.get(1).unwrap())
            .map_err(|e| e.prepend_source("index-of"))?;
        let idxable
            = Indexable::from_qexp(self.eval(arg_forms.get(0).unwrap())?)
            .map_err(|_| qerr!("index-of: second arg must be a str or list"))?;
        return idxable.index_of(&item)
            .map_err(|e| e.prepend_source("index-of"));
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

    pub fn eval_select_by(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
       if arg_forms.len() != 2 {
           return Err(qerr_fmt!(
                "select-by: expected 2 args but got {}", arg_forms.len()));
       }
       let idxable
           = Indexable::from_qexp(self.eval(arg_forms.get(1).unwrap())?)
           .map_err(|_| qerr!("select-by: second arg must be a list or str"))?;
        return match self.eval(arg_forms.get(0).unwrap())? {
            qfunc!(f) => idxable.select_by(f),
            qlambda!(ll) => {
                let f = |args: &[QExp]| {
                    let mut ll_env: QEnv = ll.env(args, self)?;
                    ll_env.eval(&ll.body_exp)
                };
                idxable.select_by(f)
            },
            _ => Err(qerr!("select-by: second arg must be a function")),
       };
    }

    pub fn eval_sort(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() != 1 {
            return Err(qerr_fmt!(
                "sort: expected 1 arg but got {}", arg_forms.len()));
        }
        let idxable
            = Indexable::from_qexp(self.eval(arg_forms.first().unwrap())?)
            .map_err(|e| e.prepend_source("sort"))?;
        return idxable.sort();
    }

    pub fn eval_sort_by(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() != 2 {
            return Err(qerr_fmt!(
                "sort-by: expected 2 args but got {}", arg_forms.len()));
        }
        let idxable
            = Indexable::from_qexp(self.eval(arg_forms.get(1).unwrap())?)
            .map_err(|_| qerr!("sort-by: second arg must be a list or str"))?;
        return match self.eval(arg_forms.get(0).unwrap())? {
            qfunc!(f) => idxable.sort_by(f),
            qlambda!(ll) => {
                let f = |args: &[QExp]| {
                    let mut ll_env: QEnv = ll.env(args, self)?;
                    ll_env.eval(&ll.body_exp)
                };
                idxable.sort_by(f)
            },
            _ => Err(qerr!("sort-by: first arg must be a function")),
        };
    }

    pub fn eval_permute(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() < 2 {
            return Err(qerr_fmt!(
                "permute: expected at least 2 args but got {}",
                arg_forms.len()
            ));
        }
        let idxable
            = Indexable::from_qexp_list(self.eval(arg_forms.get(0).unwrap())?)
            .map_err(|_| qerr!("permute: first arg must be a list"))?;
        let moves: Vec<Vec<usize>>
            = self.eval_forms(&arg_forms[1..])?
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
            .map(|qk| self.eval(qk))
            .collect::<QResult<Vec<QExp>>>()?
            .iter()
            .for_each(|qk| fmt.replace_positional(qk));
        return fmt.text()
            .map(|s| qstr!(s))
            .map_err(|e| {
                qerr_fmt!("format: could not format string: {}", e.message())
            });
    }

    pub fn eval_print(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        self.eval_format(arg_forms)
            .map(|fmtstr| {
                print!("{}", fmtstr);
                std::io::Write::flush(&mut std::io::stdout()).unwrap();
                fmtstr
            })
            .map_err(|e| e.prepend_source("print"))?;
        let vals: Vec<QExp> = self.eval_forms(&arg_forms[1..])?;
        return if vals.len() == 1 {
            Ok(vals.into_iter().next().unwrap())
        } else {
            Ok(qlist!(vals))
        };
    }

    pub fn eval_println(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        self.eval_format(arg_forms)
            .map(|fmtstr| {
                println!("{}", fmtstr);
                std::io::Write::flush(&mut std::io::stdout()).unwrap();
                fmtstr
            })
            .map_err(|e| e.prepend_source("println"))?;
        let vals: Vec<QExp> = self.eval_forms(&arg_forms[1..])?;
        return if vals.len() == 1 {
            Ok(vals.into_iter().next().unwrap())
        } else {
            Ok(qlist!(vals))
        };
    }

    pub fn eval_halt(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        return match self.eval_format(arg_forms) {
            Ok(msg) => Err(qerr_fmt!("halt: {}", msg)),
            Err(e) => Err(e.prepend_source("halt")),
        };
    }

    pub fn eval_istype(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() != 2 {
            return Err(qerr_fmt!(
                "istype: expected 2 args but got {}", arg_forms.len()));
        }
        let typespec: Vec<QExpType>
            = match self.eval(arg_forms.get(0).unwrap())? {
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
            = self.eval(arg_forms.get(1).unwrap())
            .map_err(|e| e.prepend_source("istype"))?
            .is_type_user(&typespec);
        return Ok(qbool!(ret));
    }

    pub fn eval_type(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() < 1 {
            return Err(qerr_fmt!(
                "type: expected at least 1 arg but got {}", arg_forms.len()));
        }
        let vals: Vec<QExp> = self.eval_forms(arg_forms)?;
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
                "@:"            => Some(self.eval_fn(arg_forms)),
                "defn"          => Some(self.eval_defn(arg_forms)),
                "@:="           => Some(self.eval_defn(arg_forms)),
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
                "repeat"        => Some(self.eval_repeat(arg_forms)),
                "#="            => Some(self.eval_repeat(arg_forms)),
                "length"        => Some(self.eval_length(arg_forms)),
                "#"             => Some(self.eval_length(arg_forms)),
                "get"           => Some(self.eval_get(arg_forms)),
                "."             => Some(self.eval_get(arg_forms)),
                "set"           => Some(self.eval_set(arg_forms)),
                ".:="           => Some(self.eval_set(arg_forms)),
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
                "cycle"         => Some(self.eval_cycle(arg_forms)),
                "<#>"           => Some(self.eval_cycle(arg_forms)),
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
                "split-at"      => Some(self.eval_split_at(arg_forms)),
                "|."            => Some(self.eval_split_at(arg_forms)),
                "split-on"      => Some(self.eval_split_on(arg_forms)),
                "|@"            => Some(self.eval_split_on(arg_forms)),
                "split-on-inc"  => Some(self.eval_split_on_inc(arg_forms)),
                "|@="           => Some(self.eval_split_on_inc(arg_forms)),
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
                "unique"        => Some(self.eval_unique(arg_forms)),
                "*!="           => Some(self.eval_unique(arg_forms)),
                "flatten"       => Some(self.eval_flatten(arg_forms)),
                "__"            => Some(self.eval_flatten(arg_forms)),
                "contains"      => Some(self.eval_contains(arg_forms)),
                "*="            => Some(self.eval_contains(arg_forms)),
                "index-of"      => Some(self.eval_index_of(arg_forms)),
                "#*="           => Some(self.eval_index_of(arg_forms)),
                "fold"          => Some(self.eval_fold(arg_forms)),
                "@."            => Some(self.eval_fold(arg_forms)),
                "min"           => Some(self.eval_min(arg_forms)),
                "<<"            => Some(self.eval_min(arg_forms)),
                "max"           => Some(self.eval_max(arg_forms)),
                ">>"            => Some(self.eval_max(arg_forms)),
                "select-by"     => Some(self.eval_select_by(arg_forms)),
                "*@."           => Some(self.eval_select_by(arg_forms)),
                "sort"          => Some(self.eval_sort(arg_forms)),
                "<*"            => Some(self.eval_sort(arg_forms)),
                "sort-by"       => Some(self.eval_sort_by(arg_forms)),
                "<@"            => Some(self.eval_sort_by(arg_forms)),
                "permute"       => Some(self.eval_permute(arg_forms)),
                ".~."           => Some(self.eval_permute(arg_forms)),
                "format"        => Some(self.eval_format(arg_forms)),
                "$"             => Some(self.eval_format(arg_forms)),
                "print"         => Some(self.eval_print(arg_forms)),
                "$-"            => Some(self.eval_print(arg_forms)),
                "println"       => Some(self.eval_println(arg_forms)),
                "$_"            => Some(self.eval_println(arg_forms)),
                "halt"          => Some(self.eval_halt(arg_forms)),
                "!!"            => Some(self.eval_halt(arg_forms)),
                "istype"        => Some(self.eval_istype(arg_forms)),
                "~?"            => Some(self.eval_istype(arg_forms)),
                "type"          => Some(self.eval_type(arg_forms)),
                "?~"            => Some(self.eval_type(arg_forms)),
                _ => None,
            },
            _ => None,
        };
    }

    pub fn parse_eval(&mut self, expr: String) -> QResult<Vec<QExp>> {
        let exps: Vec<QExp> = parse(&tokenize(expr)?)?;
        let evaled: Vec<QExp> = self.eval_forms(&exps)?;
        return Ok(evaled);
    }
}

