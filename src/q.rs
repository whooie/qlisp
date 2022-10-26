#![allow(dead_code, non_snake_case, non_upper_case_globals, unused_imports)]
#![allow(clippy::needless_return)]

use std::{
    cmp,
    collections::HashMap,
    fmt,
    io,
    num::ParseFloatError,
    rc::Rc,
    str::FromStr,
};
use num_complex::Complex64 as C64;
use num_traits::{
    One,
    Zero,
};

#[derive(Debug)]
enum QErr {
    Reason(String),
}

macro_rules! err(
    ( $reason:literal ) => {
        QErr::Reason($reason.to_string())
    }
);

type QResult<T> = Result<T, QErr>;

#[derive(Clone)]
enum QExp {
    Bool(bool),
    Int(i64),
    Float(f64),
    Complex(C64),
    List(Vec<QExp>),
    Symbol(String),
    Func(fn(&[QExp]) -> Result<QExp, QErr>),
    Lambda(QLambda),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum QExpType {
    Bool = 0,
    Int = 1,
    Float = 2,
    Complex = 3,
    List = 4,
    Symbol = 5,
    Func = 6,
    Lambda = 7,
}

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

impl From<usize> for QExpType {
    fn from(u: usize) -> QExpType {
        return match u {
            0 => qbool!(),
            1 => qint!(),
            2 => qfloat!(),
            3 => qcomplex!(),
            4 => qlist!(),
            5 => qsymbol!(),
            6 => qfunc!(),
            7 => qlambda!(),
            _ => qlist!(),
        };
    }
}

impl QExp {
    fn exp_type(&self) -> QExpType {
        return match self {
            qbool!(_) => qbool!(),
            qint!(_) => qint!(),
            qfloat!(_) => qfloat!(),
            qcomplex!(_) => qcomplex!(),
            qlist!(_) => qlist!(),
            qsymbol!(_) => qsymbol!(),
            qfunc!(_) => qfunc!(),
            qlambda!(_) => qlambda!(),
        };
    }

    fn zero(ty: QExpType) -> QResult<QExp> {
        return match ty {
            qbool!() => Ok(qbool!(false)),
            qint!() => Ok(qint!(0)),
            qfloat!() => Ok(qfloat!(0.0)),
            qcomplex!() => Ok(qcomplex!(C64::zero())),
            qlist!() => Ok(qlist!(vec![])),
            qsymbol!() => Err(err!("zero value for symbols does not exist")),
            qfunc!() => Err(err!("zero value for functions does not exist")),
            qlambda!() => Err(err!("zero value for lambdas does not exist")),
        };
    }

    fn is_zero(&self) -> bool {
        return match self {
            qbool!(b) => *b,
            qint!(i) => *i == 0,
            qfloat!(f) => *f == 0.0,
            qcomplex!(c) => *c == C64::zero(),
            _ => false,
        };
    }

    fn one(ty: QExpType) -> QResult<QExp> {
        return match ty {
            qbool!() => Ok(qbool!(true)),
            qint!() => Ok(qint!(1)),
            qfloat!() => Ok(qfloat!(1.0)),
            qcomplex!() => Ok(qcomplex!(C64::one())),
            qlist!() => Err(err!("unitary value for lists does not exist")),
            qsymbol!() => Err(err!("unitary value for symbols does not exist")),
            qfunc!() => Err(err!("unitary value for functions does not exist")),
            qlambda!() => Err(err!("unitary value for lambdas does not exist")),
        };
    }

    fn neg(&self) -> QResult<QExp> {
        return match self {
            qbool!(b) => Ok(qbool!(!b)),
            qint!(i) => Ok(qint!(-i)),
            qfloat!(f) => Ok(qfloat!(-f)),
            qcomplex!(c) => Ok(qcomplex!(-c)),
            _ => Err(err!("invalid type in neg")),
        };
    }

    fn add(&self, rhs: &QExp) -> QResult<QExp> {
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
            _ => Err(err!("invalid type in add")),
        };
    }

    fn sub(&self, rhs: &QExp) -> QResult<QExp> {
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
            _ => Err(err!("invalid type in sub")),
        };
    }

    fn mul(&self, rhs: &QExp) -> QResult<QExp> {
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
            _ => Err(err!("invalid type in mul")),
        };
    }

    fn div(&self, rhs: &QExp) -> QResult<QExp> {
        (!rhs.is_zero()).then(|| ()).ok_or(err!("encountered zero in div"))?;
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
            _ => Err(err!("invalid type in div")),
        };
    }

    fn eq(&self, rhs: &QExp) -> bool {
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
            _ => false,
        };
    }

    fn neq(&self, rhs: &QExp) -> bool { !self.eq(rhs) }

    fn lt(&self, rhs: &QExp) -> QResult<bool> {
        let ty: QExpType = cmp::max(self.exp_type(), rhs.exp_type());
        let lhs_c: QExp = convert_type(self, ty)?;
        let rhs_c: QExp = convert_type(rhs,  ty)?;
        return match (lhs_c, rhs_c) {
            (qint!(l), qint!(r)) => Ok(l < r),
            (qfloat!(l), qfloat!(r)) => Ok(l < r),
            _ => Err(err!("incompatible types in lt")),
        };
    }

    fn gt(&self, rhs: &QExp) -> QResult<bool> {
        let ty: QExpType = cmp::max(self.exp_type(), rhs.exp_type());
        let lhs_c: QExp = convert_type(self, ty)?;
        let rhs_c: QExp = convert_type(rhs,  ty)?;
        return match (lhs_c, rhs_c) {
            (qint!(l), qint!(r)) => Ok(l > r),
            (qfloat!(l), qfloat!(r)) => Ok(l > r),
            _ => Err(err!("incompatible types in gt")),
        };
    }

    fn le(&self, rhs: &QExp) -> QResult<bool> { Ok(!self.gt(rhs)?) }

    fn ge(&self, rhs: &QExp) -> QResult<bool> { Ok(!self.lt(rhs)?) }
}

fn convert_type(exp: &QExp, ty: QExpType) -> QResult<QExp> {
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
            } else {
                Err(err!("could not convert type to bool"))
            }
        },
        qint!() => {
            if let qbool!(b) = exp {
                Ok(qint!(if *b { 1 } else { 0 }))
            } else if let qint!(i) = exp {
                Ok(qint!(*i))
            } else if let qfloat!(f) = exp {
                Ok(qint!(*f as i64))
            } else {
                Err(err!("could not convert type to int"))
            }
        },
        qfloat!() => {
            if let qbool!(b) = exp {
                Ok(qfloat!(if *b { 1.0 } else { 0.0 }))
            } else if let qint!(i) = exp {
                Ok(qfloat!(*i as f64))
            } else if let qfloat!(f) = exp {
                Ok(qfloat!(*f))
            } else {
                Err(err!("could not convert type to float"))
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
            } else {
                Err(err!("could not convert type to complex"))
            }
        },
        qlist!() => Err(err!("could not convert type to list")),
        qsymbol!() => Err(err!("could not convert type to symbol")),
        qfunc!() => Err(err!("could not convert type to func")),
        qlambda!() => Err(err!("could not convert type to lambda")),
    };
}

fn convert_type_num(exp: &QExp, ty: QExpType) -> QResult<QExp> {
    return match ty {
        qbool!() | qint!() | qfloat!() | qcomplex!() => convert_type(exp, ty),
        _ => Err(err!("convert_type_num: encountered non-numerical type")),
    };
}

#[derive(Clone)]
struct QLambda {
    params_exp: Rc<QExp>,
    body_exp: Rc<QExp>,
}

impl QLambda {
    fn parse_symbols(form: Rc<QExp>) -> QResult<Vec<String>> {
        let list: Vec<QExp> = match form.as_ref() {
            qlist!(s) => Ok(s.clone()),
            _ => Err(err!("expected args form to be a list")),
        }?;
        return list.iter()
            .map(|x| {
                match x {
                    qsymbol!(s) => Ok(s.clone()),
                    _ => Err(err!("arguments must be a list of symbols")),
                }
            })
            .collect();
    }

    fn env<'a>(
        &self,
        arg_forms: &[QExp],
        outer_env: &'a mut QEnv,
    ) -> QResult<QEnv<'a>> {
        let symbols: Vec<String>
            = Self::parse_symbols(self.params_exp.clone())?;
        if symbols.len() != arg_forms.len() {
            return Err(QErr::Reason(
                format!(
                    "expected {} arguments but got {}",
                    symbols.len(),
                    arg_forms.len()
                )
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

impl fmt::Display for QExp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return match self {
            qbool!(x) => x.fmt(f),
            qint!(x) => x.fmt(f),
            qfloat!(x) => x.fmt(f),
            qcomplex!(x) => x.fmt(f),
            qlist!(x) => {
                write!(f, "( ")?;
                for xk in x.iter() {
                    xk.fmt(f)?;
                    write!(f, ", ")?;
                }
                write!(f, ")")
            },
            qsymbol!(x) => x.fmt(f),
            qfunc!(_) => write!(f, "function {{ ... }}"),
            qlambda!(_) => write!(f, "lambda {{ ... }}"),
        };
    }
}

fn tokenize(expr: String) -> Vec<String> {
    return expr
        .replace("(", " ( ")
        .replace(")", " ) ")
        .split_whitespace()
        .map(|x| x.to_string())
        .collect();
}

fn parse<'a>(tokens: &'a [String]) -> QResult<(QExp, &'a [String])> {
    let (token, rest)
        = tokens.split_first()
        .ok_or(err!("missing closing ')'"))?;
    return match &token[..] {
        "(" => read_seq(rest),
        ")" => Err(err!("unexpected ')'")),
        _ => Ok((parse_atom(token), rest)),
    };
}

fn read_seq<'a>(tokens: &'a [String]) -> QResult<(QExp, &'a [String])> {
    let mut res: Vec<QExp> = Vec::new();
    let mut xs = tokens;
    loop {
        let (next_token, rest)
            = xs.split_first()
            .ok_or(err!("missing closing ')'"))?;
        if next_token == ")" {
            return Ok((QExp::List(res), rest));
        }
        let (exp, new_xs) = parse(&xs)?;
        res.push(exp);
        xs = new_xs;
    }
}

fn parse_atom(token: &str) -> QExp {
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

fn parse_numbers(args: &[QExp]) -> QResult<Vec<QExp>> {
    let exp_type: QExpType
        = cmp::max(
            args.iter().map(|qexp| qexp.exp_type()).max()
            .ok_or(err!("expected at least one item"))?,
            qint!(),
        );
    if exp_type > qcomplex!() {
        return Err(err!("encountered non-numerical value"));
    }
    return
        args.iter()
        .map(|x| convert_type_num(x, exp_type))
        .collect();
}

#[derive(Clone)]
struct QEnv<'a> {
    data: HashMap<String, QExp>,
    outer: Option<&'a QEnv<'a>>,
}

fn fn_add(args: &[QExp]) -> QResult<QExp> {
    let nums: Vec<QExp> = parse_numbers(args)?;
    let ret_type: QExpType
        = nums.first().map(|x| x.exp_type())
        .unwrap_or(qint!());
    let mut acc = QExp::zero(ret_type)?;
    for x in nums.into_iter() {
        acc = acc.add(&x)?;
    }
    return Ok(acc);
}

fn fn_sub(args: &[QExp]) -> QResult<QExp> {
    let nums: Vec<QExp> = parse_numbers(args)?;
    if nums.len() == 1 {
        return Ok(nums.first().unwrap().neg()?);
    }
    let mut acc: QExp
        = nums.first()
        .ok_or(err!("expected at least one number"))?
        .clone();
    for x in nums[1..].iter() {
        acc = acc.sub(x)?;
    }
    return Ok(acc);
}

fn fn_mul(args: &[QExp]) -> QResult<QExp> {
    let nums: Vec<QExp> = parse_numbers(args)?;
    let ret_type: QExpType
        = nums.first().map(|x| x.exp_type())
        .unwrap_or(qint!());
    let mut acc = QExp::one(ret_type)?;
    for x in nums.into_iter() {
        acc = acc.mul(&x)?;
    }
    return Ok(acc);
}

fn fn_div(args: &[QExp]) -> QResult<QExp> {
    let nums: Vec<QExp> = parse_numbers(args)?;
    let ret_type: QExpType
        = nums.first().map(|x| x.exp_type())
        .unwrap_or(qfloat!());
    if nums.len() == 1 {
        return Ok(QExp::one(ret_type)?.div(nums.first().unwrap())?);
    }
    let mut acc: QExp
        = nums.first()
        .ok_or(err!("expected at least one number"))?
        .clone();
    for x in nums[1..].iter() {
        acc = acc.div(x)?;
    }
    return Ok(acc);
}

fn fn_eq(args: &[QExp]) -> QResult<QExp> {
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

fn fn_neq(args: &[QExp]) -> QResult<QExp> {
    let first: &QExp;
    if let Some(x) = args.first() {
        first = x;
    } else {
        return Ok(qbool!(true));
    }
    for x in args[1..].iter() {
        if !first.neq(x) {
            return Ok(qbool!(false));
        }
    }
    return Ok(qbool!(true));
}

fn fn_lt(args: &[QExp]) -> QResult<QExp> {
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

fn fn_leq(args: &[QExp]) -> QResult<QExp> {
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

fn fn_gt(args: &[QExp]) -> QResult<QExp> {
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

fn fn_geq(args: &[QExp]) -> QResult<QExp> {
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

fn fn_bool(args: &[QExp]) -> QResult<QExp> {
    if args.len() == 1 {
        return convert_type(&args[0], qbool!());
    } else {
        let new: Vec<QExp>
            = args.iter()
            .map(|x| convert_type(x, qbool!()))
            .collect::<QResult<Vec<QExp>>>()?;
        return Ok(qlist!(new));
    }
}

fn fn_int(args: &[QExp]) -> QResult<QExp> {
    if args.len() == 1 {
        return convert_type(&args[0], qint!());
    } else {
        let new: Vec<QExp>
            = args.iter()
            .map(|x| convert_type(x, qint!()))
            .collect::<QResult<Vec<QExp>>>()?;
        return Ok(qlist!(new));
    }
}

fn fn_float(args: &[QExp]) -> QResult<QExp> {
    if args.len() == 1 {
        return convert_type(&args[0], qfloat!());
    } else {
        let new: Vec<QExp>
            = args.iter()
            .map(|x| convert_type(x, qfloat!()))
            .collect::<QResult<Vec<QExp>>>()?;
        return Ok(qlist!(new));
    }
}

fn fn_complex(args: &[QExp]) -> QResult<QExp> {
    if args.len() == 1 {
        return convert_type(&args[0], qcomplex!());
    } else {
        let new: Vec<QExp>
            = args.iter()
            .map(|x| convert_type(x, qcomplex!()))
            .collect::<QResult<Vec<QExp>>>()?;
        return Ok(qlist!(new));
    }
}

macro_rules! add_fn(
    ( $env:ident, $name:literal, $fn:ident ) => {
        $env.insert($name.to_string(), QExp::Func($fn));
    }
);

impl<'a> Default for QEnv<'a> {
    fn default() -> QEnv<'a> {
        let mut env: HashMap<String, QExp> = HashMap::new();
        add_fn!(env, "+", fn_add);
        // add_fn!(env, "add", fn_add);
        add_fn!(env, "-", fn_sub);
        // add_fn!(env, "sub", fn_sub);
        add_fn!(env, "*", fn_mul);
        // add_fn!(env, "mul", fn_mul);
        add_fn!(env, "/", fn_div);
        // add_fn!(env, "div", fn_div);
        add_fn!(env, "=", fn_eq);
        // add_fn!(env, "eq", fn_eq);
        add_fn!(env, "!=", fn_neq);
        // add_fn!(env, "neq", fn_neq);
        add_fn!(env, "<", fn_lt);
        // add_fn!(env, "lt", fn_lt);
        add_fn!(env, "<=", fn_leq);
        // add_fn!(env, "leq", fn_leq);
        add_fn!(env, ">", fn_gt);
        // add_fn!(env, "gt", fn_gt);
        add_fn!(env, ">=", fn_geq);
        // add_fn!(env, "geq", fn_geq);
        add_fn!(env, "bool", fn_bool);
        add_fn!(env, "int", fn_int);
        add_fn!(env, "float", fn_float);
        add_fn!(env, "complex", fn_complex);
        return QEnv { data: env, outer: None };
    }
}

impl<'a> QEnv<'a> {
    fn get(&self, k: &str) -> Option<QExp> {
        return if let Some(exp) = self.data.get(k) {
            Some(exp.clone())
        } else {
            if let Some(outer) = &self.outer {
                outer.get(k)
            } else {
                None
            }
        };
    }

    fn insert(&mut self, k: String, val: QExp) {
        self.data.insert(k, val);
    }

    fn eval(&mut self, exp: &QExp) -> QResult<QExp> {
        return match exp {
            qbool!(_) | qint!(_) | qfloat!(_) | qcomplex!(_)
                => Ok(exp.clone()),
            qlist!(list) => {
                let first_form: &QExp
                    = list.first()
                    .ok_or(err!("expected a non-empty list"))?;
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
                                QErr::Reason(
                                    format!("could not eval symbol {}", s)
                                )
                            ),
                            _ => Ok(qlist!(self.eval_forms(list)?)),
                        }
                    }
                }
            },
            qsymbol!(k)
                => self.get(k)
                    .ok_or(QErr::Reason(format!("symbol '{}' is undefined", k))),
            qfunc!(_) => Err(err!("encountered unexpected function")),
            qlambda!(_) => Err(err!("encountered unexpected lambda")),
        };
    }

    fn eval_forms(&mut self, arg_forms: &[QExp]) -> QResult<Vec<QExp>> {
        return arg_forms.iter().map(|x| self.eval(x)).collect();
    }

    fn eval_if_args(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        let test_form: &QExp
            = arg_forms.first().ok_or(err!("expected test form"))?;
        let test_eval: QExp = self.eval(test_form)?;
        return match test_eval {
            qbool!(b) => {
                let form_idx: usize = if b { 1 } else { 2 };
                let res_form: &QExp
                    = arg_forms.get(form_idx)
                    .ok_or(
                        QErr::Reason(
                            format!("missing condition case {}", form_idx)
                        )
                    )?;
                self.eval(res_form)
            },
            _ => Err(err!("invalid test form")),
        };
    }

    fn eval_def_args(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() > 2 {
            return Err(err!("'def' must have exactly two forms"));
        }
        let first_form: &QExp
            = arg_forms.first().ok_or(err!("missing first form"))?;
        let symbol: String = match first_form {
            qsymbol!(s) => Ok(s.clone()),
            _ => Err(err!("first form must be a symbol")),
        }?;

        let second_form: &QExp
            = arg_forms.get(1).ok_or(err!("missing second form"))?;
        let value: QExp = self.eval(second_form)?;
        self.insert(symbol, value.clone());
        return Ok(value);
    }

    fn eval_lambda_args(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() > 2 {
            return Err(err!("'lambda' must have exactly two forms"));
        }
        let params_exp: &QExp
            = arg_forms.first().ok_or(err!("missing function args"))?;
        let body_exp: &QExp
            = arg_forms.get(1).ok_or(err!("missing function body"))?;
        return Ok(
            qlambda!(QLambda {
                params_exp: Rc::new(params_exp.clone()),
                body_exp: Rc::new(body_exp.clone()),
            })
        );
    }

    fn eval_defn_args(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() > 3 {
            return Err(err!("'defn' must have exactly three forms"));
        }
        let first_form: &QExp
            = arg_forms.first().ok_or(err!("missing first form"))?;
        let symbol: String = match first_form {
            qsymbol!(s) => Ok(s.clone()),
            _ => Err(err!("first form must be a symbol")),
        }?;

        let params_exp: &QExp
            = arg_forms.get(1).ok_or(err!("missing function args"))?;
        let body_exp: &QExp
            = arg_forms.get(2).ok_or(err!("missing function body"))?;
        let function = QLambda {
            params_exp: Rc::new(params_exp.clone()),
            body_exp: Rc::new(body_exp.clone()),
        };

        self.insert(symbol, qlambda!(function.clone()));
        return Ok(qlambda!(function));
    }

    fn eval_builtin(&mut self, exp: &QExp, arg_forms: &[QExp])
        -> Option<QResult<QExp>>
    {
        return match exp {
            qsymbol!(s) => match s.as_ref() {
                "if" => Some(self.eval_if_args(arg_forms)),
                "def" => Some(self.eval_def_args(arg_forms)),
                "fn" => Some(self.eval_lambda_args(arg_forms)),
                "defn" => Some(self.eval_defn_args(arg_forms)),
                _ => None,
            },
            _ => None,
        };
    }

    fn parse_eval(&mut self, expr: String) -> QResult<QExp> {
        let (parsed_exp, _): (QExp, _) = parse(&tokenize(expr))?;
        let evaled: QExp = self.eval(&parsed_exp)?;
        return Ok(evaled);
    }
}

fn slurp_expr() -> String {
    let mut expr = String::new();
    io::stdin().read_line(&mut expr).expect("failed to read line");
    return expr;
}

macro_rules! print_flush {
    ( $fmt:literal, $( $val:expr ),* ) => {
        print!($fmt, $( $val, )*);
        std::io::Write::flush(&mut std::io::stdout()).unwrap();
    }
}

macro_rules! println_flush {
    ( $fmt:literal, $( $val:expr ),* ) => {
        println!($fmt, $( $val, )*);
        std::io::Write::flush(&mut std::io::stdout()).unwrap();
    }
}

fn main() {
    let env = &mut QEnv::default();
    loop {
        print_flush!("{}>> ", "q");
        let expr: String = slurp_expr();
        if expr.trim() == "quit" {
            break;
        }
        match env.parse_eval(expr) {
            Ok(res) => { println_flush!("{}", res); },
            Err(e) => match e {
                QErr::Reason(msg) => { println_flush!("Error: {}", msg); },
            },
        }
    }
}

