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
use crate::functions as fns;

#[derive(Debug)]
pub enum QErr {
    Reason(String),
}

#[macro_export]
macro_rules! qerr(
    ( $reason:literal ) => {
        QErr::Reason($reason.to_string())
    }
);

pub type QResult<T> = Result<T, QErr>;

#[derive(Clone)]
pub enum QExp {
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
pub enum QExpType {
    Bool = 0,
    Int = 1,
    Float = 2,
    Complex = 3,
    List = 4,
    Symbol = 5,
    Func = 6,
    Lambda = 7,
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
    pub fn exp_type(&self) -> QExpType {
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

    pub fn zero(ty: QExpType) -> QResult<QExp> {
        return match ty {
            qbool!() => Ok(qbool!(false)),
            qint!() => Ok(qint!(0)),
            qfloat!() => Ok(qfloat!(0.0)),
            qcomplex!() => Ok(qcomplex!(C64::zero())),
            qlist!() => Ok(qlist!(vec![])),
            qsymbol!() => Err(qerr!("zero value for symbols does not exist")),
            qfunc!() => Err(qerr!("zero value for functions does not exist")),
            qlambda!() => Err(qerr!("zero value for lambdas does not exist")),
        };
    }

    pub fn is_zero(&self) -> bool {
        return match self {
            qbool!(b) => *b,
            qint!(i) => *i == 0,
            qfloat!(f) => *f == 0.0,
            qcomplex!(c) => *c == C64::zero(),
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
            qsymbol!() => Err(qerr!("unitary value for symbols does not exist")),
            qfunc!() => Err(qerr!("unitary value for functions does not exist")),
            qlambda!() => Err(qerr!("unitary value for lambdas does not exist")),
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
            _ => Err(qerr!("incompatible types in gt")),
        };
    }

    pub fn le(&self, rhs: &QExp) -> QResult<bool> { Ok(!self.gt(rhs)?) }

    pub fn ge(&self, rhs: &QExp) -> QResult<bool> { Ok(!self.lt(rhs)?) }
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
            } else {
                Err(qerr!("could not convert type to complex"))
            }
        },
        qlist!() => Err(qerr!("could not convert type to list")),
        qsymbol!() => Err(qerr!("could not convert type to symbol")),
        qfunc!() => Err(qerr!("could not convert type to func")),
        qlambda!() => Err(qerr!("could not convert type to lambda")),
    };
}

pub fn convert_type_num(exp: &QExp, ty: QExpType) -> QResult<QExp> {
    return match ty {
        qbool!() | qint!() | qfloat!() | qcomplex!() => convert_type(exp, ty),
        _ => Err(qerr!("convert_type_num: encountered non-numerical type")),
    };
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

pub fn parse_numbers(args: &[QExp]) -> QResult<Vec<QExp>> {
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
        .collect();
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
        add_fn!(env, "+", fns::fn_add);
        // add_fn!(env, "add", fns::fn_add);
        add_fn!(env, "-", fns::fn_sub);
        // add_fn!(env, "sub", fns::fn_sub);
        add_fn!(env, "*", fns::fn_mul);
        // add_fn!(env, "mul", fns::fn_mul);
        add_fn!(env, "/", fns::fn_div);
        // add_fn!(env, "div", fns::fn_div);
        add_fn!(env, "=", fns::fn_eq);
        // add_fn!(env, "eq", fns::fn_eq);
        add_fn!(env, "!=", fns::fn_neq);
        // add_fn!(env, "neq", fns::fn_neq);
        add_fn!(env, "<", fns::fn_lt);
        // add_fn!(env, "lt", fns::fn_lt);
        add_fn!(env, "<=", fns::fn_leq);
        // add_fn!(env, "leq", fns::fn_leq);
        add_fn!(env, ">", fns::fn_gt);
        // add_fn!(env, "gt", fns::fn_gt);
        add_fn!(env, ">=", fns::fn_geq);
        // add_fn!(env, "geq", fns::fn_geq);
        add_fn!(env, "bool", fns::fn_bool);
        add_fn!(env, "int", fns::fn_int);
        add_fn!(env, "float", fns::fn_float);
        add_fn!(env, "complex", fns::fn_complex);
        add_fn!(env, "list", fns::fn_list);
        return QEnv { data: env, outer: None };
    }
}

impl<'a> QEnv<'a> {
    pub fn get_data(&self) -> &HashMap<String, QExp> { &self.data }

    pub fn get_outer(&'a self) -> Option<&'a QEnv<'a>> { (&self.outer).as_deref() }

    pub fn get(&self, k: &str) -> Option<QExp> {
        return if let Some(exp) = self.data.get(k) {
            Some(exp.clone())
        } else if let Some(outer) = &self.outer {
            outer.get(k)
        } else {
            None
        };
    }

    pub fn insert(&mut self, k: String, val: QExp) {
        self.data.insert(k, val);
    }

    pub fn eval(&mut self, exp: &QExp) -> QResult<QExp> {
        return match exp {
            qbool!(_) | qint!(_) | qfloat!(_) | qcomplex!(_)
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
            qfunc!(_) => Err(qerr!("encountered unexpected function")),
            qlambda!(_) => Err(qerr!("encountered unexpected lambda")),
        };
    }

    pub fn eval_forms(&mut self, arg_forms: &[QExp]) -> QResult<Vec<QExp>> {
        return arg_forms.iter().map(|x| self.eval(x)).collect();
    }

    pub fn eval_if_args(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        let test_form: &QExp
            = arg_forms.first().ok_or(qerr!("expected test form"))?;
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
            _ => Err(qerr!("invalid test form")),
        };
    }

    pub fn eval_def_args(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() > 2 {
            return Err(qerr!("'def' must have exactly two forms"));
        }
        let first_form: &QExp
            = arg_forms.first().ok_or(qerr!("missing first form"))?;
        let symbol: String = match first_form {
            qsymbol!(s) => Ok(s.clone()),
            _ => Err(qerr!("first form must be a symbol")),
        }?;

        let second_form: &QExp
            = arg_forms.get(1).ok_or(qerr!("missing second form"))?;
        let value: QExp = self.eval(second_form)?;
        self.insert(symbol, value.clone());
        return Ok(value);
    }

    pub fn eval_fn_args(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() > 2 {
            return Err(qerr!("'fn' must have exactly two forms"));
        }
        let params_exp: &QExp
            = arg_forms.first().ok_or(qerr!("missing function args"))?;
        let body_exp: &QExp
            = arg_forms.get(1).ok_or(qerr!("missing function body"))?;
        return Ok(
            qlambda!(QLambda {
                params_exp: Rc::new(params_exp.clone()),
                body_exp: Rc::new(body_exp.clone()),
            })
        );
    }

    pub fn eval_defn_args(&mut self, arg_forms: &[QExp]) -> QResult<QExp> {
        if arg_forms.len() > 3 {
            return Err(qerr!("'defn' must have exactly three forms"));
        }
        let first_form: &QExp
            = arg_forms.first().ok_or(qerr!("missing first form"))?;
        let symbol: String = match first_form {
            qsymbol!(s) => Ok(s.clone()),
            _ => Err(qerr!("first form must be a symbol")),
        }?;

        let params_exp: &QExp
            = arg_forms.get(1).ok_or(qerr!("missing function args"))?;
        let body_exp: &QExp
            = arg_forms.get(2).ok_or(qerr!("missing function body"))?;
        let function = QLambda {
            params_exp: Rc::new(params_exp.clone()),
            body_exp: Rc::new(body_exp.clone()),
        };

        self.insert(symbol, qlambda!(function.clone()));
        return Ok(qlambda!(function));
    }

    pub fn eval_builtin(&mut self, exp: &QExp, arg_forms: &[QExp])
        -> Option<QResult<QExp>>
    {
        return match exp {
            qsymbol!(s) => match s.as_ref() {
                "if" => Some(self.eval_if_args(arg_forms)),
                "def" => Some(self.eval_def_args(arg_forms)),
                "fn" => Some(self.eval_fn_args(arg_forms)),
                "defn" => Some(self.eval_defn_args(arg_forms)),
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

