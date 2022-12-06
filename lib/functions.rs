use std::{
    cmp,
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
        PROTECTED,
        QErr,
        QResult,
        QExp,
        QExpType,
        QLambda,
        QEnv,
        Indexable,
        convert_type,
        convert_type_num,
        convert_numbers_sametype,
        typecast,
        FormatX,
    },
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
            } else {
                Ok(s.clone())
            }
        },
        _ => Err(qerr!("def: first arg must be a symbol")),
    }?;
    let val: QExp = env.eval(args.get(1).unwrap())?;
    env.insert(symbol, val.clone());
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
                            (qsymbol!(_), _)
                                => Ok((sk.clone(), vk.clone())),
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
                env.insert(s, v.clone());
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
    fn subs_from_env(env: &mut QEnv, body_exp: &QExp) -> QExp {
        return match body_exp {
            qsymbol!(s) => env.get_cloned(s).unwrap_or(body_exp.clone()),
            qlist!(l) => qlist!(
                l.iter().map(|qk| subs_from_env(env, qk)).collect()
            ),
            q => q.clone(),
        };
    }

    if args.len() != 2 {
        return Err(qerr_fmt!(
            "fn: expected 2 args but got {}", args.len()));
    }
    let params_exp: &QExp = match args.get(0).unwrap() {
        qlist!(l) => {
            if l.iter().all(|qk| qk.exp_type() == qsymbol!()) {
                Ok(args.get(0).unwrap())
            } else {
                Err(qerr!("fn: function args must be a list of symbols"))
            }
        },
        _ => Err(qerr!("fn: function args must be a list of symbols")),
    }?;
    let body_exp: QExp
        = subs_from_env(
            env,
            args.get(1).ok_or(qerr!("fn: missing function body"))?
        );
    return Ok(
        qlambda!(QLambda {
            params_exp: Rc::new(params_exp.clone()),
            body_exp: Rc::new(body_exp.clone()),
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
            } else {
                Ok(s.clone())
            }
        },
        _ => Err(qerr!("defn: first arg must be a symbol")),
    }?;
    let params_exp: &QExp = match args.get(1).unwrap() {
        qlist!(l) => {
            if l.iter().any(|qk| qk.exp_type() != qsymbol!()) {
                Err(qerr!("defn: function args must be a list of symbols"))
            } else {
                Ok(args.get(1).unwrap())
            }
        },
        _ => Err(qerr!("defn: function args must be a list of symbols")),
    }?;
    let body_exp: &QExp
        = args.get(2).ok_or(qerr!("defn: missing function body"))?;
    let function = QLambda {
        params_exp: Rc::new(params_exp.clone()),
        body_exp: Rc::new(body_exp.clone()),
    };
    env.insert(symbol, qlambda!(function.clone()));
    return Ok(qlambda!(function));
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

/*
 * systems
 */

pub fn fn_format(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() < 1 {
        return Err(qerr!("format: missing format string"));
    }
    let fmtstr: String = match env.eval(args.get(0).unwrap())? {
        qstr!(s) => Ok(s.clone()),
        _ => Err(qerr!("format: first arg must be a format string")),
    }?;
    let vals: Vec<QExp> = env.eval_multi(&args[1..])?;
    let formatted: String
        = fmtstr.formatx(&vals)
        .map_err(|e| e.prepend_source("format"))?;
    return Ok(qstr!(formatted));
}

pub fn fn_print(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() < 1 {
        return Err(qerr!("print: missing format string"));
    }
    let fmtstr: String = match env.eval(args.get(0).unwrap())? {
        qstr!(s) => Ok(s.clone()),
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
    if args.len() < 1 {
        return Err(qerr!("println: missing format string"));
    }
    let fmtstr: String = match env.eval(args.get(0).unwrap())? {
        qstr!(s) => Ok(s.clone()),
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
    if args.len() < 1 {
        return Err(qerr!("halt"));
    }
    let fmtstr: String = match env.eval(args.get(0).unwrap())? {
        qstr!(s) => Ok(s.clone()),
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
    if args.len() < 1 {
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
        for x in nums.into_iter() {
            acc = acc.add(&x)?;
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
            .ok_or(qerr!("expected at least one number"))?
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
        for x in nums.into_iter() {
            acc = acc.mul(&x)?;
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
            .ok_or(qerr!("expected at least one number"))?
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
            .ok_or(qerr!("expected at least one number"))?
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
            if !first.neq(x) {
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
    return idxable.get(idx.abs() as usize)
        .ok_or(qerr_fmt!(
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
    return idxable.slice(beg.abs() as usize, end.abs() as usize, 1)
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
    return idxable.slice_inc(beg.abs() as usize, end.abs() as usize, 1)
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
        beg.abs() as usize,
        end.abs() as usize,
        step.abs() as usize
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
        beg.abs() as usize,
        end.abs() as usize,
        step.abs() as usize
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
    return idxable.slice(0, idxable.len(), step.abs() as usize)
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
                        acc.append(&mut l.iter().cloned().collect());
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
                        acc.append(&mut l.iter().cloned().collect());
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

macro_rules! element_wise_fn(
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

element_wise_fn!(fn_neg,    do_neg,     neg  );
element_wise_fn!(fn_recip,  do_recip,   recip);
element_wise_fn!(fn_abs,    do_abs,     abs  );
element_wise_fn!(fn_sqrt,   do_sqrt,    sqrt );
element_wise_fn!(fn_cbrt,   do_cbrt,    cbrt );
element_wise_fn!(fn_exp,    do_exp,     exp  );
element_wise_fn!(fn_floor,  do_floor,   floor);
element_wise_fn!(fn_ceil,   do_ceil,    ceil );
element_wise_fn!(fn_round,  do_round,   round);
element_wise_fn!(fn_ln,     do_ln,      ln   );
element_wise_fn!(fn_sin,    do_sin,     sin  );
element_wise_fn!(fn_cos,    do_cos,     cos  );
element_wise_fn!(fn_tan,    do_tan,     tan  );
element_wise_fn!(fn_asin,   do_asin,    asin );
element_wise_fn!(fn_acos,   do_acos,    acos );
element_wise_fn!(fn_atan,   do_atan,    atan );

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

element_wise_fn!(fn_sinh,   do_sinh,    sinh );
element_wise_fn!(fn_cosh,   do_cosh,    cosh );
element_wise_fn!(fn_tanh,   do_tanh,    tanh );
element_wise_fn!(fn_asinh,  do_asinh,   asinh);
element_wise_fn!(fn_acosh,  do_acosh,   acosh);
element_wise_fn!(fn_atanh,  do_atanh,   atanh);
element_wise_fn!(fn_arg,    do_arg,     arg  );
element_wise_fn!(fn_cis,    do_cis,     cis  );
element_wise_fn!(fn_conj,   do_conj,    conj );
element_wise_fn!(fn_real,   do_real,    real );
element_wise_fn!(fn_imag,   do_imag,    imag );

/*
 * parameterized element-wise math
 */

pub fn fn_mod(env: &mut QEnv, args: &[QExp]) -> QResult<QExp> {
    if args.len() != 2 {
        return Err(qerr!("mod must have exactly two arguments"));
    }
    let mut M: QExp = env.eval(args.get(0).unwrap())?;
    let (nums, mut ty): (QExp, QExpType)
        = match env.eval(args.get(1).unwrap())? {
            qbool!(b) => Ok((qbool!(b), qbool!())),
            qint!(i) => Ok((qint!(i), qint!())),
            qfloat!(f) => Ok((qfloat!(f), qfloat!())),
            qlist!(l) => {
                convert_numbers_sametype(&l)
                    .map(|(n, t)| (qlist!(n), t))
                    .map_err(|e| e.prepend_source("mod"))
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
                .map_err(|e| e.prepend_source("mod"))
        },
        qlist!(l) => {
            Ok(qlist!(
                l.iter()
                .map(|qk| convert_type_num(qk, ty))
                .collect::<QResult<Vec<QExp>>>()
                .map_err(|e| e.prepend_source("mod"))?
                .iter()
                .map(|qk| qk.modulo(&M))
                .collect::<QResult<Vec<QExp>>>()
                .map_err(|e| e.prepend_source("mod"))?
            ))
        },
        _ => Err(qerr!("unexpected state")),
    };
}

// pub fn eval_log(&mut self, args: &[QExp]) -> QResult<QExp>;
// pub fn eval_pow(&mut self, args: &[QExp]) -> QResult<QExp>;

/*
 * list -> list math
 */

// pub fn eval_convolve(&mut self, args: &[QExp]) -> QResult<QExp>;
// pub fn eval_histogram(&mut self, args: &[QExp]) -> QResult<QExp>;
// pub fn eval_histogram_prob(&mut self, args: &[QExp]) -> QResult<QExp>;
// pub fn eval_fft(&mut self, args: &[QExp]) -> QResult<QExp>;
// pub fn eval_ifft(&mut self, args: &[QExp]) -> QResult<QExp>;
// pub fn eval_findpeaks(&mut self, args: &[QExp]) -> QResult<QExp>;
// pub fn eval_covariance(&mut self, args: &[QExp]) -> QResult<QExp>;
// pub fn eval_correlation(&mut self, args: &[QExp]) -> QResult<QExp>;

/*
 * list -> value math
 */

// pub fn eval_mean(&mut self, args: &[QExp]) -> QResult<QExp>;
// pub fn eval_variance(&mut self, args: &[QExp]) -> QResult<QExp>;
// pub fn eval_stddev(&mut self, args: &[QExp]) -> QResult<QExp>;

/*
 * parameterized list -> value math
 */

// pub fn eval_pnorm(&mut self, args: &[QExp]) -> QResult<QExp>;
// pub fn eval_moment(&mut self, args: &[QExp]) -> QResult<QExp>;

/*
 * special-arg math
 */

// pub fn eval_sample(&mut self, args: &[QExp]) -> QResult<QExp>;

