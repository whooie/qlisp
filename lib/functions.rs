use std::cmp;
use crate::{
    qerr,
    qbool,
    qint,
    qfloat,
    qcomplex,
    qlist,
    qstr,
    lang::{
        QErr,
        QResult,
        QExp,
        QExpType,
        convert_type,
        convert_numbers_sametype,
    },
};

pub fn fn_add(args: &[QExp]) -> QResult<QExp> {
    let (nums, ret_type): (Vec<QExp>, QExpType)
        = convert_numbers_sametype(args)?;
    let mut acc = QExp::zero(ret_type)?;
    for x in nums.into_iter() {
        acc = acc.add(&x)?;
    }
    return Ok(acc);
}

pub fn fn_sub(args: &[QExp]) -> QResult<QExp> {
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

pub fn fn_mul(args: &[QExp]) -> QResult<QExp> {
    let (nums, ret_type): (Vec<QExp>, QExpType)
        = convert_numbers_sametype(args)?;
    let mut acc = QExp::one(ret_type)?;
    for x in nums.into_iter() {
        acc = acc.mul(&x)?;
    }
    return Ok(acc);
}

pub fn fn_div(args: &[QExp]) -> QResult<QExp> {
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

pub fn fn_eq(args: &[QExp]) -> QResult<QExp> {
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

pub fn fn_neq(args: &[QExp]) -> QResult<QExp> {
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

pub fn fn_lt(args: &[QExp]) -> QResult<QExp> {
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

pub fn fn_leq(args: &[QExp]) -> QResult<QExp> {
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

pub fn fn_gt(args: &[QExp]) -> QResult<QExp> {
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

pub fn fn_geq(args: &[QExp]) -> QResult<QExp> {
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

pub fn fn_join(args: &[QExp]) -> QResult<QExp> {
    let mut acc: Vec<QExp> = Vec::new();
    for exp in args.iter() {
        if let qlist!(l) = exp {
            acc.append(&mut l.iter().cloned().collect());
        } else if let qstr!(s) = exp {
            acc.append(
                &mut s.chars().map(|sk| qstr!(sk.to_string())).collect()
            );
        } else {
            return Err(qerr!("join: args must be lists or strs"));
        }
    }
    return Ok(qlist!(acc));
}

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

pub fn fn_zip(args: &[QExp]) -> QResult<QExp> {
    let mut acc: Vec<Vec<QExp>> = match args.first() {
        Some(qlist!(l)) => Ok(l.iter().map(|qk| vec![qk.clone()]).collect()),
        Some(qstr!(s)) => Ok(
            s.chars().map(|sk| vec![qstr!(sk.to_string())]).collect()
        ),
        Some(_) => Err(qerr!("zip: args must be strs or lists")),
        None => Err(qerr!("zip: expected at least one str or list")),
    }?;
    construct_zip(&mut acc, &args[1..])?;
    return Ok(qlist!(acc.into_iter().map(|zk| qlist!(zk)).collect()));
}

pub fn fn_bool(args: &[QExp]) -> QResult<QExp> {
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

pub fn fn_int(args: &[QExp]) -> QResult<QExp> {
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

pub fn fn_float(args: &[QExp]) -> QResult<QExp> {
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

pub fn fn_complex(args: &[QExp]) -> QResult<QExp> {
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

pub fn fn_list(args: &[QExp]) -> QResult<QExp> {
    return Ok(qlist!(args.iter().cloned().collect()));
}

pub fn fn_str(args: &[QExp]) -> QResult<QExp> {
    if args.len() == 1 {
        return convert_type(&args[0], qstr!());
    } else {
        let new: Vec<QExp>
            = args.iter()
            .map(|x| convert_type(x, qstr!()))
            .collect::<QResult<Vec<QExp>>>()?;
        return Ok(qlist!(new));
    }
}

