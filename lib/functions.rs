use crate::{
    qerr,
    qbool,
    qint,
    qfloat,
    qcomplex,
    qlist,
    lang::{
        QErr,
        QResult,
        QExp,
        QExpType,
        convert_type,
        parse_numbers,
    },
};

pub fn fn_add(args: &[QExp]) -> QResult<QExp> {
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

pub fn fn_sub(args: &[QExp]) -> QResult<QExp> {
    let nums: Vec<QExp> = parse_numbers(args)?;
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

pub fn fn_div(args: &[QExp]) -> QResult<QExp> {
    let nums: Vec<QExp> = parse_numbers(args)?;
    let ret_type: QExpType
        = nums.first().map(|x| x.exp_type())
        .unwrap_or(qfloat!());
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

