use std::borrow::Cow;
use reedline as reed;
use crate::{
    qerr,
    qbool,
    qint,
    qfloat,
    qcomplex,
    qlist,
    qsymbol,
    qfunc,
    qlambda,
    lang::{
        QResult,
        QErr,
        QExp,
        QEnv,
        tokenize,
        parse,
    },
};

macro_rules! println_flush {
    ( $fmt:literal, $( $val:expr ),* ) => {
        println!($fmt, $( $val, )*);
        std::io::Write::flush(&mut std::io::stdout()).unwrap();
    }
}

enum ReplOut {
    Print(QExp),
    NoPrint(QExp),
}

impl ReplOut {
    fn unpack(self) -> QExp {
        return match self {
            ReplOut::Print(exp) => {
                println_flush!("{}", exp);
                exp
            },
            ReplOut::NoPrint(exp) => exp,
        };
    }

    fn unpack_unchecked(self) -> QExp {
        return match self {
            ReplOut::Print(exp) | ReplOut::NoPrint(exp) => exp
        };
    }

    fn print(self) -> ReplOut {
        return match self {
            ReplOut::Print(_) => self,
            ReplOut::NoPrint(exp) => ReplOut::Print(exp),
        };
    }

    fn noprint(self) -> ReplOut {
        return match self {
            ReplOut::Print(exp) => ReplOut::NoPrint(exp),
            ReplOut::NoPrint(_) => self,
        };
    }
}

trait ReplEnv {
    fn repl_eval(&mut self, exp: &QExp) -> QResult<ReplOut>;

    fn repl_eval_forms(&mut self, arg_forms: &[QExp]) -> QResult<Vec<ReplOut>>;

    fn repl_eval_if_args(&mut self, arg_forms: &[QExp]) -> QResult<ReplOut>;

    fn repl_eval_def_args(&mut self, arg_forms: &[QExp]) -> QResult<ReplOut>;

    fn repl_eval_fn_args(&mut self, arg_forms: &[QExp]) -> QResult<ReplOut>;

    fn repl_eval_defn_args(&mut self, arg_forms: &[QExp]) -> QResult<ReplOut>;

    fn repl_eval_builtin(&mut self, exp: &QExp, arg_forms: &[QExp])
        -> Option<QResult<ReplOut>>;

    fn repl_parse_eval(&mut self, expr: String) -> QResult<ReplOut>;
}

impl<'a> ReplEnv for QEnv<'a> {
    fn repl_eval(&mut self, exp: &QExp) -> QResult<ReplOut> {
        return match exp {
            qbool!(_) | qint!(_) | qfloat!(_) | qcomplex!(_)
                => Ok(ReplOut::Print(exp.clone())),
            qlist!(list) => {
                let first_form: &QExp = match list.first() {
                    Some(exp) => exp,
                    None => { return Ok(ReplOut::Print(exp.clone())); },
                };
                let arg_forms: &[QExp] = &list[1..];
                match self.repl_eval_builtin(first_form, arg_forms) {
                    Some(res) => res,
                    None => {
                        let first_eval: QExp = self.eval(first_form)?;
                        match first_eval {
                            qfunc!(f) => {
                                f(&self.eval_forms(arg_forms)?)
                                    .map(|qexp| ReplOut::Print(qexp))
                            },
                            qlambda!(ll) => {
                                let mut ll_env: QEnv
                                    = ll.env(arg_forms, self)?;
                                ll_env.repl_eval(ll.get_body_exp())
                            },
                            qsymbol!(s) => Err(
                                QErr::Reason(
                                    format!("could not eval symbol {}", s)
                                )
                            ),
                            _ => Ok(
                                ReplOut::Print(qlist!(self.eval_forms(list)?))
                            ),
                        }
                    }
                }
            },
            qsymbol!(k)
                => self.get(k)
                .ok_or(QErr::Reason(format!("symbol '{}' is undefined", k)))
                .map(|qexp| ReplOut::Print(qexp)),
            qfunc!(_) => Err(qerr!("encountered unexpected function")),
            qlambda!(_) => Err(qerr!("encountered unexpected lambda")),
        };
    }

    // fn repl_eval(&mut self, exp: &QExp) -> QResult<ReplOut> {
    //     return self.eval(exp)
    //         .map(|qexp| ReplOut::Print(qexp));
    // }

    fn repl_eval_forms(&mut self, arg_forms: &[QExp]) -> QResult<Vec<ReplOut>> {
        return self.eval_forms(arg_forms)
            .map(|qexps| {
                qexps.into_iter().map(|qexp| ReplOut::Print(qexp)).collect()
            });
    }

    fn repl_eval_if_args(&mut self, arg_forms: &[QExp]) -> QResult<ReplOut> {
        return self.eval_if_args(arg_forms)
            .map(|qexp| ReplOut::Print(qexp));
    }

    fn repl_eval_def_args(&mut self, arg_forms: &[QExp]) -> QResult<ReplOut> {
        return self.eval_def_args(arg_forms)
            .map(|qexp| ReplOut::NoPrint(qexp));
    }

    fn repl_eval_fn_args(&mut self, arg_forms: &[QExp]) -> QResult<ReplOut> {
        return self.eval_fn_args(arg_forms)
            .map(|qexp| ReplOut::Print(qexp));
    }

    fn repl_eval_defn_args(&mut self, arg_forms: &[QExp]) -> QResult<ReplOut> {
        return self.eval_defn_args(arg_forms)
            .map(|qexp| ReplOut::NoPrint(qexp));
    }

    fn repl_eval_builtin(&mut self, exp: &QExp, arg_forms: &[QExp])
        -> Option<QResult<ReplOut>>
    {
        return match exp {
            qsymbol!(s) => match s.as_ref() {
                "if" => Some(self.repl_eval_if_args(arg_forms)),
                "def" => Some(self.repl_eval_def_args(arg_forms)),
                "fn" => Some(self.repl_eval_fn_args(arg_forms)),
                "defn" => Some(self.repl_eval_defn_args(arg_forms)),
                _ => None,
            },
            _ => None,
        };
    }

    fn repl_parse_eval(&mut self, expr: String) -> QResult<ReplOut> {
        let (parsed_exp, _): (QExp, _) = parse(&tokenize(expr))?;
        let evaled: ReplOut = self.repl_eval(&parsed_exp)?;
        return Ok(evaled);
    }
}

#[derive(Clone)]
pub struct QPrompt { }

impl reed::Prompt for QPrompt {
    fn render_prompt_left(&self) -> Cow<str> { Cow::from("q") }

    fn render_prompt_right(&self) -> Cow<str> { Cow::from("") }

    fn render_prompt_indicator(&self, prompt_mode: reed::PromptEditMode)
        -> Cow<str>
    {
        return match prompt_mode {
            reed::PromptEditMode::Default => Cow::from(">> "),
            reed::PromptEditMode::Emacs => Cow::from(">> "),
            reed::PromptEditMode::Vi(vimode) => match vimode {
                    reed::PromptViMode::Normal => Cow::from("|> "),
                    reed::PromptViMode::Insert => Cow::from(">> "),
            },
            reed::PromptEditMode::Custom(_) => Cow::from("c> "),
        };
    }

    fn render_prompt_multiline_indicator(&self) -> Cow<str> {
        return Cow::from("... ");
    }

    fn render_prompt_history_search_indicator(
        &self,
        _history_search: reed::PromptHistorySearch,
    ) -> Cow<str>
    {
        return Cow::from("history: ");
    }
}

pub fn run_repl() {
    let mut env = QEnv::default();
    let mut line_editor = reed::Reedline::create();
    let prompt = QPrompt { };
    loop {
        match line_editor.read_line(&prompt) {
            Ok(reed::Signal::Success(s)) => {
                let (expr, _comment): (String, String)
                    = match s.split_once('#') {
                        Some((e, c)) => (e.to_string(), c.to_string()),
                        None => (s, "".to_string()),
                    };
                if ["quit", "exit"].contains(&expr.trim()) {
                    break;
                }
                match env.repl_parse_eval(expr) {
                    Ok(res) => { res.unpack(); },
                    Err(e) => match e {
                        QErr::Reason(msg)
                            => { println_flush!("Error: {}", msg); },
                    },
                }
            },
            Ok(reed::Signal::CtrlD) | Ok(reed::Signal::CtrlC) => {
                break;
            },
            x => { println_flush!("Error: {:?}", x); },
        }
    }
}

