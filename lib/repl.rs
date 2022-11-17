use std::borrow::Cow;
use reedline as reed;
use crate::{
    qlist,
    qstr,
    qsymbol,
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
    fn as_print(exp: QExp) -> Self { Self::Print(exp) }

    fn as_noprint(exp: QExp) -> Self { Self::NoPrint(exp) }

    fn unpack(self) -> QExp {
        return match self {
            ReplOut::Print(exp) => {
                if let qstr!(s) = &exp {
                    println_flush!("\"{}\"", s);
                } else {
                    println_flush!("{}", exp);
                }
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

    fn repl_eval_builtin(&mut self, exp: &QExp, arg_forms: &[QExp])
        -> Option<QResult<ReplOut>>;

    fn repl_parse_eval(&mut self, expr: String) -> QResult<ReplOut>;
}

impl<'a> ReplEnv for QEnv<'a> {
    fn repl_eval(&mut self, exp: &QExp) -> QResult<ReplOut> {
        return match exp {
            qlist!(list) => {
                if let Some(first_form) = list.first() {
                    match self.repl_eval_builtin(first_form, &list[1..]) {
                        Some(res) => res,
                        None => self.eval(exp).map(|qexp| ReplOut::Print(qexp))
                    }
                } else {
                    self.eval(exp).map(|qexp| ReplOut::Print(qexp))
                }
            },
            _ => self.eval(exp).map(|qexp| ReplOut::Print(qexp)),
        };
    }

    fn repl_eval_builtin(&mut self, exp: &QExp, arg_forms: &[QExp])
        -> Option<QResult<ReplOut>>
    {
        return match exp {
            qsymbol!(s) => match s.as_ref() {
                "def" => Some(
                    self.eval_def(arg_forms).map(ReplOut::as_noprint)),
                ":=" => Some(
                    self.eval_def(arg_forms).map(ReplOut::as_noprint)),
                "let" => Some(
                    self.eval_let(arg_forms).map(ReplOut::as_print)),
                ":=*" => Some(
                    self.eval_let(arg_forms).map(ReplOut::as_print)),
                "fn" => Some(
                    self.eval_fn(arg_forms).map(ReplOut::as_print)),
                "`" => Some(
                    self.eval_fn(arg_forms).map(ReplOut::as_print)),
                "defn" => Some(
                    self.eval_defn(arg_forms).map(ReplOut::as_noprint)),
                ":`" => Some(
                    self.eval_defn(arg_forms).map(ReplOut::as_noprint)),
                "if" => Some(
                    self.eval_if(arg_forms).map(ReplOut::as_print)),
                "=>" => Some(
                    self.eval_if(arg_forms).map(ReplOut::as_print)),
                "and" => Some(
                    self.eval_and(arg_forms).map(ReplOut::as_print)),
                "&&" => Some(
                    self.eval_and(arg_forms).map(ReplOut::as_print)),
                "all" => Some(
                    self.eval_all(arg_forms).map(ReplOut::as_print)),
                "&&*" => Some(
                    self.eval_all(arg_forms).map(ReplOut::as_print)),
                "or" => Some(
                    self.eval_or(arg_forms).map(ReplOut::as_print)),
                "||" => Some(
                    self.eval_or(arg_forms).map(ReplOut::as_print)),
                "any" => Some(
                    self.eval_any(arg_forms).map(ReplOut::as_print)),
                "||*" => Some(
                    self.eval_any(arg_forms).map(ReplOut::as_print)),
                "xor" => Some(
                    self.eval_xor(arg_forms).map(ReplOut::as_print)),
                "^" => Some(
                    self.eval_xor(arg_forms).map(ReplOut::as_print)),
                "xany" => Some(
                    self.eval_xany(arg_forms).map(ReplOut::as_print)),
                "^*" => Some(
                    self.eval_xany(arg_forms).map(ReplOut::as_print)),
                "neg" => Some(
                    self.eval_neg(arg_forms).map(ReplOut::as_print)),
                "!" => Some(
                    self.eval_neg(arg_forms).map(ReplOut::as_print)),
                "mod" => Some(
                    self.eval_mod(arg_forms).map(ReplOut::as_print)),
                "%" => Some(
                    self.eval_mod(arg_forms).map(ReplOut::as_print)),
                "range" => Some(
                    self.eval_range(arg_forms).map(ReplOut::as_print)),
                ".." => Some(
                    self.eval_range(arg_forms).map(ReplOut::as_print)),
                "range-inc" => Some(
                    self.eval_range_inc(arg_forms).map(ReplOut::as_print)),
                "..=" => Some(
                    self.eval_range_inc(arg_forms).map(ReplOut::as_print)),
                "length" => Some(
                    self.eval_length(arg_forms).map(ReplOut::as_print)),
                "#" => Some(
                    self.eval_length(arg_forms).map(ReplOut::as_print)),
                "get" => Some(
                    self.eval_get(arg_forms).map(ReplOut::as_print)),
                "." => Some(
                    self.eval_get(arg_forms).map(ReplOut::as_print)),
                "slice" => Some(
                    self.eval_slice(arg_forms).map(ReplOut::as_print)),
                "--" => Some(
                    self.eval_slice(arg_forms).map(ReplOut::as_print)),
                "slice-inc" => Some(
                    self.eval_slice_inc(arg_forms).map(ReplOut::as_print)),
                "--=" => Some(
                    self.eval_slice_inc(arg_forms).map(ReplOut::as_print)),
                "slice-by" => Some(
                    self.eval_slice_by(arg_forms).map(ReplOut::as_print)),
                "~~" => Some(
                    self.eval_slice_by(arg_forms).map(ReplOut::as_print)),
                "slice-inc-by" => Some(
                    self.eval_slice_inc_by(arg_forms).map(ReplOut::as_print)),
                "~~=" => Some(
                    self.eval_slice_inc_by(arg_forms).map(ReplOut::as_print)),
                "step-by" => Some(
                    self.eval_step_by(arg_forms).map(ReplOut::as_print)),
                "~" => Some(
                    self.eval_step_by(arg_forms).map(ReplOut::as_print)),
                "enumerate" => Some(
                    self.eval_enumerate(arg_forms).map(ReplOut::as_print)),
                "##" => Some(
                    self.eval_enumerate(arg_forms).map(ReplOut::as_print)),
                "pick" => Some(
                    self.eval_pick(arg_forms).map(ReplOut::as_print)),
                ".*" => Some(
                    self.eval_pick(arg_forms).map(ReplOut::as_print)),
                "reverse" => Some(
                    self.eval_reverse(arg_forms).map(ReplOut::as_print)),
                "<>" => Some(
                    self.eval_reverse(arg_forms).map(ReplOut::as_print)),
                "first" => Some(
                    self.eval_first(arg_forms).map(ReplOut::as_print)),
                ".-" => Some(
                    self.eval_first(arg_forms).map(ReplOut::as_print)),
                "take" => Some(
                    self.eval_take(arg_forms).map(ReplOut::as_print)),
                "~." => Some(
                    self.eval_take(arg_forms).map(ReplOut::as_print)),
                "take-while" => Some(
                    self.eval_take_while(arg_forms).map(ReplOut::as_print)),
                "~.@" => Some(
                    self.eval_take_while(arg_forms).map(ReplOut::as_print)),
                "last" => Some(
                    self.eval_last(arg_forms).map(ReplOut::as_print)),
                "-." => Some(
                    self.eval_last(arg_forms).map(ReplOut::as_print)),
                "skip" => Some(
                    self.eval_skip(arg_forms).map(ReplOut::as_print)),
                ".~" => Some(
                    self.eval_skip(arg_forms).map(ReplOut::as_print)),
                "skip-while" => Some(
                    self.eval_skip_while(arg_forms).map(ReplOut::as_print)),
                ".~@" => Some(
                    self.eval_skip_while(arg_forms).map(ReplOut::as_print)),
                "append" => Some(
                    self.eval_append(arg_forms).map(ReplOut::as_print)),
                "+." => Some(
                    self.eval_append(arg_forms).map(ReplOut::as_print)),
                "prepend" => Some(
                    self.eval_prepend(arg_forms).map(ReplOut::as_print)),
                ".+" => Some(
                    self.eval_prepend(arg_forms).map(ReplOut::as_print)),
                "insert" => Some(
                    self.eval_insert(arg_forms).map(ReplOut::as_print)),
                "+.+" => Some(
                    self.eval_insert(arg_forms).map(ReplOut::as_print)),
                "map" => Some(
                    self.eval_map(arg_forms).map(ReplOut::as_print)),
                "@" => Some(
                    self.eval_map(arg_forms).map(ReplOut::as_print)),
                "filter" => Some(
                    self.eval_filter(arg_forms).map(ReplOut::as_print)),
                "@!" => Some(
                    self.eval_filter(arg_forms).map(ReplOut::as_print)),
                "flatten" => Some(
                    self.eval_flatten(arg_forms).map(ReplOut::as_print)),
                "__" => Some(
                    self.eval_flatten(arg_forms).map(ReplOut::as_print)),
                "contains" => Some(
                    self.eval_contains(arg_forms).map(ReplOut::as_print)),
                "=*" => Some(
                    self.eval_contains(arg_forms).map(ReplOut::as_print)),
                "fold" => Some(
                    self.eval_fold(arg_forms).map(ReplOut::as_print)),
                "@." => Some(
                    self.eval_fold(arg_forms).map(ReplOut::as_print)),
                "min" => Some(
                    self.eval_min(arg_forms).map(ReplOut::as_print)),
                "<<" => Some(
                    self.eval_min(arg_forms).map(ReplOut::as_print)),
                "max" => Some(
                    self.eval_max(arg_forms).map(ReplOut::as_print)),
                ">>" => Some(
                    self.eval_max(arg_forms).map(ReplOut::as_print)),
                "format" => Some(
                    self.eval_format(arg_forms).map(ReplOut::as_print)),
                "$" => Some(
                    self.eval_format(arg_forms).map(ReplOut::as_print)),
                "print" => Some(
                    self.eval_print(arg_forms).map(ReplOut::as_noprint)),
                "$-" => Some(
                    self.eval_print(arg_forms).map(ReplOut::as_noprint)),
                "println" => Some(
                    self.eval_println(arg_forms).map(ReplOut::as_noprint)),
                "$_" => Some(
                    self.eval_println(arg_forms).map(ReplOut::as_noprint)),
                "halt" => Some(
                    self.eval_halt(arg_forms).map(ReplOut::as_noprint)),
                "!!" => Some(
                    self.eval_halt(arg_forms).map(ReplOut::as_noprint)),
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
                    = match s.split_once(';') {
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
            Ok(reed::Signal::CtrlD) => {
                break;
            },
            Ok(reed::Signal::CtrlC) => {
                continue;
            },
            x => { println_flush!("Error: {:?}", x); },
        }
    }
}

