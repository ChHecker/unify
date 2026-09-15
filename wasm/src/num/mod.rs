use std::borrow::Cow;
use std::fmt::Display;

use serde::Deserialize;

pub mod lexer;
pub mod parser;
pub mod typst;

pub trait ToTypst
where
    Self: Sized,
{
    fn write_typst(self, buf: &mut String, conf: &NumFmtConf);

    fn to_typst(self, conf: &NumFmtConf) -> String {
        let mut buf = String::with_capacity(64);
        buf.push('$');
        self.write_typst(&mut buf, conf);
        buf.push('$');
        buf
    }
}

#[derive(Debug, Deserialize)]
pub struct TypstNum {
    pub config: NumFmtConf,
    pub num: String,
}

#[derive(Debug, Deserialize)]
pub struct NumFmtConf {
    pub thousand_sep: Cow<'static, str>,
    pub dec_sep: Cow<'static, str>,
    pub multiplier: Cow<'static, str>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Sign {
    Plus,
    Minus,
}

impl Display for Sign {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Sign::Plus => write!(f, "+"),
            Sign::Minus => write!(f, "-"),
        }
    }
}
