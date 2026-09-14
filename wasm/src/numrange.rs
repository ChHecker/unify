use std::borrow::Cow;

use serde::Deserialize;

use crate::num::lexer::Token;
use crate::num::parser::Num;
use crate::num::{NumFmtConf, ToTypst as _};

pub trait ToTypst
where
    Self: Sized,
{
    fn write_typst(self, buf: &mut String, conf_num: &NumFmtConf, conf_range: &RangeFmtConf);

    fn to_typst(self, conf_num: &NumFmtConf, conf_range: &RangeFmtConf) -> String {
        let mut buf = String::with_capacity(64);
        buf.push('$');
        self.write_typst(&mut buf, conf_num, conf_range);
        buf.push('$');
        buf
    }
}

#[derive(Debug, Deserialize)]
pub struct TypstNumRange {
    pub config_num: NumFmtConf,
    pub config_range: RangeFmtConf,
    pub lower: String,
    pub upper: String,
}

#[derive(Debug, Deserialize)]
pub struct RangeFmtConf {
    pub delimiter: Cow<'static, str>,
    pub space: Cow<'static, str>,
}

#[derive(Debug)]
pub struct NumRange {
    pub lower: Num,
    pub upper: Num,
}

impl NumRange {
    pub fn new<T1, T2>(iter_lower: T1, iter_upper: T2) -> crate::Result<Self>
    where
        T1: IntoIterator<Item = crate::Result<Token>>,
        T2: IntoIterator<Item = crate::Result<Token>>,
    {
        let lower = Num::new(iter_lower)?;
        let upper = Num::new(iter_upper)?;
        Ok(Self { lower, upper })
    }
}

impl ToTypst for NumRange {
    fn write_typst(self, buf: &mut String, conf_num: &NumFmtConf, conf_range: &RangeFmtConf) {
        let same_exp = self.lower.exp == self.upper.exp;

        match same_exp {
            true => {
                let mut lower = self.lower;
                let exp = lower.exp.take();
                let mut upper = self.upper;
                upper.exp = None;

                if exp.is_some() {
                    buf.push_str("lr((");
                }

                lower.write_typst(buf, conf_num);
                buf.push(' ');
                buf.push_str(&conf_range.space);
                buf.push(' ');
                buf.push_str(&conf_range.delimiter);
                buf.push(' ');
                buf.push_str(&conf_range.space);
                buf.push(' ');
                upper.write_typst(buf, conf_num);

                if let Some(exp) = exp {
                    buf.push_str("))");

                    buf.push(' ');
                    buf.push_str(&conf_num.multiplier);
                    buf.push(' ');

                    exp.write_typst(buf, conf_num);
                }
            }
            false => {
                self.lower.write_typst(buf, conf_num);
                buf.push(' ');
                buf.push_str(&conf_range.space);
                buf.push(' ');
                buf.push_str(&conf_range.delimiter);
                buf.push(' ');
                buf.push_str(&conf_range.space);
                buf.push(' ');
                self.upper.write_typst(buf, conf_num);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::borrow::Cow;

    use crate::num::NumFmtConf;
    use crate::num::lexer::Tokenizer;
    use crate::numrange::{NumRange, RangeFmtConf, ToTypst};

    #[test]
    #[ignore]
    fn range_same_exp() {
        let text_lower = String::from("23e5");
        let tokenizer_lower = Tokenizer::new(text_lower.chars());
        let iter_lower = tokenizer_lower.peekable();

        let text_upper = String::from("43e5");
        let tokenizer_upper = Tokenizer::new(text_upper.chars());
        let iter_upper = tokenizer_upper.peekable();

        let conf_num = NumFmtConf {
            thousand_sep: Cow::Borrowed(","),
            dec_sep: Cow::Borrowed("."),
            multiplier: Cow::Borrowed("dot"),
        };
        let conf_range = RangeFmtConf {
            delimiter: Cow::Borrowed("\"to\""),
            space: Cow::Borrowed("#h(0.167777em)"),
        };

        let range = NumRange::new(iter_lower, iter_upper).unwrap();
        let range = range.to_typst(&conf_num, &conf_range);

        dbg!(range);
    }

    #[test]
    #[ignore]
    fn range() {
        let text_lower = String::from("23e5");
        let tokenizer_lower = Tokenizer::new(text_lower.chars());
        let iter_lower = tokenizer_lower.peekable();

        let text_upper = String::from("43e6");
        let tokenizer_upper = Tokenizer::new(text_upper.chars());
        let iter_upper = tokenizer_upper.peekable();

        let conf_num = NumFmtConf {
            thousand_sep: Cow::Borrowed(","),
            dec_sep: Cow::Borrowed("."),
            multiplier: Cow::Borrowed("dot"),
        };
        let conf_range = RangeFmtConf {
            delimiter: Cow::Borrowed("\"to\""),
            space: Cow::Borrowed("#h(0.167777em)"),
        };

        let range = NumRange::new(iter_lower, iter_upper).unwrap();
        let range = range.to_typst(&conf_num, &conf_range);

        dbg!(range);
    }
}
