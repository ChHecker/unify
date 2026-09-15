use crate::Integer;
use crate::num::parser::{Exponent, Float, Num, Uncertainty};
use crate::num::{NumFmtConf, Sign, ToTypst};

impl ToTypst for Num {
    fn write_typst(self, buf: &mut String, config: &NumFmtConf) {
        let has_val = self.float.is_some();
        let has_uncert = self.uncert.is_some();
        let has_exp = self.exp.is_some();

        let need_delim = has_val && has_uncert && has_exp;
        let need_multiplier = has_val && has_exp;

        if need_delim {
            buf.push_str("lr((");
        }

        if let Some(float) = self.float {
            float.write_typst(buf, config);
        }

        if let Some(uncert) = self.uncert {
            uncert.write_typst(buf, config);
        }

        if need_delim {
            buf.push_str("))");
        }

        if need_multiplier {
            buf.push(' ');
            buf.push_str(&config.multiplier);
            buf.push(' ');
        }

        if let Some(exp) = self.exp {
            exp.write_typst(buf, config);
        }
    }
}

impl ToTypst for Float {
    fn write_typst(self, buf: &mut String, config: &NumFmtConf) {
        if self.sign == Sign::Minus {
            buf.push('-');
        }

        write_int(self.int, buf, config);

        if let Some(dec) = self.dec {
            buf.push_str(&config.dec_sep);
            write_dec(dec, buf, config);
        }
    }
}

impl ToTypst for Uncertainty {
    fn write_typst(self, buf: &mut String, config: &NumFmtConf) {
        match self {
            Uncertainty::Shorthand(int) => {
                buf.push('(');
                buf.push_str(&int);
                buf.push(')');
            }
            Uncertainty::Explicit(float) => {
                buf.push_str(" plus.minus ");
                float.write_typst(buf, config);
            }
            Uncertainty::Asymmetric { plus, mut minus } => {
                buf.push_str("^(+");
                plus.write_typst(buf, config);
                buf.push(')');

                buf.push_str("_(-");
                minus.sign = Sign::Plus;
                minus.write_typst(buf, config);
                buf.push(')');
            }
        }
    }
}

impl ToTypst for Exponent {
    fn write_typst(self, buf: &mut String, config: &NumFmtConf) {
        buf.push_str("10^(");
        if self.sign == Sign::Minus {
            buf.push('-');
        }
        write_int(self.int, buf, config);
        buf.push(')');
    }
}

fn write_int(int: Integer, buf: &mut String, config: &NumFmtConf) {
    let len = int.len() / 3;
    let rem = int.len() % 3;

    buf.push_str(&int[0..rem]);
    for i in 0..len {
        buf.push_str(&config.thousand_sep);
        let range = rem + 3 * i..rem + 3 * (i + 1);
        buf.push_str(&int[range]);
    }
}

fn write_dec(int: Integer, buf: &mut String, config: &NumFmtConf) {
    let len = int.len() / 3;
    let rem = int.len() % 3;

    if len > 0 {
        for i in 0..len - 1 {
            let range = 3 * i..3 * (i + 1);
            buf.push_str(&int[range]);
            buf.push_str(&config.thousand_sep);
        }

        let range = 3 * (len - 1)..3 * len;
        buf.push_str(&int[range]);

        if rem != 0 {
            buf.push_str(&config.thousand_sep);
        }
    }

    if rem != 0 {
        buf.push_str(&int[3 * len..]);
    }
}

#[cfg(test)]
mod tests {
    use std::borrow::Cow;

    use crate::num::parser::{Exponent, Float, Num, Uncertainty};
    use crate::num::{NumFmtConf, Sign, ToTypst};

    #[test]
    #[ignore]
    fn num() {
        let float = Some(Float {
            sign: Sign::Minus,
            int: String::from("1234"),
            dec: Some(String::from("567")),
        });
        let uncert = Some(Uncertainty::Asymmetric {
            plus: Float {
                sign: Sign::Plus,
                int: String::from("23"),
                dec: None,
            },
            minus: Float {
                sign: Sign::Minus,
                int: String::from("15"),
                dec: None,
            },
        });
        let exp = Some(Exponent {
            sign: Sign::Minus,
            int: String::from("23"),
        });
        let num = Num { float, uncert, exp };

        let mut text = String::new();
        let config = NumFmtConf {
            thousand_sep: Cow::Borrowed(","),
            dec_sep: Cow::Borrowed("."),
            multiplier: Cow::Borrowed("dot"),
        };
        num.write_typst(&mut text, &config);

        dbg!(text);
    }

    #[test]
    fn float() {
        let float = Float {
            sign: Sign::Minus,
            int: String::from("1234567"),
            dec: Some(String::from("7654321")),
        };

        let mut text = String::new();
        let config = NumFmtConf {
            thousand_sep: Cow::Borrowed(","),
            dec_sep: Cow::Borrowed("."),
            multiplier: Cow::Borrowed("dot"),
        };
        float.write_typst(&mut text, &config);

        assert_eq!(&text, "-1,234,567.765,432,1")
    }

    #[test]
    fn uncertainty_sym() {
        let uncert = Uncertainty::Explicit(Float {
            sign: Sign::Plus,
            int: String::from("10"),
            dec: None,
        });

        let mut text = String::new();
        let config = NumFmtConf {
            thousand_sep: Cow::Borrowed(","),
            dec_sep: Cow::Borrowed("."),
            multiplier: Cow::Borrowed("dot"),
        };
        uncert.write_typst(&mut text, &config);

        assert_eq!(&text, " plus.minus 10")
    }

    #[test]
    fn uncertainty_asym() {
        let uncert = Uncertainty::Asymmetric {
            plus: Float {
                sign: Sign::Plus,
                int: String::from("20"),
                dec: None,
            },
            minus: Float {
                sign: Sign::Minus,
                int: String::from("10"),
                dec: None,
            },
        };

        let mut text = String::new();
        let config = NumFmtConf {
            thousand_sep: Cow::Borrowed(","),
            dec_sep: Cow::Borrowed("."),
            multiplier: Cow::Borrowed("dot"),
        };
        uncert.write_typst(&mut text, &config);

        assert_eq!(&text, "^(+20)_(-10)")
    }

    #[test]
    fn uncertainty_short() {
        let uncert = Uncertainty::Shorthand(String::from("10"));

        let mut text = String::new();
        let config = NumFmtConf {
            thousand_sep: Cow::Borrowed(","),
            dec_sep: Cow::Borrowed("."),
            multiplier: Cow::Borrowed("dot"),
        };
        uncert.write_typst(&mut text, &config);

        assert_eq!(&text, "(10)")
    }

    #[test]
    fn exponential() {
        let exp = Exponent {
            sign: Sign::Minus,
            int: String::from("1234"),
        };

        let mut text = String::new();
        let config = NumFmtConf {
            thousand_sep: Cow::Borrowed(","),
            dec_sep: Cow::Borrowed("."),
            multiplier: Cow::Borrowed("dot"),
        };
        exp.write_typst(&mut text, &config);

        assert_eq!(&text, "10^(-1,234)")
    }

    #[test]
    fn int() {
        let int = String::from("1234567");

        let mut text = String::new();
        let config = NumFmtConf {
            thousand_sep: Cow::Borrowed(","),
            dec_sep: Cow::Borrowed("."),
            multiplier: Cow::Borrowed("dot"),
        };
        super::write_int(int, &mut text, &config);

        assert_eq!(&text, "1,234,567")
    }

    #[test]
    fn dec() {
        let int = String::from("1234567");

        let mut text = String::new();
        let config = NumFmtConf {
            thousand_sep: Cow::Borrowed(","),
            dec_sep: Cow::Borrowed("."),
            multiplier: Cow::Borrowed("dot"),
        };
        super::write_dec(int, &mut text, &config);

        assert_eq!(&text, "123,456,7")
    }
}
