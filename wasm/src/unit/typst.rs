use crate::num::Sign;
use crate::unit::parser::{Exponent, Unit, Units as PUnits};
use crate::unit::{PerMode, ToTypst, UnitFmtConf, Units};

impl<'a> ToTypst for PUnits<'a> {
    fn write_typst(&self, buf: &mut String, conf: &UnitFmtConf, units: &Units) {
        let has_denom = !self.units_denom.is_empty();

        if (!self.units_num.is_empty() && self.units_num[0].unit.space)
            || (self.units_num.is_empty() && has_denom && self.units_denom[0].unit.space)
        {
            buf.push_str(&conf.space_first);
            buf.push(' ');
        }

        if has_denom && conf.per_mode == PerMode::Fraction {
            buf.push('(');
        }

        if self.units_num.is_empty()
            && matches!(conf.per_mode, PerMode::Fraction | PerMode::InlineFraction)
        {
            buf.push('1');
        }

        let mut units_iter = self.units_num.iter();
        if let Some(unit) = units_iter.next() {
            unit.write_typst(buf, conf, units);
        }
        for unit in units_iter {
            if unit.unit.space {
                buf.push(' ');
                buf.push_str(&conf.space);
                buf.push(' ');
            }
            unit.write_typst(buf, conf, units);
        }

        if has_denom {
            let space_per = match conf.per_mode {
                PerMode::Symbol => {
                    buf.push(' ');
                    buf.push_str(&conf.space);
                    buf.push(' ');
                    format!(" {} ", conf.space)
                }
                PerMode::Fraction => {
                    buf.push_str(")/(");
                    format!(" {} ", conf.space)
                }
                PerMode::InlineFraction => {
                    buf.push_str("\\/");
                    String::from("\\/")
                }
            };

            let mut units_iter = self.units_denom.iter();
            if let Some(unit) = units_iter.next() {
                unit.write_typst(buf, conf, units);
            }
            for unit in units_iter {
                if unit.unit.space {
                    buf.push_str(&space_per);
                }
                unit.write_typst(buf, conf, units);
            }

            if conf.per_mode == PerMode::Fraction {
                buf.push(')');
            }
        }
    }
}

impl<'a> ToTypst for Unit<'a> {
    fn write_typst(&self, buf: &mut String, conf: &UnitFmtConf, units: &Units) {
        if self.sqrt {
            buf.push_str("sqrt(");
        }

        if let Some(prefix) = self.prefix {
            buf.push_str(prefix);
        }

        buf.push_str(self.unit.symbol);

        if let Some(exp) = &self.exp {
            exp.write_typst(buf, conf, units);
        }

        if self.sqrt {
            buf.push(')');
        }
    }
}

impl ToTypst for Exponent {
    fn write_typst(&self, buf: &mut String, conf: &UnitFmtConf, _units: &Units) {
        if (conf.per_mode != PerMode::Symbol) && &self.num == "1" && self.denom.is_none() {
            return;
        }

        buf.push_str("^(");

        if conf.per_mode == PerMode::Symbol && self.sign == Sign::Minus {
            buf.push('-');
        }

        buf.push_str(&self.num);

        if let Some(denom) = &self.denom {
            buf.push_str("\\/");
            buf.push_str(denom);
        }

        buf.push(')');
    }
}

#[cfg(test)]
mod tests {
    use crate::unit::short::lexer::Tokenizer;

    use super::*;

    #[test]
    #[ignore]
    fn units() {
        let text = String::from("kg sqrt(m^3) / s^(2/3) / nb");
        let tokenizer = Tokenizer::new(text.chars());
        let mut iter = tokenizer.peekable();
        let units_lookup = Default::default();
        let units = PUnits::short(&mut iter, &units_lookup).unwrap();

        let conf = UnitFmtConf {
            space: String::from(" "),
            space_first: String::from(""),
            per_mode: PerMode::Symbol,
        };
        let typst = units.to_typst(&conf, &units_lookup);

        dbg!(typst);
    }
}
