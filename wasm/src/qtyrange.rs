use serde::Deserialize;

use crate::num::{NumFmtConf, ToTypst as _};
use crate::numrange::{NumRange, RangeFmtConf, TypstNumRange};
use crate::unit::parser::Units as PUnits;
use crate::unit::{ToTypst as _, TypstUnit, UnitFmtConf, Units};

#[derive(Debug, Deserialize)]
pub struct TypstQtyRange {
    pub range: TypstNumRange,
    pub unit: TypstUnit,
}

pub struct QtyRange<'a> {
    pub range: NumRange,
    pub unit: PUnits<'a>,
}

pub trait ToTypst
where
    Self: Sized,
{
    fn write_typst(
        self,
        buf: &mut String,
        conf_num: &NumFmtConf,
        conf_range: &RangeFmtConf,
        conf_unit: &UnitFmtConf,
        units: &Units,
    );

    fn to_typst(
        self,
        conf_num: &NumFmtConf,
        conf_range: &RangeFmtConf,
        conf_unit: &UnitFmtConf,
        units: &Units,
    ) -> String {
        let mut buf = String::with_capacity(64);
        buf.push('$');
        self.write_typst(&mut buf, conf_num, conf_range, conf_unit, units);
        buf.push('$');
        buf
    }
}

impl<'a> ToTypst for QtyRange<'a> {
    fn write_typst(
        self,
        buf: &mut String,
        conf_num: &NumFmtConf,
        conf_range: &RangeFmtConf,
        conf_unit: &UnitFmtConf,
        units: &Units,
    ) {
        let mut lower = self.range.lower;
        let mut upper = self.range.upper;
        let same_exp = lower.exp == upper.exp;

        buf.push_str("lr((");

        let exp = match same_exp {
            true => {
                let exp = lower.exp.take();
                upper.exp = None;

                lower.write_typst(buf, conf_num);
                buf.push(' ');
                buf.push_str(&conf_range.space);
                buf.push(' ');
                buf.push_str(&conf_range.delimiter);
                buf.push(' ');
                buf.push_str(&conf_range.space);
                buf.push(' ');
                upper.write_typst(buf, conf_num);

                exp
            }
            false => {
                lower.write_typst(buf, conf_num);
                buf.push(' ');
                buf.push_str(&conf_range.space);
                buf.push(' ');
                buf.push_str(&conf_range.delimiter);
                buf.push(' ');
                buf.push_str(&conf_range.space);
                buf.push(' ');
                upper.write_typst(buf, conf_num);

                None
            }
        };

        buf.push_str("))");

        if let Some(exp) = exp {
            buf.push(' ');
            buf.push_str(&conf_num.multiplier);
            buf.push(' ');

            exp.write_typst(buf, conf_num);
        }

        buf.push(' ');
        buf.push_str(&conf_unit.space_first);
        buf.push(' ');
        self.unit.write_typst(buf, conf_unit, units);
    }
}

#[cfg(test)]
mod tests {
    use std::borrow::Cow;

    use crate::num::Sign;
    use crate::num::parser::{Exponent, Float, Num};
    use crate::unit::PerMode;
    use crate::unit::parser::Unit;
    use crate::unit::unit_lookup::UnitSpec;

    use super::*;

    #[test]
    #[ignore]
    fn qtyrange() {
        let range = QtyRange {
            range: NumRange {
                lower: Num {
                    float: Some(Float {
                        sign: Sign::Plus,
                        int: String::from("12"),
                        dec: None,
                    }),
                    uncert: None,
                    exp: Some(Exponent {
                        sign: Sign::Plus,
                        int: String::from("6"),
                    }),
                },
                upper: Num {
                    float: Some(Float {
                        sign: Sign::Plus,
                        int: String::from("14"),
                        dec: None,
                    }),
                    uncert: None,
                    exp: Some(Exponent {
                        sign: Sign::Plus,
                        int: String::from("6"),
                    }),
                },
            },
            unit: PUnits {
                units_num: vec![Unit {
                    prefix: None,
                    unit: UnitSpec {
                        symbol: "m",
                        space: true,
                    },
                    exp: None,
                }],
                units_denom: vec![],
            },
        };

        let conf_num = NumFmtConf {
            thousand_sep: Cow::Borrowed(","),
            dec_sep: Cow::Borrowed("."),
            multiplier: Cow::Borrowed("dot"),
        };
        let conf_range = RangeFmtConf {
            delimiter: Cow::Borrowed("\"to\""),
            space: Cow::Borrowed("#h(0.167777em)"),
        };
        let conf_unit = UnitFmtConf {
            space: String::from(" "),
            space_first: String::from(" "),
            per_mode: PerMode::Symbol,
        };
        let units_lookup = Default::default();

        let range = range.to_typst(&conf_num, &conf_range, &conf_unit, &units_lookup);
        dbg!(range);
    }
}
