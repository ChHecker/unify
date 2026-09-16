use std::str::FromStr;

use serde::Deserialize;

use crate::num::{NumFmtConf, ToTypst as _};
use crate::numrange::{ExpPos, NumRange, RangeFmtConf, TypstNumRange};
use crate::unit::parser::Units as PUnits;
use crate::unit::{ToTypst as _, TypstUnit, UnitFmtConf, Units};

#[derive(Debug, Deserialize)]
pub struct TypstQtyRange {
    pub range: TypstNumRange,
    pub unit: TypstUnit,
    pub raw_unit: bool,
    pub unit_pos: TypstUnitPos,
}

pub struct QtyRange<'a> {
    pub range: NumRange,
    pub unit: RawUnit<'a>,
    pub unit_pos: UnitPos,
}

pub enum RawUnit<'a> {
    Unit(PUnits<'a>),
    Raw(&'a str),
}

#[derive(Debug, Deserialize)]
pub struct TypstUnitPos {
    pub variant: String,
    pub manually_set: bool,
}

pub struct UnitPos {
    pub variant: UnitPosVariants,
    pub manually_set: bool,
}

impl TryFrom<TypstUnitPos> for UnitPos {
    type Error = String;

    fn try_from(value: TypstUnitPos) -> Result<Self, Self::Error> {
        Ok(Self {
            variant: value.variant.parse()?,
            manually_set: value.manually_set,
        })
    }
}

pub enum UnitPosVariants {
    Factored,
    Single,
    Both,
}

impl UnitPos {
    pub fn validate(&self, exp_pos: &mut ExpPos) -> crate::Result<()> {
        if !self.manually_set
            && matches!(
                &self.variant,
                UnitPosVariants::Both | UnitPosVariants::Single
            )
        {
            *exp_pos = ExpPos::Both;
        }

        match (&self.variant, exp_pos) {
            (UnitPosVariants::Both, ExpPos::Auto) => Err(String::from(
                "cannot combine unit position 'both' and exponential position 'auto'",
            )),
            (UnitPosVariants::Single, ExpPos::Auto) => Err(String::from(
                "cannot combine unit position 'single' and exponential position 'auto'",
            )),
            _ => Ok(()),
        }
    }
}

impl FromStr for UnitPosVariants {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "factor" | "paren" | "bracket" => Ok(Self::Factored),
            "single" | "outside" => Ok(Self::Single),
            "both" | "repeat" => Ok(Self::Both),
            s => Err(format!("invalid unit position {s}")),
        }
    }
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
        self.write_typst(&mut buf, conf_num, conf_range, conf_unit, units);
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
        let same_exp = matches!(conf_range.exp_pos, ExpPos::Auto)
            && matches!(self.unit_pos.variant, UnitPosVariants::Factored)
            && lower.exp == upper.exp;

        if matches!(self.unit_pos.variant, UnitPosVariants::Factored) {
            buf.push_str("lr((");
        }

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
                if matches!(self.unit_pos.variant, UnitPosVariants::Both) {
                    // buf.push(' ');
                    // buf.push_str(&conf_unit.space_first);
                    // buf.push(' ');

                    match &self.unit {
                        RawUnit::Unit(unit) => unit.write_typst(buf, conf_unit, units),
                        RawUnit::Raw(unit) => buf.push_str(unit),
                    }
                }

                buf.push(' ');
                buf.push_str(&conf_range.space);
                buf.push(' ');
                buf.push_str(&conf_range.delimiter);
                buf.push(' ');
                buf.push_str(&conf_range.space);
                buf.push(' ');

                upper.write_typst(buf, conf_num);
                if matches!(self.unit_pos.variant, UnitPosVariants::Both) {
                    // buf.push(' ');
                    // buf.push_str(&conf_unit.space_first);
                    // buf.push(' ');

                    match &self.unit {
                        RawUnit::Unit(unit) => unit.write_typst(buf, conf_unit, units),
                        RawUnit::Raw(unit) => buf.push_str(unit),
                    }
                }

                None
            }
        };

        if matches!(self.unit_pos.variant, UnitPosVariants::Factored) {
            buf.push_str("))");

            if let Some(exp) = exp {
                buf.push(' ');
                buf.push_str(&conf_num.multiplier);
                buf.push(' ');

                exp.write_typst(buf, conf_num);
            }
        }

        if matches!(
            self.unit_pos.variant,
            UnitPosVariants::Factored | UnitPosVariants::Single
        ) {
            // buf.push(' ');
            // buf.push_str(&conf_unit.space_first);
            // buf.push(' ');

            match &self.unit {
                RawUnit::Unit(unit) => unit.write_typst(buf, conf_unit, units),
                RawUnit::Raw(unit) => buf.push_str(unit),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::borrow::Cow;

    use crate::num::Sign;
    use crate::num::parser::{Exponent, Float, Num};
    use crate::numrange::ExpPos;
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
            unit: RawUnit::Unit(PUnits {
                units_num: vec![Unit {
                    prefix: None,
                    unit: UnitSpec {
                        symbol: "m",
                        space: true,
                    },
                    exp: None,
                    sqrt: false,
                }],
                units_denom: vec![],
            }),
            unit_pos: UnitPos {
                variant: UnitPosVariants::Factored,
                manually_set: false,
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
            exp_pos: ExpPos::Auto,
        };
        let conf_unit = UnitFmtConf {
            space: String::from(""),
            space_first: String::from(""),
            per_mode: PerMode::Symbol,
        };
        let units_lookup = Default::default();

        let range = range.to_typst(&conf_num, &conf_range, &conf_unit, &units_lookup);
        dbg!(range);
    }
}
