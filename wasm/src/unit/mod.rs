use std::str::FromStr;

use serde::Deserialize;

pub mod long;
pub mod short;

pub mod parser;
pub mod typst;
pub mod unit_lookup;

pub trait ToTypst
where
    Self: Sized,
{
    fn write_typst(&self, buf: &mut String, conf: &UnitFmtConf, units: &Units);

    fn to_typst(self, conf: &UnitFmtConf, units: &Units) -> String {
        let mut buf = String::with_capacity(64);
        self.write_typst(&mut buf, conf, units);
        buf
    }
}

#[derive(Debug, Deserialize)]
pub struct TypstUnit {
    pub config: TypstUnitFmtConf,
    pub units: Units,
    pub unit: String,
}

#[derive(Debug, Deserialize)]
pub struct TypstUnitFmtConf {
    pub space: String,
    pub space_first: String,
    pub per_mode: String,
}

#[derive(Debug)]
pub struct UnitFmtConf {
    pub space: String,
    pub space_first: String,
    pub per_mode: PerMode,
}

impl TryFrom<TypstUnitFmtConf> for UnitFmtConf {
    type Error = String;

    fn try_from(value: TypstUnitFmtConf) -> Result<Self, Self::Error> {
        Ok(Self {
            space: value.space,
            space_first: value.space_first,
            per_mode: value.per_mode.parse()?,
        })
    }
}

#[derive(Debug, Deserialize)]
pub struct Units {
    pub lang: String,
    pub prefixes: Vec<CustomPrefix>,
    pub units: Vec<CustomUnit>,
    pub postfixes: Vec<CustomPostfix>,
}

#[derive(Debug, Deserialize)]
pub struct CustomPrefix {
    pub long: String,
    pub short: String,
    pub symbol: String,
}

#[derive(Debug, Deserialize)]
pub struct CustomUnit {
    pub long: String,
    pub short: String,
    pub symbol: String,
    pub space: bool,
}

#[derive(Debug, Deserialize)]
pub struct CustomPostfix {
    pub long: String,
    pub symbol: String,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PerMode {
    Symbol,
    Fraction,
    InlineFraction,
}

impl FromStr for PerMode {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "symbol" => Ok(Self::Symbol),
            "fraction" | "/" => Ok(Self::Fraction),
            "fraction-short" | "short-fraction" | "\\/" => Ok(Self::InlineFraction),
            s => Err(format!("invalid per mode {s}")),
        }
    }
}

#[allow(clippy::derivable_impls)]
#[cfg(test)]
impl Default for Units {
    fn default() -> Self {
        Self {
            lang: String::from("en"),
            prefixes: Default::default(),
            units: Default::default(),
            postfixes: Default::default(),
        }
    }
}
