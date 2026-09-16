use crate::num::Sign;
use crate::unit::unit_lookup::UnitSpec;

#[derive(Debug, PartialEq, Eq)]
pub struct Units<'a> {
    pub units_num: Vec<Unit<'a>>,
    pub units_denom: Vec<Unit<'a>>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Unit<'a> {
    pub prefix: Option<&'a str>,
    pub unit: UnitSpec<'a>,
    pub exp: Option<Exponent>,
    pub sqrt: bool,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Exponent {
    pub sign: Sign,
    pub num: String,
    pub denom: Option<String>,
}

impl<'a> Units<'a> {
    pub fn validate(&self) -> crate::Result<()> {
        if self.units_num.is_empty() && self.units_denom.is_empty() {
            return Err(String::from("empty unit"));
        }

        Ok(())
    }
}
