use serde::Deserialize;

use crate::num::TypstNum;
use crate::unit::TypstUnit;

#[derive(Debug, Deserialize)]
pub struct TypstQty {
    pub num: TypstNum,
    pub unit: TypstUnit,
    pub raw_unit: bool,
}
