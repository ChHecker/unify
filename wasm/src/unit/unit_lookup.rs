#![allow(unused)]

use crate::unit::Units;

include!(concat!(env!("OUT_DIR"), "/codegen_units.rs"));

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct UnitSpec<'a> {
    pub symbol: &'a str,
    pub space: bool,
}

impl Units {
    pub fn contains_prefix(&self, prefix: &str) -> bool {
        let prefix_custom = self.prefixes.iter().find(|u| u.long == prefix);
        if prefix_custom.is_some() {
            return true;
        }
        PREFIXES.contains_key(prefix)
    }

    pub fn get_prefix(&self, prefix: &str) -> Option<&'_ str> {
        let prefix_custom = self.prefixes.iter().find(|u| u.long == prefix);
        match prefix_custom {
            Some(prefix) => Some(&prefix.symbol),
            None => PREFIXES.get(prefix).copied(),
        }
    }

    pub fn contains_unit(&self, unit: &str) -> bool {
        let unit_custom = self.units.iter().find(|u| u.long == unit);
        if unit_custom.is_some() {
            return true;
        }
        UNITS.contains_key(unit)
    }

    pub fn get_unit(&self, unit: &str) -> Option<UnitSpec<'_>> {
        let unit_custom = self.units.iter().find(|u| u.long == unit);
        match unit_custom {
            Some(unit) => Some(UnitSpec {
                symbol: &unit.symbol,
                space: unit.space,
            }),
            None => UNITS.get(unit).copied(),
        }
    }

    pub fn contains_postfix(&self, postfix: &str) -> bool {
        let postfix_custom = self.postfixes.iter().find(|u| u.long == postfix);
        if postfix_custom.is_some() {
            return true;
        }
        POSTFIXES.contains_key(postfix)
    }

    pub fn get_postfix(&self, postfix: &str) -> Option<&'_ str> {
        let postfix_custom = self.postfixes.iter().find(|u| u.long == postfix);
        match postfix_custom {
            Some(postfix) => Some(&postfix.symbol),
            None => POSTFIXES.get(postfix).copied(),
        }
    }

    pub fn contains_prefix_short(&self, prefix: &str) -> bool {
        let prefix_custom = self.prefixes.iter().find(|u| u.short == prefix);
        if prefix_custom.is_some() {
            return true;
        }
        PREFIXES_SHORT.contains_key(prefix)
    }

    pub fn get_prefix_short(&self, prefix: &str) -> Option<&'_ str> {
        let prefix_custom = self.prefixes.iter().find(|u| u.short == prefix);
        match prefix_custom {
            Some(prefix) => Some(&prefix.symbol),
            None => PREFIXES_SHORT.get(prefix).copied(),
        }
    }

    pub fn contains_unit_short(&self, unit: &str) -> bool {
        let unit_custom = self.units.iter().find(|u| u.short == unit);
        if unit_custom.is_some() {
            return true;
        }
        UNITS_SHORT.contains_key(unit)
    }

    pub fn get_unit_short(&self, unit: &str) -> Option<UnitSpec<'_>> {
        let unit_custom = self.units.iter().find(|u| u.short == unit);
        match unit_custom {
            Some(unit) => Some(UnitSpec {
                symbol: &unit.symbol,
                space: unit.space,
            }),
            None => UNITS_SHORT.get(unit).copied(),
        }
    }
}
