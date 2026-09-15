use ciborium::de::from_reader;
use wasm_minimal_protocol::*;

use crate::num::lexer::Tokenizer as NumTokenizer;
use crate::num::parser::Num;
use crate::num::{ToTypst as _, TypstNum};
use crate::numrange::{NumRange, ToTypst as _, TypstNumRange};
use crate::qty::TypstQty;
use crate::qtyrange::{QtyRange, RawUnit, ToTypst, TypstQtyRange};
use crate::unit::long::lexer::Tokenizer as LongUnitTokenizer;
use crate::unit::parser::Units;
use crate::unit::short::lexer::Tokenizer as ShortUnitTokenizer;
use crate::unit::{ToTypst as _, TypstUnit, UnitFmtConf};

mod num;
mod numrange;
mod qty;
mod qtyrange;
mod result;
mod unit;
pub use result::Result;

type Integer = String;

initiate_protocol!();

#[wasm_func]
pub fn num(arg: &[u8]) -> crate::Result<Vec<u8>> {
    let args: TypstNum = from_reader(arg).map_err(|e| format!("error reading argument: {e}"))?;

    let tokenizer = NumTokenizer::new(args.num.chars());
    let iter = tokenizer.peekable();
    let num = Num::new(iter)?;

    let num = num.to_typst(&args.config);

    Ok(num.as_bytes().to_vec())
}

#[wasm_func]
pub fn numrange(arg: &[u8]) -> crate::Result<Vec<u8>> {
    let args: TypstNumRange =
        from_reader(arg).map_err(|e| format!("error reading argument: {e}"))?;

    let tokenizer_lower = NumTokenizer::new(args.lower.chars());
    let iter_lower = tokenizer_lower.peekable();

    let tokenizer_upper = NumTokenizer::new(args.upper.chars());
    let iter_upper = tokenizer_upper.peekable();

    let range = NumRange::new(iter_lower, iter_upper)?;

    let range = range.to_typst(&args.config_num, &args.config_range);

    Ok(range.as_bytes().to_vec())
}

#[wasm_func]
pub fn unit(arg: &[u8]) -> crate::Result<Vec<u8>> {
    let args: TypstUnit = from_reader(arg).map_err(|e| format!("error reading argument: {e}"))?;
    let mut conf: UnitFmtConf = args.config.try_into()?;
    conf.space_first = String::from("");

    let tokenizer = LongUnitTokenizer::new(args.unit.chars());
    let iter = tokenizer.peekable();
    let units_long = Units::long(iter, &args.units);

    let units = match units_long {
        Ok(units) => units,
        Err(_) => {
            let tokenizer = ShortUnitTokenizer::new(args.unit.chars());
            let iter = tokenizer.peekable();
            Units::short(iter, &args.units)?
        }
    };

    let units = units.to_typst(&conf, &args.units);

    Ok(units.as_bytes().to_vec())
}

#[wasm_func]
pub fn qty(arg: &[u8]) -> crate::Result<Vec<u8>> {
    let args: TypstQty = from_reader(arg).map_err(|e| format!("error reading argument: {e}"))?;
    let mut out = String::from('$');

    let args_num = args.num;
    let conf_num = args_num.config;

    let tokenizer = NumTokenizer::new(args_num.num.chars());
    let iter = tokenizer.peekable();
    let num = Num::new(iter)?;

    num.write_typst(&mut out, &conf_num);

    let args_unit = args.unit;
    let conf_unit: UnitFmtConf = args_unit.config.try_into()?;

    if args.raw_unit {
        out.push_str(&conf_unit.space_first);
        out.push(' ');
        out.push_str(&args_unit.unit);
    } else {
        let tokenizer = LongUnitTokenizer::new(args_unit.unit.chars());
        let iter = tokenizer.peekable();
        let units_long = Units::long(iter, &args_unit.units);

        let units = match units_long {
            Ok(units) => units,
            Err(_) => {
                let tokenizer = ShortUnitTokenizer::new(args_unit.unit.chars());
                let iter = tokenizer.peekable();
                Units::short(iter, &args_unit.units)?
            }
        };

        units.write_typst(&mut out, &conf_unit, &args_unit.units);
    }

    out.push('$');
    Ok(out.as_bytes().to_vec())
}

#[wasm_func]
pub fn qtyrange(arg: &[u8]) -> crate::Result<Vec<u8>> {
    let args: TypstQtyRange =
        from_reader(arg).map_err(|e| format!("error reading argument: {e}"))?;

    let args_range = args.range;
    let conf_num = args_range.config_num;
    let conf_range = args_range.config_range;

    let tokenizer_lower = NumTokenizer::new(args_range.lower.chars());
    let iter_lower = tokenizer_lower.peekable();
    let tokenizer_upper = NumTokenizer::new(args_range.upper.chars());
    let iter_upper = tokenizer_upper.peekable();
    let range = NumRange::new(iter_lower, iter_upper)?;

    let args_unit = args.unit;
    let conf_unit: UnitFmtConf = args_unit.config.try_into()?;

    let unit = if args.raw_unit {
        RawUnit::Raw(&args_unit.unit)
    } else {
        let tokenizer = LongUnitTokenizer::new(args_unit.unit.chars());
        let iter = tokenizer.peekable();
        let units_long = Units::long(iter, &args_unit.units);

        RawUnit::Unit(match units_long {
            Ok(units) => units,
            Err(_) => {
                let tokenizer = ShortUnitTokenizer::new(args_unit.unit.chars());
                let iter = tokenizer.peekable();
                Units::short(iter, &args_unit.units)?
            }
        })
    };

    let qtyrange = QtyRange { range, unit };
    let qtyrange = qtyrange.to_typst(&conf_num, &conf_range, &conf_unit, &args_unit.units);

    Ok(qtyrange.as_bytes().to_vec())
}
