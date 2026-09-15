#import "format.typ": *

#let num(value, multiplier: "dot", thousandsep: "#h(0.166667em)", decsep: ".") = {
  /// Format a number.
  /// - `value`: String with the number.
  /// - `multiplier`: The symbol used to indicate multiplication
  /// - `thousandsep`: The separator between the thousands of the float.

  // str() converts minus "-" of a number to unicode "\u2212"
  value = _to-string(value).replace("−", "-").replace(" ", "")

  let conf = (thousand_sep: thousandsep, dec_sep: decsep, multiplier: multiplier)
  let cbor = cbor.encode((config: conf, num: value))
  eval(str(wasm.num(cbor)))
}

#let add-unit(unit, shorthand, symbol, space: true) = {
  /// Add a new unit.
  /// - `unit`: Full name of the unit.
  /// - `shorthand`: Shorthand of the unit, usually only 1-2 letters.
  /// - `symbol`: String that will be inserted as the unit symbol.
  /// - `space`: Whether to put a space before the unit.
  context {
    _units.update(units => {
      units
        .at("units")
        .push((
          long: unit,
          short: shorthand,
          symbol: symbol,
          space: space,
        ))
      units
    })
  }
}

#let add-prefix(prefix, shorthand, symbol) = {
  /// Add a new prefix.
  /// - `prefix`: Full name of the prefix.
  /// - `shorthand`: Shorthand of the prefix, usually only 1-2 letters.
  /// - `symbol`: String that will be inserted as the prefix symbol.
  context {
    _units.update(units => {
      units
        .at("prefixes")
        .push((
          long: prefix,
          short: shorthand,
          symbol: symbol,
        ))
      units
    })
  }
}

#let add-postfix(postfix, symbol) = {
  /// Add a new postfix.
  /// - `postfix`: Full name of the postfix.
  /// - `shorthand`: Shorthand of the postfix, usually only 1-2 letters.
  /// - `symbol`: String that will be inserted as the postfix symbol.
  context {
    _units.update(units => {
      units
        .at("postfixes")
        .push((
          long: postfix,
          symbol: symbol,
        ))
      units
    })
  }
}


#let unit(unit, space: "#h(0.166667em)", per: "symbol") = {
  /// Format a unit.
  /// - `unit`: String containing the unit.
  /// - `space`: Space between units.
  /// - `per`: Whether to format the units after `per` or `/` with a fraction or exponent.

  let conf = (space: space, space_first: "", per_mode: per)
  context {
    let units = _units.get()
    let cbor = cbor.encode((config: conf, units: units, unit: unit))
    eval(str(wasm.unit(cbor)))
  }
}

#let qty(
  value,
  unit,
  rawunit: false,
  space: "#h(0.166667em)",
  num-unit-space: "#h(0.166667em)",
  multiplier: "dot",
  thousandsep: "#h(0.166667em)",
  decsep: ".",
  per: "symbol",
) = {
  /// Format a quantity (i.e. number with a unit).
  /// - `value`: String containing the number.
  /// - `unit`: String containing the unit.
  /// - `multiplier`: The symbol used to indicate multiplication
  /// - `rawunit`: Whether to transform the unit or keep the raw string.
  /// - `space`: Space between units.
  /// - `num-unit-space`: Space between the number and the units.
  /// - `thousandsep`: The separator between the thousands of the float.
  /// - `per`: Whether to format the units after `per` or `/` with a fraction or exponent.

  value = _to-string(value).replace("−", "-").replace(" ", "")

  let conf-num = (thousand_sep: thousandsep, dec_sep: decsep, multiplier: multiplier)
  let conf-unit = (space: space, space_first: num-unit-space, per_mode: per)

  context {
    let units = _units.get()

    let num = (config: conf-num, num: value)
    let unit = (config: conf-unit, units: units, unit: unit)

    let cbor = cbor.encode((num: num, unit: unit))
    eval(str(wasm.qty(cbor)))
  }
}

#let numrange(
  lower,
  upper,
  multiplier: "dot",
  delimiter: "-",
  space: "#h(0.16667em)",
  thousandsep: "#h(0.166667em)",
  decsep: ".",
) = {
  /// Format a range.
  /// - `(lower, upper)`: Strings containing the numbers.
  /// - `multiplier`: The symbol used to indicate multiplication
  /// - `delimiter`: Symbol between the numbers.
  /// - `space`: Space between the numbers and the delimiter.
  /// - `thousandsep`: The separator between the thousands of the float.
  lower = _to-string(lower).replace("−", "-").replace(" ", "")
  upper = _to-string(upper).replace("−", "-").replace(" ", "")

  let conf-num = (thousand_sep: thousandsep, dec_sep: decsep, multiplier: multiplier)
  let conf-range = (delimiter: delimiter, space: space)

  let cbor = cbor.encode((config_num: conf-num, config_range: conf-range, lower: lower, upper: upper))
  eval(str(wasm.numrange(cbor)))
}

#let qtyrange(
  lower,
  upper,
  unit,
  rawunit: false,
  multiplier: "dot",
  delimiter: "-",
  space: "",
  unitspace: "#h(0.16667em)",
  range-unit-space: "#h(0.166667em)",
  thousandsep: "#h(0.166667em)",
  decsep: ".",
  per: "symbol",
) = {
  /// Format a range with a unit.
  /// - `(lower, upper)`: Strings containing the numbers.
  /// - `unit`: String containing the unit.
  /// - `rawunit`: Whether to transform the unit or keep the raw string.
  /// - `multiplier`: The symbol used to indicate multiplication
  /// - `delimiter`: Symbol between the numbers.
  /// - `space`: Space between the numbers and the delimiter.
  /// - `unitspace`: Space between units.
  /// - `range-unit-space`: Space between the range/exponential and the units.
  /// - `thousandsep`: The separator between the thousands of the float.
  /// - `per`: Whether to format the units after `per` or `/` with a fraction or exponent.

  lower = _to-string(lower).replace("−", "-").replace(" ", "")
  upper = _to-string(upper).replace("−", "-").replace(" ", "")

  let conf-num = (thousand_sep: thousandsep, dec_sep: decsep, multiplier: multiplier)
  let conf-range = (delimiter: delimiter, space: space)
  let conf-unit = (space: space, space_first: range-unit-space, per_mode: per)

  context {
    let units = _units.get()

    let range = (config_num: conf-num, config_range: conf-range, lower: lower, upper: upper)
    let unit = (config: conf-unit, units: units, unit: unit)

    let cbor = cbor.encode((range: range, unit: unit))
    eval(str(wasm.qtyrange(cbor)))
  }
}
