// #let re-num = regex("^(-?\d+\.?\d*)?(((\+(\d+\.?\d*)-(\d+\.?\d*)))|((((\+-)|(-\+))(\d+\.?\d*))))?(e(-?\d+))?$")
#let re-num = regex("^(-?\d+(\.|,)?\d*)?(((\+(\d+(\.|,)?\d*)-(\d+(\.|,)?\d*)))|((((\+-)|(-\+))(\d+(\.|,)?\d*))))?(e([-\+]?\d+))?$")


#let _format-float(f, decsep: "auto", thousandsep: "#h(0.166667em)") = {
  /// Formats a float with thousands separator.
  /// - `f`: Float to format.
  /// - `decsep`: Which decimal separator to use. This must be the same as the one used in `f`. Set it to `auto` to automatically choose it. Falls back to `.`.
  /// - `thousandsep`: The seperator between the thousands.
  let string = ""
  if decsep == "auto" {
    if "," in f {
      decsep = ","
    } else{
      decsep = "."
    }
  }

  let split = str(f).split(decsep)
  let int-part = split.at(0)
  let dec-part = split.at(1, default: none)
  let int-list = int-part.clusters()

  for (i, n) in int-list.enumerate() {
    let mod = (i - int-list.len()) / 3
    if int(mod) == mod {
      string += " " + thousandsep + " "
    }
    string += str(n)
  }

  if dec-part != none {
    let dec-list = dec-part.clusters()
    string += decsep
    for (i, n) in dec-list.enumerate() {
      let mod = i / 3
      if int(mod) == mod and i != 0 {
        string += " " + thousandsep + " "
      }
      string += str(n)
    }
  }

  string
}

#let _format-num(value, exponent: none, upper: none, lower: none, thousandsep: "#h(0.166667em)") = {
  /// Format a number.
  /// - `value`: Value of the number.
  /// - `exponent`: Exponent in the exponential notation.
  /// - `upper`: Upper uncertainty.
  /// - `lower`: Lower uncertainty.
  /// - `thousandsep`: The seperator between the thousands of the float.

  let formatted-value = ""
  if value != none {
    formatted-value += _format-float(value, thousandsep: thousandsep).replace(",", ",#h(0pt)")
  }
  if upper != none and lower != none {
    if upper != lower {
      formatted-value += "^(+" + _format-float(upper, thousandsep: thousandsep) + ")"
      formatted-value += "_(-" + _format-float(lower, thousandsep: thousandsep) + ")"
    } else {
      formatted-value += " plus.minus " + _format-float(upper, thousandsep: thousandsep).replace(",", ",#h(0pt)")
    }
  } else if upper != none {
    formatted-value += " plus.minus " + _format-float(upper, thousandsep: thousandsep).replace(",", ",#h(0pt)")
  } else if lower != none {
    formatted-value += " plus.minus " + _format-float(lower, thousandsep: thousandsep).replace(",", ",#h(0pt)")
  }
  if not (upper == none and lower == none) {
    formatted-value = "lr((" + formatted-value
    formatted-value += "))"
  }
  if exponent != none {
    if value != none {
      formatted-value += " dot "
    }
    formatted-value += "10^(" + str(exponent) + ")"
  }
  formatted-value
}

#let num(value, thousandsep: "#h(0.166667em)") = {
  /// Format a number.
  /// - `value`: String with the number.
  /// - `thousandsep`: The seperator between the thousands of the float.

  value = str(value).replace(" ", "")//.replace(",", ".")
  let match-value = value.match(re-num)
  assert.ne(match-value, none, message: "invalid number: " + value)
  let captures-value = match-value.captures

  let upper = none
  let lower = none
  if captures-value.at(14) != none {
    upper = captures-value.at(14)
    lower = none
  } else {
    upper = captures-value.at(5)
    lower = captures-value.at(7)
  }

  let formatted = _format-num(
    captures-value.at(0),
    exponent: captures-value.at(17),
    upper: upper,
    lower: lower,
    thousandsep: thousandsep
  )

  formatted = "$" + formatted + "$"
  eval(formatted)
}

#let _fix-csv(path, delimiter: ",") = {
  /// Load a CSV file with pre- or postfixes.
  /// - `path`: Path of the CSV file.
  /// - `delimiter`: Passed to the `csv` function.

  let array = csv(path, delimiter: delimiter)
  let dict = (:)

  for line in array {
    dict.insert(lower(line.at(0)), line.at(1))
  }
  dict
}

#let _unit-csv(path, delimiter: ",") = {
  /// Load a CSV file with units.
  /// - `path`: Path of the CSV file.
  /// - `delimiter`: Passed to the `csv` function.

  let array = csv(path, delimiter: delimiter)
  let units = (:)
  let units-space = (:)

  for line in array {
    units.insert(line.at(0), line.at(1))
    if line.at(2) == "false" or line.at(2) == "0" {
      units-space.insert(lower(line.at(0)), false)
    } else {
      units-space.insert(lower(line.at(0)), true)
    }
  }
  (units, units-space)
}

#let prefixes = _fix-csv("prefixes.csv")
#let (units, units-space) = _unit-csv("units.csv")
#let postfixes = _fix-csv("postfixes.csv")

#let _format-unit(string, space: "#h(0.166667em)") = {
  /// Format a unit.
  /// - `string`: String containing the unit.
  /// - `space`: Space between units.

  let formatted = ""

  // whether per was used
  let per = false
  // whether waiting for a postfix
  let post = false
  // one unit
  let unit = ""

  let split = lower(string).split(" ")
  split.push("")

  for u in split {
    // expecting postfix
    if post {
      // add postfix
      if u in postfixes {
        if per {
          unit += "^(-"
        } else {
          unit += "^("
        }
        unit += postfixes.at(u)
        unit += ")"
        per = false
        post = false

        formatted += unit
        unit = ""
        continue
      // add per
      } else if per {
        unit += "^(-1)"
        per = false
        post = false

        formatted += unit
        unit = ""
      // finish unit
      } else {
        post = false

        formatted += unit
        unit = ""
      }
    }

    // detected per
    if u == "per" {
      per = true
    // add prefix
    } else if u in prefixes {
      unit += prefixes.at(u)
    // add unit
    } else if u in units {
      unit += units.at(u)
      if units-space.at(u) {
        unit = space + unit
      }
      post = true
    } else if u != "" {
      panic("invalid unit: " + u)
    }
  }
  formatted
}

#let unit(unit, space: "#h(0.166667em)") = {
  /// Format a unit.
  /// - `unit`: String containing the unit.
  /// - `space`: Space between units.

  let formatted-unit = ""
  formatted-unit = _format-unit(unit, space: space)

  let formatted = "$" + formatted-unit + "$"
  eval(formatted)
}

#let qty(value, unit, rawunit: false, space: "#h(0.166667em)", thousandsep: "#h(0.166667em)") = {
  /// Format a quantity (i.e. number with a unit).
  /// - `value`: String containing the number.
  /// - `unit`: String containing the unit.
  /// - `rawunit`: Whether to transform the unit or keep the raw string.
  /// - `space`: Space between units.
  /// - `thousandsep`: The seperator between the thousands of the float.

  value = str(value).replace(" ", "")
  let match-value = value.match(re-num)
  assert.ne(match-value, none, message: "invalid number: " + value)
  let captures-value = match-value.captures

  let upper = none
  let lower = none
  if captures-value.at(14) != none {
    upper = captures-value.at(14)
    lower = none
  } else {
    upper = captures-value.at(5)
    lower = captures-value.at(7)
  }

  let formatted-value = _format-num(
    captures-value.at(0),
    exponent: captures-value.at(17),
    upper: upper,
    lower: lower,
    thousandsep: thousandsep
  )

  let formatted-unit = ""
  if rawunit {
    formatted-unit = space + unit
  } else {
    formatted-unit = _format-unit(unit, space: space)
  }

  let formatted = "$" + formatted-value + formatted-unit + "$"
  eval(formatted)
}

#let _format-range(
  lower, upper, exponent-lower: none, exponent-upper: none,
  delimiter: "-", space: "#h(0.16667em)", thousandsep: "#h(0.166667em)", force-parentheses: false
) = {
  /// Format a range.
  /// - `(lower, upper)`: Strings containing the numbers.
  /// - `(exponent-lower, exponent-upper)`: Strings containing the exponentials in exponential notation.
  /// - `delimiter`: Symbol between the numbers.
  /// - `space`: Space between the numbers and the delimiter.
  /// - `thousandsep`: The seperator between the thousands of the float.
  /// - `force-parentheses`: Whether to force parentheses around the range.

  let formatted-value = ""

  formatted-value += _format-float(lower, thousandsep: thousandsep).replace(",", ",#h(0pt)")
  if exponent-lower != exponent-upper and exponent-lower != none {
    if lower != none {
      formatted-value += "dot "
    }
    formatted-value += "10^(" + str(exponent-lower) + ")"
  }
  formatted-value += space + delimiter + space + _format-float(upper, thousandsep: thousandsep).replace(",", ",#h(0pt)")
  if exponent-lower != exponent-upper and exponent-upper != none {
    if upper != none {
      formatted-value += "dot "
    }
    formatted-value += "10^(" + str(exponent-upper) + ")"
  }
  if exponent-lower == exponent-upper and (exponent-lower != none and exponent-upper != none) {
    formatted-value = "lr((" + formatted-value
    formatted-value += ")) dot 10^(" + str(exponent-lower) + ")"
  } else if force-parentheses {
    formatted-value = "lr((" + formatted-value
    formatted-value += "))"
  }
  formatted-value
}

#let numrange(lower, upper, delimiter: "-", space: "#h(0.16667em)", thousandsep: "#h(0.166667em)") = {
  /// Format a range.
  /// - `(lower, upper)`: Strings containing the numbers.
  /// - `delimiter`: Symbol between the numbers.
  /// - `space`: Space between the numbers and the delimiter.
  /// - `thousandsep`: The seperator between the thousands of the float.
  lower = str(lower).replace(" ", "")
  let match-lower = lower.match(re-num)
  assert.ne(match-lower, none, message: "invalid lower number: " + lower)
  let captures-lower = match-lower.captures

  upper = str(upper).replace(" ", "")
  let match-upper = upper.match(re-num)
  assert.ne(match-upper, none, message: "invalid upper number: " + upper)
  let captures-upper = match-upper.captures

  let formatted = _format-range(
    captures-lower.at(0),
    captures-upper.at(0),
    exponent-lower: captures-lower.at(17),
    exponent-upper: captures-upper.at(17),
    delimiter: delimiter,
    thousandsep: thousandsep,
    space: space,
  )
  formatted = "$" + formatted + "$"

  eval(formatted)
}

#let qtyrange(
  lower, upper, unit, rawunit: false, delimiter: "-", space: "",
  unitspace: "#h(0.16667em)", thousandsep: "#h(0.166667em)"
) = {
  /// Format a range with a unit.
  /// - `(lower, upper)`: Strings containing the numbers.
  /// - `unit`: String containing the unit.
  /// - `rawunit`: Whether to transform the unit or keep the raw string.
  /// - `delimiter`: Symbol between the numbers.
  /// - `space`: Space between the numbers and the delimiter.
  /// - `unitspace`: Space between units.
  /// - `thousandsep`: The seperator between the thousands of the float.

  lower = str(lower).replace(" ", "")
  let match-lower = lower.match(re-num)
  assert.ne(match-lower, none, message: "invalid lower number: " + lower)
  let captures-lower = match-lower.captures

  upper = str(upper).replace(" ", "")
  let match-upper = upper.match(re-num)
  assert.ne(match-upper, none, message: "invalid upper number: " + upper)
  let captures-upper = match-upper.captures

  let formatted-value = _format-range(
    captures-lower.at(0),
    captures-upper.at(0),
    exponent-lower: captures-lower.at(17),
    exponent-upper: captures-upper.at(17),
    delimiter: delimiter,
    space: space,
    thousandsep: thousandsep,
    force-parentheses: true
  )

  let formatted-unit = ""
  if rawunit {
    formatted-unit = space + unit
  } else {
    formatted-unit = _format-unit(unit, space: unitspace)
  }

  let formatted = "$" + formatted-value + formatted-unit + "$"
  eval(formatted)
}
