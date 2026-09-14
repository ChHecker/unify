#import "init.typ": *

#let _re-num = regex(
  "^(-?\d+(\.|,)?\d*)?(((\+(\d+(\.|,)?\d*)-(\d+(\.|,)?\d*)))|((((\+-)|(-\+)|(\u00B1))(\d+(\.|,)?\d*))))?((e|E)([-\+]?\d+))?$",
)

#let _unicode-exponents = (
  ("\u2070", "0"),
  ("\u00B9", "1"),
  ("\u00B2", "2"),
  ("\u00B3", "3"),
  ("\u2074", "4"),
  ("\u2075", "5"),
  ("\u2076", "6"),
  ("\u2077", "7"),
  ("\u2078", "8"),
  ("\u2079", "9"),
  ("\u207A", "+"),
  ("\u207B", "-"),
)

#let _to-string(it) = {
  if type(it) == str {
    it
  } else if type(it) != content {
    str(it)
  } else if it.has("text") {
    it.text
  } else if it.has("children") {
    it.children.map(_to-string).join()
  } else if it.has("body") {
    _to-string(it.body)
  } else if it == [ ] {
    " "
  } else {
    panic("invalid value")
  }
}

#let _format-float(f, decsep: "auto", thousandsep: "#h(0.166667em)") = {
  /// Formats a float with thousands separator.
  /// - `f`: Float to format.
  /// - `decsep`: Which decimal separator to use. This must be the same as the one used in `f`. Set it to `auto` to automatically choose it. Falls back to `.`.
  /// - `thousandsep`: The separator between the thousands.
  let string = ""
  if decsep == "auto" {
    if "," in f {
      decsep = ","
    } else {
      decsep = "."
    }
  }

  if thousandsep.trim() == "." {
    thousandsep = ".#h(0mm)"
  }

  let split = str(f).split(decsep)
  let int-part = split.at(0)
  let dec-part = split.at(1, default: none)
  let int-list = int-part.clusters()

  string += str(int-list.remove(0))
  for (i, n) in int-list.enumerate() {
    if int-list.len() > 4 {
      let mod = (i - int-list.len()) / 3
      if int(mod) == mod {
        string += " " + thousandsep + " "
      }
    }
    string += str(n)
  }

  if dec-part != none {
    let dec-list = dec-part.clusters()
    string += decsep
    for (i, n) in dec-list.enumerate() {
      if dec-list.len() > 4 {
        let mod = i / 3
        if int(mod) == mod and i != 0 {
          string += " " + thousandsep + " "
        }
      }
      string += str(n)
    }
  }

  string
}

#let _format-num(
  value,
  exponent: none,
  upper: none,
  lower: none,
  multiplier: "dot",
  thousandsep: "#h(0.166667em)",
) = {
  /// Format a number.
  /// - `value`: Value of the number.
  /// - `exponent`: Exponent in the exponential notation.
  /// - `upper`: Upper uncertainty.
  /// - `lower`: Lower uncertainty.
  /// - `multiplier`: The symbol used to indicate multiplication
  /// - `thousandsep`: The separator between the thousands of the float.

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
      formatted-value += " " + multiplier + " "
    }
    formatted-value += "10^(" + str(exponent) + ")"
  }
  formatted-value
}

#let _unicode-exponent-list = for (unicode, ascii) in _unicode-exponents {
  (unicode,)
}

#let _exponent-pattern = regex("[" + _unicode-exponent-list.join("|") + "]+")

#let _replace-unicode-exponents(unit-str) = {
  let exponent-matches = unit-str.matches(_exponent-pattern)
  let exponent = ""
  for match in exponent-matches {
    exponent = "^" + match.text
    for (unicode, ascii) in _unicode-exponents {
      exponent = exponent.replace(regex(unicode), ascii)
    }
    unit-str = unit-str.replace(match.text, exponent)
  }
  unit-str
}

#let _unit(symbol, exponent, space) = (symbol: symbol, exponent: exponent, space: space)

#let _format-unit-from-lists(normal-list, per-list, per, space, first-space) = {
  let formatted = ""

  if (
    (normal-list.len() > 0 and normal-list.at(0).at("space")) or (normal-list.len() == 0 and per-list.at(0).at("space"))
  ) {
    formatted += first-space
  }

  if per == "symbol" {
    for (i, chunk) in normal-list.enumerate() {
      let (symbol: n, exponent: exponent, space: space-set) = chunk
      if i != 0 and space-set {
        formatted += space
      }
      formatted += n
      if exponent != none {
        formatted += "^(" + exponent + ")"
      }
    }

    for (i, chunk) in per-list.enumerate() {
      let (symbol: p, exponent: exponent, space: space-set) = chunk
      if space-set and (i != 0 or normal-list.len() != 0) {
        formatted += space
      }
      formatted += p + "^(-"
      if exponent != none {
        formatted += exponent
      } else {
        formatted += "1"
      }
      formatted += ")"
    }
  } else if per == "fraction" or per == "/" {
    if per-list.len() > 0 {
      formatted += " ("
    }

    for (i, chunk) in normal-list.enumerate() {
      let (symbol: n, exponent: exponent, space: space-set) = chunk
      if i != 0 and space-set {
        formatted += space
      }
      formatted += n
      if exponent != none {
        formatted += "^(" + exponent + ")"
      }
    }

    if per-list.len() == 0 {
      return formatted
    }

    formatted += ")/("
    for (i, chunk) in per-list.enumerate() {
      let (symbol: p, exponent: exponent, space: space-set) = chunk
      if i != 0 and space-set {
        formatted += space
      }
      formatted += p
      if exponent != none {
        formatted += "^(" + exponent + ")"
      }
    }
    formatted += ")"
  } else if per == "fraction-short" or per == "\\/" {
    for (i, chunk) in normal-list.enumerate() {
      let (symbol: n, exponent: exponent, space: space-set) = chunk
      if i != 0 and space-set {
        formatted += space
      }
      formatted += n
      if exponent != none {
        formatted += "^(" + exponent + ")"
      }
    }

    for (i, chunk) in per-list.enumerate() {
      let (symbol: p, exponent: exponent, space: space-set) = chunk
      formatted += "\\/" + p
      if exponent != none {
        formatted += "^(" + exponent + ")"
      }
    }
  }

  formatted
}

#let _format-unit-short(
  string,
  units-short,
  units-short-space,
  prefixes-short,
  space: "#h(0.166667em)",
  first-space: "#h(0.166667em)",
  per: "symbol",
) = {
  /// Format a unit using the shorthand notation.
  /// - `string`: String containing the unit.
  /// - `space`: Space between units.
  /// - `per`: Whether to format the units after `/` with a fraction or exponent.

  assert(("symbol", "fraction", "/", "fraction-short", "short-fraction", "\\/").contains(per))

  let formatted = ""

  string = _replace-unicode-exponents(string)
  string = string.replace(regex(" */ *"), "/").replace(regex(" +"), " ")

  let paren-depth = 0
  let split = (("", false),) // the string and whether it's in the denominator
  for s in string {
    if s == " " {
      split.push(("", false))
    } else if s == "/" {
      if paren-depth == 0 {
        split.push(("", true))
      } else {
        split.at(-1).at(0) += "\/"
      }
    } else if s == "(" {
      paren-depth += 1
    } else if s == ")" {
      if paren-depth == 0 {
        panic("closing parenthesis without opening")
      }
      paren-depth -= 1
    } else {
      split.at(-1).at(0) += s
    }
  }

  if paren-depth != 0 {
    panic("unclosed parenthesis")
  }

  let chunks = ()
  for (s, p) in split {
    let exp-split = s.split("^")
    let exp = exp-split.at(1, default: none)
    chunks.push(_unit(exp-split.at(0), exp, p))
  }

  // needed for fraction formatting
  let normal-list = ()
  let per-list = ()

  let prefixes = ()
  for (symbol: symbol, exponent: exponent, space: per-set) in chunks {
    let u-space = true
    let prefix = none
    let unit = ""

    if symbol in units-short {
      // Match units without prefixes
      unit = units-short.at(symbol)
      u-space = units-short-space.at(symbol)
    } else {
      // Match prefix + unit
      let pre = ""
      for char in symbol.clusters() {
        pre += char
        // Divide `symbol` into `pre`+`u` and check validity
        if pre in prefixes-short {
          let u = symbol.trim(pre, at: start, repeat: false)
          if u in units-short {
            prefix = prefixes-short.at(pre)
            unit = units-short.at(u)
            u-space = units-short-space.at(u)

            pre = none
            break
          }
        }
      }
    }

    if unit == "" {
      panic("invalid unit: " + symbol)
    }

    if per-set {
      per-list.push(_unit(prefix + unit, exponent, u-space))
    } else {
      normal-list.push(_unit(prefix + unit, exponent, u-space))
    }
  }

  _format-unit-from-lists(normal-list, per-list, per, space, first-space)
}

#let _format-unit(string, space: "#h(0.166667em)", first-space: "#h(0.166667em)", per: "symbol") = {
  /// Format a unit using written-out words.
  /// - `string`: String containing the unit.
  /// - `space`: Space between units.
  /// - `per`: Whether to format the units after `per` with a fraction or exponent.

  assert(("symbol", "fraction", "/", "fraction-short", "short-fraction", "\\/").contains(per))

  string = _to-string(string)
  first-space = " " + first-space + " "
  space = " " + space + " "

  // load data
  let (units, units-short, units-space, units-short-space) = _units()

  let (prefixes, prefixes-short) = _prefixes()

  let formatted = ""

  // needed for fraction formatting
  let normal-list = ()
  let per-list = ()

  // whether per was used
  let per-set = false
  // whether waiting for a postfix
  let post = false
  // one unit
  let unit = _unit("", none, false)

  let split = lower(string).split(" ")
  split.push("")

  let has_prefix = false

  for (i, u) in split.enumerate() {
    if u == "" {
      continue
    }

    // expecting postfix
    if post {
      let is_postfix = u in _postfixes
      if is_postfix {
        unit.at("exponent") = _postfixes.at(u)
      }

      if per-set {
        per-list.push(unit)
      } else {
        normal-list.push(unit)
      }

      per-set = false
      post = false

      unit = _unit("", none, false)
      has_prefix = false

      if is_postfix {
        continue
      }
    }

    // detected per
    if u == "per" {
      per-set = true
      // add prefix
    } else if u in prefixes {
      if has_prefix {
        panic("double prefix")
      }

      has_prefix = true
      unit.at("symbol") += prefixes.at(u)
      // add unit
    } else if u in units {
      unit.at("symbol") += units.at(u)
      unit.at("space") = units-space.at(u)
      post = true
    } else if u != "" and i == 0 {
      return _format-unit-short(
        string,
        units-short,
        units-short-space,
        prefixes-short,
        space: space,
        per: per,
        first-space: first-space,
      )
    } else {
      panic("invalid unit: " + u)
    }
  }

  if has_prefix {
    panic("prefix without unit")
  }

  _format-unit-from-lists(normal-list, per-list, per, space, first-space)
}

#let _format-range(
  lower,
  upper,
  exponent-lower: none,
  exponent-upper: none,
  multiplier: "dot",
  delimiter: "-",
  space: "#h(0.16667em)",
  thousandsep: "#h(0.166667em)",
  force-parentheses: false,
) = {
  /// Format a range.
  /// - `(lower, upper)`: Strings containing the numbers.
  /// - `(exponent-lower, exponent-upper)`: Strings containing the exponentials in exponential notation.
  /// - `multiplier`: The symbol used to indicate multiplication
  /// - `delimiter`: Symbol between the numbers.
  /// - `space`: Space between the numbers and the delimiter.
  /// - `thousandsep`: The separator between the thousands of the float.
  /// - `force-parentheses`: Whether to force parentheses around the range.

  space = " " + space + " "
  let formatted-value = ""

  formatted-value += _format-num(lower, thousandsep: thousandsep).replace(",", ",#h(0pt)")
  if exponent-lower != exponent-upper and exponent-lower != none {
    if lower != none {
      formatted-value += multiplier + " "
    }
    formatted-value += "10^(" + str(exponent-lower) + ")"
  }
  formatted-value += (
    space
      + " "
      + delimiter
      + " "
      + space
      + _format-num(upper, thousandsep: thousandsep).replace(
        ",",
        ",#h(0pt)",
      )
  )
  if exponent-lower != exponent-upper and exponent-upper != none {
    if upper != none {
      formatted-value += multiplier + " "
    }
    formatted-value += "10^(" + str(exponent-upper) + ")"
  }
  if exponent-lower == exponent-upper and (exponent-lower != none and exponent-upper != none) {
    formatted-value = "lr((" + formatted-value
    formatted-value += ")) " + multiplier + " 10^(" + str(exponent-lower) + ")"
  } else if force-parentheses {
    formatted-value = "lr((" + formatted-value
    formatted-value += "))"
  }
  formatted-value
}
