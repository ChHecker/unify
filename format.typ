#import "init.typ": *

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

#let _get-num-conf(multiplier: none, thousandsep: none, decsep: none) = {
  let conf = _config.get().at("num")

  if multiplier == none {
    multiplier = conf.at("multiplier")
  }
  if thousandsep == none {
    thousandsep = conf.at("thousandsep")
  }
  if decsep == none {
    decsep = conf.at("decsep")
  }

  (multiplier: multiplier, thousand_sep: thousandsep, dec_sep: decsep)
}

#let _get-unit-conf(space: none, per: none, first-space: "") = {
  let conf = _config.get().at("unit")

  if space == none {
    space = conf.at("space")
  }
  if per == none {
    per = conf.at("per")
  }

  (space: space, space_first: first-space, per_mode: per)
}

#let _get-range-conf(delimiter: none, space: none) = {
  let conf = _config.get().at("range")

  if delimiter == none {
    delimiter = conf.at("delimiter")
  }
  if space == none {
    space = conf.at("space")
  }

  (delimiter: delimiter, space: space)
}
