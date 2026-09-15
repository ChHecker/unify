#import "init.typ": *

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

#let _get-units() = {
  let units = _units.get()
  units.insert("lang", text.lang)
  units
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

  if thousandsep == "," {
    thousandsep = ",#h(0pt)"
  }
  if decsep == "," {
    decsep = ",#h(0pt)"
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
