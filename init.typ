#let format = plugin("format.wasm")

#let _units = state("units", (
  "prefixes": (),
  "units": (),
  "postfixes": (),
))

#let _config = state("config", (
  "global": (mode: "text"),
  "num": (multiplier: "dot", thousandsep: "#h(0.166667em)", decsep: "."),
  "unit": (space: "#h(0.166667em)", per: "symbol"),
  "range": (delimiter: "-", space: "#h(0.166667em)", exppos: "auto"),
  "qty": (unit-space: "#h(0.166667em)", rawunit: false),
  "qtyrange": (unitpos: "factor"),
))
