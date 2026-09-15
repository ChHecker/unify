#let wasm = plugin("unify.wasm")

#let _units = state("units", (
  "prefixes": (),
  "units": (),
  "postfixes": (),
))

#let _config = state("config", (
  "num": (multiplier: "dot", thousandsep: "#h(0.166667em)", decsep: "."),
  "unit": (space: "#h(0.166667em)", per: "symbol"),
  "range": (delimiter: "-", space: "#h(0.166667em)"),
  "qty": (unit-space: "#h(0.166667em)")
))
