#let unify-rs = plugin("unify.wasm")

#let num-config = (thousand_sep: " ", dec_sep: ".", multiplier: "dot")

#let unit-config = (
  space: "#h(0.166667em)",
  space_first: "",
  per_mode: "symbol",
)
#let units = (
  prefixes: (
    (long: "test", short: "t", symbol: "upright(\"t\")"),
  ),
  units: (),
  postfixes: (),
)

#let num = (
  config: num-config,
  num: "-1234.567+23-15e-23",
)
#let unit = (
  config: unit-config,
  units: units,
  unit: "tg m / s^(2/3) / nb",
)

// #for i in range(10000) {
#eval(
  str(
    unify-rs.qty(
      cbor.encode((
        num: num,
        unit: unit,
      )),
    ),
  ),
)
// }

#let range-config = (
  delimiter: "\"to\"",
  space: "#h(0.3em)",
)
#let numrange = (
  config_num: num-config,
  config_range: range-config,
  lower: "23e6",
  upper: "43e6",
)
#eval(
  str(
    unify-rs.numrange(
      cbor.encode(numrange),
    ),
  ),
)
