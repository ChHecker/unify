// update this to true to enable alt-texts
#let enable-alt-text = state("alt-text-enable", false)

// cache the alt-text dictionary
// format: _alt-text-lang-dict[key][language]
#let _alt-text-lang-dict = {
  let _csv = csv("alt-translations.csv", delimiter: ",")

  let dict = (:)
  // the first row contains the language-names
  let langs = _csv.at(0).slice(1)

  // iterate over the different keys
  for row in _csv.slice(1) {
    let text-key = row.at(0)
    let values = row.slice(1)
    let lang-dict = values
      .enumerate()
      .map(entry => {
        let ii = entry.at(0) // the enumeration value
        let translated-value = entry.at(1) // the text for the current language

        (langs.at(ii), translated-value)
      })
      .to-dict()

    dict.insert(text-key, lang-dict)
  }

  dict
}

// shortcut-function to extract values from the alt-text-dictionary without specifying the language every time
#let _get-text(key) = {
  let translations = _alt-text-lang-dict.at(key)
  let lang = text.lang

  assert(
    lang in translations.keys(),
    message: "language '"
      + lang
      + "' isn't supported for alt-texts YET. See the errors origin in sourcecode to learn more.",
  )
  // To add support for a language it must be included in alt-translations.csv
  // Add a new column with the language and the corresponding languages and create a PR on github

  translations.at(lang)
}

// pieces together the alt-text for numbers with the words from the dictionary
#let _create-alt-text-for-number(value, upper: none, lower: none, exponent: none) = {
  if value != none {
    value

    // add uncertainity
    if upper != none or lower != none {
      if upper == lower {
        " plus minus " + upper
      } else {
        if upper != none {
          " "
          _get-text("plus")
          " "
          upper
        }
        if lower != none {
          " "
          _get-text("minus")
          " "
          lower
        }
      }
    }

    // connect with exponent
    if exponent != none {
      " "
      _get-text("times")
      " "
    }
  }
  if exponent != none {
    "10 "
    _get-text("power")
    " "
    exponent
  }
}
