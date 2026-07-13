#import "@local/unify:0.8.1": *

#set text(lang: "en")
Working with English characters:
$ num("-1.32865+-0.50273e-6") $
$ qty("1.3+1.2-0.3e3", "erg/cm^2/s", space: "dot", num-unit-space: "#h(2mm)") $
$ numrange("1,1238e-2", "3,0868e5", thousandsep: "'") $
$ qtyrange("1e3", "2e3", "meter per second squared", per: "/", delimiter: "\"to\"", range-unit-space: "#h(3mm)") $
$ qty("55.36", "usd") $

Rendering in the document font with `mode: "text"`:

The speed of light is #qty("2.99792458e8", "m/s", mode: "text"), *even in bold: #qty("2.99792458e8", "m/s", mode: "text")*, compared to math mode #qty("2.99792458e8", "m/s").

Adding your own prefix and unit:
#add-prefix("pre", "P", "upright(\"pre\")")
#add-unit("unit", "U", "bold(\"unit\")")
$ unit("PU") $

#set text(lang: "ru")
Работа пакета с русскими символами:
$ num("-1.32865+-0.50273e-6") $
$ qty("1.3+1.2-0.3e3", "erg/cm^2/s", space: "#h(2mm)") $
$ numrange("1,1238e-2", "3,0868e5", thousandsep: "'") $
$ qtyrange("1e3", "2e3", "meter per second squared", per: "/", delimiter: "\"до\"") $

#set text(lang: "de")
Other languages fall back to English units:
$ num("-1.32865+-0.50273e-6") $
$ qty("1.3+1.2-0.3e3", "erg/cm^2/s", space: "#h(2mm)") $
$ numrange("1,1238e-2", "3,0868e5", thousandsep: "'") $
$ qtyrange("1e3", "2e3", "meter per second squared", per: "/", delimiter: "\"to\"") $
