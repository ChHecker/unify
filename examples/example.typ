// #import "@preview/unify:0.8.1": *
#import "../lib.typ": *

#set text(lang: "en")
Working with English characters:
$ num("-1.32865+-0.50273e-6") $
$ qty("1.3+1.2-0.3e3", "erg/cm^(3/2)/s", space: "dot", num-unit-space: "#h(2mm)") $
$ numrange("1,1238e-2", "3,0868e5", thousandsep: "'") $
$ qtyrange("1e3", "2e3", "meter per second squared", per: "/", delimiter: "\"to\"", range-unit-space: "#h(3mm)") $
$ qty("55.36", "usd") $

Adding your own prefix and unit:
#add-prefix("pre", "P", "upright(\"pre\")")
#add-unit("unit", "U", "bold(\"unit\")")
$ unit("PU") $

Changing default arguments to the functions:
#update-num-config("decsep", ",")
#update-num-config("thousandsep", ".")
$ num("-1.32865+-0.50273e-6") $

#update-num-config("decsep", ".")
#update-num-config("thousandsep", "#h(0.166667em)")
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
