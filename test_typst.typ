#import "lib.typ": *

#set text(lang: "ru")

#add-prefix("test", "t", "upright(t)")
#add-postfix("test", "upright(t)")
#add-unit("test", "t", "upright(t)")

#update-num-config("multiplier", "times")
#update-range-config("delimiter", "\"to\"")
#update-range-config("space", "#h(0.4em)")

#qty("-1234.567+23-15e-23", "kg m^2/s^2")

#qtyrange("1234.567e-23", "1234.567e-22", "dB")