#import "lib.typ": *

#add-prefix("test", "t", "upright(t)")
#add-postfix("test", "upright(t)")
#add-unit("test", "t", "upright(t)")
// #for i in range(10000) {
#qty("-1234.567+23-15e-23", "kg m^2/s^2")
// }

#qtyrange("1234.567e-23", "1234.567e-22", "b")
// #for i in range(10000) {
//   unit("kg m / s^(2/3) / nb")
// }
