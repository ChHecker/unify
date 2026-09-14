#import "lib.typ": num, unit

// #for i in range(10000) {
//   num("-1234.567+23-15e-23")
// }

#for i in range(10000) {
  unit("kg m / s^(2/3) / nb")
}
