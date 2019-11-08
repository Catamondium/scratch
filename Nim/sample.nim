import sugar
import complex
import math

proc iroots[T](c: Complex[T], nth: int): seq[Complex[T]] =
  let (r, phi) = polar(c)
  let phis = lc[phi + float(n) * (TAU / float(nth)) | (n <- 0..<nth), T]
  let rroot = pow(r, float(-nth))
  result = lc[rect[T](rroot, phi = x) | (x <- phis), Complex[T]]

let c = complex64(1.0, 0)
echo TAU
for rt in iroots[float64](c, 4):
  echo polar(rt)
