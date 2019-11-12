import sugar
import complex
import math
import terminal
import colors

proc iroots[T](c: Complex[T], nth: int): seq[Complex[T]] =
  ## docstring
  let (r, phi) = polar(c)
  let phis = lc[phi + float(n) * (TAU / float(nth)) | (n <- 0..<nth), T]
  let rroot = pow(r, float(-nth))
  result = lc[rect[T](rroot, phi = x) | (x <- phis), Complex[T]]

let c = complex64(1.0, 0)
echo TAU
for rt in iroots[float64](c, 4):
  echo polar(rt)

# Terminal manipulation

proc bg(c: Color): string = ansiBackgroundColorCode(c)
proc fg(c: Color): string = ansiForegroundColorCode(c)

let str = readPasswordFromStdin() # getpass
styledEcho(fg(Color(0xFF0000)), bg(Color(0x00FF00)), str)
