import terminal
import os
import colors

proc `*`(s: string, n: Natural): string =
    result = ""
    for _ in 0..<n:
        result &= s

hideCursor(stdout)
for i in 0..<15:
    let str = "#" * i
    styledWrite(stdout, ansiBackgroundColorCode(Color(0x0000FF)), str & "\r") # force to beginning of line
    flushFile(stdout) # ~unbuffered
    sleep(500)
write(stdout, "\n")