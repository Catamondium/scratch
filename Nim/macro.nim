import macros
import sequtils

proc appendCall(para, callee: NimNode): NimNode =
    case callee.kind:
        of nnkIdent:
            result = newCall(callee, para)
        of nnkCall:
            result = newCall(callee[0])
            for arg in callee[1..^1]:
                result.add(arg)
            result.add(para)
        else:
            raise newException(ValueError, "Invalid call")

macro pThreaded(body: untyped): untyped =
    body.expectMinLen(1)
    foldl(body, appendCall(a, b))

proc passecho[T](a: T): T =
    echo a
    a
let v = pThreaded:
    5
    `+`(3)
    passecho
echo v