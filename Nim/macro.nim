import macros
import sequtils

# Some arrow syntax from Lisp-syntaxes
# Will NOT implement -<> magic wand
# w/o a recursive subsitution function

proc addCall(pre: static[bool] = true, para, callee: NimNode): NimNode =
    ## insert syntactic para into call
    case callee.kind:
        of nnkIdent:
            result = newCall(callee, para)
        of nnkCall:
            result = newCall(callee[0])
            when pre:
                result.add(para)

            for arg in callee[1..^1]:
                result.add(arg)

            when not pre:
                result.add(para)
        else:
            raise newException(ValueError, "Invalid call")

macro `->`(body: untyped): untyped =
    ## Pre-inserting threading operator
    body.expectMinLen(1)
    foldl(body, addCall(pre = true, a, b))

macro `->>`(body: untyped): untyped =
    ## Post-inserting threading operator
    body.expectMinLen(1)
    foldl(body, addCall(pre = false, a, b))

proc passecho[T](a: T): T =
    echo a
    a

block:
    echo "postarrow"
    let v = `->>`:
        5
        `-`(3)
        passecho
    echo v
block:
    echo "prearrow"
    let v = `->`:
        5
        `-`(3)
        passecho
    echo v