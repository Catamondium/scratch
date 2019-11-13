import macros

proc subst(target: string, val, body: NimNode): NimNode =
    result = newNimNode(body.kind)
    for obj in body:
      if obj.kind == nnkIdent and obj.repr == target: # If target
        result.add(val)
      elif obj.len >= 1: # Branching nodes
        result.add(subst(target, val, obj))
      else: # Leaf nodes
        result.add(obj)

macro printall(body: untyped): untyped =
    result = subst("SUB", newLit(55), body)
    echo toStrLit(result)

let
  a = 5
  b = 15
printall:
  echo(a + b + SUB + -SUB)
  if a == 5:
    echo(SUB)
dumpTree:
  echo(a + b + 55 + -55)