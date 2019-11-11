import strformat

type
    A* = object
        id*: Natural

proc `$`(obj: A): string =
    fmt"A({obj.id:X})"

proc `=destroy`(obj: var A) =
    echo fmt"dtor {obj}"

proc `=sink`(dest: var A, src: A) =
    `=destroy`(dest)
    dest.id = src.id
    echo fmt"move {dest}"

echo "preblock"
block:
    let b = A(id: 0xA)
    echo b
echo "endblock"
var a = A(id: 0xB)
let b = move(a)
