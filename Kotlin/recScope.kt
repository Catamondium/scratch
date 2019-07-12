interface plussyString {
    // Extension only exists within subclasses? Yes
    operator fun String.unaryPlus()
}

open class Liner : plussyString {
    var body = mutableListOf<String>()
    override operator fun String.unaryPlus() {
        body.add(this)
    }

    override fun toString() = body.joinToString(separator="\n")
}

class Indenter(val pref: String = "\t") : Liner() {
    override fun toString() = body.map({pref + it}).joinToString(separator="\n")
}

fun liner(init: Liner.() -> Unit): Liner {
    return Liner().apply(init)
}

fun indenter(pref: String = "\t", init: Indenter.() -> Unit) : Indenter {
    return Indenter(pref).apply(init)
}

fun main() {
    //println(+"Hello, world!!!") // Unresolved reference: +
    println(liner {
        +"1: ASS"
        +"2: BASS"
        +"3: CASS"
    })

    println(indenter("\t") {
        +"a: ASS"
        +"b: BASS"
    })

}