class color(val rgb: Int) : Comparable<color> {
    override fun compareTo(other: color): Int = rgb.compareTo(other.rgb)
    operator fun inc() = color(rgb + 1)
    operator fun rangeTo(other: color) = colorRange(this, other)
    override fun toString() = "color($rgb)"
}

class colorRange(override val start: color, override val endInclusive: color) : ClosedRange<color>, Iterable<color> {
   override fun iterator(): Iterator<color> = colorIterator(start, endInclusive)
}

class colorIterator(val start: color, val endInclusive: color) : Iterator<color> {
    var initval = start
    override operator fun hasNext(): Boolean = initval <= endInclusive
    override operator fun next(): color = initval++
}

fun main() {
    for (c in color(0)..color(10)) {
        println(c)
    }
}