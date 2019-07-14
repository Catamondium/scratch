/* VARIANCE
 * Any <- Number <- Int // Covariant <-, out
 * Int <- Number <- Any // Contravariant <-, in
 *
 * Covariant param: Y isa X, Number isa Any
 * Contravariant: X contains Ys, Array<Any> contains Numbers
 */

// implicit Array<in T>, contravariant
fun <T>fill(arr: Array<T>, value: T) {
    for (x in 0 until arr.size) {
        arr[x] = value
    }
}

// source must be `out T` or reciever `in T`
fun <T>copy(source: Array<out T>, reciever: Array<T>) {
    assert(source.size == reciever.size)
    for(x in 0 until source.size) {
        reciever[x] = source[x]
    }
}

fun main() {
    var a: Array<Any?> = arrayOfNulls(5)
    var i: Array<Int?> = arrayOf(0, 1, 2, 3, null)
    copy(i, a)
    println(a.joinToString(separator=", "))
}