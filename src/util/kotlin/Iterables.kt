package util.kotlin

fun <A, B, C, R> List<A>.zip3(b: List<B>, c: List<C>, transform: (A, B, C) -> R): List<R> =
    indices.map { transform(this[it], b[it], c[it]) }

fun <T> List<List<T>>.transpose(): List<List<T>> =
    if (this.isEmpty() || this[0].isEmpty()) emptyList()
    else listOf(this.map { it.first() }) + this.map { it.drop(1) }.transpose()