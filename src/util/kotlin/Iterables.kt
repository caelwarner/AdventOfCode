package util.kotlin

fun <A, B, C, R> List<A>.zip3(b: List<B>, c: List<C>, transform: (A, B, C) -> R): List<R> =
    indices.map { transform(this[it], b[it], c[it]) }

fun <T> List<List<T>>.transpose(): List<List<T>> =
    this[0].indices.map { i -> this.map { it[i] } }