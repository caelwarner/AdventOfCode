package util.kotlin

fun <A, B, C, R> List<A>.zip3(b: List<B>, c: List<C>, transform: (A, B, C) -> R): List<R> =
    indices.map { transform(this[it], b[it], c[it]) }