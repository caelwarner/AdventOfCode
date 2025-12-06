package util.kotlin

fun String.splitAt(indices: List<Int>): List<String> =
    (listOf(0) + indices + listOf(this.length))
        .zipWithNext { a, b -> this.substring(a, b) }