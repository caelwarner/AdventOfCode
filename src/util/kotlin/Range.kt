package util.kotlin

infix fun LongRange.intersect(other: LongRange): LongRange? {
    val start = maxOf(this.first, other.first)
    val end = minOf(this.last, other.last)
    return if (start <= end) start..end else null
}

infix fun LongRange.union(other: LongRange): List<LongRange> {
    val (r1, r2) = listOf(this, other).sortedBy { it.first }

    return if (r1.first <= r2.last + 1 && r2.first <= r1.last + 1) {
        listOf(minOf(r1.first, r2.first)..maxOf(r1.last, r2.last))
    } else {
        listOf(r1, r2)
    }
}
