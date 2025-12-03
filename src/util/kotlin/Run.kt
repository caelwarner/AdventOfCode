package util.kotlin

import kotlin.time.measureTimedValue

enum class Part(val value: Int) {
    ONE(1),
    TWO(2)
}

fun <T> runPart(part: Part, f: () -> T) {
    val (result, duration) = measureTimedValue { f() }
    println("[Part ${part.value}] \u001b[92m$result \u001b[0m| \u001b[34m$duration\u001b[0m")
}
