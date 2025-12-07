package everybodycodes.events.thesongofducksanddragons.two

import util.kotlin.Part
import util.kotlin.Read
import util.kotlin.runPart

fun main() {
    runPart(Part.ONE) { solveComplexNumberProblem(Read.asString("everybodycodes/events/thesongofducksanddragons/two/input1.txt")) }
    runPart(Part.TWO) { countEngravedPoints(Read.asString("everybodycodes/events/thesongofducksanddragons/two/input2.txt"), 100) }
    runPart(Part.THREE) { countEngravedPoints(Read.asString("everybodycodes/events/thesongofducksanddragons/two/input3.txt"), 1000) }
}

private fun solveComplexNumberProblem(input: String): String {
    val a = parseComplexNumber(input)
    var res = 0L to 0L
    
    repeat(3) {
        res = res
            .complexMultiply(res)
            .complexDivide(10L to 10L)
            .complexAdd(a)
    }
    
    return "[${res.first},${res.second}]"
}

private fun countEngravedPoints(input: String, resolution: Int): Int {
    val a = parseComplexNumber(input)
    var count = 0
    
    for (y in 0L..resolution) {
        cols@ for (x in 0L..resolution) {
            val point = a.complexAdd(x * 1000L / resolution to y * 1000L / resolution)
            var res = 0L to 0L
            
            repeat(100) { 
                res = res
                    .complexMultiply(res)
                    .complexDivide(100000L to 100000L)
                    .complexAdd(point)
                
                if (res.first > 1000000L || res.first < -1000000L || res.second > 1000000L || res.second < -1000000L) {
                    continue@cols
                }
            }
            
            count++
        }
    }
        
    return count
}

private fun parseComplexNumber(input: String): Pair<Long, Long> =
    """\[(-?\d+),(-?\d+)\]"""
        .toRegex()
        .find(input)!!
        .destructured
        .let { (a, b) -> a.toLong() to b.toLong() }

private fun Pair<Long, Long>.complexAdd(other: Pair<Long, Long>): Pair<Long, Long> =
    Pair(this.first + other.first, this.second + other.second)

private fun Pair<Long, Long>.complexMultiply(other: Pair<Long, Long>): Pair<Long, Long> {
    val realPart = this.first * other.first - this.second * other.second
    val imaginaryPart = this.first * other.second + this.second * other.first
    return Pair(realPart, imaginaryPart)
}

private fun Pair<Long, Long>.complexDivide(other: Pair<Long, Long>): Pair<Long, Long> =
    Pair(this.first / other.first, this.second / other.second)
