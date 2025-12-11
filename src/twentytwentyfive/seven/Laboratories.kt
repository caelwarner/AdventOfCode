package twentytwentyfive.seven

import util.kotlin.Part
import util.kotlin.Read
import util.kotlin.Vec2
import util.kotlin.findAllChars
import util.kotlin.findChar
import util.kotlin.runPart

fun main() {
    runPart(Part.ONE) { countBeamSplits(Read.asStringList("twentytwentyfive/seven/input.txt")) }
    runPart(Part.ONE) { countBeamTimelines(Read.asStringList("twentytwentyfive/seven/input.txt")) }
}

private fun countBeamSplits(input: List<String>): Int =
    stepBeams(input.size, listOf(input.findChar('S')), input.findAllChars('^').toSet())

private fun countBeamTimelines(input: List<String>): Long =
    stepBeam(input.size, input.findChar('S'), input.findAllChars('^').toSet())

private fun stepBeams(height: Int, beams: List<Vec2>, splitters: Set<Vec2>): Int =
    beams
        .flatMap { beam ->
            if (beam.down() in splitters) listOf(beam.down().left(), beam.down().right()) 
            else listOf(beam + Vec2(0, 1)) 
        }
        .let { next ->
            if (next[0].y >= height) next.size - beams.size
            else stepBeams(height, next.toSet().toList(), splitters) + next.size - beams.size
        }

val stepBeamMemo = mutableMapOf<Vec2, Long>()

private fun stepBeam(height: Int, beam: Vec2, splitters: Set<Vec2>): Long =
    stepBeamMemo.getOrPut(beam) {
        when {
            beam.down() in splitters -> stepBeam(height, beam.down().left(), splitters) + stepBeam(height, beam.down().right(), splitters)
            beam.y >= height -> 1L
            else -> stepBeam(height, beam.down(), splitters)
        }
    }