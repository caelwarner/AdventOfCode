package util.kotlin

fun List<String>.findChar(c: Char): Vec2 =
    this
        .map { it.indexOf(c) }
        .indexOfFirst { it != -1 }
        .let { row -> Vec2(this[row].indexOf(c), row) }

fun List<String>.findAllChars(c: Char): List<Vec2> =
    this
        .mapIndexed { rowIndex, row ->
            row.mapIndexedNotNull { colIndex, char ->
                if (char == c) Vec2(colIndex, rowIndex) else null
            }
        }
        .flatten()
