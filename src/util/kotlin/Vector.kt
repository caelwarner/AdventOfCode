package util.kotlin

data class Vec2(val x: Int, val y: Int) {
    operator fun plus(other: Vec2) = Vec2(x + other.x, y + other.y)
    operator fun minus(other: Vec2) = Vec2(x - other.x, y - other.y)
    operator fun times(scalar: Int) = Vec2(x * scalar, y * scalar)
    operator fun div(scalar: Int) = Vec2(x / scalar, y / scalar)
    
    fun down(): Vec2 = Vec2(x, y + 1)
    fun up(): Vec2 = Vec2(x, y - 1)
    fun left(): Vec2 = Vec2(x - 1, y)
    fun right(): Vec2 = Vec2(x + 1, y)
}