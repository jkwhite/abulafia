class Link(
    val src:Word,
    val dst:Word,
    val distance:Int,
    var strength:Long
    ) {


    def this(src:Word, dst:Word, distance:Int) = {
        this(src, dst, distance, 1)
    }

    def strengthen() = {
        strength = 1+strength
    }
}
