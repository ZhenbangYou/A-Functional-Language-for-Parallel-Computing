@main def main() = {
    val a = FloatType("a")
    val b = FloatType("b")
    val c = FloatType("c")
    val d = FloatType("d")
    val e = a + b + c
    println(a * b + c / d - (-a))
    println((a < b && d != c).codeGen)
    println(GlobalFunc("add")(a, b)(a == b))
    println(GlobalFunc("idx")()(Index.idx))
    println(DeviceFunc("add")(a, b)(a == b))
    println(DeviceFunc("idx")()(Index.idx))
    val arr = OneDimFloatArrayType("arr")(IntType("n"))
    println(DeviceFunc("access")(arr)(arr(Index.idx)))
    println(GlobalFunc("inc")(a)(a + 1))
    val i = IntType("i")
    println(DeviceFunc("inc")(i)(i + 1))
    println(DeviceFunc("branch")(i, a, b)(If(i < Index.idx) {
        a
    } {
        b
    }))
    println(GlobalFunc("branch")(i, a, b)(If(i < Index.idx) {
        a
    } {
        b
    }))
    val n = IntType("n")
    val arrA = OneDimFloatArrayType("arrA")(n)
    val arrB = OneDimFloatArrayType("arrB")(n)
    println(
        GlobalFunc("min")(arrA, arrB)(
            If(arrA(Index.idx) < arrB(Index.idx)) {
                arrA(Index.idx)
            } {
                arrB(Index.idx)
            }
        )
    )
    println(
        DeviceFunc("min")(arrA, arrB)(
            If(arrA(Index.idx) < arrB(Index.idx)) {
                arrA(Index.idx)
            } {
                arrB(Index.idx)
            }
        )
    )
}

