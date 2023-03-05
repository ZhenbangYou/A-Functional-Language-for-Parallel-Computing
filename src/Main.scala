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
}

