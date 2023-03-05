@main def main() = {
    val a = FloatType("a")
    val b = FloatType("b")
    val c = FloatType("c")
    val d = FloatType("d")
    val e = a + b + c
    println((a * b + c / d - (-a)).codeGen)
    println((a < b && d != c).codeGen)
    println(GlobalFunc("add")(a, b)(a == b).codeGen)
    println(GlobalFunc("idx")()(Index.idx).codeGen)
    println(DeviceFunc("add")(a, b)(a == b).codeGen)
    println(DeviceFunc("idx")()(Index.idx).codeGen)
    val arr = OneDimFloatArrayType("arr")(IntType("n"))
    println(DeviceFunc("access")(arr)(arr(Index.idx)).codeGen)
}

