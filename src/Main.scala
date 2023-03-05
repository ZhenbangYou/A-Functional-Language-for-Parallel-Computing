@main def main() = {
    val a = FloatType("a")
    val b = FloatType("b")
    val c = FloatType("c")
    val d = FloatType("d")
    println((a*b+c/d-(-a)).codeGen)
    println((a<b&&d!=c).codeGen)
    println(Func("add")(List(a,b))(a+b).codeGen)
}

