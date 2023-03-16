@main def main(): Unit = {
    val a = FloatType("a")
    val b = FloatType("b")
    val c = FloatType("c")
    println(
        GlobalFunc("multiplyAdd")(a, b, c) {
            a * b + c
        }
    )
    println(
        DeviceFunc("multiplyAdd")(a, b, c) {
            a * b + c
        }
    )
    val n = IntType("n")
    val alpha = FloatType("alpha")
    val x = OneDimFloatArrayType("x")(n)
    val y = OneDimFloatArrayType("y")(n)
    println(
        GlobalFunc("saxpy")(alpha, x, y) {
            alpha * x + y
        }
    )
    val x2 = x.createStaticArray(-1, 1)
    println(
        GlobalFunc("blur")(x) {
            x2.map(_ => (
                x2(x2.getIndex - 1) +
                    x2(x2.getIndex) +
                    x2(x2.getIndex + 1)) / 3)
        }
    )
    val fn = DeviceFunc("plus")(a, b) {
        a + b
    }
    println(GlobalFunc("reduce")(x) {
        x.reduceInBlock((a, b) => fn(a, b))
    })

    val size = IntType("n")
    val arrayA = OneDimFloatArrayType("arrayA")(size)
    val arrayB = OneDimFloatArrayType("arrayB")(size)
    println(
        GlobalFunc("arrayAdd")(arrayA, arrayB) {
            arrayA + arrayB
        }
    )
    println(
        GlobalFunc("arrayAdd")(arrayA, arrayB) {
            arrayA.zipWith(arrayB)(_ + _)
        }
    )
}