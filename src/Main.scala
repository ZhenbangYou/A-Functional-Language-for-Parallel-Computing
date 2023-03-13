@main def main(): Unit = {
    val a = FloatType("a")
    val b = FloatType("b")
    val c = FloatType("c")
    val d = FloatType("d")
    println(a * b + c / d - (-a))
    println((a < b && d != c).codeGen)
    println(GlobalFunc("add")(a, b)(a + b))
    println(DeviceFunc("add")(a, b)(a + b))
    println(GlobalFunc("idx")()(Index.idx))
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
    println(
        DeviceFunc("plus")(arrA) {
            arrA.map(x => x + 1)
        }
    )
    println(
        GlobalFunc("plus")(arrA) {
            arrA.map(x => x + 1)
        }
    )
    println(
        GlobalFunc("add")(arrA, arrB) {
            arrA.zipWith(arrB)(_ + _)
        }
    )
    println(
        DeviceFunc("add")(arrA, arrB) {
            arrA.zipWith(arrB)(_ + _)
        }
    )
    println(
        GlobalFunc("add")(arrA, arrB) {
            arrA + arrB
        }
    )
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
        GlobalFunc("shift")(x) {
            x2.map(y => (x2(Index.idx - 1) + x2(Index.idx) + x2(Index.idx + 1)) / 3)
        }
    )
    val fn = DeviceFunc("plus")(a, b) {
        a + b
    }
    println(
        GlobalFunc("call")(x) {
            x.map(y => fn(x2, x2))
        }
    )
}

