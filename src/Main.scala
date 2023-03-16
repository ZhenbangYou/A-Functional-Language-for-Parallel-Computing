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
    val alpha = FloatType("alpha")
    val x = OneDimFloatArrayType("x")(n)
    val y = OneDimFloatArrayType("y")(n)
    println(
        GlobalFunc("saxpy")(alpha, x, y) {
            alpha * x + y
        }
    )
    println(
        GlobalFunc("saxpy")(alpha, x, y) {
            x.zipWith(y)((xx, yy) => alpha * xx + yy)
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

trait ICloneable[T] {
    def myClone(): T
}

trait Base extends ICloneable[Base]

class Derived extends Base {
    override def myClone(): Derived = Derived()
}

def getNewCopy[T <: Base](arg: T): T = arg.myClone().asInstanceOf[T]
