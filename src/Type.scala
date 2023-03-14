import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

trait Typable {
    val typeName: String

    val refTypeName: String
}

sealed trait Type extends Typable {
    val varName: String // name of the variable

    def argsName: List[String] // names in the parameter list
    // can have multiple elements, e.g., arrays need to be specified size

    def defName: String // name in variable definition, zero initialization
}

trait ScalarType extends Type

class FloatType(val varName: String) extends ScalarType with PolyExpr[FloatType] {
    override val typeName: String = "float"

    override val refTypeName: String = s"$typeName*"

    override def argsName: List[String] = List(s"$typeName $varName")

    override def defName: String = s"$typeName $varName = 0.0f;\n"

    override def codeGen: String = varName

    override def newInstance: FloatType = FloatType(TemporaryName())

    override def genStatements: Vector[Statement] = Vector()

    override def getResult: FloatType = this

    override val conditions: Set[BoolExpr] = Set()

    override def statementsAtFuncBegin: Set[Vector[Statement]] = Set()
}

implicit def floatConst(f: Float): FloatType = FloatType(s"(${f.toString})")

class IntType(val varName: String) extends ScalarType with PolyExpr[IntType] {
    override val typeName: String = "int"

    override val refTypeName: String = s"$typeName*"

    override def argsName: List[String] = List(s"$typeName $varName")

    override def defName: String = s"$typeName $varName = 0;\n"

    override def codeGen: String = varName

    override def newInstance: IntType = IntType(TemporaryName())

    override def genStatements: Vector[Statement] = Vector()

    override def getResult: IntType = this

    override val conditions: Set[BoolExpr] = Set()

    override def statementsAtFuncBegin: Set[Vector[Statement]] = Set()
}

implicit def intConst(i: Int): IntType = IntType(s"(${i.toString})")

trait ArrayType[T <: ScalarType with NewInstance[T]] extends Type with Expr {
    val varName: String
    val size: IntType
    val baseTypeName: String

    def newBaseTypeInstance: T

    def apply(index: PolyExpr[IntType]): PolyExpr[T]

    def codeGen: String = varName

    val typeName: String
    val refTypeName: String

    val conditions: Set[BoolExpr] = Set()

    def statementsAtFuncBegin: Set[Vector[Statement]]
}

class OneDimFloatArrayType(val varName: String)(val size: IntType) extends ArrayType[FloatType] {
    val typeName: String = "float*"
    val refTypeName: String = typeName
    override val baseTypeName: String = "float"

    def argsName: List[String] = List(s"float $varName[]", s"int ${size.varName}")

    def defName: String = ???

    private var _lowOffset = 0

    private var _isStatic = false

    def isStatic = _isStatic

    def lowOffset: Int = _lowOffset

    private var _highOffset = 0

    def highOffset: Int = _highOffset

    private val _statementsAtFuncBegin: ArrayBuffer[Statement] = ArrayBuffer()

    def statementsAtFuncBegin: Set[Vector[Statement]] = Set(_statementsAtFuncBegin.toVector)

    def createStaticArray(low: Int, high: Int): OneDimFloatArrayType = {
        val res = OneDimFloatArrayType(TemporaryName())(size)
        res._isStatic = true
        res._lowOffset = low
        res._highOffset = high
        res._statementsAtFuncBegin.addOne(
            DeclareStaticArray(res, res.lowOffset, res.highOffset)
        )
        res._statementsAtFuncBegin.addOne({
            val i = IntType("i")
            val body = this (i).genStatements :+ ArrayStoreWithoutBoundCheck(res, Index.idx, this (i).getResult)
            ForLoop(InitializedDeclaration(i, res.lowOffset + Index.idx),
                i < res.highOffset + res.size, Assignment(i, i + Constants.WARP_SIZE))(body: _*)
        })
        res
    }

    override def newBaseTypeInstance: FloatType = FloatType(TemporaryName())

    def apply(index: PolyExpr[IntType]): ArrayAccess[FloatType] = ArrayAccess(this, index % Index.blockDim.x - lowOffset)

    def map(f: PolyExpr[FloatType] => PolyExpr[FloatType]): TmpOneDimFloatArrayType = {
        val idx = if (isStatic) Index.threadIdx.x - lowOffset else Index.idx
        val element = this (idx)
        TmpOneDimFloatArrayType(f(element))(size)(Index.idx)(statementsAtFuncBegin)
    }

    def zipWith(other: OneDimFloatArrayType)(f: (PolyExpr[FloatType], PolyExpr[FloatType]) => PolyExpr[FloatType]): TmpOneDimFloatArrayType = {
        val idx = if (isStatic) Index.threadIdx.x - lowOffset else Index.idx
        val otherIdx = if (other.isStatic) Index.threadIdx.x - other.lowOffset else Index.idx
        TmpOneDimFloatArrayType(f(this (idx), other(otherIdx)))(size)(Index.idx)(
            statementsAtFuncBegin)
    }

    def zipWith(other: TmpOneDimFloatArrayType)(f: (PolyExpr[FloatType], PolyExpr[FloatType]) => PolyExpr[FloatType]): TmpOneDimFloatArrayType = {
        val idx = if (isStatic) Index.threadIdx.x - lowOffset else Index.idx
        TmpOneDimFloatArrayType(f(this (idx), other.element))(size)(Index.idx)(
            statementsAtFuncBegin)
    }

    def +(other: OneDimFloatArrayType): TmpOneDimFloatArrayType = zipWith(other)(_ + _)

    def -(other: OneDimFloatArrayType): TmpOneDimFloatArrayType = zipWith(other)(_ - _)

    def *(other: OneDimFloatArrayType): TmpOneDimFloatArrayType = zipWith(other)(_ * _)

    def /(other: OneDimFloatArrayType): TmpOneDimFloatArrayType = zipWith(other)(_ / _)

    def %(other: OneDimFloatArrayType): TmpOneDimFloatArrayType = zipWith(other)(_ % _)

    def +(other: TmpOneDimFloatArrayType): TmpOneDimFloatArrayType = zipWith(other)(_ + _)

    def -(other: TmpOneDimFloatArrayType): TmpOneDimFloatArrayType = zipWith(other)(_ - _)

    def *(other: TmpOneDimFloatArrayType): TmpOneDimFloatArrayType = zipWith(other)(_ * _)

    def /(other: TmpOneDimFloatArrayType): TmpOneDimFloatArrayType = zipWith(other)(_ / _)

    def %(other: TmpOneDimFloatArrayType): TmpOneDimFloatArrayType = zipWith(other)(_ % _)

    def unary_+ : OneDimFloatArrayType = this

    def unary_- : TmpOneDimFloatArrayType = map(-_)

    def +(other: FloatType): TmpOneDimFloatArrayType = map(_ + other)

    def -(other: FloatType): TmpOneDimFloatArrayType = map(_ - other)

    def *(other: FloatType): TmpOneDimFloatArrayType = map(_ * other)

    def /(other: FloatType): TmpOneDimFloatArrayType = map(_ / other)

    def %(other: FloatType): TmpOneDimFloatArrayType = map(_ % other)
}

class TmpOneDimFloatArrayType(val element: PolyExpr[FloatType])(val size: IntType)(val index: PolyExpr[IntType])(_statementsAtFuncBegin: Set[Vector[Statement]]) extends ArrayType[FloatType] {
    val typeName: String = "float*"
    val refTypeName: String = typeName
    override val baseTypeName: String = "float"
    override val varName: String = element.codeGen

    def argsName: List[String] = List(s"float $varName[]", s"int ${size.varName}")

    def defName: String = ???

    def statementsAtFuncBegin: Set[Vector[Statement]] = _statementsAtFuncBegin

    def apply(index: PolyExpr[IntType]): TmpFloatArrayAccess = TmpFloatArrayAccess(this)

    override def newBaseTypeInstance: FloatType = FloatType(TemporaryName())

    def map(f: PolyExpr[FloatType] => PolyExpr[FloatType]): TmpOneDimFloatArrayType =
        TmpOneDimFloatArrayType(f(element))(size)(index)(statementsAtFuncBegin)

    def zipWith(other: OneDimFloatArrayType)(f: (PolyExpr[FloatType], PolyExpr[FloatType]) => PolyExpr[FloatType]): TmpOneDimFloatArrayType = {
        val idx = if (other.isStatic) Index.threadIdx.x - other.lowOffset else Index.idx
        TmpOneDimFloatArrayType(f(element, other(idx)))(size)(index)(statementsAtFuncBegin)
    }

    def zipWith(other: TmpOneDimFloatArrayType)(f: (PolyExpr[FloatType], PolyExpr[FloatType]) => PolyExpr[FloatType]): TmpOneDimFloatArrayType = {
        TmpOneDimFloatArrayType(f(element, other.element))(size)(index)(statementsAtFuncBegin)
    }

    def +(other: OneDimFloatArrayType): TmpOneDimFloatArrayType = zipWith(other)(_ + _)

    def -(other: OneDimFloatArrayType): TmpOneDimFloatArrayType = zipWith(other)(_ - _)

    def *(other: OneDimFloatArrayType): TmpOneDimFloatArrayType = zipWith(other)(_ * _)

    def /(other: OneDimFloatArrayType): TmpOneDimFloatArrayType = zipWith(other)(_ / _)

    def %(other: OneDimFloatArrayType): TmpOneDimFloatArrayType = zipWith(other)(_ % _)

    def +(other: TmpOneDimFloatArrayType): TmpOneDimFloatArrayType = zipWith(other)(_ + _)

    def -(other: TmpOneDimFloatArrayType): TmpOneDimFloatArrayType = zipWith(other)(_ - _)

    def *(other: TmpOneDimFloatArrayType): TmpOneDimFloatArrayType = zipWith(other)(_ * _)

    def /(other: TmpOneDimFloatArrayType): TmpOneDimFloatArrayType = zipWith(other)(_ / _)

    def %(other: TmpOneDimFloatArrayType): TmpOneDimFloatArrayType = zipWith(other)(_ % _)

    def unary_+ : TmpOneDimFloatArrayType = this

    def unary_- : TmpOneDimFloatArrayType = map(-_)

    def +(other: FloatType): TmpOneDimFloatArrayType = map(_ + other)

    def -(other: FloatType): TmpOneDimFloatArrayType = map(_ - other)

    def *(other: FloatType): TmpOneDimFloatArrayType = map(_ * other)

    def /(other: FloatType): TmpOneDimFloatArrayType = map(_ / other)

    def %(other: FloatType): TmpOneDimFloatArrayType = map(_ / other)
}

extension (f: FloatType) {
    def +(other: OneDimFloatArrayType): TmpOneDimFloatArrayType = other.map(f + _)
    def -(other: OneDimFloatArrayType): TmpOneDimFloatArrayType = other.map(f - _)
    def *(other: OneDimFloatArrayType): TmpOneDimFloatArrayType = other.map(f * _)
    def /(other: OneDimFloatArrayType): TmpOneDimFloatArrayType = other.map(f / _)
    def %(other: OneDimFloatArrayType): TmpOneDimFloatArrayType = other.map(f % _)
    def +(other: TmpOneDimFloatArrayType): TmpOneDimFloatArrayType = other.map(f + _)
    def -(other: TmpOneDimFloatArrayType): TmpOneDimFloatArrayType = other.map(f - _)
    def *(other: TmpOneDimFloatArrayType): TmpOneDimFloatArrayType = other.map(f * _)
    def /(other: TmpOneDimFloatArrayType): TmpOneDimFloatArrayType = other.map(f / _)
    def %(other: TmpOneDimFloatArrayType): TmpOneDimFloatArrayType = other.map(f % _)
}
