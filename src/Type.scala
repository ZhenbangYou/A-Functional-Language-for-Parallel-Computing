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

    override def genStatements: Vector[String] = Vector()

    override def getResult: String = varName

    override def newInstance: FloatType = FloatType(TemporaryName())

    override def genStmts: Vector[Statement] = Vector()

    override def getRes: FloatType = this
}

implicit def floatConst(f: Float): FloatType = FloatType(f.toString)

class IntType(val varName: String) extends ScalarType with PolyExpr[IntType] {
    override val typeName: String = "int"

    override val refTypeName: String = s"$typeName*"

    override def argsName: List[String] = List(s"$typeName $varName")

    override def defName: String = s"$typeName $varName = 0;\n"

    override def codeGen: String = varName

    override def genStatements: Vector[String] = Vector()

    override def getResult: String = varName

    override def newInstance: IntType = IntType(TemporaryName())

    override def genStmts: Vector[Statement] = Vector()

    override def getRes: IntType = this
}

implicit def intConst(i: Int): IntType = IntType(i.toString)

trait ArrayType[T <: ScalarType with NewInstance[T]] extends Type {
    val varName: String
    val size: IntType
    val baseTypeName: String

    def newBaseTypeInstance: T

    def apply(index: PolyExpr[IntType]): PolyExpr[T]
}

class OneDimFloatArrayType(val varName: String)(val size: IntType) extends ArrayType[FloatType] {
    val typeName: String = "float*"
    val refTypeName: String = typeName
    override val baseTypeName: String = "float"

    def argsName: List[String] = List(s"float $varName[]", s"int ${size.varName}")

    def defName: String = ???

    override def newBaseTypeInstance: FloatType = FloatType(TemporaryName())

    def apply(index: PolyExpr[IntType]): ArrayAccess[FloatType] = ArrayAccess(this, index)

    def map(f: PolyExpr[FloatType] => PolyExpr[FloatType]): TmpOneDimFloatArrayType = {
        val element = this (Index.idx)
        TmpOneDimFloatArrayType(f(element))(size)
    }

    def zipWith(other: OneDimFloatArrayType)(f: (PolyExpr[FloatType], PolyExpr[FloatType]) => PolyExpr[FloatType]): TmpOneDimFloatArrayType = {
        TmpOneDimFloatArrayType(f(this (Index.idx), other(Index.idx)))(size)
    }

    def zipWith(other: TmpOneDimFloatArrayType)(f: (PolyExpr[FloatType], PolyExpr[FloatType]) => PolyExpr[FloatType]): TmpOneDimFloatArrayType = {
        TmpOneDimFloatArrayType(f(this (Index.idx), other.element))(size)
    }

    def +(other: OneDimFloatArrayType) = zipWith(other)(_ + _)

    def -(other: OneDimFloatArrayType) = zipWith(other)(_ - _)

    def *(other: OneDimFloatArrayType) = zipWith(other)(_ * _)

    def /(other: OneDimFloatArrayType) = zipWith(other)(_ / _)

    def +(other: TmpOneDimFloatArrayType) = zipWith(other)(_ + _)

    def -(other: TmpOneDimFloatArrayType) = zipWith(other)(_ - _)

    def *(other: TmpOneDimFloatArrayType) = zipWith(other)(_ * _)

    def /(other: TmpOneDimFloatArrayType) = zipWith(other)(_ / _)

    def unary_+ = this

    def unary_- = map(-_)

    def +(other: FloatType) = map(_ + other)

    def -(other: FloatType) = map(_ - other)

    def *(other: FloatType) = map(_ * other)

    def /(other: FloatType) = map(_ / other)
}

class TmpOneDimFloatArrayType(val element: PolyExpr[FloatType])(val size: IntType) extends ArrayType[FloatType] {
    val typeName: String = "float*"
    val refTypeName: String = typeName
    override val baseTypeName: String = "float"
    override val varName: String = element.codeGen

    def argsName: List[String] = List(s"float $varName[]", s"int ${size.varName}")

    def defName: String = ???

    def apply(index: PolyExpr[IntType]): TmpFloatArrayAccess = TmpFloatArrayAccess(this)

    override def newBaseTypeInstance: FloatType = FloatType(TemporaryName())

    def map(f: PolyExpr[FloatType] => PolyExpr[FloatType]): TmpOneDimFloatArrayType = TmpOneDimFloatArrayType(f(element))(size)

    def zipWith(other: OneDimFloatArrayType)(f: (PolyExpr[FloatType], PolyExpr[FloatType]) => PolyExpr[FloatType]): TmpOneDimFloatArrayType = {
        TmpOneDimFloatArrayType(f(element, other(Index.idx)))(size)
    }

    def zipWith(other: TmpOneDimFloatArrayType)(f: (PolyExpr[FloatType], PolyExpr[FloatType]) => PolyExpr[FloatType]): TmpOneDimFloatArrayType = {
        TmpOneDimFloatArrayType(f(element, other.element))(size)
    }

    def +(other: OneDimFloatArrayType) = zipWith(other)(_ + _)

    def -(other: OneDimFloatArrayType) = zipWith(other)(_ - _)

    def *(other: OneDimFloatArrayType) = zipWith(other)(_ * _)

    def /(other: OneDimFloatArrayType) = zipWith(other)(_ / _)

    def +(other: TmpOneDimFloatArrayType) = zipWith(other)(_ + _)

    def -(other: TmpOneDimFloatArrayType) = zipWith(other)(_ - _)

    def *(other: TmpOneDimFloatArrayType) = zipWith(other)(_ * _)

    def /(other: TmpOneDimFloatArrayType) = zipWith(other)(_ / _)

    def unary_+ = this

    def unary_- = map(-_)

    def +(other: FloatType) = map(_ + other)

    def -(other: FloatType) = map(_ - other)

    def *(other: FloatType) = map(_ * other)

    def /(other: FloatType) = map(_ / other)
}

extension (f: FloatType) {
    def +(other: OneDimFloatArrayType) = other.map(f + _)
    def -(other: OneDimFloatArrayType) = other.map(f - _)
    def *(other: OneDimFloatArrayType) = other.map(f * _)
    def /(other: OneDimFloatArrayType) = other.map(f / _)
    def +(other: TmpOneDimFloatArrayType) = other.map(f + _)
    def -(other: TmpOneDimFloatArrayType) = other.map(f - _)
    def *(other: TmpOneDimFloatArrayType) = other.map(f * _)
    def /(other: TmpOneDimFloatArrayType) = other.map(f / _)
}
