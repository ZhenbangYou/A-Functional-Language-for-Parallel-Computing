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

case class UnitType(statementsAtFuncBegin: Set[Vector[Statement]], prevStatements: Vector[Statement])
    extends Type with PolyExpr[UnitType] {
    override val varName: String = ""

    override val typeName: String = ""

    override val refTypeName: String = ""

    override def argsName: List[String] = throw Exception("Unit type should not appear in arguments!")

    override def defName: String = throw Exception("Unit type cannot be defined!")

    override def codeGen: String = throw Exception("Unit type cannot generate expression!")

    override def genStatements: Vector[Statement] = prevStatements

    override val conditions: Set[BoolExpr] = Set()

    override def getResult: UnitType = throw Exception("Unit type does not have a result!")

    override def newInstance: UnitType = throw Exception("You should not need to get a new instance of unit!")

    override def globalFuncRet: String = ""

    override def deviceFuncRet: String = ""

    override def +(other: PolyExpr[UnitType]): Add[UnitType] =
        throw Exception("Unit type does not support operation!")

    override def -(other: PolyExpr[UnitType]): Sub[UnitType] =
        throw Exception("Unit type does not support operation!")

    override def *(other: PolyExpr[UnitType]): Mul[UnitType] =
        throw Exception("Unit type does not support operation!")

    override def /(other: PolyExpr[UnitType]): Div[UnitType] =
        throw Exception("Unit type does not support operation!")

    override def %(other: PolyExpr[UnitType]): Mod[UnitType] =
        throw Exception("Unit type does not support operation!")

    override def unary_- : Neg[UnitType] =
        throw Exception("Unit type does not support operation!")

    override def unary_+ : PolyExpr[UnitType] =
        throw Exception("Unit type does not support operation!")

    override def ==(other: PolyExpr[UnitType]): EQ[UnitType] =
        throw Exception("Unit type does not support operation!")

    override def !=(other: PolyExpr[UnitType]): NE[UnitType] =
        throw Exception("Unit type does not support operation!")

    override def <(other: PolyExpr[UnitType]): LT[UnitType] =
        throw Exception("Unit type does not support operation!")

    override def <=(other: PolyExpr[UnitType]): LE[UnitType] =
        throw Exception("Unit type does not support operation!")

    override def >(other: PolyExpr[UnitType]): GT[UnitType] =
        throw Exception("Unit type does not support operation!")

    override def >=(other: PolyExpr[UnitType]): GE[UnitType] =
        throw Exception("Unit type does not support operation!")
}

trait ScalarType extends Type

case class FloatType(varName: String) extends ScalarType with PolyExpr[FloatType] {
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

    override def globalFuncRet: String = s"*result = $getResult;\n"

    override def deviceFuncRet: String = s"return $getResult;\n"
}

implicit def floatConst(f: Float): FloatType = FloatType(s"(${f.toString})")

case class IntType(varName: String) extends ScalarType with PolyExpr[IntType] {
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

    override def globalFuncRet: String = s"*result = $getResult;\n"

    override def deviceFuncRet: String = s"return $getResult;\n"
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
    override val typeName: String = "float*"
    override val refTypeName: String = typeName
    override val baseTypeName: String = "float"

    override def argsName: List[String] = List(s"float $varName[]", s"int ${size.varName}")

    override def defName: String = throw Exception("Array should not be defined!")

    private var _lowOffset = 0

    private var _isStatic = false

    private def isStatic = _isStatic

    private def lowOffset: Int = _lowOffset

    private var _highOffset = 0

    private def highOffset: Int = _highOffset

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
                i < res.highOffset + res.size, Assignment(i, i + Index.lanes))(body: _*)
        })
        res
    }

    override def newBaseTypeInstance: FloatType = FloatType(TemporaryName())

    def getIndex: PolyExpr[IntType] = if (isStatic) Index.threadIdx.x - lowOffset else Index.idx

    def apply(index: PolyExpr[IntType]): ArrayAccess[FloatType] = ArrayAccess(this, index)

    private def getWithoutBoundCheck(index: PolyExpr[IntType]): ArrayAccessWithoutBoundCheck[FloatType] =
        ArrayAccessWithoutBoundCheck(this, index)

    def map(f: PolyExpr[FloatType] => PolyExpr[FloatType]): TmpOneDimFloatArrayType =
        TmpOneDimFloatArrayType(f(this (getIndex)))(size)(Index.idx)(statementsAtFuncBegin)

    def zipWith(other: OneDimFloatArrayType)(f: (PolyExpr[FloatType], PolyExpr[FloatType]) => PolyExpr[FloatType]): TmpOneDimFloatArrayType =
        TmpOneDimFloatArrayType(f(this (getIndex), other(other.getIndex)))(size)(Index.idx)(
            statementsAtFuncBegin)

    def zipWith(other: TmpOneDimFloatArrayType)(f: (PolyExpr[FloatType], PolyExpr[FloatType]) => PolyExpr[FloatType]): TmpOneDimFloatArrayType =
        TmpOneDimFloatArrayType(f(this (getIndex), other.element))(size)(Index.idx)(
            statementsAtFuncBegin)


    def reduceInBlock(f: (PolyExpr[FloatType], PolyExpr[FloatType]) => PolyExpr[FloatType]): PolyExpr[UnitType] = {
        // load to shared memory
        val sharedArray = createStaticArray(0, 0)

        // for loop
        val i = IntType("i")
        val reductionLoop = ForLoop(
            InitializedDeclaration(i, 1),
            i < Index.blockDim.x,
            Assignment(i, i * 2)
        ) {
            val idx = Index.threadIdx.x
            val tmpSum = FloatType("tmpSum")
            IfThen(idx + i < Index.blockDim.x)(
                InitializedDeclaration(tmpSum, sharedArray.getWithoutBoundCheck(idx) +
                    sharedArray.getWithoutBoundCheck(idx + i)),
                ArrayStoreWithoutBoundCheck(sharedArray, idx, tmpSum)
            )
        }

        // store
        val res = OneDimFloatArrayType("result")(Index.gridDim.x)
        val sum = FloatType("sum")
        val dec = InitializedDeclaration(sum, sharedArray.getWithoutBoundCheck(0))
        val storeStmt = IfThen(Index.threadIdx.x == 0)(
            ArrayStoreWithoutBoundCheck(res, Index.blockIdx.x, sum))
        UnitType(sharedArray.statementsAtFuncBegin, Vector(
            reductionLoop,
            dec,
            storeStmt
        ))
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

trait TmpArrayType[T <: ScalarType with NewInstance[T]] extends ArrayType[T] {
    def element: PolyExpr[T]

    def index: PolyExpr[IntType]
}

class TmpOneDimFloatArrayType(val element: PolyExpr[FloatType])(val size: IntType)(val index: PolyExpr[IntType])(_statementsAtFuncBegin: Set[Vector[Statement]]) extends TmpArrayType[FloatType] {
    override val typeName: String = "float*"
    override val refTypeName: String = typeName
    override val baseTypeName: String = "float"
    override val varName: String = element.codeGen

    override def argsName: List[String] = List(s"float $varName[]", s"int ${size.varName}")

    override def defName: String = throw Exception("Array should not be defined!")

    def statementsAtFuncBegin: Set[Vector[Statement]] = _statementsAtFuncBegin

    def apply(index: PolyExpr[IntType]): TmpArrayAccess[FloatType] = TmpArrayAccess(this)

    override def newBaseTypeInstance: FloatType = FloatType(TemporaryName())

    def map(f: PolyExpr[FloatType] => PolyExpr[FloatType]): TmpOneDimFloatArrayType =
        TmpOneDimFloatArrayType(f(element))(size)(index)(statementsAtFuncBegin)

    def zipWith(other: OneDimFloatArrayType)(f: (PolyExpr[FloatType], PolyExpr[FloatType]) => PolyExpr[FloatType]): TmpOneDimFloatArrayType =
        TmpOneDimFloatArrayType(f(element, other(other.getIndex)))(size)(index)(statementsAtFuncBegin)

    def zipWith(other: TmpOneDimFloatArrayType)(f: (PolyExpr[FloatType], PolyExpr[FloatType]) => PolyExpr[FloatType]): TmpOneDimFloatArrayType =
        TmpOneDimFloatArrayType(f(element, other.element))(size)(index)(statementsAtFuncBegin)

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
