import scala.language.implicitConversions

sealed trait Expr extends Typable {
    def codeGen: String

    val typeName: String
    val refTypeName: String

    override def toString: String = codeGen

    def genStatements: Vector[String]

    def getResult: String
}

trait PolyExpr[T <: Type] extends Expr with NewInstance[T] {
    def +(other: PolyExpr[T]): Add[T] = Add(this, other)

    def -(other: PolyExpr[T]): Sub[T] = Sub(this, other)

    def *(other: PolyExpr[T]): Mul[T] = Mul(this, other)

    def /(other: PolyExpr[T]): Div[T] = Div(this, other)

    def unary_- : Neg[T] = Neg(this)

    def unary_+ : PolyExpr[T] = this

    def ==(other: PolyExpr[T]): EQ[T] = EQ(this, other)

    def !=(other: PolyExpr[T]): NE[T] = NE(this, other)

    def <(other: PolyExpr[T]): LT[T] = LT(this, other)

    def <=(other: PolyExpr[T]): LE[T] = LE(this, other)

    def >(other: PolyExpr[T]): GT[T] = GT(this, other)

    def >=(other: PolyExpr[T]): GE[T] = GE(this, other)
}

case class Add[T <: Type](srcA: PolyExpr[T], srcB: PolyExpr[T]) extends PolyExpr[T] {
    override def codeGen: String = s"(${srcA.codeGen} + ${srcB.codeGen})"

    override val typeName: String = srcA.typeName
    override val refTypeName: String = srcA.refTypeName

    private val resultName = TemporaryName()

    override def genStatements: Vector[String] =
        srcA.genStatements ++
            srcB.genStatements ++
            Vector(
                s"$typeName $resultName;\n",
                s"$resultName = ${srcA.getResult} + ${srcB.getResult};\n"
            )

    override def getResult: String = resultName

    override def newInstance: T = srcA.newInstance
}

case class Sub[T <: Type](srcA: PolyExpr[T], srcB: PolyExpr[T]) extends PolyExpr[T] {
    override def codeGen: String = s"(${srcA.codeGen} - ${srcB.codeGen})"

    override val typeName: String = srcA.typeName
    override val refTypeName: String = srcA.refTypeName

    private val resultName = TemporaryName()

    override def genStatements: Vector[String] =
        srcA.genStatements ++
            srcB.genStatements ++
            Vector(
                s"$typeName $resultName;\n",
                s"$resultName = ${srcA.getResult} - ${srcB.getResult};\n"
            )

    override def getResult: String = resultName

    override def newInstance: T = srcA.newInstance
}

case class Mul[T <: Type](srcA: PolyExpr[T], srcB: PolyExpr[T]) extends PolyExpr[T] {
    override def codeGen: String = s"(${srcA.codeGen} * ${srcB.codeGen})"

    override val typeName: String = srcA.typeName
    override val refTypeName: String = srcA.refTypeName

    private val resultName = TemporaryName()

    override def genStatements: Vector[String] =
        srcA.genStatements ++
            srcB.genStatements ++
            Vector(
                s"$typeName $resultName;\n",
                s"$resultName = ${srcA.getResult} * ${srcB.getResult};\n"
            )

    override def getResult: String = resultName

    override def newInstance: T = srcA.newInstance
}

case class Div[T <: Type](srcA: PolyExpr[T], srcB: PolyExpr[T]) extends PolyExpr[T] {
    override def codeGen: String = s"(${srcA.codeGen} / ${srcB.codeGen})"

    override val typeName: String = srcA.typeName
    override val refTypeName: String = srcA.refTypeName

    private val resultName = TemporaryName()

    override def genStatements: Vector[String] =
        srcA.genStatements ++
            srcB.genStatements ++
            Vector(
                s"$typeName $resultName;\n",
                s"$resultName = ${srcA.getResult} / ${srcB.getResult};\n"
            )

    override def getResult: String = resultName

    override def newInstance: T = srcA.newInstance
}

case class Neg[T <: Type](srcA: PolyExpr[T]) extends PolyExpr[T] {
    override def codeGen: String = s"(-${srcA.codeGen})"

    override val typeName: String = srcA.typeName
    override val refTypeName: String = srcA.refTypeName

    private val resultName = TemporaryName()

    override def genStatements: Vector[String] =
        srcA.genStatements ++
            Vector(
                s"$typeName $resultName;\n",
                s"$resultName = -${srcA.getResult};\n"
            )

    override def getResult: String = resultName

    override def newInstance: T = srcA.newInstance
}

case class ArrayAccess[T <: ScalarType with NewInstance[T]](array: ArrayType[T], index: PolyExpr[IntType]) extends PolyExpr[T] {
    def codeGen: String = s"${array.varName}[${index.codeGen}]"

    override val typeName: String = array.baseTypeName
    override val refTypeName: String = s"$typeName*"

    private val resultName = TemporaryName()

    override def genStatements: Vector[String] =
        index.genStatements ++
            Vector(
                s"$typeName $resultName;\n",
                s"$resultName = ${array.varName}[${index.getResult}];\n"
            )

    override def getResult: String = resultName

    override def newInstance: T = array.newBaseTypeInstance
}

trait BoolExpr extends Expr {
    def &&(other: BoolExpr): And = And(this, other)

    def ||(other: BoolExpr): Or = Or(this, other)

    def unary_! : Not = Not(this)

    override val typeName: String = "bool"
    override val refTypeName: String = "bool*"
}

case class EQ[T <: Type](srcA: PolyExpr[T], srcB: PolyExpr[T]) extends BoolExpr {
    override def codeGen: String = s"(${srcA.codeGen} == ${srcB.codeGen})"

    private val resultName = TemporaryName()

    override def genStatements: Vector[String] =
        srcA.genStatements ++
            srcB.genStatements ++
            Vector(
                s"$typeName $resultName;\n",
                s"$resultName = ${srcA.getResult} == ${srcB.getResult};\n"
            )

    override def getResult: String = resultName
}

case class NE[T <: Type](srcA: PolyExpr[T], srcB: PolyExpr[T]) extends BoolExpr {
    override def codeGen: String = s"(${srcA.codeGen} != ${srcB.codeGen})"

    private val resultName = TemporaryName()

    override def genStatements: Vector[String] =
        srcA.genStatements ++
            srcB.genStatements ++
            Vector(
                s"$typeName $resultName;\n",
                s"$resultName = ${srcA.getResult} != ${srcB.getResult};\n"
            )

    override def getResult: String = resultName
}

case class LT[T <: Type](srcA: PolyExpr[T], srcB: PolyExpr[T]) extends BoolExpr {
    override def codeGen: String = s"(${srcA.codeGen} < ${srcB.codeGen})"

    private val resultName = TemporaryName()

    override def genStatements: Vector[String] =
        srcA.genStatements ++
            srcB.genStatements ++
            Vector(
                s"$typeName $resultName;\n",
                s"$resultName = ${srcA.getResult} < ${srcB.getResult};\n"
            )

    override def getResult: String = resultName
}

case class LE[T <: Type](srcA: PolyExpr[T], srcB: PolyExpr[T]) extends BoolExpr {
    override def codeGen: String = s"(${srcA.codeGen} <= ${srcB.codeGen})"

    private val resultName = TemporaryName()

    override def genStatements: Vector[String] =
        srcA.genStatements ++
            srcB.genStatements ++
            Vector(
                s"$typeName $resultName;\n",
                s"$resultName = ${srcA.getResult} <= ${srcB.getResult};\n"
            )

    override def getResult: String = resultName
}

case class GT[T <: Type](srcA: PolyExpr[T], srcB: PolyExpr[T]) extends BoolExpr {
    override def codeGen: String = s"(${srcA.codeGen} > ${srcB.codeGen})"

    private val resultName = TemporaryName()

    override def genStatements: Vector[String] =
        srcA.genStatements ++
            srcB.genStatements ++
            Vector(
                s"$typeName $resultName;\n",
                s"$resultName = ${srcA.getResult} > ${srcB.getResult};\n"
            )

    override def getResult: String = resultName
}

case class GE[T <: Type](srcA: PolyExpr[T], srcB: PolyExpr[T]) extends BoolExpr {
    override def codeGen: String = s"(${srcA.codeGen} >= ${srcB.codeGen})"

    private val resultName = TemporaryName()

    override def genStatements: Vector[String] =
        srcA.genStatements ++
            srcB.genStatements ++
            Vector(
                s"$typeName $resultName;\n",
                s"$resultName = ${srcA.getResult} >= ${srcB.getResult};\n"
            )

    override def getResult: String = resultName
}

case class And(srcA: BoolExpr, srcB: BoolExpr) extends BoolExpr {
    override def codeGen: String = s"(${srcA.codeGen} && ${srcB.codeGen})"

    private val resultName = TemporaryName()

    override def genStatements: Vector[String] =
        srcA.genStatements ++
            srcB.genStatements ++
            Vector(
                s"$typeName $resultName;\n",
                s"$resultName = ${srcA.getResult} && ${srcB.getResult};\n"
            )

    override def getResult: String = resultName
}

case class Or(srcA: BoolExpr, srcB: BoolExpr) extends BoolExpr {
    override def codeGen: String = s"(${srcA.codeGen} || ${srcB.codeGen})"

    private val resultName = TemporaryName()

    override def genStatements: Vector[String] =
        srcA.genStatements ++
            srcB.genStatements ++
            Vector(
                s"$typeName $resultName;\n",
                s"$resultName = ${srcA.getResult} || ${srcB.getResult};\n"
            )

    override def getResult: String = resultName
}

case class Not(srcA: BoolExpr) extends BoolExpr {
    override def codeGen: String = s"(!${srcA.codeGen})"

    private val resultName = TemporaryName()

    override def genStatements: Vector[String] =
        srcA.genStatements ++
            Vector(
                s"$typeName $resultName;\n",
                s"$resultName = !${srcA.getResult};\n"
            )

    override def getResult: String = resultName
}

case class If[T <: Type](cond: BoolExpr)(thenBody: PolyExpr[T])(elseBody: PolyExpr[T]) extends PolyExpr[T] {
    override val typeName: String = thenBody.typeName
    override val refTypeName: String = thenBody.refTypeName

    override def codeGen: String =
        s"${cond.codeGen} ? ${thenBody.codeGen} : ${elseBody.codeGen}"

    private val resultName = TemporaryName()

    override def genStatements: Vector[String] =
        cond.genStatements ++
            Vector(
                s"$typeName $resultName;\n",
                s"$resultName = ${cond.getResult} ? ${thenBody.codeGen} : ${elseBody.codeGen};\n"
            )

    override def getResult: String = resultName

    override def newInstance: T = thenBody.newInstance
}

case class TmpFloatArrayAccess(array: TmpOneDimFloatArrayType) extends PolyExpr[FloatType] {
    def codeGen: String = array.element.codeGen

    override val typeName: String = array.baseTypeName
    override val refTypeName: String = s"$typeName*"

    private val resultName = TemporaryName()

    override def genStatements: Vector[String] = Vector()

    override def getResult: String = resultName

    override def newInstance: FloatType = array.newBaseTypeInstance
}

implicit def tmpArray(t: TmpOneDimFloatArrayType): TmpFloatArrayAccess = TmpFloatArrayAccess(t)