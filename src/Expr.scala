import scala.language.implicitConversions

trait Expr extends Typable {
    def codeGen: String

    val typeName: String
    val refTypeName: String

    override def toString: String = codeGen

    val conditions: Set[BoolExpr]

    def statementsAtFuncBegin: Set[Vector[Statement]]
}

trait PolyExpr[T <: Type] extends Expr with NewInstance[T] {
    def +(other: PolyExpr[T]): Add[T] = Add(this, other)

    def -(other: PolyExpr[T]): Sub[T] = Sub(this, other)

    def *(other: PolyExpr[T]): Mul[T] = Mul(this, other)

    def /(other: PolyExpr[T]): Div[T] = Div(this, other)

    def %(other: PolyExpr[T]): Mod[T] = Mod(this, other)

    def unary_- : Neg[T] = Neg(this)

    def unary_+ : PolyExpr[T] = this

    def ==(other: PolyExpr[T]): EQ[T] = EQ(this, other)

    def !=(other: PolyExpr[T]): NE[T] = NE(this, other)

    def <(other: PolyExpr[T]): LT[T] = LT(this, other)

    def <=(other: PolyExpr[T]): LE[T] = LE(this, other)

    def >(other: PolyExpr[T]): GT[T] = GT(this, other)

    def >=(other: PolyExpr[T]): GE[T] = GE(this, other)

    def genStatements: Vector[Statement]

    def getResult: T

    def globalFuncRet: String

    def deviceFuncRet: String
}

case class Add[T <: Type](srcA: PolyExpr[T], srcB: PolyExpr[T]) extends PolyExpr[T] {
    override def codeGen: String = s"(${srcA.codeGen} + ${srcB.codeGen})"

    override val typeName: String = srcA.typeName
    override val refTypeName: String = srcA.refTypeName

    override def newInstance: T = srcA.newInstance

    private val result = srcA.newInstance

    override def genStatements: Vector[Statement] =
        srcA.genStatements ++ srcB.genStatements ++ Vector(
            InitializedDeclaration(result, Add(srcA.getResult.asInstanceOf[PolyExpr[T]], srcB.getResult.asInstanceOf[PolyExpr[T]]))
        )

    override def getResult: T = result

    override val conditions: Set[BoolExpr] = srcA.conditions | srcB.conditions

    override val statementsAtFuncBegin: Set[Vector[Statement]] =
        srcA.statementsAtFuncBegin | srcB.statementsAtFuncBegin

    override def globalFuncRet: String = s"*result = $getResult;\n"

    override def deviceFuncRet: String = s"return $getResult;\n"
}

case class Sub[T <: Type](srcA: PolyExpr[T], srcB: PolyExpr[T]) extends PolyExpr[T] {
    override def codeGen: String = s"(${srcA.codeGen} - ${srcB.codeGen})"

    override val typeName: String = srcA.typeName
    override val refTypeName: String = srcA.refTypeName

    override def newInstance: T = srcA.newInstance

    private val result = srcA.newInstance

    override def genStatements: Vector[Statement] =
        srcA.genStatements ++ srcB.genStatements ++ Vector(
            InitializedDeclaration(result, Sub(srcA.getResult.asInstanceOf[PolyExpr[T]], srcB.getResult.asInstanceOf[PolyExpr[T]]))
        )

    override def getResult: T = result

    override val conditions: Set[BoolExpr] = srcA.conditions | srcB.conditions

    override val statementsAtFuncBegin: Set[Vector[Statement]] =
        srcA.statementsAtFuncBegin | srcB.statementsAtFuncBegin

    override def globalFuncRet: String = s"*result = $getResult;\n"

    override def deviceFuncRet: String = s"return $getResult;\n"
}

case class Mul[T <: Type](srcA: PolyExpr[T], srcB: PolyExpr[T]) extends PolyExpr[T] {
    override def codeGen: String = s"(${srcA.codeGen} * ${srcB.codeGen})"

    override val typeName: String = srcA.typeName
    override val refTypeName: String = srcA.refTypeName

    override def newInstance: T = srcA.newInstance

    private val result = srcA.newInstance

    override def genStatements: Vector[Statement] =
        srcA.genStatements ++ srcB.genStatements ++ Vector(
            InitializedDeclaration(result, Mul(srcA.getResult.asInstanceOf[PolyExpr[T]], srcB.getResult.asInstanceOf[PolyExpr[T]]))
        )

    override def getResult: T = result

    override val conditions: Set[BoolExpr] = srcA.conditions | srcB.conditions

    override val statementsAtFuncBegin: Set[Vector[Statement]] =
        srcA.statementsAtFuncBegin | srcB.statementsAtFuncBegin

    override def globalFuncRet: String = s"*result = $getResult;\n"

    override def deviceFuncRet: String = s"return $getResult;\n"
}

case class Div[T <: Type](srcA: PolyExpr[T], srcB: PolyExpr[T]) extends PolyExpr[T] {
    override def codeGen: String = s"(${srcA.codeGen} / ${srcB.codeGen})"

    override val typeName: String = srcA.typeName
    override val refTypeName: String = srcA.refTypeName

    override def newInstance: T = srcA.newInstance

    private val result = srcA.newInstance

    override def genStatements: Vector[Statement] =
        srcA.genStatements ++ srcB.genStatements ++ Vector(
            InitializedDeclaration(result, Div(srcA.getResult.asInstanceOf[PolyExpr[T]], srcB.getResult.asInstanceOf[PolyExpr[T]]))
        )

    override def getResult: T = result

    override val conditions: Set[BoolExpr] = srcA.conditions | srcB.conditions

    override val statementsAtFuncBegin: Set[Vector[Statement]] =
        srcA.statementsAtFuncBegin | srcB.statementsAtFuncBegin

    override def globalFuncRet: String = s"*result = $getResult;\n"

    override def deviceFuncRet: String = s"return $getResult;\n"
}

case class Mod[T <: Type](srcA: PolyExpr[T], srcB: PolyExpr[T]) extends PolyExpr[T] {
    override def codeGen: String = s"(${srcA.codeGen} % ${srcB.codeGen})"

    override val typeName: String = srcA.typeName
    override val refTypeName: String = srcA.refTypeName

    override def newInstance: T = srcA.newInstance

    private val result = srcA.newInstance

    override def genStatements: Vector[Statement] =
        srcA.genStatements ++ srcB.genStatements ++ Vector(
            InitializedDeclaration(result, Mod(srcA.getResult.asInstanceOf[PolyExpr[T]], srcB.getResult.asInstanceOf[PolyExpr[T]]))
        )

    override def getResult: T = result

    override val conditions: Set[BoolExpr] = srcA.conditions | srcB.conditions

    override val statementsAtFuncBegin: Set[Vector[Statement]] =
        srcA.statementsAtFuncBegin | srcB.statementsAtFuncBegin

    override def globalFuncRet: String = s"*result = $getResult;\n"

    override def deviceFuncRet: String = s"return $getResult;\n"
}

case class Neg[T <: Type](srcA: PolyExpr[T]) extends PolyExpr[T] {
    override def codeGen: String = s"(-${srcA.codeGen})"

    override val typeName: String = srcA.typeName
    override val refTypeName: String = srcA.refTypeName

    override def newInstance: T = srcA.newInstance

    private val result = srcA.newInstance

    override def genStatements: Vector[Statement] =
        srcA.genStatements ++ Vector(
            InitializedDeclaration(result, Neg(srcA.getResult.asInstanceOf[PolyExpr[T]]))
        )

    override def getResult: T = result

    override val conditions: Set[BoolExpr] = srcA.conditions

    override val statementsAtFuncBegin: Set[Vector[Statement]] =
        srcA.statementsAtFuncBegin

    override def globalFuncRet: String = s"*result = $getResult;\n"

    override def deviceFuncRet: String = s"return $getResult;\n"
}

case class ArrayAccess[T <: ScalarType with NewInstance[T]](array: ArrayType[T], index: PolyExpr[IntType]) extends PolyExpr[T] {
    def codeGen: String = s"${array.varName}[${index.codeGen}]"

    override val typeName: String = array.baseTypeName
    override val refTypeName: String = s"$typeName*"

    override def newInstance: T = array.newBaseTypeInstance

    private val result = array.newBaseTypeInstance

    override def genStatements: Vector[Statement] =
        index.genStatements ++ Vector(
            Declaration(result),
            if ((index eq Index.idx) || (index eq Index.threadIdx.x)) IfThen(index.getResult < array.size) {
                Assignment(result, array(index.getResult))
            }
            else
                IfThen(0 <= index.getResult && index.getResult < array.size) {
                    Assignment(result, array(index.getResult))
                }
        )

    override def getResult: T = result

    override val conditions: Set[BoolExpr] = index.conditions + (index < array.size)

    override val statementsAtFuncBegin: Set[Vector[Statement]] =
        array.statementsAtFuncBegin | index.statementsAtFuncBegin

    override def globalFuncRet: String = s"*result = $getResult;\n"

    override def deviceFuncRet: String = s"return $getResult;\n"
}

case class ArrayAccessWithoutBoundCheck[T <: ScalarType with NewInstance[T]](array: ArrayType[T], index: PolyExpr[IntType]) extends PolyExpr[T] {
    def codeGen: String = s"${array.varName}[${index.codeGen}]"

    override val typeName: String = array.baseTypeName
    override val refTypeName: String = s"$typeName*"

    override def newInstance: T = array.newBaseTypeInstance

    private val result = array.newBaseTypeInstance

    override def genStatements: Vector[Statement] =
        index.genStatements ++ Vector(
            InitializedDeclaration(result, array(index.getResult))
        )

    override def getResult: T = result

    override val conditions: Set[BoolExpr] = index.conditions + (index < array.size)

    override val statementsAtFuncBegin: Set[Vector[Statement]] =
        array.statementsAtFuncBegin | index.statementsAtFuncBegin

    override def globalFuncRet: String = s"*result = $getResult;\n"

    override def deviceFuncRet: String = s"return $getResult;\n"
}

case class TmpArrayAccess[T <: ScalarType with NewInstance[T]](array: TmpArrayType[T]) extends PolyExpr[T] {
    override def codeGen: String = array.element.codeGen

    override val typeName: String = array.baseTypeName
    override val refTypeName: String = s"$typeName*"

    override def newInstance: T = array.newBaseTypeInstance

    private val result = array.newBaseTypeInstance

    override def genStatements: Vector[Statement] = {
        val stmts = array.element.genStatements :+
            InitializedDeclaration(result, array.element.getResult.asInstanceOf[PolyExpr[T]])
        Vector(stmts: _*)
    }

    override def getResult: T = result

    override val conditions: Set[BoolExpr] = Set(array.index < array.size)

    override val statementsAtFuncBegin: Set[Vector[Statement]] =
        array.statementsAtFuncBegin | array.index.statementsAtFuncBegin

    override def globalFuncRet: String = s"result[${Index.idx.varName}] = $getResult;\n"

    override def deviceFuncRet: String = s"return $getResult;\n"
}

implicit def tmpArray(t: TmpOneDimFloatArrayType): TmpArrayAccess[FloatType] = TmpArrayAccess(t)

case class FunctionApplication[T <: Type](fn: DeviceFunc[T], args: Expr*) extends PolyExpr[T] {
    override def codeGen: String = {
        val argList = args.map(_.codeGen + ", ").foldLeft("")(_ + _)
        val argListStripped = if (argList.isEmpty) " " else argList.substring(0, argList.length - 2)
        s"${fn.name}($argListStripped)"
    }

    override val typeName: String = fn.body.typeName
    override val refTypeName: String = fn.body.refTypeName

    override def newInstance: T = fn.body.newInstance

    private val result = fn.body.newInstance

    override def genStatements: Vector[Statement] =
        Vector(
            Declaration(result),
            Assignment(result, FunctionApplication(fn, args: _*))
        )

    override def getResult: T = result

    override val conditions: Set[BoolExpr] = args.map(_.conditions).fold(Set())(_ ++ _)

    override val statementsAtFuncBegin: Set[Vector[Statement]] =
        args.map(_.statementsAtFuncBegin).fold(Set())(_ ++ _)

    override def globalFuncRet: String = s"*result = $getResult;\n"

    override def deviceFuncRet: String = s"return $getResult;\n"
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

    override val conditions: Set[BoolExpr] = srcA.conditions | srcB.conditions

    override val statementsAtFuncBegin: Set[Vector[Statement]] =
        srcA.statementsAtFuncBegin | srcB.statementsAtFuncBegin
}

case class NE[T <: Type](srcA: PolyExpr[T], srcB: PolyExpr[T]) extends BoolExpr {
    override def codeGen: String = s"(${srcA.codeGen} != ${srcB.codeGen})"

    override val conditions: Set[BoolExpr] = srcA.conditions | srcB.conditions

    override val statementsAtFuncBegin: Set[Vector[Statement]] =
        srcA.statementsAtFuncBegin | srcB.statementsAtFuncBegin
}

case class LT[T <: Type](srcA: PolyExpr[T], srcB: PolyExpr[T]) extends BoolExpr {
    override def codeGen: String = s"(${srcA.codeGen} < ${srcB.codeGen})"

    override val conditions: Set[BoolExpr] = srcA.conditions | srcB.conditions

    override val statementsAtFuncBegin: Set[Vector[Statement]] =
        srcA.statementsAtFuncBegin | srcB.statementsAtFuncBegin
}

case class LE[T <: Type](srcA: PolyExpr[T], srcB: PolyExpr[T]) extends BoolExpr {
    override def codeGen: String = s"(${srcA.codeGen} <= ${srcB.codeGen})"

    override val conditions: Set[BoolExpr] = srcA.conditions | srcB.conditions

    override val statementsAtFuncBegin: Set[Vector[Statement]] =
        srcA.statementsAtFuncBegin | srcB.statementsAtFuncBegin
}

case class GT[T <: Type](srcA: PolyExpr[T], srcB: PolyExpr[T]) extends BoolExpr {
    override def codeGen: String = s"(${srcA.codeGen} > ${srcB.codeGen})"

    override val conditions: Set[BoolExpr] = srcA.conditions | srcB.conditions

    override val statementsAtFuncBegin: Set[Vector[Statement]] =
        srcA.statementsAtFuncBegin | srcB.statementsAtFuncBegin
}

case class GE[T <: Type](srcA: PolyExpr[T], srcB: PolyExpr[T]) extends BoolExpr {
    override def codeGen: String = s"(${srcA.codeGen} >= ${srcB.codeGen})"

    override val conditions: Set[BoolExpr] = srcA.conditions | srcB.conditions

    override val statementsAtFuncBegin: Set[Vector[Statement]] =
        srcA.statementsAtFuncBegin | srcB.statementsAtFuncBegin
}

case class And(srcA: BoolExpr, srcB: BoolExpr) extends BoolExpr {
    override def codeGen: String = s"(${srcA.codeGen} && ${srcB.codeGen})"

    override val conditions: Set[BoolExpr] = srcA.conditions | srcB.conditions

    override val statementsAtFuncBegin: Set[Vector[Statement]] =
        srcA.statementsAtFuncBegin | srcB.statementsAtFuncBegin
}

case class Or(srcA: BoolExpr, srcB: BoolExpr) extends BoolExpr {
    override def codeGen: String = s"(${srcA.codeGen} || ${srcB.codeGen})"

    override val conditions: Set[BoolExpr] = srcA.conditions | srcB.conditions

    override val statementsAtFuncBegin: Set[Vector[Statement]] =
        srcA.statementsAtFuncBegin | srcB.statementsAtFuncBegin
}

case class Not(srcA: BoolExpr) extends BoolExpr {
    override def codeGen: String = s"(!${srcA.codeGen})"

    override val conditions: Set[BoolExpr] = srcA.conditions

    override val statementsAtFuncBegin: Set[Vector[Statement]] =
        srcA.statementsAtFuncBegin
}

case class Ternary[T <: Type](cond: BoolExpr)(val thenBody: PolyExpr[T])(val elseBody: PolyExpr[T]) extends PolyExpr[T] {
    override val typeName: String = thenBody.typeName
    override val refTypeName: String = thenBody.refTypeName

    override def codeGen: String =
        s"${cond.codeGen} ? ${thenBody.codeGen} : ${elseBody.codeGen}"

    override def newInstance: T = thenBody.newInstance

    private val result = thenBody.newInstance

    override def genStatements: Vector[Statement] = {
        val thenStmts = thenBody.genStatements :+ Assignment(result, thenBody.getResult.asInstanceOf[PolyExpr[T]])
        val elseStmts = elseBody.genStatements :+ Assignment(result, elseBody.getResult.asInstanceOf[PolyExpr[T]])
        Vector(
            Declaration(result),
            IfThenElse(cond)(thenStmts: _*)(elseStmts: _*)
        )
    }

    override def getResult: T = result

    override val conditions: Set[BoolExpr] = thenBody.conditions | elseBody.conditions

    override val statementsAtFuncBegin: Set[Vector[Statement]] =
        thenBody.statementsAtFuncBegin | elseBody.statementsAtFuncBegin

    override def globalFuncRet: String = s"*result = $getResult;\n"

    override def deviceFuncRet: String = s"return $getResult;\n"
}
