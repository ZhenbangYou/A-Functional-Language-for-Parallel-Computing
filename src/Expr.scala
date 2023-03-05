sealed trait Expr {
    def codeGen: String
}

trait PolyExpr[T] extends Expr {
    def +(other: PolyExpr[T]): Add[T] = Add(this, other)

    def -(other: PolyExpr[T]): Sub[T] = Sub(this, other)

    def *(other: PolyExpr[T]): Mul[T] = Mul(this, other)

    def /(other: PolyExpr[T]): Div[T] = Div(this, other)

    def unary_- : Neg[T] = Neg(this)
}

case class Add[T](srcA: PolyExpr[T], srcB: PolyExpr[T]) extends PolyExpr[T] {
    override def codeGen: String = s"(${srcA.codeGen} + ${srcB.codeGen})"
}

case class Sub[T](srcA: PolyExpr[T], srcB: PolyExpr[T]) extends PolyExpr[T] {
    override def codeGen: String = s"(${srcA.codeGen} - ${srcB.codeGen})"
}

case class Mul[T](srcA: PolyExpr[T], srcB: PolyExpr[T]) extends PolyExpr[T] {
    override def codeGen: String = s"(${srcA.codeGen} * ${srcB.codeGen})"
}

case class Div[T](srcA: PolyExpr[T], srcB: PolyExpr[T]) extends PolyExpr[T] {
    override def codeGen: String = s"(${srcA.codeGen} / ${srcB.codeGen})"
}

case class Neg[T](srcA: PolyExpr[T]) extends PolyExpr[T] {
    override def codeGen: String = s"(-${srcA.codeGen})"
}

trait BoolExpr extends Expr {
    def &&(other: BoolExpr): And = And(this, other)

    def ||(other: BoolExpr): Or = Or(this, other)

    def unary_! : Not = Not(this)
}

case class EQ[T](srcA: PolyExpr[T], srcB: PolyExpr[T]) extends BoolExpr {
    override def codeGen: String = s"(${srcA.codeGen} == ${srcB.codeGen})"
}

case class NE[T](srcA: PolyExpr[T], srcB: PolyExpr[T]) extends BoolExpr {
    override def codeGen: String = s"(${srcA.codeGen} != ${srcB.codeGen})"
}

case class LT[T](srcA: PolyExpr[T], srcB: PolyExpr[T]) extends BoolExpr {
    override def codeGen: String = s"(${srcA.codeGen} < ${srcB.codeGen})"
}

case class LE[T](srcA: PolyExpr[T], srcB: PolyExpr[T]) extends BoolExpr {
    override def codeGen: String = s"(${srcA.codeGen} <= ${srcB.codeGen})"
}

case class GT[T](srcA: PolyExpr[T], srcB: PolyExpr[T]) extends BoolExpr {
    override def codeGen: String = s"(${srcA.codeGen} > ${srcB.codeGen})"
}

case class GE[T](srcA: PolyExpr[T], srcB: PolyExpr[T]) extends BoolExpr {
    override def codeGen: String = s"(${srcA.codeGen} >= ${srcB.codeGen})"
}

case class And(srcA: BoolExpr, srcB: BoolExpr) extends BoolExpr {
    override def codeGen: String = s"(${srcA.codeGen} && ${srcB.codeGen})"
}

case class Or(srcA: BoolExpr, srcB: BoolExpr) extends BoolExpr {
    override def codeGen: String = s"(${srcA.codeGen} || ${srcB.codeGen})"
}

case class Not(srcA: BoolExpr) extends BoolExpr {
    override def codeGen: String = s"(!${srcA.codeGen})"
}
