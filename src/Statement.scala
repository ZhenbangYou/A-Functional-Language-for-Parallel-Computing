sealed trait Statement

case class Assignment[T <: Expr](val lhs: T, val rhs: T) extends Statement {
    def codeGen = s"${lhs.codeGen} = ${rhs.codeGen};\n"
}