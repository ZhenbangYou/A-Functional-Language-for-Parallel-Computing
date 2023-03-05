sealed trait Statement {
    def codeGen: String

    override def toString: String = codeGen
}

case class Assignment[T <: Expr](val lhs: T, val rhs: T) extends Statement {
    def codeGen = s"${lhs.codeGen} = ${rhs.codeGen};\n"
}