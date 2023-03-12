sealed trait Statement {
    def codeGen: String // must include \n

    override def toString: String = codeGen
}

case class Assignment[T <: Expr](lhs: T, rhs: T) extends Statement {
    def codeGen = s"${lhs.codeGen} = ${rhs.codeGen};\n"
}

case class Declaration[T <: Type](variable: T) extends Statement {
    def codeGen: String = variable.defName
}

case class InitializedDeclaration[T <: Type](variable: T, initVal: PolyExpr[T]) extends Statement {
    def codeGen: String = s"${variable.defName.split('=').head}= ${initVal.codeGen};\n"
}

case class IfStmt(cond: BoolExpr)(thenBody: Statement)(elseBody: Statement) extends Statement {
    def codeGen: String = s"if(${cond.codeGen}) {\n\t${thenBody.codeGen}} else {\n\t${elseBody.codeGen}}\n"

}

case class WhileStmt(cond: BoolExpr)(body: Statement) extends Statement {
    def codeGen: String = s"while (${cond.codeGen}) {\n\t${body.codeGen}}\n"
}

case class For(init: Statement, cond: BoolExpr, post: Statement)(body: Statement) extends Statement {
    override def codeGen: String = s"for (${init.codeGen}; ${cond.codeGen}; ${post.codeGen}) {\n\t${body.codeGen}}\n"
}