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
    def codeGen: String = s"${variable.defName.split('=').head} = ${initVal.codeGen};\n"
}

case class IfStmt(cond: BoolExpr)(thenBody: Vector[Statement])(elseBody: Vector[Statement]) extends Statement {
    def codeGen: String =
        s"""if ${cond.codeGen} {
           |${statements2String(thenBody, "\t").stripTrailing}
           |} else {
           |${statements2String(elseBody, " \t").stripTrailing}
           |}""".stripMargin

}

case class WhileLoop(cond: BoolExpr)(body: Vector[Statement]) extends Statement {
    def codeGen: String =
        s"""while ${cond.codeGen} {
           |${statements2String(body, "\t").stripTrailing}
           |}""".stripMargin
}

case class For(init: Statement, cond: BoolExpr, post: Statement)(body: Vector[Statement]) extends Statement {
    override def codeGen: String =
        s"""for (${init.codeGen}; ${cond.codeGen}; ${post.codeGen}) {
           |${statements2String(body, "\t")}
           |}""".stripMargin
}

def statements2String(statements: Vector[Statement], prepend: String): String =
    statements.map(x => s"${
        val xs = x.codeGen.split('\n')
        val prepended = xs.map(prepend + _ + '\n')
        prepended.foldLeft("")(_ + _)
    }").foldLeft("")(_ + _)