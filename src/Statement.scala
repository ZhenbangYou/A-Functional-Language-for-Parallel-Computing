trait Statement {
    def codeGen: String // must include \n

    override def toString: String = codeGen
}

case class Assignment[T <: Type](lhs: T, rhs: PolyExpr[T]) extends Statement {
    def codeGen: String = s"${lhs.varName} = ${rhs.codeGen};\n"
}

case class Declaration[T <: Type](variable: T) extends Statement {
    def codeGen: String = variable.defName
}

case class InitializedDeclaration[T <: Type](variable: T, initVal: PolyExpr[T]) extends Statement {
    def codeGen: String = s"${variable.defName.split('=').head} = ${initVal.codeGen};\n"
}

case class Ternary[T <: Type](dst: PolyExpr[T])(cond: BoolExpr)(trueBranch: PolyExpr[T])(falseBranch: PolyExpr[T]) extends Statement {
    def codeGen: String = s"${dst.codeGen} = ${cond.codeGen} ? ${trueBranch.codeGen} : ${falseBranch.codeGen};\n"
}

case class IfThen(cond: BoolExpr)(thenBody: Statement*) extends Statement {
    def codeGen: String =
        s"""if ${cond.codeGen} {
           |${statements2String(thenBody.toVector, "\t").stripTrailing}
           |}""".stripMargin

}

case class IfThenElse(cond: BoolExpr)(thenBody: Statement*)(elseBody: Statement*) extends Statement {
    def codeGen: String =
        s"""if ${cond.codeGen} {
           |${statements2String(thenBody.toVector, "\t").stripTrailing}
           |} else {
           |${statements2String(elseBody.toVector, " \t").stripTrailing}
           |}""".stripMargin

}

case class WhileLoop(cond: BoolExpr)(body: Statement*) extends Statement {
    def codeGen: String =
        s"""while ${cond.codeGen} {
           |${statements2String(body.toVector, "\t").stripTrailing}
           |}""".stripMargin
}

case class For(init: Statement, cond: BoolExpr, post: Statement)(body: Statement*) extends Statement {
    override def codeGen: String =
        s"""for (${init.codeGen}; ${cond.codeGen}; ${post.codeGen}) {
           |${statements2String(body.toVector, "\t")}
           |}""".stripMargin
}

def statements2String(statements: Vector[Statement], prepend: String = ""): String =
    statements.map(x => s"${
        val xs = x.codeGen.split('\n')
        val prepended = xs.map(prepend + _ + '\n')
        prepended.foldLeft("")(_ + _)
    }").foldLeft("")(_ + _)