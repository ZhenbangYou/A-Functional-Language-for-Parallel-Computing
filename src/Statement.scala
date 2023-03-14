trait Statement {
    def codeGen: String // must include \n

    override def toString: String = codeGen
}

case class Assignment[T <: Type](lhs: T, rhs: PolyExpr[T]) extends Statement {
    override def codeGen: String = s"${lhs.varName} = ${rhs.codeGen};\n"
}

case class ArrayStoreWithoutBoundCheck[T <: ScalarType with NewInstance[T]](array: ArrayType[T], index: IntType, value: T) extends Statement {
    override def codeGen: String =
        s"${array.varName}[${index.getResult}] = ${value.varName};"
}

case class Declaration[T <: Type](variable: T) extends Statement {
    override def codeGen: String = variable.defName
}

case class DeclareStaticArray[T <: ScalarType with NewInstance[T]](array: ArrayType[T], low: Int, high: Int) extends Statement {
    override def codeGen: String =
        s"__static__ ${array.varName}[${Constants.WARP_SIZE + high - low}];\n"
}

case class InitializedDeclaration[T <: Type](variable: T, initVal: PolyExpr[T]) extends Statement {
    override def codeGen: String = s"${variable.defName.split('=').head} = ${initVal.codeGen};\n"
}

case class IfThen(cond: BoolExpr)(thenBody: Statement*) extends Statement {
    override def codeGen: String =
        s"""if ${cond.codeGen} {
           |${statements2String(thenBody.toVector, "\t").stripTrailing}
           |}""".stripMargin

}

case class IfThenElse(cond: BoolExpr)(thenBody: Statement*)(elseBody: Statement*) extends Statement {
    override def codeGen: String =
        s"""if ${cond.codeGen} {
           |${statements2String(thenBody.toVector, "\t").stripTrailing}
           |} else {
           |${statements2String(elseBody.toVector, " \t").stripTrailing}
           |}""".stripMargin

}

case class WhileLoop(cond: BoolExpr)(body: Statement*) extends Statement {
    override def codeGen: String =
        s"""while ${cond.codeGen} {
           |${statements2String(body.toVector, "\t").stripTrailing}
           |}""".stripMargin
}

case class ForLoop(init: Statement, cond: BoolExpr, post: Statement)(body: Statement*) extends Statement {
    override def codeGen: String = {
        val initCode = init.codeGen.substring(0, init.codeGen.length - 2)
        val condCode = cond.codeGen
        val postCode = post.codeGen.substring(0, post.codeGen.length - 2)
        s"""for ($initCode; $condCode; $postCode) {
           |${statements2String(body.toVector, "\t").stripTrailing}
           |}""".stripMargin
    }
}

class SyncThreads extends Statement {
    override def codeGen: String = "__syncthreads();\n"
}

def statements2String(statements: Vector[Statement], prepend: String = ""): String =
    statements.map(x => s"${
        val xs = x.codeGen.split('\n')
        val prepended = xs.map(prepend + _ + '\n')
        prepended.foldLeft("")(_ + _)
    }").foldLeft("")(_ + _)