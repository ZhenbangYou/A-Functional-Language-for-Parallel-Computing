sealed trait Func extends Statement {
    def codeGen: String

    override def toString: String = codeGen
}

case class GlobalFunc[T <: Type](name: String)(args: Type*)(body: PolyExpr[T]) extends Func {
    override def codeGen: String = {
        val resultRefType = body.refTypeName // return type of the function, in reference type
        val resultArg = "result"
        val resultAddress = if (!body.isInstanceOf[TmpFloatArrayAccess]) "*result" else "result[idx]"
        val namesInArgs = args.map(_.argsName.head)
        val sizeInArgs = args.filter(_.argsName.length == 2).map(_.argsName(1)).toSet.toList
        val argList = (namesInArgs ++ sizeInArgs :+ s"$resultRefType $resultArg").reduce((a, b) => s"$a, $b")
        val statementsAtFuncBegin = statements2String(body.statementsAtFuncBegin.fold(Vector())(_ ++ _), "\t")
        if (body.conditions.isEmpty)
            s"""__global__ void $name($argList) {
               |\t${Index.defineIdx.codeGen.stripTrailing}
               |${statementsAtFuncBegin.stripTrailing}
               |${statements2String(body.genStatements, "\t").stripTrailing}
               |\t$resultAddress = ${body.getResult.varName};
               |}""".stripMargin
        else {
            val cond = body.conditions.map(_.codeGen).reduce((a, b) => a + " && " + b)
            s"""__global__ void $name($argList) {
               |\t${Index.defineIdx.codeGen.stripTrailing}
               |${statementsAtFuncBegin.stripTrailing}
               |${statements2String(body.genStatements, "\t").stripTrailing}
               |\tif $cond {
               |\t\t$resultAddress = ${body.getResult.varName};
               |\t}
               |}""".stripMargin
        }
    }
}

case class DeviceFunc[T <: Type](name: String)(val args: Type*)(val body: PolyExpr[T]) extends Func {
    override def codeGen: String = {
        val resultType = body.typeName // return type of the function, in reference type
        val namesInArgs = args.map(_.argsName.head)
        val sizeInArgs = args.filter(_.argsName.length == 2).map(_.argsName(1)).toSet.toList
        val argList = if ((namesInArgs ++ sizeInArgs).isEmpty) "" else (namesInArgs ++ sizeInArgs).reduce((a, b) => s"$a, $b")
        val statementsAtFuncBegin = statements2String(body.statementsAtFuncBegin.fold(Vector())(_ ++ _), "\t")
        s"""__device__ $resultType $name($argList) {
           |\t${Index.defineIdx.codeGen.stripTrailing}
           |${statementsAtFuncBegin.stripTrailing}
           |${statements2String(body.genStatements, "\t").stripTrailing}
           |\treturn ${body.getResult.varName};
           |}""".stripMargin
    }

    def apply(params: Expr*): FunctionApplication[T] = {
        assert(params.length == args.length)
        FunctionApplication(this, params: _*)
    }
}
