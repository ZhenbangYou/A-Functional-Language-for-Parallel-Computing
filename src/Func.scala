sealed trait Func {
    def codeGen: String

    override def toString: String = codeGen
}

case class GlobalFunc[T <: Expr](name: String)(args: Type*)(body: T) extends Func {
    def codeGen: String = {
        val resultRefType = body.refTypeName // return type of the function, in reference type
        val RESULT_ARG = "result"
        val RESULT_ADDR = if (!body.isInstanceOf[TmpFloatArrayAccess]) "*result" else "result[idx]"
        val namesInArgs = args.map(_.argsName.head)
        val sizeInArgs = args.filter(_.argsName.length == 2).map(_.argsName(1)).toSet.toList
        val argList = (namesInArgs ++ sizeInArgs :+ s"$resultRefType $RESULT_ARG").reduce((a, b) => s"$a, $b")
        val bodyCode = s"$RESULT_ADDR = ${body.codeGen};\n"
        s"__global__ void $name($argList) {\n\t${Index.defineIdx}${
            val stmts = body.genStatements
            if (stmts.isEmpty) "" else body.genStatements.map(x => s"\t$x").reduce((a, b) => s"$a$b")
        }\t*result = ${body.getResult};\n}\n"
    }
}

case class DeviceFunc[T <: Expr](name: String)(args: Type*)(body: T) extends Func {
    def codeGen: String = {
        val resultType = body.typeName // return type of the function, in reference type
        val namesInArgs = args.map(_.argsName.head)
        val sizeInArgs = args.filter(_.argsName.length == 2).map(_.argsName(1)).toSet.toList
        val argList = if ((namesInArgs ++ sizeInArgs).isEmpty) "" else (namesInArgs ++ sizeInArgs).reduce((a, b) => s"$a, $b")
        val bodyCode = s"return ${body.codeGen};\n"
        s"__device__ $resultType $name($argList) {\n\t${Index.defineIdx}${
            val stmts = body.genStatements
            if (stmts.isEmpty) "" else body.genStatements.map(x => s"\t$x").reduce((a, b) => s"$a$b")
        }\treturn ${body.getResult};\n}\n"
    }
}
