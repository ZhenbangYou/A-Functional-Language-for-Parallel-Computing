sealed trait Func {
    def codeGen: String

    override def toString: String = codeGen
}

case class GlobalFunc[T <: Type](name: String)(args: Type*)(body: PolyExpr[T]) extends Func {
    def codeGen: String = {
        val resultRefType = body.refTypeName // return type of the function, in reference type
        val RESULT_ARG = "result"
        val RESULT_ADDR = if (!body.isInstanceOf[TmpFloatArrayAccess]) "*result" else "result[idx]"
        val namesInArgs = args.map(_.argsName.head)
        val sizeInArgs = args.filter(_.argsName.length == 2).map(_.argsName(1)).toSet.toList
        val argList = (namesInArgs ++ sizeInArgs :+ s"$resultRefType $RESULT_ARG").reduce((a, b) => s"$a, $b")
        s"__global__ void $name($argList) {\n\t${Index.defineIdx}\n${statements2String(body.genStatements, "\t")}\t*result = ${body.getResult.varName};\n}\n"
    }
}

case class DeviceFunc[T <: Type](name: String)(args: Type*)(body: PolyExpr[T]) extends Func {
    def codeGen: String = {
        val resultType = body.typeName // return type of the function, in reference type
        val namesInArgs = args.map(_.argsName.head)
        val sizeInArgs = args.filter(_.argsName.length == 2).map(_.argsName(1)).toSet.toList
        val argList = if ((namesInArgs ++ sizeInArgs).isEmpty) "" else (namesInArgs ++ sizeInArgs).reduce((a, b) => s"$a, $b")
        val bodyCode = s"return ${body.codeGen};\n"
        s"__device__ $resultType $name($argList) {\n\t${Index.defineIdx}\n${statements2String(body.genStatements, "\t")}\treturn ${body.getResult.varName};\n}\n"
    }
}
