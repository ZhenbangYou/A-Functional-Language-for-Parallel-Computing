sealed trait Func {
    def codeGen: String

    override def toString: String = codeGen
}

case class GlobalFunc[T <: Type](name: String)(args: Type*)(body: PolyExpr[T]) extends Func {
    def codeGen: String = {
        val resultRefType = body.refTypeName // return type of the function, in reference type
        val resultArg = "result"
        val resultAddress = if (!body.isInstanceOf[TmpFloatArrayAccess]) "*result" else "result[idx]"
        val namesInArgs = args.map(_.argsName.head)
        val sizeInArgs = args.filter(_.argsName.length == 2).map(_.argsName(1)).toSet.toList
        val argList = (namesInArgs ++ sizeInArgs :+ s"$resultRefType $resultArg").reduce((a, b) => s"$a, $b")
        s"""__global__ void $name($argList) {
           |\t${Index.defineIdx.codeGen.stripTrailing}
           |${statements2String(body.genStatements, "\t").stripTrailing}
           |\t${resultAddress} = ${body.getResult.varName};
           |}""".stripMargin
    }
}

case class DeviceFunc[T <: Type](name: String)(args: Type*)(body: PolyExpr[T]) extends Func {
    def codeGen: String = {
        val resultType = body.typeName // return type of the function, in reference type
        val namesInArgs = args.map(_.argsName.head)
        val sizeInArgs = args.filter(_.argsName.length == 2).map(_.argsName(1)).toSet.toList
        val argList = if ((namesInArgs ++ sizeInArgs).isEmpty) "" else (namesInArgs ++ sizeInArgs).reduce((a, b) => s"$a, $b")
        s"""__device__ $resultType $name($argList) {
           |\t${Index.defineIdx.codeGen.stripTrailing}
           |${statements2String(body.genStatements, "\t").stripTrailing}
           |\treturn ${body.getResult.varName};
           |}""".stripMargin
    }
}
