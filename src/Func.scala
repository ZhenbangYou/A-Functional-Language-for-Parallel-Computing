sealed trait Func

case class GlobalFunc[T <: Expr](name: String)(args: Type*)(body: T) extends Func {
    def codeGen: String = {
        val resultRefType = body.refTypeName // return type of the function, in reference type
        val RESULT_NAME = "result"
        val argList = (args.map(_.argsName) :+ s"$resultRefType $RESULT_NAME").reduce((a, b) => s"$a, $b")
        val bodyCode = s"*$RESULT_NAME = ${body.codeGen};\n"
        s"__global__ void $name($argList) {\n\t${Index.defineIdx}\t$bodyCode}\n"
    }
}

case class DeviceFunc[T <: Expr](name: String)(args: Type*)(body: T) extends Func {
    def codeGen: String = {
        val resultType = body.typeName // return type of the function, in reference type
        val argList = if (args.isEmpty) "" else args.flatMap(_.argsName).reduce((a, b) => s"$a, $b")
        val bodyCode = s"return ${body.codeGen};\n"
        s"__device__ $resultType $name($argList) {\n\t${Index.defineIdx}\t$bodyCode}\n"
    }
}
