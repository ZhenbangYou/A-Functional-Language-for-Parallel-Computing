case class Func[T <: Expr](name: String)(args: List[Type])(body: T) {
    def codeGen: String = {
        val resultRefType = body.refTypeName // return type of the function, in reference type
        val RESULT_NAME = "result"
        val argList = (args.map(_.argName) :+ s"$resultRefType $RESULT_NAME").reduce((a, b) => s"$a, $b")
        val bodyCode = s"*$RESULT_NAME = ${body.codeGen};\n"
        s"__global__ void $name($argList) {\n\t$bodyCode}\n"
    }
}
