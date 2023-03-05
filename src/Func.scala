case class Func[T <: Expr](name: String)(args: List[Type])(body: T) {
    def codeGen: String = {
        val argList = args.map(_.argName).reduce((a, b) => s"$a, $b")
        val bodyCode = body.codeGen
        s"__global__ void $name($argList) {\n\t$bodyCode;\n}\n"
    }
}
