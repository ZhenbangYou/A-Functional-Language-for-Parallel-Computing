import scala.language.implicitConversions

trait Typable {
    val typeName: String

    val refTypeName: String
}

sealed trait Type extends Typable {
    val varName: String // name of the variable

    def argsName: List[String] // names in the parameter list
    // can have multiple elements, e.g., arrays need to be specified size

    def defName: String // name in variable definition, zero initialization
}

trait ScalarType extends Type

class FloatType(val varName: String) extends ScalarType with PolyExpr[FloatType] {
    override val typeName: String = "float"

    override val refTypeName: String = s"$typeName*"

    override def argsName: List[String] = List(s"$typeName $varName")

    override def defName: String = s"$typeName $varName = 0.0f;\n"

    override def codeGen: String = varName
}

implicit def floatConst(f: Float): FloatType = FloatType(f.toString)

class IntType(val varName: String) extends ScalarType with PolyExpr[IntType] {
    override val typeName: String = "int"

    override val refTypeName: String = s"$typeName*"

    override def argsName: List[String] = List(s"$typeName $varName")

    override def defName: String = s"$typeName $varName = 0;\n"

    override def codeGen: String = varName
}

implicit def intConst(i: Int): IntType = IntType(i.toString)

trait ArrayType[T <: ScalarType] extends Type {
    val varName: String
    val size: IntType
    val baseTypeName: String
}

class OneDimFloatArrayType(val varName: String)(val size: IntType) extends ArrayType[FloatType] {
    val typeName: String = "float*"
    val refTypeName: String = typeName
    override val baseTypeName: String = "float"

    def argsName: List[String] = List(s"float $varName[]", s"int ${size.varName}")

    def defName: String = ???

    def apply(index: PolyExpr[IntType]): ArrayAccess[FloatType] = ArrayAccess(this, index)
}