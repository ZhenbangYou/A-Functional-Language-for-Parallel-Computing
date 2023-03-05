trait Typable {
    val typeName: String

    val refTypeName: String
}

sealed trait Type extends Typable {
    val varName: String // name of the variable

    def argName: String // name in the parameter list

    def defName: String // name in variable definition, zero initialization
}

trait ScalarType extends Type

class FloatType(val varName: String) extends ScalarType with PolyExpr[FloatType] {
    override val typeName: String = "float"

    override val refTypeName: String = s"$typeName*"

    override def argName: String = s"$typeName $varName"

    override def defName: String = s"$typeName $varName = 0.0f;"

    override def codeGen: String = varName

    def ==(other: FloatType): EQ[FloatType] = EQ(this, other)

    def !=(other: FloatType): NE[FloatType] = NE(this, other)

    def <(other: FloatType): LT[FloatType] = LT(this, other)

    def <=(other: FloatType): LE[FloatType] = LE(this, other)

    def >(other: FloatType): GT[FloatType] = GT(this, other)

    def >=(other: FloatType): GE[FloatType] = GE(this, other)
}

class IntType(val varName: String) extends ScalarType with PolyExpr[IntType] {
    override val typeName: String = "int"

    override val refTypeName: String = s"$typeName*"

    override def argName: String = s"$typeName $varName"

    override def defName: String = s"$typeName $varName = 0;"

    override def codeGen: String = varName

    def ==(other: IntType): EQ[IntType] = EQ(this, other)

    def !=(other: IntType): NE[IntType] = NE(this, other)

    def <(other: IntType): LT[IntType] = LT(this, other)

    def <=(other: IntType): LE[IntType] = LE(this, other)

    def >(other: IntType): GT[IntType] = GT(this, other)

    def >=(other: IntType): GE[IntType] = GE(this, other)
}

trait ArrayType[T <: ScalarType] extends Type
