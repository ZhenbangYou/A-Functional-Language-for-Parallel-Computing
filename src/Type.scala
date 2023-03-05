sealed trait Type {
    val typeName: String

    val varName: String // name of the variable
    val argName: String // name in the parameter list
    val defName: String // name in variable definition, zero initialization
}

trait ScalarType extends Type

class FloatType(name: String) extends ScalarType with PolyExpr[FloatType] {
    override val typeName: String = "float"
    override val varName: String = name
    override val argName: String = s"float $varName"
    override val defName: String = s"float $varName = 0.0f;"

    override def codeGen: String = varName

    def ==(other: FloatType): EQ[FloatType] = EQ(this, other)

    def !=(other: FloatType): NE[FloatType] = NE(this, other)

    def <(other: FloatType): LT[FloatType] = LT(this, other)

    def <=(other: FloatType): LE[FloatType] = LE(this, other)

    def >(other: FloatType): GT[FloatType] = GT(this, other)

    def >=(other: FloatType): GE[FloatType] = GE(this, other)
}

trait ArrayType[T <: ScalarType] extends Type
