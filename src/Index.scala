object Index {
    object threadIdx {
        val x: IntType = IntType("threadIdx.x")
        val y: IntType = IntType("threadIdx.y")
        val z: IntType = IntType("threadIdx.z")
    }

    object blockIdx {
        val x: IntType = IntType("blockIdx.x")
        val y: IntType = IntType("blockIdx.y")
        val z: IntType = IntType("blockIdx.z")
    }

    object blockDim {
        val x: IntType = IntType("blockDim.x")
        val y: IntType = IntType("blockDim.y")
        val z: IntType = IntType("blockDim.z")
    }

    object gridDim {
        val x: IntType = IntType("gridDim.x")
        val y: IntType = IntType("gridDim.y")
        val z: IntType = IntType("gridDim.z")
    }

    private val idxExpr = blockIdx.x * blockDim.x + threadIdx.x

    private val IDX_NAME = "idx"

    val idx: IntType = IntType(IDX_NAME)

    def defineIdx: InitializedDeclaration[IntType] = InitializedDeclaration(idx, idxExpr)
}