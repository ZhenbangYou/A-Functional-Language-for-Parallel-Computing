object Index {
    object threadIdx {
        val x = IntType("threadIdx.x")
        val y = IntType("threadIdx.y")
        val z = IntType("threadIdx.z")
    }

    object blockIdx {
        val x = IntType("blockIdx.x")
        val y = IntType("blockIdx.y")
        val z = IntType("blockIdx.z")
    }

    object blockDim {
        val x = IntType("blockDim.x")
        val y = IntType("blockDim.y")
        val z = IntType("blockDim.z")
    }

    object gridDim {
        val x = IntType("gridDim.x")
        val y = IntType("gridDim.y")
        val z = IntType("gridDim.z")
    }

    val id = blockIdx.x * blockDim.x + threadIdx.x;
}