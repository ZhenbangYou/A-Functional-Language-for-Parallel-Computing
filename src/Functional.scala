// map (and unary element-wise)
// map with idx
// zip with (and binary element-wise)
// reduce

extension (array: OneDimFloatArrayType) {
    def map(f: PolyExpr[FloatType] => PolyExpr[FloatType]): PolyExpr[FloatType] = {
        val element = array(Index.idx)
        f(element)
    }
}