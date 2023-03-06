// map (and unary element-wise)
// map with idx
// zip with (and binary element-wise)
// reduce

extension (array: OneDimFloatArrayType) {
    def map(f: PolyExpr[FloatType] => PolyExpr[FloatType]): TmpOneDimFloatArrayType = {
        val element = array(Index.idx)
        TmpOneDimFloatArrayType(f(element))(array.size)
    }
}

extension (array: TmpOneDimFloatArrayType) {
    def map(f: PolyExpr[FloatType] => PolyExpr[FloatType]): TmpOneDimFloatArrayType = {
        val element = array.element
        TmpOneDimFloatArrayType(f(element))(array.size)
    }
}

extension (array: OneDimFloatArrayType) {
    def zipWith(other: OneDimFloatArrayType)(f: (PolyExpr[FloatType], PolyExpr[FloatType]) => PolyExpr[FloatType]): TmpOneDimFloatArrayType = {
        TmpOneDimFloatArrayType(f(array(Index.idx), other(Index.idx)))(array.size)
    }
}

extension (array: OneDimFloatArrayType) {
    def zipWith(other: TmpOneDimFloatArrayType)(f: (PolyExpr[FloatType], PolyExpr[FloatType]) => PolyExpr[FloatType]): TmpOneDimFloatArrayType = {
        TmpOneDimFloatArrayType(f(array(Index.idx), other.element))(array.size)
    }
}

extension (array: TmpOneDimFloatArrayType) {
    def zipWith(other: OneDimFloatArrayType)(f: (PolyExpr[FloatType], PolyExpr[FloatType]) => PolyExpr[FloatType]): TmpOneDimFloatArrayType = {
        TmpOneDimFloatArrayType(f(array.element, other(Index.idx)))(array.size)
    }
}

extension (array: TmpOneDimFloatArrayType) {
    def zipWith(other: TmpOneDimFloatArrayType)(f: (PolyExpr[FloatType], PolyExpr[FloatType]) => PolyExpr[FloatType]): TmpOneDimFloatArrayType = {
        TmpOneDimFloatArrayType(f(array.element, other.element))(array.size)
    }
}