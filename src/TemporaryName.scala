object TemporaryName {
    private var cnt = -1

    def apply(): String = {
        cnt += 1
        s"t$cnt"
    }
}
