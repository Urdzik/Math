object TaculatorExtension {

    fun String.have(char: Char): Boolean {
        var result = false
        for (it in this) {
            if (it == char) {
                result = true
                break
            }
        }
        return result
    }
}