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


    fun String.fracReplace(): Int {
        val oldValue = "frac"


        var occurrenceIndex: Int = indexOf(oldValue, 0)

        return occurrenceIndex
    }

    fun String.replaceFrac(oldValue: String, newValue: String, ignoreCase: Boolean = false): String {
        run {
            var occurrenceIndex: Int = indexOf(oldValue, 0, ignoreCase)
            // FAST PATH: no match
            if (occurrenceIndex < 0) return this

            val oldValueLength = oldValue.length
            val searchStep = oldValueLength.coerceAtLeast(1)
            val newLengthHint = length - oldValueLength + newValue.length
            if (newLengthHint < 0) throw OutOfMemoryError()
            val stringBuilder = StringBuilder(newLengthHint)

            var i = 0
            do {
                stringBuilder.append(this, i, occurrenceIndex).append(newValue)
                i = occurrenceIndex + oldValueLength
                if (occurrenceIndex >= length) break
                occurrenceIndex = indexOf(oldValue, occurrenceIndex + searchStep, ignoreCase)
            } while (occurrenceIndex > 0)
            return stringBuilder.append(this, i, length).toString()
        }
    }
}