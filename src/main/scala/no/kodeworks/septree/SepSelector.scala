package no.kodeworks.septree

/*
index = 0 means include always
 */
case class SepSelector(
                        index: Int,
                        subSelectors: Array[SepSelector]
                      ) {
  if (0 != index) {
    assume(1 <= index && index <= 7, "index must be between 1 and 7, inclusive")
    assume(7 == subSelectors.size, "must provide exactly 7 subSelectors")
    val noZeros = subSelectors.filter(_.index != 0)
    assume(noZeros.distinct.size == noZeros.size, "cannot provide duplicate subSelectors per index")
  }
}

object SepSelector {
  val empty: SepSelector = apply(0, Array.empty[SepSelector])

  def apply(index: Int, subSelectors: SepSelector*): SepSelector = apply(index, subSelectors.toList)

  def apply(index: Int, subSelectors: List[SepSelector] = Nil): SepSelector = {
    assume(subSelectors.size <= 7, "must provide less than or equal to 7 subSelectors")
    val subSels =
      if (subSelectors.isEmpty) {
        Array.fill(7)(empty)
      } else {
        val sortedSubSels = subSelectors.sortBy(_.index).toArray
        var i = 0
        var j = -1
        Array.ofDim[SepSelector](7).mapInPlace { _ =>
          j += 1
          if (i < sortedSubSels.size) {
            val s = sortedSubSels(i)
            if (s.index == j + 1) {
              i += 1
              s
            } else SepSelector.empty
          } else SepSelector.empty
        }
      }
    apply(index, subSels)
  }
}
