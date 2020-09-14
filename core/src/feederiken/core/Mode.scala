package feederiken.core

/**
  * A mode of rating key fingerprints. An instance allows the brute-forcing process to compare its finding to the goal of the search.
  */
sealed abstract class Mode extends Product with Serializable {

  /**
    * Rate the similarity of two byte sequences according to the criteria. Both sequences may be of any length. This operation must be commutative.
    */
  def score(fpr1: IndexedSeq[Byte], fpr2: IndexedSeq[Byte]): Int

  /**
    * Compute the highest possible similarity score according to the criteria.
    */
  def maxScore(fpr1: IndexedSeq[Byte], fpr2len: Int): Int
}

object Mode {

  /**
    * Law definitions and testing for Mode instances.
    */
  sealed trait Lawful extends Mode {

    /**
      * Provide evidence that there exists a fingerprint that achieves the maximum score.
      */
    def maxScoreEvidence(fpr1: IndexedSeq[Byte], fpr2len: Int): IndexedSeq[Byte]

    /**
      * Help ensure commutativity
      */
    final def `score is commutative`(
        fpr1: IndexedSeq[Byte],
        fpr2: IndexedSeq[Byte],
    ) =
      score(fpr1, fpr2) == score(fpr2, fpr1)

    /**
      * Help ensure that `maxScore` is an upper bound
      */
    final def `score <= maxScore`(
        fpr1: IndexedSeq[Byte],
        fpr2: IndexedSeq[Byte],
    ) =
      score(fpr1, fpr2) <= maxScore(fpr1, fpr2.length)

    /**
      * Help ensure that there exists an input that achives the maximum score.
      */
    final def `maxScoreEvidence is valid`(
        fpr1: IndexedSeq[Byte],
        fpr2len: Int,
    ) =
      score(fpr1, maxScoreEvidence(fpr1, fpr2len)) == maxScore(fpr1, fpr2len)
  }

  /**
    * A mode to search for a key with a given prefix.
    */
  case object Prefix extends Lawful {
    override def score(fpr1: IndexedSeq[Byte], fpr2: IndexedSeq[Byte]): Int =
      (fpr1.iterator zip fpr2).takeWhile(x => x._1 == x._2).length
    override def maxScore(fpr1: IndexedSeq[Byte], fpr2len: Int): Int =
      fpr1.length min fpr2len
    override def maxScoreEvidence(
        fpr1: IndexedSeq[Byte],
        fpr2len: Int,
    ): IndexedSeq[Byte] = fpr1.take(fpr2len)
  }

  /**
    * A mode to search for a key with a given suffix.
    */
  case object Suffix extends Lawful {
    override def score(fpr1: IndexedSeq[Byte], fpr2: IndexedSeq[Byte]): Int =
      (fpr1.reverseIterator zip fpr2.reverseIterator)
        .takeWhile(x => x._1 == x._2)
        .length
    override def maxScore(fpr1: IndexedSeq[Byte], fpr2len: Int): Int =
      fpr1.length min fpr2len
    override def maxScoreEvidence(
        fpr1: IndexedSeq[Byte],
        fpr2len: Int,
    ): IndexedSeq[Byte] = fpr1.takeRight(fpr2len)
  }
}
