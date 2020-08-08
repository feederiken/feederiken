package feederiken

import org.scalacheck._

object ModeSuite extends Properties("Mode") {
  import Prop._
  def laws(m: Mode.Lawful) = {
    implicit val arbPositiveInt = Arbitrary { Gen.posNum[Int] }
    property(s"$m.maxScoreEvidence is valid") = forAll(m.`maxScoreEvidence is valid` _)
    property(s"$m.score is commutative") = forAll(m.`score is commutative` _)
    property(s"$m.score <= maxScore") = forAll(m.`score <= maxScore` _)
  }

  laws(Mode.Prefix)
  laws(Mode.Suffix)
}
