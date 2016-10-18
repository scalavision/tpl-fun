package tpl.peano2

import org.specs2.mutable.Specification
import tpl.peano2.PeanoModule2._

/**
  * Created by tomsorlie on 10/14/16.
  */
class PeanoModule2Spec extends Specification {
  import shapeless.test.illTyped

  "simple natural numbers" >> {

    val nat1 = NatN(Nat0)
    val nat2 = NatN(nat1)
    val nat3 = NatN(nat2)

    Nat0 + nat1 must_== nat1
    nat1 + nat1 must_== nat2

  }

  "testing on the value level" >> {
    import NatT._

    type Nat1 = NatT_N[NatT_0]
    type Nat2 = NatT_N[Nat1]
    type Nat3 = NatT_N[Nat2]

    implicitly[NatT_0 + Nat1 =:= Nat1]
    illTyped("implicitly[Nat0 =:= Nat1]")

    implicitly[Nat1 + Nat1 =:= Nat2]

    // if we reach this, everything is OK!
    1 === 1
  }


}
