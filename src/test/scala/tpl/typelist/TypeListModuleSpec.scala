package tpl.typelist

import org.specs2.mutable.Specification

/**
  * Created by tomsorlie on 10/15/16.
  */
class TypeListModuleSpec extends Specification {
  import NatModule._
  import Nat._
  import TypeListModule._

  "should be able to make type lists " >>  {
    implicitly[ L#size =:= Nat3 ]

    1 == 1
  }



  "mapping over types, i.e. a type lambda " >> {
    type L1 = Nat0 :: Nat1 :: Nat2 :: TNil
    type L2 = Nat1 :: Nat2 :: Nat3 :: TNil

    // Holy crap y'all, a type lambda ...
    type LM = L1#map[({type F[i <: Nat] = NatN[i]  }) #F]

    implicitly[LM =:= L2]

    1 == 1
  }

  "folding over types, i.e. a foldLeft " >> {
    type L1 = Nat2 :: Nat1 :: Nat0 :: Nat1 :: TNil

    type LF = L1#fold[Nat0, ({type F[A <: Nat, B <: Nat] = A + B})#F]

    implicitly[ LF =:= Nat4]

    1 == 1
  }

}
