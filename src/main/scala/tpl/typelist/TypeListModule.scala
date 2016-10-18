package tpl.typelist

import tpl.matrix.VectorModule.NonEmptyVector

/**
  * Created by tomsorlie on 10/15/16.
  */

object NatModule {

  sealed trait Nat {
    type plus[That <: Nat] <: Nat
  }

  sealed trait Nat0 extends Nat {
    type plus[That <: Nat] = That
  }

  sealed trait NatN[Prev <: Nat] extends Nat {
    type plus[That <: Nat] = NatN[Prev#plus[That]]
  }


  object Nat {

    type +[A <:Nat, B <: Nat] = A#plus[B]

  }


}

object TypeListModule {

  import NatModule._
  import Nat._

  sealed trait TypeList {
    type size <: Nat
    type reduce <: Nat
    type map[F[Nat] <: Nat] <: TypeList
    type fold[A <: Nat, F[_ <: Nat, _ <: Nat] <: Nat ] <: Nat
  }

  sealed trait TNil extends TypeList {
    type size = Nat0
    type reduce = Nat0
    type map[F[Nat] <: Nat] = TNil
    type fold[A <: Nat, F[_ <: Nat, _ <: Nat] <: Nat ]  = A
  }


  // H = Head and T = Tail
  sealed trait ::[H <: Nat, T <: TypeList] extends TypeList {
    type size = NatN[T#size]
    type reduce = H + (T#reduce)
    type map[F[Nat] <: Nat] = F[H] :: T#map[F]
    type fold[A <: Nat, F[_ <: Nat, _ <: Nat] <: Nat ] =
      F[H, T#fold[A,F]]

  }

  type Nat1 = NatN[Nat0]
  type Nat2 = NatN[Nat1]
  type Nat3 = NatN[Nat2]
  type Nat4 = NatN[Nat3]

  type L = Nat0 :: Nat1 :: Nat2 :: TNil

}
