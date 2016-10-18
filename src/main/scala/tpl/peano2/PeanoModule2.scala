package tpl.peano2

/**
  * Created by tomsorlie on 10/14/16.
  */
object PeanoModule2 {

  sealed trait Nat {
    def +(that: Nat): Nat  }

  case object Nat0 extends Nat {
    override def +(that: Nat) = that
  }

  case class NatN(prev: Nat) extends Nat {
    override def +(that: Nat): Nat = NatN(prev + that)
  }


  // Implemented with types
  sealed trait NatT {
    type plus[That <: NatT] <: NatT
  }

  sealed trait NatT_0 extends NatT {
    type plus[That <: NatT] = That
  }

  sealed trait NatT_N[Prev <: NatT] extends NatT {
    type plus[That <: NatT] = NatT_N[Prev#plus[That]]
  }


  object NatT {

    type +[A <:NatT, B <: NatT] = A#plus[B]

  }


}
