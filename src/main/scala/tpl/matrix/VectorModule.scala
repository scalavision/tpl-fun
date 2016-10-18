package tpl.matrix

/**
  * Created by tomsorlie on 10/14/16.
  */
object VectorModule {



  //   Validates only at runtime, not typesafe

 /* sealed trait Vector {
    def size: Int
    def ::(head: Int): Vector = NonEmptyVector(head, this)
    def +(that: Vector): Vector
  }

  case object VNil extends Vector {

    override def size: Int = 0

    override def +(that: Vector): Vector =  {
      require(that == VNil)
      this
    }
  }

  case class NonEmptyVector(head: Int, tail: Vector) extends Vector {
    override def size: Int = 1 + tail.size

    override def +(that: Vector): Vector = {

      require(that.size == this.size)
      that match {
        case NonEmptyVector(h, t) => (head + h):: (tail + t)
        case _ =>
          throw new Exception("Destination is unreachable")
      }

    }

  }*/

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

  sealed trait Vector[Size <: Nat] {
    import Nat._

    def size: Int

    // creating a vector with one more element
    // thus we have NatN
    def ::(head: Int) : Vector[NatN[Size]] =
      NonEmptyVector(head, this)

    // based on the type signature you should be
    // able to tell what this function do ...
    def ++[ThatSize <: Nat](that: Vector[ThatSize]): Vector[Size + ThatSize]

    def +(that: Vector[Size]): Vector[Size]

  }

  case object VNil extends Vector[Nat0] {
    import Nat._

    type Size = Nat0

    override def size = 0

    override def ::(
      head: Int
    ) = NonEmptyVector(head, this)

    override def +(

      that: Vector[Nat0]

    ): Vector[Nat0] =
      this

    override def ++[ThatSize <: Nat](

      that: Vector[ThatSize]

    ): Vector[+[Size, ThatSize]] =
      that
  }

  case class NonEmptyVector[TailSize <: Nat](head: Int, tail:Vector[TailSize]) extends Vector[NatN[TailSize]] {
    type Size = NatN[TailSize]
    import Nat._

    override def size: Int = 1 + tail.size

    override def +(that: Vector[Size]): Vector[Size] =
      that match {
        case NonEmptyVector(h,t) =>
          NonEmptyVector(h + head, tail + t)
      }

    override def ++[ThatSize <: Nat](

      that: Vector[ThatSize]

    ): Vector[+[Size, ThatSize]] =
      NonEmptyVector(head, tail ++ that)

  }


}
