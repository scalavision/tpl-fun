package tpl.peano

/**
  * Created by tomsorlie on 10/14/16.
  */
object TypeConstructor {

  type Id[A] = A
  type MyListAlias[A] = List[A]

}

object HigherKindedTypes {

  type Id[A[_],B] = A[B]
  type MyOption = Id[Option,Int]

}

object PeanoNumbers {

/*
  (1) 0 E N
  (2) n E N => n' E N
  (3) n E N => n' != 0
  (4) m,n E N => (m' = n' => m = n )
  (5) 0 E X given that For All n E N: (n E X => n' E X) => N c X

  If some predicate S
    * Is true for 0 and
    * if from the fact that S is true for n it follows that it's also true for n + 1

  Then it's also true for every natural number

  -> Principle of induction
 */


  trait Nat
  trait _0 extends Nat

  //for each number N there exists a successor
  trait Succ[N <: Nat] extends Nat

  // Type Level representation
  object Nat {
    class _0 extends Nat
    type _1 = Succ[_0]
    type _2 = Succ[_1]
    type _3 = Succ[_2]
    type _4 = Succ[_3]
    type _5 = Succ[_4]
    type _6 = Succ[_5]
    type _7 = Succ[_6]
    type _8 = Succ[_7]
    type _9 = Succ[_8]
  }

  /*
   Base Case
   Type Level      Value
   n = 0  =>       0

   Induction step:
   n -> n + 1   =>   v(n+1) = v(n) + 1
   */

  // Evidence type (View)

  // Integer representation for every type
  // "Functor" that takes no arguments and return an Int
  // () => Int

}

object PeanoInt {
  import tpl.peano.PeanoNumbers._

  trait ToInt[N <: Nat] {
    def apply(): Int
  }

  // This function gives us the Value Representation
  // of our Type representation
  // i.e. Type _4 gives 4 as a value
  def toInt[N <: Nat](implicit iv: ToInt[N]) = iv()

  object ToInt {
    import tpl.peano.PeanoNumbers.Nat._0

    // Evidence in the form of object
    implicit val _toInt0 = new ToInt[_0] { def apply() = 0 }

    // The next Int is given by ...
    // Instances of Int
    implicit def toIntN[N <: Nat](implicit iv: ToInt[N]) =
    // iv() is the NoArg Functor that returns an Int
      new ToInt[Succ[N]] { def apply() = iv() + 1 }
  }


  /*
  Summation:

    Recursive definition:
      n + 0   :=    n
      n + m'  :=   (n + m)'

    Example:
      a + 1 = a + 0'  = (a + 0)' = a'
      // "one" is the successor of zero
      // a + 0 can be replaced by a
   */


  /*
  A,B,C are Natural Numbers of type Nat
  A + B = C
   */
  trait SumAux[A <: Nat, B <: Nat, C <: Nat]

  object SumAux {
    import Nat._0
    // base case
    implicit def sum0[A <: Nat] = new SumAux[A, _0, A]{}

    // a + b' = (a + b)' can also be expressed like
    // a + b' = a' + b  be can be reduced further and further, until it is 0
    implicit def sumN[ A <: Nat, B <: Nat, C <: Nat](
      // The compiler will recursively traverse the types
      // down to Nat._0 for us !! because of Succ[A]
      // a + b' = a' + b where a' is Succ[A]
      implicit ev: SumAux[Succ[A],B,C]
    ) =
      new SumAux[A, Succ[B], C] {}
  }

 /*
  Multiplication

    n * 0  := 0
    n * m' := n * m + n

    a * 2 = a * 1'      : a * 1' = a * 1 + a <=> n * m' = n * m + n
          = a * 1 + a
          = a * 0' + a  : 0 is successor of 1
          = a * 0 + a + a
          = a + a
 */


  trait ProdAux[A <: Nat, B <: Nat, C <: Nat]

  object ProdAux {
    import Nat._0

    implicit def prod0[A <: Nat] = new ProdAux[A, _0, _0]{}

    // a * b' = d => a * b + a = d
    implicit def prodN[A <: Nat, B <: Nat, C <: Nat, D <: Nat](
      implicit evp: ProdAux[A,B,C], evs: SumAux[A, C, D]
    ) =
      new ProdAux[A, Succ[B], D] {}
  }

  /*
      Factorial

      0!  :=  1
      n'! := n! * n!

      3!  = 2'!
          = 3!  * 2'
          = 1'! * 2'
          = 1!  * 1' * 2'
          = 0'! * 1' * 2
          = 0!  * 0' * 1'  * 2'
          :=1  := 1  :=2   :=3

      = 6
   */

  trait FactAux[A <: Nat, B <: Nat]

  object FactAux {
    import Nat._


    implicit val fact0 = new FactAux[_0, _1]{}

    implicit def factN[A <: Nat, B <: Nat, C <: Nat](
      implicit evf: FactAux[A,B], evp: ProdAux[B, Succ[A], C]
    ) =
      new FactAux[Succ[A], C] { }

  }

}


object PrintOut {
  import tpl.peano.PeanoNumbers.Nat
  import tpl.peano.PeanoInt.FactAux
  import tpl.peano.PeanoInt.ToInt

  trait Fact[A <: Nat] {
    type Out <: Nat
  }

  object Fact {
    implicit def fact[A <: Nat, B <: Nat](implicit ev: FactAux[A, B]) =
      new Fact[A] { type Out = B }
  }

  def toInt[A <: Nat, B <: Nat](f: Fact[A])(
    implicit ev: FactAux[A,B], iv: ToInt[B]
  ) = iv()

}


object PeanoNumberTest {

  def demo() = {
    import tpl.peano.PeanoNumbers.Nat._
    import PeanoInt._
    import tpl.peano.PrintOut._

    println(PeanoInt.toInt[_7])

    implicitly[SumAux[_2, _3, _5]]
    implicitly[SumAux[_4, _5, _9]]

    implicitly[ProdAux[_2,_3,_6]]


    implicitly[FactAux[_3, _6]]

    println(PrintOut.toInt(implicitly[Fact[_4]]))

    // this will not work
//    implicitly[SumAux[_4, _5, _3]]

  }

}
