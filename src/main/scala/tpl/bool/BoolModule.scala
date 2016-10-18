package tpl.bool

/**
  * Created by tomsorlie on 10/14/16.
  */
object BoolModule {

  sealed trait BoolVal {
    def not: BoolVal
    def or(that: BoolVal): BoolVal
  }

  case object TrueVal extends BoolVal {
    override def not = FalseVal
    override def or(that: BoolVal) = TrueVal
  }

  case object FalseVal extends BoolVal {
    override def not = FalseVal
    override def or(that: BoolVal) = that
  }


  sealed trait BoolType {
    type Not <: BoolType
    type Or[That <: BoolType] <: BoolType
  }

  object BoolType {
    // refining the syntax
    type \/[A <: BoolType, B <: BoolType] = A#Or[B]

  }

  sealed trait TrueType extends BoolType {
    type Not = FalseType
    type Or[That <: BoolType] = TrueType
  }

  sealed trait FalseType extends BoolType {
    type Not = TrueType
    type Or[That <: BoolType] = That
  }




  trait Bool {
    type If[T,F]
  }

  trait True extends Bool {
    type If[T,F] = T
  }

  trait False extends Bool {
    type If[T,F] = F
  }


  // # on type level is like `.` on value level
  type IfIntElseLong[A <: Bool] = A#If[Int, Long]

//  implicitly[ IfIntElseLong[True] ] =:= Int ]


}

object BoolTest {
  import BoolModule._



}