package tpl.bool

import org.specs2.mutable.Specification

/**
  * Created by tomsorlie on 10/14/16.
  */
class BoolModuleSpec extends Specification {
  import BoolModule._
  import BoolType._
  import shapeless.test.illTyped

  "working on the compile level for BoolTypes " >> {

    implicitly[TrueType =:= TrueType]
    implicitly[FalseType =:= FalseType]
    implicitly[TrueType#Not =:= FalseType]
    implicitly[FalseType#Not =:= TrueType]

    // passed as a string to the compiler via a  macro
    // if it fails, the test is successfull !!
    // not 100% typesafe though, arbitrary string will pass
    // unless it is valid scala code

    implicitly[\/[FalseType, FalseType] =:= (FalseType \/ FalseType)]

    illTyped("implicitly[TrueType =:= FalseType]")
    illTyped("implicitly[TrueType#Not =:= TrueType]")

    2 === 2

  }




}
