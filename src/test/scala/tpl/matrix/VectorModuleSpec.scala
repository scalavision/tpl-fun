package tpl.matrix

import org.specs2.mutable.Specification

/**
  * Created by tomsorlie on 10/14/16.
  */
class VectorModuleSpec extends Specification {

  import VectorModule._

  "should be able to add vectors " >> {

    val sum = ( 1 :: 2 :: VNil) + (3 :: 4 :: VNil)
    sum  mustEqual 4 :: 6 ::VNil

  }

  "should be able to concatenate vectors " >> {

    val  concat = ((1 :: 2 :: VNil) ++ (3 :: VNil))

    concat mustEqual 1 :: 2 :: 3 :: VNil

  }

}
