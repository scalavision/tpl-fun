package tpl

import tpl.hmap.TestHMap
import tpl.peano.{PeanoNumberTest, PeanoNumbers}

/**
  * Created by tomsorlie on 10/14/16.
  */
object Main extends App {

  println("Hello World")

//  TestHMap.demo()

  PeanoNumberTest.demo()


  val num = 1 + 2 + 3

  lazy val str = "a" + "b" + "c"

  def now = new java.util.Date


  // just like typedef from C
  // evaluated at compile time
  type MyMap = Map[Int, String]






}
