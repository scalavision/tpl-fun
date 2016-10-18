package tpl.hmap

/**
  * Created by tomsorlie on 10/14/16.
  */
object HMapModule {

  class HMap[K[_], V[_]](delegate: Map[K[Any], V[Any]]) {
    def apply[A](key: K[A]): V[A] =
      delegate(key.asInstanceOf[K[Any]]).asInstanceOf[V[A]]
  }

  object HMap {
    type Pair[K[_], V[_]] = (K[A], V[A]) forSome { type A }

    def apply[K[_], V[_]](tuples: (K[Any], V[Any])*) =
      new HMap[K, V](Map(tuples: _*))

  }

}

object TestHMap {

  import HMapModule._

  def demo() = {

    val someMap: Map[Option[Any], List[Any]] = Map(
      Some("foo") -> List("a", "b", "c"),
      Some(42) -> List(1, 2, 3)
    )

    val x: List[String] =
      someMap(Some("foo")).asInstanceOf[List[String]]

    // This is not typesafe, we have to cast it, if not it is going to be Any ..!!!
    val y: List[Int] =
    someMap(Some(42)).asInstanceOf[List[Int]]

    println(y)

    val hMap = HMap[Option, List](
      Some("foo") -> List("a", "b", "c"),
      Some(42) -> List(1, 2, 3)
    )

    val xh: List[String] = hMap(Some("foo"))
    // Does not compile !!!
//       val yh : List[String] = hMap(Some(42))
//    val yh: List[Int] = hMap(Some(42))



  }

}