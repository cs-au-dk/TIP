package tip.util

/**
  * Implicit classes providing convenience methods for maps.
  */
object MapUtils {

  /**
    * Makes a 'reverse' method available on objects of type `Map[A, Set[B]]`.
    */
  implicit class ReverseOp[A, B](m: Map[A, Set[B]]) {
    def reverse: Map[B, Set[A]] = {
      var res = Map[B, Set[A]]()
      m.keys.foreach { k =>
        m(k).foreach { v =>
          val ins = res.getOrElse(v, Set[A]())
          res += (v -> (ins + k))
        }
      }
      res
    }
  }

  /**
    * Makes a 'reverse' method available on objects of type `Map[A, B]`.
    */
  implicit class ReverseOp2[A, B](m: Map[A, B]) {
    def reverse: Map[B, Set[A]] = {
      var res = Map[B, Set[A]]()
      m.keys.foreach { k =>
        val ins = res.getOrElse(m(k), Set[A]())
        res += (m(k) -> (ins + k))
      }
      res
    }
  }

}
