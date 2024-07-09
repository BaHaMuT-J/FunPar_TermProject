import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.collection.parallel.CollectionConverters._

object BST {

  trait BST[+T]

  case object Empty extends BST[Nothing]

  case class Node[T](key: T, left: BST[T], right: BST[T]) extends BST[T] {
    var k: T = key
    var l: BST[T] = left
    var r: BST[T] = right

    // auxiliary constructors
    def this(k: T) = this(k, Empty, Empty)
    def this(k: T, l: BST[T]) = this(k, l, Empty)
  }

  class ParBST[T](initialRoot: BST[T] = Empty)(implicit ord: Ordering[T]) {
    @volatile var root: BST[T] = initialRoot

    private def inserthelpfunction(node: BST[T], key: T): BST[T] = node match {
      case Empty => Node(key)
      case Node(k, l, r) =>
        if (ord.lt(key, k)) {
          Node(k, insertHelper(l, key), r)
        } else if (ord.gt(key, k)) {
          Node(k, l, insertHelper(r, key))
        } else {
          node 
        }
    }

    def insert(keys: Seq[T]): Future[Unit] = {
      val futures = keys.par.map { key =>
        Future {
          this.synchronized {
            root = inserthelpfunction(root, key)
          }
        }
      }.toList

      Future.sequence(futures).map(_ => ())
    }
  }
}
