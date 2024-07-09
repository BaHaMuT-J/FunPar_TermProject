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

    //Not sure whether this is enough parallelization or not. Will edit soon
    
    def combineTrees[T](tree1: BST[T], tree2: BST[T])(implicit ord: Ordering[T]): Future[BST[T]] = {
      def traverse(tree: BST[T]): Future[List[T]] = Future {
        def traverseHelper(node: BST[T]): List[T] = node match {
          case Empty => List()
          case Node(k, l, r) =>
            traverseHelper(l) ::: List(k) ::: traverseHelper(r)
        }
        traverseHelper(tree)
      }

      val futureList1 = traverse(tree1)
      val futureList2 = traverse(tree2)

      for {
        list1 <- futureList1
        list2 <- futureList2
        mergedList = (list1.par ++ list2.par).toList.sorted(ord)
      } yield sortedListToBST(mergedList)
    }
  }
  def traverseHelper[T](node: BST[T]): List[T] = node match {
  case Empty => List()
  case Node(k, l, r) =>
    traverseHelper(l) ::: List(k) ::: traverseHelper(r)
  }

  def combineTrees[T](tree1: BST[T], tree2: BST[T])(implicit ord: Ordering[T]): Future[BST[T]] = {
    val list_1 = Future(traverseHelper(tree1))
    val list_2 = Future(traverseHelper(tree2))

    for {
      list1 <- list_1
      list2 <- list_2
      merge_tgt = (list1.par ++ list2.par).toList.sorted(ord)
    } yield sortedListToBST(merge_tgt)
  }
}
