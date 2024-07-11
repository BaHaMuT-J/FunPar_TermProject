import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
//import scala.collection.parallel.CollectionConverters.* // Error: can't import

object BST {

  trait BST[+T]

  case object Empty extends BST[Nothing]

  case class Node[T](key: T, left: BST[T], right: BST[T]) extends BST[T]

  class ParBST[T](initialRoot: BST[T])(implicit ord: Ordering[T]) extends BST[T] {
    private var root: BST[T] = initialRoot

    def this(e: T)(implicit ord: Ordering[T]) = this(Node(e, Empty, Empty))

    override def toString: String = {
      def stringHelper(node: BST[T]): String = node match {
        case Empty => ""
        case Node(k, l, r) =>
          stringHelper(l) + " " + k + " " + stringHelper(r)
      }

      "[" + stringHelper(root) + "]"
    }

    def getRoot: BST[T] = root

    def insertSeq(e: T): Unit = {
      def insertHelper[K](e: K, rt: BST[K])(implicit ord: Ordering[K]): BST[K] = rt match {
        case Empty => Node(e, Empty, Empty)
        case Node(k, l, r) =>
          val cmp = ord.compare(e, k)
          if cmp < 0 then Node(k, insertHelper(e, l), r)
          else if cmp == 0 then rt
          else Node(k, l, insertHelper(e, r))
      }

      root = insertHelper(e, root)
    }

    def +(e: T): ParBST[T] = {
      val newRoot = ParBST[T](root)
      newRoot.insertSeq(e)
      newRoot
    }

    def insertAllSeq(elements: Seq[T]): Unit = for (elem <- elements) {
      insertSeq(elem)
    }

    def ++(s: Seq[T]): ParBST[T] = {
      val newRoot = ParBST[T](root)
      newRoot.insertAllSeq(s)
      newRoot
    }

    def combineSeq(tree: ParBST[T]): ParBST[T] = {
      var newTree = this

      def combineHelper(node: BST[T]): Unit = node match {
        case Empty =>
        case Node(k, l, r) => {
          combineHelper(l)
          newTree = newTree + k
          combineHelper(r)
        }
      }

      combineHelper(tree.getRoot)
      newTree
    }

    def levelTraverseSeq: Vector[Map[Int, Vector[T]]] = {
      var result = Vector[Map[Int, Vector[T]]]()
      val q = mutable.Queue[BST[T]]()

      @tailrec
      def levelMap(m: mutable.Queue[BST[T]], d: Int): Unit = {
        var tempVec = Vector[T]()
        val newQueue = new mutable.Queue[BST[T]]()

        for (node <- m) {
          node match {
            case Empty =>
            case Node(k, l, r) =>
              newQueue.enqueue(l, r)
              tempVec = tempVec.prepended(k)
          }
        }

        if tempVec.nonEmpty then result = result.prepended(Map(d -> tempVec.reverse))
        if newQueue.nonEmpty then levelMap(newQueue, d + 1)
      }

      levelMap(new mutable.Queue[BST[T]]().enqueue(root), 1)
      result
    }


    // Parallel Programming

    private def insertParHelper(node: BST[T], key: T): BST[T] = node match {
      case Empty => Node(key, Empty, Empty)
      case Node(k, l, r) =>
        if (ord.lt(key, k)) {
          Node(k, insertParHelper(l, key), r)
        } else if (ord.gt(key, k)) {
          Node(k, l, insertParHelper(r, key))
        } else {
          node
        }
    }
    
//    def insertPar(keys: Seq[T]): Future[Unit] = {
//      // Error: value par is not a member of Seq[T]
//      val futures = keys.par.map { key =>
//        Future {
//          this.synchronized {
//            root = insertParHelper(root, key)
//          }
//        }
//      }.toList
//
//      // Error: Cannot construct a collection of type To with elements of type A 
//      // based on a collection of type IterableOnce[scala.concurrent.Future[A]]
//      Future.sequence(futures).map(_ => ())
//    }

    def traverseHelper[T](node: BST[T]): List[T] = node match {
      case Empty => List()
      case Node(k, l, r) =>
        traverseHelper(l) ::: List(k) ::: traverseHelper(r)
    }

//    //Not sure whether this is enough parallelization or not. Will edit soon
//    def combinePar[T](tree1: BST[T], tree2: BST[T])(implicit ord: Ordering[T]): Future[BST[T]] = {
//      val list_1 = Future(traverseHelper(tree1))
//      val list_2 = Future(traverseHelper(tree2))
//
//      for {
//        list1 <- list_1
//        list2 <- list_2
//        
//        // Error: value par is not a member of List[T]
//        merge_tgt = (list1.par ++ list2.par).toList.sorted(ord)
//      } yield sortedListToBST(merge_tgt) // Error: Not found: sortedListToBST
//    }
  }
}