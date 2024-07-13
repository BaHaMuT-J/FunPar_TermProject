import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.collection.parallel.CollectionConverters.*

object BST {

  trait BST[+T]

  case object Empty extends BST[Nothing]

  case class Node[T](key: T, left: BST[T], right: BST[T]) extends BST[T]

  class ParBST[T](initialRoot: BST[T])(implicit ord: Ordering[T]) extends BST[T] {
    private var root: BST[T] = initialRoot

    def this(e: T)(implicit ord: Ordering[T]) = this(Node(e, Empty, Empty))

    override def toString: String = {
      def stringHelper(node: BST[T], str: String): String = node match {
        case Empty => str
        case Node(k, l, r) =>
          stringHelper(r, stringHelper(l, str) + " " + k + " ")
      }

      "[" + stringHelper(root, "") + "]"
    }

    def getRoot: BST[T] = root

    def insertSeq(e: T): Unit = {
      def insertHelper[K](e: K, rt: BST[K])(implicit ord: Ordering[K]): BST[K] = rt match {
        case Empty => Node(e, Empty, Empty)
        case Node(k, l, r) =>
          val cmp = ord.compare(e, k)
          if (cmp < 0) Node(k, insertHelper(e, l), r)
          else if (cmp == 0) rt
          else Node(k, l, insertHelper(e, r))
      }

      root = insertHelper(e, root)
    }

    def +(e: T): ParBST[T] = {
      val newTree = new ParBST(root)
      newTree.insertSeq(e)
      newTree
    }

    def insertAllSeq(elements: Seq[T]): Unit = for (elem <- elements) {
      insertSeq(elem)
    }

    def ++(s: Seq[T]): ParBST[T] = {
      val newTree = new ParBST(root)
      newTree.insertAllSeq(s)
      newTree
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

        if (tempVec.nonEmpty) result = result.prepended(Map(d -> tempVec.reverse))
        if (newQueue.nonEmpty) levelMap(newQueue, d + 1)
      }

      levelMap(new mutable.Queue[BST[T]]().enqueue(root), 1)
      result
    }

    def levelSeq(depth: Int): Vector[T] = {
      if (depth < 1) return Vector()

      @tailrec
      def levelHelper(m: mutable.Queue[BST[T]], d: Int): Vector[T] = {
        val newQueue = new mutable.Queue[BST[T]]()
        var result = Vector[T]()

        for (node <- m) {
          node match {
            case Empty =>
            case Node(k, l, r) =>
              if (d == depth) result = result.prepended(k) else newQueue.enqueue(l, r)
          }
        }

        if (d == depth) result.reverse
        else if (newQueue.nonEmpty) levelHelper(newQueue, d + 1)
        else Vector()
      }

      levelHelper(new mutable.Queue[BST[T]]().enqueue(root), 1)
    }

    def findLevelSeq(key: T): Option[Int] = {
      @tailrec
      def findLevelHelper(m: mutable.Queue[BST[T]], d: Int): Option[Int] = {
        val newQueue = new mutable.Queue[BST[T]]()

        for (node <- m) {
          node match {
            case Empty =>
            case Node(k, l, r) =>
              if (k.equals(key)) return Some(d) else newQueue.enqueue(l, r)
          }
        }

        if (newQueue.nonEmpty) findLevelHelper(newQueue, d + 1) else None
      }

      findLevelHelper(new mutable.Queue[BST[T]]().enqueue(root), 1)
    }

    // Parallel Programming

    private def insertParHelper(node: BST[T], key: T): BST[T] = node match {
      case Empty => Node(key, Empty, Empty)
      case Node(k, l, r) =>
        val cmp = ord.compare(key, k)
        if (cmp < 0) Node(k, insertParHelper(l, key), r)
        else if (cmp == 0) node
        else Node(k, l, insertParHelper(r, key))
    }

    def insertAllPar(keys: Seq[T]) = {
      val futures = keys.par.map { key =>
        Future {
          this.synchronized {
            root = insertParHelper(root, key)
          }
        }
      }

      Await.ready(Future.sequence(futures.toList), Duration.Inf)
    }

    def traverseHelper[T](node: BST[T]): List[T] = node match {
      case Empty => List()
      case Node(k, l, r) =>
        traverseHelper(l) ::: List(k) ::: traverseHelper(r)
    }

    def sortedListToBST(sortedList: List[T]): ParBST[T] = {
      def buildTree(lst: List[T]): BST[T] = lst match {
        case Nil => Empty
        case _ =>
          val mid = lst.length / 2
          val (left, right) = lst.splitAt(mid)
          Node(right.head, buildTree(left), buildTree(right.tail))
      }

      new ParBST[T](buildTree(sortedList))
    }

    def combinePar(tree: ParBST[T])(implicit ord: Ordering[T]): ParBST[T] = {
      val list_1 = Future(traverseHelper(root))
      val list_2 = Future(traverseHelper(tree.getRoot))

      val c = for {
        list1 <- list_1
        list2 <- list_2
        merge_tgt = (list1.par ++ list2.par).toSet.toList.sorted(ord)
      } yield sortedListToBST(merge_tgt)

      Await.result(c, Duration.Inf)
    }
  }
}
