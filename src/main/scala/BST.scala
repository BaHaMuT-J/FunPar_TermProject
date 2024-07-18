import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.collection.parallel.CollectionConverters.*
import java.util.concurrent.{ConcurrentSkipListMap, ConcurrentSkipListSet, CountDownLatch}
import scala.jdk.CollectionConverters.*

object BST {

  // Defined as a synchronized data structure
  class Synchro[T] {
    private var count: Int = 0
    private var set: Set[T] = Set()
    private val map = mutable.Map[Int, Vector[T]]()

    def increment(): Unit = this.synchronized {
      this.count += 1
    }

    def addInSet(k: T): Unit = this.synchronized {
      this.set = this.set + k
    }

    def pushInMap(d: Int, vt: Vector[T]): Unit = this.synchronized {
      this.map.put(d, vt)
    }

    def getFromMap(d: Int): Vector[T] = this.synchronized {
      this.map.getOrElse(d, Vector())
    }

    def getCount: Int = this.synchronized {
      count
    }

    def getSet: Set[T] = this.synchronized {
      set
    }

    def getMap: Map[Int, Vector[T]] = this.synchronized {
      map.toMap
    }
  }

  // Thread create function
  private def ThreadBuilder[F](f: => F): Thread = {
    new Thread {
      override def run: Unit = {
        f
        ()
      }
    }
  }

  trait BST[+T]

  private case object Empty extends BST[Nothing]

  private case class Node[T](key: T, left: BST[T], right: BST[T]) extends BST[T]

  // Main BST class
  class ParBST[T](initialRoot: BST[T], initialSize: Int)(implicit ord: Ordering[T]) extends BST[T] {
    private var root: BST[T] = initialRoot
    private var sizes: Int = initialSize

    def this(e: T)(implicit ord: Ordering[T]) = this(Node(e, Empty, Empty), 1)

    def getRoot: BST[T] = root

    def size: Int = sizes

    // In-order traversed string for debugging
    override def toString: String = {
      def stringHelper(node: BST[T], str: String): String = node match {
        case Empty => str
        case Node(k, l, r) =>
          stringHelper(r, stringHelper(l, str) + " " + k + " ")
      }

      "[" + stringHelper(root, "") + "]"
    }

    /** ***************************** Sequential Programming ****************************** */

    // Sequential Insert
    def insertSeq(e: T): Unit = {
      def insertHelper[K](key: K, node: BST[K])(implicit ord: Ordering[K]): BST[K] = {
        @tailrec
        def loop(curr: BST[K], path: List[BST[K]]): BST[K] = curr match {
          case Empty =>
            path.foldLeft(Node(key, Empty, Empty): BST[K]) {
              case (subtree, Node(k, l, r)) =>
                if (ord.lt(key, k)) Node(k, subtree, r)
                else Node(k, l, subtree)
            }
          case n @ Node(k, l, r) =>
            if (ord.lt(key, k)) loop(l, n :: path)
            else if (ord.gt(key, k)) loop(r, n :: path)
            else node
        }

        loop(node, Nil)
      }

      root = insertHelper(e, root)
      sizes += 1
    }

    def +(e: T): ParBST[T] = {
      val newTree = new ParBST(root, sizes)
      newTree.insertSeq(e)
      newTree
    }

    // Sequential Insert All
    def insertAllSeq(elements: Seq[T]): Unit = for (elem <- elements) {
      insertSeq(elem)
    }

    def ++(s: Seq[T]): ParBST[T] = {
      val newTree = new ParBST(root, sizes)
      newTree.insertAllSeq(s)
      newTree
    }

    // Sequential Combine
    def combineSeq(tree: ParBST[T]): ParBST[T] = {
      var newTree = this

      def combineHelper(node: BST[T]): Unit = node match {
        case Empty =>
        case Node(k, l, r) =>
          combineHelper(l)
          newTree = newTree + k
          combineHelper(r)
      }

      combineHelper(tree.getRoot)
      newTree
    }

    // Sequential Level Traversing
    def levelTraverseSeq: Map[Int, Vector[T]] = {
      var result = Map[Int, Vector[T]]()
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

        if (tempVec.nonEmpty) result = result + (d -> tempVec.reverse)
        if (newQueue.nonEmpty) levelMap(newQueue, d + 1)
      }

      levelMap(new mutable.Queue[BST[T]]().enqueue(root), 1)
      result
    }

    // Sequential Level Finding (find all elements in input depth)
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

    // Sequential Level Searching (find depth that key exists)
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

    /******************************* Parallel Programming *******************************/
      
    // Parallel Insert All using Synchronized
    def insertAllPar(keys: Seq[T]): Future[List[Unit]] = {
      val futures = keys.par.map { key =>
        Future {
          this.synchronized {
            this.insertSeq(key)
          }
        }
      }

      Await.ready(Future.sequence(futures.toList), Duration.Inf)
    }

    // Parallel Combine using Synchronized
    def combinePar(tree: ParBST[T])(implicit ord: Ordering[T]): ParBST[T] = {
      def traverseHelper(node: BST[T]): Set[T] = {
        @tailrec
        def traverseTailRec(stack: List[BST[T]], acc: Set[T]): Set[T] = stack match {
          case Nil => acc
          case Empty :: rest => traverseTailRec(rest, acc)
          case Node(k, l, r) :: rest => traverseTailRec(l :: r :: rest, acc + k)
        }

        traverseTailRec(List(node), Set.empty)
      }

      def sortedListToBST(sortedList: List[T]): ParBST[T] = {
        def buildTree(lst: List[T]): BST[T] = lst match {
          case Nil => Empty
          case _ =>
            val mid = lst.length / 2
            val (left, right) = lst.splitAt(mid)
            Node(right.head, buildTree(left), buildTree(right.tail))
        }

        new ParBST[T](buildTree(sortedList), sortedList.size)
      }
      
      val list_1 = Future(traverseHelper(root))
      val list_2 = Future(traverseHelper(tree.getRoot))

      val c = for {
        list1 <- list_1
        list2 <- list_2
        merge_tgt = (list1.par ++ list2.par).toList.sorted(ord)
      } yield sortedListToBST(merge_tgt)

      Await.result(c, Duration.Inf)
    }

    // Parallel Level Traversing using Thread and ConcurrentMap
    def levelTraverseThread: Map[Int, Vector[T]] = {
      val result = new ConcurrentSkipListMap[Int, Vector[T]]()

      def levelHelper(node: BST[T], d: Int, latch: CountDownLatch): Unit = {
        node match {
          case Empty => latch.countDown()
          case Node(k, l, r) =>
            val leftLatch = new CountDownLatch(1)
            val rightLatch = new CountDownLatch(1)

            val leftThread = ThreadBuilder {
              levelHelper(l, d + 1, leftLatch)
            }
            leftThread.start()

            val rightThread = ThreadBuilder {
              levelHelper(r, d + 1, rightLatch)
            }
            rightThread.start()

            result.compute(d, (_, v) => {
              if (v == null) Vector(k) else v :+ k
            })

            leftLatch.await()
            rightLatch.await()
            latch.countDown()
        }
      }

      val rootLatch = new CountDownLatch(1)
      val rootThread = ThreadBuilder {
        levelHelper(root, 1, rootLatch)
      }
      rootThread.start()
      rootLatch.await()

      result.asScala.toMap
    }
    
    // Parallel Level Traversing using Thread and Synchronized
    def levelTraverseThreadSync: Map[Int, Vector[T]] = {
      val result = new Synchro[T]

      def levelHelper(node: BST[T], d: Int, latch: CountDownLatch): Unit = {
        node match {
          case Empty => latch.countDown()
          case Node(k, l, r) =>
            val leftLatch = new CountDownLatch(1)
            val rightLatch = new CountDownLatch(1)

            val leftThread = ThreadBuilder {
              levelHelper(l, d + 1, leftLatch)
            }
            leftThread.start()

            val rightThread = ThreadBuilder {
              levelHelper(r, d + 1, rightLatch)
            }
            rightThread.start()

            leftLatch.await()
            rightLatch.await()

            val v = result.getFromMap(d)
            result.pushInMap(d, v :+ k)

            latch.countDown()
        }
      }

      val rootLatch = new CountDownLatch(1)
      val rootThread = ThreadBuilder {
        levelHelper(root, 1, rootLatch)
      }
      rootThread.start()
      rootLatch.await()

      result.getMap
    }

    // Parallel Level Traversing using Future and ConcurrentSet
    def levelTraverseFuture: Map[Int, Vector[T]] = {
      val result = new ConcurrentSkipListMap[Int, Vector[T]]()

      def levelHelper(node: BST[T], d: Int): Future[Unit] = node match {
        case Empty => Future.successful(())
        case Node(k, l, r) =>
          val fl = Future {
            levelHelper(l, d + 1)
          }
          val fr = Future {
            levelHelper(r, d + 1)
          }

          result.compute(d, (_, v) => {
            if (v == null) Vector(k) else v :+ k
          })

          for {
            _ <- fl
            _ <- fr
          } yield ()
      }

      val rootFuture = levelHelper(root, 1)
      Await.result(rootFuture, Duration.Inf)

      result.asScala.toMap
    }

    // Parallel Level Traversing using Future and Synchronized
    def levelTraverseFutureSync: Map[Int, Vector[T]] = {
      val result = new Synchro[T]

      def levelHelper(node: BST[T], d: Int): Future[Unit] = node match {
        case Empty => Future.successful(())
        case Node(k, l, r) =>
          result.pushInMap(d, result.getFromMap(d) :+ k)
          
          val f1 = levelHelper(l, d + 1)
          val f2 = levelHelper(r, d + 1)

          Future.sequence(Seq(f1, f2)).map(_ => ())
      }

      val rootFuture = levelHelper(root, 1)
      Await.result(rootFuture, Duration.Inf)

      result.getMap
    }

    // Parallel Level Finding using Thread and ConcurrentSet
    def levelThread(depth: Int): Vector[T] = {
      val result = new ConcurrentSkipListSet[T]()

      def levelHelper(node: BST[T], d: Int, latch: CountDownLatch): Unit = {
        if d == depth then {
          node match {
            case Empty => latch.countDown()
            case Node(k, l, r) =>
              result.add(k)
              latch.countDown()
          }
        } else {
          node match {
            case Empty => latch.countDown()
            case Node(k, l, r) =>
              val leftLatch = new CountDownLatch(1)
              val rightLatch = new CountDownLatch(1)

              val leftThread = ThreadBuilder {
                levelHelper(l, d + 1, leftLatch)
              }
              leftThread.start()

              val rightThread = ThreadBuilder {
                levelHelper(r, d + 1, rightLatch)
              }
              rightThread.start()

              leftLatch.await()
              rightLatch.await()
              latch.countDown()
          }
        }
      }

      val rootLatch = new CountDownLatch(1)
      val rootThread = ThreadBuilder {
        levelHelper(root, 1, rootLatch)
      }
      rootThread.start()
      rootLatch.await()

      result.asScala.toVector
    }

    // Parallel Level Finding using Thread and Synchronized
    def levelThreadSync(depth: Int): Vector[T] = {
      val result = new Synchro[T]

      def levelHelper(node: BST[T], d: Int, latch: CountDownLatch): Unit = {
        if d == depth then {
          node match {
            case Empty => latch.countDown()
            case Node(k, l, r) =>
              result.addInSet(k)
              latch.countDown()
          }
        } else {
          node match {
            case Empty => latch.countDown()
            case Node(k, l, r) =>
              val leftLatch = new CountDownLatch(1)
              val rightLatch = new CountDownLatch(1)

              val leftThread = ThreadBuilder {
                levelHelper(l, d + 1, leftLatch)
              }
              leftThread.start()

              val rightThread = ThreadBuilder {
                levelHelper(r, d + 1, rightLatch)
              }
              rightThread.start()

              leftLatch.await()
              rightLatch.await()
              latch.countDown()
          }
        }
      }

      val rootLatch = new CountDownLatch(1)
      val rootThread = ThreadBuilder {
        levelHelper(root, 1, rootLatch)
      }
      rootThread.start()
      rootLatch.await()

      result.getSet.toVector
    }

    // Parallel Level Finding using Future and ConcurrentSet
    def levelFuture(depth: Int): Vector[T] = {
      val result = new ConcurrentSkipListSet[T]()

      def levelHelper(node: BST[T], d: Int): Future[Unit] = {
        if d == depth then {
          node match {
            case Empty => Future.successful(())
            case Node(k, _, _) =>
              result.add(k)
              Future.successful(())
          }
        } else {
          node match {
            case Empty => Future.successful(())
            case Node(_, l, r) =>
              val fl = Future {
                levelHelper(l, d + 1)
              }
              val fr = Future {
                levelHelper(r, d + 1)
              }

              for {
                _ <- fl
                _ <- fr
              } yield ()
          }
        }
      }

      val rootFuture = levelHelper(root, 1)
      Await.result(rootFuture, Duration.Inf)

      result.asScala.toVector
    }

    // Parallel Level Finding using Future and Synchronized
    def levelFutureSync(depth: Int): Vector[T] = {
      val result = new Synchro[T]

      def levelHelper(node: BST[T], d: Int): Future[Unit] = {
        if (d == depth) {
          node match {
            case Empty => Future.successful(())
            case Node(k, _, _) =>
              result.addInSet(k)
              Future.successful(())
          }
        } else {
          node match {
            case Empty => Future.successful(())
            case Node(_, l, r) =>
              val fl = levelHelper(l, d + 1)
              val fr = levelHelper(r, d + 1)

              for {
                _ <- fl
                _ <- fr
              } yield ()
          }
        }
      }

      val rootFuture = levelHelper(root, 1)
      Await.result(rootFuture, Duration.Inf)

      result.getSet.toVector
    }
  }
}