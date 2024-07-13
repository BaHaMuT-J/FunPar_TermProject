import scala.concurrent.Await
import scala.concurrent.duration.Duration

object TestBST extends App {
//  val t = new BST.ParBST[Int](10)
//  val nt = t ++ {1 to 10000}.toList
//  println(t)
//  println(nt)
//  t.insertAllPar({1 to 10000}.toList)
//  println(t)

  val a = new BST.ParBST[Int](10)
  a.insertAllPar({1 to 50000}.toList)
  val b = new BST.ParBST[Int](10)
//  b.insertAllPar({50001 to 100000}.toList)
  //println(a)

  val d = a.findLevelSeq(50000)
  println(a.levelTraverseSeq)
  println(d)
  println(a.levelSeq(d.getOrElse(1)))

  println(b)
  //println(a.combineSeq(b))
  //println(a.combinePar(b))
}
