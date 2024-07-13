import scala.concurrent.Await
import scala.concurrent.duration.Duration

object TestBST extends App {
//  val t = new BST.ParBST[Int](10)
//  val nt = t ++ {1 to 10000}.toList
//  println(t)
//  println(nt)
//  t.insertAllPar({1 to 10000}.toList)
//  println(t)

  val a = new BST.ParBST[Int](10) ++ Vector(2,1,3,12,11,13)
  val b = new BST.ParBST[Int](10) ++ Vector(5,4,6,15,14,16)
  println(a)
  println(b)
  val c = a.combinePar(b)
  println(a)
  println(b)
  println(c)
  println(a.combineSeq(b))
}
