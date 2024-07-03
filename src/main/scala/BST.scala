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

  case class ParBST[T] (r: BST[T]) extends BST[T] {
    var root: BST[T] = r

    // auxiliary constructors
    def this() = this(Empty)

  }

}
