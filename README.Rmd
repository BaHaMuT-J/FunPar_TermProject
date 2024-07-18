# Parallel Insert,Combine,and Level Traversing for BST

This Project aims to find ways to speed up the process of adding new elements to a Binary Search Tree and level traversing the tree by utilizing concepts and techniques of parallelization in scala.

## Table of Contents

1. [Steps of working](#steps-of-working)
2. [Results](#results)

- [InsertAll](#insertall)
- [Combine](#combine)
- [Level Traversing](#level-traversing)
- [Level Finding](#level-finding)

3. [Discussion](#discussion)
4. [Conclusion](#conclusion)
5. [Further Improvements](#further-improvements)
6. [Contributing](#contributing)

## Steps of working

1. Define BST related classes

```scala
trait BST[+T]
private case object Empty extends BST[Nothing]
private case class Node[T](key: T, left: BST[T], right: BST[T]) extends BST[T]

// Main class for BST
class ParBST[T](initialRoot: BST[T], initialSize: Int)(implicit ord: Ordering[T]) extends BST[T]
```

2. Write simple insert, combine, and level traversing in sequential ways

```scala
// Sequential Insert All
def insertAllSeq(elements: Seq[T]): Unit

// Sequential Combine
def combineSeq(tree: ParBST[T]): ParBST[T]

// Sequential Level Traversing
def levelTraverseSeq: Map[Int, Vector[T]]

// Sequential Level Finding (find all elements in input depth)
def levelSeq(depth: Int): Vector[T]
```

3. Make the function parallel utilizing some combination of parallelism techniques

```scala
// Parallel Insert All using Synchronized
def insertAllPar(keys: Seq[T]): Future[List[Unit]]

// Parallel Combine using Synchronized
def combinePar(tree: ParBST[T])(implicit ord: Ordering[T]): ParBST[T]

// Parallel Level Traversing using Thread and ConcurrentMap
def levelTraverseThread: Map[Int, Vector[T]]

// Parallel Level Traversing using Thread and Synchronized
def levelTraverseThreadSync: Map[Int, Vector[T]]

// Parallel Level Traversing using Future and ConcurrentSet
def levelTraverseFuture: Map[Int, Vector[T]]

// Parallel Level Traversing using Future and Synchronized
def levelTraverseFutureSync: Map[Int, Vector[T]]

// Parallel Level Finding using Thread and ConcurrentSet
def levelThread(depth: Int): Vector[T]

// Parallel Level Finding using Thread and Synchronized
def levelThreadSync(depth: Int): Vector[T]

// Parallel Level Finding using Future and ConcurrentSet
def levelFuture(depth: Int): Vector[T]

// Parallel Level Finding using Future and Synchronized
def levelFutureSync(depth: Int): Vector[T]
```

4. Benchmarking with different number of inputs: 2<sup>5</sup>, 2<sup>10</sup>, 2<sup>15</sup>, and 2<sup>20</sup>

## Results

We will provide tables that record average times each function takes in seconds for different number of inputs.

The bold and italic number means that it is the minimum time for that number of inputs.

### InsertAll

<table>
  <thead>
    <tr>
      <th rowspan="2" style="text-align:center;">InsertAllSeq</th>
      <th colspan="4" style="text-align:center;">Number of Input</th>
    </tr>
    <tr>
      <th style="text-align:center;">2<sup>5</sup></th>
      <th style="text-align:center;">2<sup>10</sup></th>
      <th style="text-align:center;">2<sup>15</sup></th>
      <th style="text-align:center;">2<sup>20</sup></th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td style="text-align:center;">Seq</td>
      <td style="text-align:center;"><i><b>0.0082778</b></i></td>
      <td style="text-align:center;">0.0216687</td>
      <td style="text-align:center;">6.0669018</td>
      <td style="text-align:center;">&gt;10 minutes</td>
    </tr>
    <tr>
      <td style="text-align:center;">Par</td>
      <td style="text-align:center;">0.1119541</td>
      <td style="text-align:center;"><i><b>0.0194145</b></i></td>
      <td style="text-align:center;"><i><b>0.947256</b></i></td>
      <td style="text-align:center;"><i><b>498.4290413</b></i></td>
    </tr>
  </tbody>
</table>

### Combine

<table>
  <thead>
    <tr>
      <th rowspan="2" style="text-align:center;">Method</th>
      <th colspan="4" style="text-align:center;">Number of Input</th>
    </tr>
    <tr>
      <th style="text-align:center;">2<sup>5</sup></th>
      <th style="text-align:center;">2<sup>10</sup></th>
      <th style="text-align:center;">2<sup>15</sup></th>
      <th style="text-align:center;">2<sup>20</sup></th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td style="text-align:center;">Seq</td>
      <td style="text-align:center;"><i><b>2.88E-04</b></i></td>
      <td style="text-align:center;">0.0207358</td>
      <td style="text-align:center;">2.1553263</td>
      <td style="text-align:center;">18179.80155</td>
    </tr>
    <tr>
      <td style="text-align:center;">Par</td>
      <td style="text-align:center;">0.0412975</td>
      <td style="text-align:center;"><i><b>0.0108732</b></i></td>
      <td style="text-align:center;"><i><b>0.1100244</b></i></td>
      <td style="text-align:center;"><i><b>1.9928678</b></i></td>
    </tr>
  </tbody>
</table>

### Level Traversing

<table>
  <thead>
    <tr>
      <th rowspan="2" style="text-align:center;">Method</th>
      <th colspan="4" style="text-align:center;">Number of Input</th>
    </tr>
    <tr>
      <th style="text-align:center;">2<sup>5</sup></th>
      <th style="text-align:center;">2<sup>10</sup></th>
      <th style="text-align:center;">2<sup>15</sup></th>
      <th style="text-align:center;">2<sup>20</sup></th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td style="text-align:center;">Seq</td>
      <td style="text-align:center;"><i><b>0.00180494</b></i></td>
      <td style="text-align:center;">0.00493906</td>
      <td style="text-align:center;">0.02306656</td>
      <td style="text-align:center;">0.05961594</td>
    </tr>
    <tr>
      <td style="text-align:center;">Thread</td>
      <td style="text-align:center;">0.0073392</td>
      <td style="text-align:center;">0.13829712</td>
      <td style="text-align:center;">15.36578592</td>
      <td style="text-align:center;">&gt;10 minutes</td>
    </tr>
    <tr>
      <td style="text-align:center;">ThreadSync</td>
      <td style="text-align:center;">0.00596998</td>
      <td style="text-align:center;">0.13456836</td>
      <td style="text-align:center;">16.23841918</td>
      <td style="text-align:center;">&gt;10 minutes</td>
    </tr>
    <tr>
      <td style="text-align:center;">Future</td>
      <td style="text-align:center;">0.00392286</td>
      <td style="text-align:center;"><i><b>1.24E-03</b></i></td>
      <td style="text-align:center;">9.70E-03</td>
      <td style="text-align:center;">0.06019244</td>
    </tr>
    <tr>
      <td style="text-align:center;">FutureSync</td>
      <td style="text-align:center;">2.43E-03</td>
      <td style="text-align:center;">0.00163928</td>
      <td style="text-align:center;"><i><b>0.00226282</b></i></td>
      <td style="text-align:center;"><i><b>0.00977444</b></i></td>
    </tr>
  </tbody>
</table>

### Level Finding

<table>
  <thead>
    <tr>
      <th rowspan="2" style="text-align:center;">Method</th>
      <th colspan="4" style="text-align:center;">Number of Input</th>
    </tr>
    <tr>
      <th style="text-align:center;">2<sup>5</sup></th>
      <th style="text-align:center;">2<sup>10</sup></th>
      <th style="text-align:center;">2<sup>15</sup></th>
      <th style="text-align:center;">2<sup>20</sup></th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td style="text-align:center;">Seq</td>
      <td style="text-align:center;">6.96E-04</td>
      <td style="text-align:center;">8.86E-04</td>
      <td style="text-align:center;">0.00436338</td>
      <td style="text-align:center;">0.07009238</td>
    </tr>
    <tr>
      <td style="text-align:center;">Thread</td>
      <td style="text-align:center;">6.01E-03</td>
      <td style="text-align:center;">1.33E-01</td>
      <td style="text-align:center;">18.68602718</td>
      <td style="text-align:center;">&gt;10 minutes</td>
    </tr>
    <tr>
      <td style="text-align:center;">ThreadSync</td>
      <td style="text-align:center;">5.63E-03</td>
      <td style="text-align:center;">1.32E-01</td>
      <td style="text-align:center;">18.1339388</td>
      <td style="text-align:center;">&gt;10 minutes</td>
    </tr>
    <tr>
      <td style="text-align:center;">Future</td>
      <td style="text-align:center;">1.69E-03</td>
      <td style="text-align:center;">2.21E-04</td>
      <td style="text-align:center;">0.01692882</td>
      <td style="text-align:center;">0.02852126</td>
    </tr>
    <tr>
      <td style="text-align:center;">FutureSync</td>
      <td style="text-align:center;"><i><b>5.51E-05</b></i></td>
      <td style="text-align:center;"><i><b>4.98E-06</b></i></td>
      <td style="text-align:center;"><i><b>6.14E-06</b></i></td>
      <td style="text-align:center;"><i><b>1.40E-04</b></i></td>
    </tr>
  </tbody>
</table>

## Discussion

The sequential function of InsertAll and Combine takes a very long time to execute when the number of input is only 2<sup>20</sup> which make our results not completely accurate. This may caused by the simplicity of the function that only adds new elements to BST without balancing the tree so it has to traverse deeper when the tree is heavy in one side. This is also the case for thread function of Level Traversing and Finding which may due to the memory usage of the thread when the number of input is large that makes the function works very slow or stop working along the way.

## Conclusion

Our project shows that parallelism can reduce the time taken for insert, combine, level traversing, and level finding. However, we are still lacking some information to be confident that this result is trustworthy so we need some improvements to analyzes and conclude for more accurate result.

## Further Improvements

The sequential and thread function should be improved to get and see the actual time difference which allows us to have a better performance comparison. Furthermore, we could utilize the concept of actors as another way of parallelism and compare its performance with our function. Lastly, to make this project more realistic, the BST should be implemented as a balanced tree so that it can be worked with more easily and may further reduce the time taken for each function.

## Contributing

Siranut Jongdee 6481261

Devya Shah 6480253
