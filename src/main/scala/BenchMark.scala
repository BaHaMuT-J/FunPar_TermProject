object BenchMark {

  def timed[A](name: String)(f: => A): (Double, A) = {
    println(s"Running $name ...");
    Console.flush()
    val start = System.nanoTime
    val res = f
    val stop = System.nanoTime
    println("Done");
    Console.flush()
    ((stop - start) / 1e9, res)
  }
  
}
