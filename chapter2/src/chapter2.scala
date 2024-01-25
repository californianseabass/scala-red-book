object MyProgram {
  def abs(n: Int): Int =
    if n < 0 then -n
    else n

  private def formatAbs(x:Int) = {
    val msg = "the absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def factorial (n: Int): Int = {
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n - 1, acc * n)
    }

    go(n, 1)
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(x: Int, y: Int, n: Int): Int = {
      println(y)
      if (n <= 0) {
        return y
      }
      go(y, x + y, n- 1)
    }

    if (n == 1) {
      return 0
    }
    if (n == 2) {
      return 1
    }

    go(0, 1, n - 2)
  }

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean = {
      if (n == as.length) {
        return true
      }
      if (!gt(as(n), as(n - 1))) {
        return false
      }
      go(n + 1)
    }

    if (as.length <= 1) {
      return true
    }

    go(1)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

  @main def printAbs: Unit = {
    println(formatAbs(-42))
    println(fib(7))

    println(isSorted(Array(1, 2, 3), _ > _))
    println(isSorted(Array(1, 2, 1), _ > _))
    println(isSorted(Array(3, 2, 1), _ < _))
    println(isSorted(Array(1, 2, 3), _ < _))
  }
}
