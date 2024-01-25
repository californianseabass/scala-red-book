package chapter3

enum Tree[+A] {
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

    def size: Int = this match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + l.size + r.size
    }

    def map[B](f: A => B): Tree[B] = this match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l, r) => Branch(l.map(f), r.map(f))
    }

    def fold[B](f: A => B, g: (B,B) => B): B = this match {
      case Leaf(a) => f(a)
      case Branch(l, r) => g(l.fold(f, g), r.fold(f, g))
    }
}


object Tree {
  extension (t: Tree[Int]) {
    def firstPositive: Int = t match {
      case Leaf(i) => i
      case Branch(l, r) => {
        val lpos = l.firstPositive
        if (lpos > 0) then lpos else r.firstPositive
      }
    }

    def maximum: Int = t match {
      case Leaf(i) => i
      case Branch(l, r) => {
        val lside = l.maximum
        val rside = r.maximum
        if (lside > rside) then lside else rside
      }
    }

    def depth: Int = t match {
      case Leaf(_) => 1
      case Branch(l, r) => {
        val ldepth = l.depth + 1
        val rdepth = r.depth + 1
        if (ldepth > 0) then ldepth else rdepth
      }
    }
  }
}
