package chapter3
import List._
import Tree._

object chapter3 {
  def addOne(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x + 1, addOne(xs))
  }

  def runLists(): Unit = {
    println("chapter 3")
    val alpha = Cons("a", Cons("b", Cons("c", Cons("d", Cons("e", Nil)))))
    val alpha2 = List("f", "g")
    val alpha3 = List("h", "i", "j")
    val numeric =  Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
    val t = dropWhile(alpha, (a) => a == "b" || a ==("a"))
    println(asString(t))
    val s = sumViaFoldRight(numeric)
    println(s)
    val rev = reverse(alpha)
    println(sumViaFoldRight2(numeric))
    val lists = Cons(alpha, Cons(alpha2, Cons(alpha3, Nil)))
    println(asString(concat(lists)))
    println(asString(mapViaFold(numeric, _ + 3)))
    println(asString(filterViaFL(numeric, _ % 2 == 0)))
    println(asString(zip(List(1,2,3), List(4,5,6))))
    println(asString(zip(List(1,2,3), alpha)))
    println(testSubsequence(List(1, 2, 3, 4), List(1, 2)))
    println(testSubsequence(List(1, 2, 3, 4), List(1, 2, 3)))
    println(testSubsequence(List(1, 2, 3, 4), List(1, 2, 3, 4)))
    println(testSubsequence(List(1, 2, 3, 4), List( 2, 3)))
    println(testSubsequence(List(1, 2, 3, 4), List(3)))
    println(testSubsequence(List(1, 2, 3, 4), List(8, 7)))
  }

  def main(args: Array[String]): Unit = {
    val branch12 = Branch(Leaf(8), Leaf(2))
    val branch34 = Branch(Leaf(3), Leaf(4))
    val tree = Branch(branch12, branch34)
    println(tree.size)
  }
}
