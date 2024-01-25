package chapter3

enum List[+A] {
  case Nil
  case Cons(head: A, tail: List[A])
}

object List {
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) then Nil
    else Cons(as.head, apply(as.tail*))
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(doubles: List[Double]): Double = doubles match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => throw new Exception("Cannot return tail of empty list")
    case Cons(_, xs) => xs
  }

  def setHead[A](as: List[A], newHead: A) = as match {
    case Nil => Nil
    case Cons(_, xs) => Cons(newHead, xs)
  }

  def drop[A](as: List[A], n: Int): List[A] = (as, n) match {
    case (Nil, _) => Nil
    case (xs, 0) => xs
    case (Cons(x, xs), n) => drop(xs, n - 1)
  }

  def drop2[A](as: List[A], n: Int): List[A] = {
    if (n <= 0) {
      as
    } else as match {
      case Nil  => Nil
      case Cons(_, xs) => drop2(xs, n - 1)
    }
  }

  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => f(x) match {
      case true => dropWhile(xs, f)
      case false => Cons(x, xs)
    }
  }

  def init[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A, B](as: List[A], acc: B, f:(A, B) => B): B = as match {
    case Nil => acc
    case Cons(x, xs) => f(x, foldRight(xs, acc, f))
  }

  def sumViaFoldRight(xs: List[Int]): Int = {
    foldRight(xs, 0, (x, y) => {
                println(s"${x}, ${y}")
                x + y
              })
  }

  // fold right is a name that references the way collapsing each element begins

  def productViaFoldRight(xs: List[Double]): Double = {
    foldRight(xs, 1.0, _ *_)
  }

  def length[A](as: List[A]): Int = {
    foldRight(as, 0, (_, acc) => 1 + acc)
  }

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], acc: B, f:(B, A) => B): B = as match {
      case Nil => acc
      case Cons(x, xs) => foldLeft(xs, f(acc, x), f)
  }

  def length2[A](as: List[A]): Int = {
    foldLeft(as, 0, (acc, c) => acc + 1)
  }

  def sum2(as: List[Int]): Int = {
    foldLeft(as, 0, _ + _)
  }

  def product2(as: List[Double]): Double = {
    foldLeft(as, 1.0, _ * _)
  }

  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, Nil: List[A], (acc, curr) => Cons(curr, acc))
  }

  def foldRight2[A, B](as: List[A], acc: B, f: (A, B) => B) = {
    foldLeft(reverse(as), acc, (acc, curr) => f(curr, acc))
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2, Cons(_, _))
  }

  def sumViaFoldRight2(xs: List[Int]): Int = {
    foldRight2(xs, 0, (x, y) => {
                println(s"${x}, ${y}")
                x + y
              })
  }

  def concat[A](as: List[List[A]]): List[A] = {
    foldRight(as, Nil: List[A], append(_, _))
  }

  def map[A, B](as: List[A], f: (A) => B): List[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs, f))
  }

  def mapViaFold[A, B](as: List[A], f: (A) => B): List[B] = {
    foldRight(as, Nil: List[B], (curr, acc) => Cons(f(curr), acc))
  }

  def filter[A](as: List[A], f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) Cons(x, filter(xs, f)) else filter(xs, f)
  }

  def flatMap[A, B](as: List[A], f: A => List[B]): List[B] = {
    foldRight(as, Nil: List[B], (curr, acc) => append(f(curr), acc) )
  }

  def filterViaFL[A](as: List[A], f:(A) => Boolean): List[A] = {
    flatMap(as, (a) => if (f(a)) List(a) else Nil)
  }

  def asString[A](input: List[A]): String = {
    def go(as: List[A]): String = as match {
      case Nil => ")"
      case Cons(x, Nil) => x.toString + ")"
      case Cons(x, xs) => x.toString + ", " + go(xs)
    }

    "(" + go(input)
  }

  def addListsTogether(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(a, as), Cons(b, bs)) => Cons(a + b, addListsTogether(as, bs))
  }

  def zip[A, B](as: List[A], bs: List[B]): List[(A, B)] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(a, as), Cons(b, bs)) => Cons((a, b), zip(as, bs))
  }

  /*
   * Common strucutre of the nested match with the take and hasSubsequence methods
   */
  def take[A](as: List[A], n: Int): List[A] = {
    def go(as: List[A], n: Int, acc: List[A]): List[A] = {
      n match {
        case 0 => Nil
        case _ => as match {
          case Nil => throw new Exception("Cannot take from empty list!")
          case Cons(x, xs) =>  Cons(x, take(xs, n - 1))
        }
      }
    }

    go(as, n, Nil)
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean  = {
    val size = length(sub)
    if (length(sup) < length(sub)) {
      return false
    }
    println(format("test", take(sup, length(sub)), sub, take(sup, length(sub)) == sub))
    if (take(sup, length(sub)) == sub) {
      return true
    }
    sup match {
      case Nil => false
      case Cons(x, xs) => hasSubsequence(xs, sub)
    }
  }

  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil) => true
    case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }

  def format[A](name: String, sequence: List[A], subsequence: List[A], result: Boolean): String = {
    val s1 = asString(sequence)
    val s2 = asString(subsequence)
    return s"$name has received $s1 and $s2, it returns ${result.toString}"
  }

  def testSubsequence[A](sup: List[A], sub: List[A]): Unit = {
    val output = hasSubsequence(sup, sub)
    val s = format("hasSubsequence", sup, sub, output)
    println(s)
  }
}
















// // List data type, parameterized on a type A
// sealed trait List[+A]
// // A list data constructor representing the empty list.
// case object Nil extends List[Nothing]
// // Another data consturctor, representing nonempty lists. Note that tail is another List[A], which may be Nil or another Cons
// case class Cons[+A](head: A, tail: List[A]) extends List[A]

// // List companion object. Contains functions for creating and working with lists
// object List {
//   def prettyPrint[A](as: List[A]): String = {
//     def go(a: List[A]): String = {
//       a match {
//         case Nil => ")"
//         case Cons(x, Nil) => x.toString + ")"
//         case Cons(x, xs) => x.toString + ", " + go(xs)
//       }
//     }
//     "(" + go(as)
//   }


//   def sum(ints: List[Int]): Int = ints match {
//     case Nil => 0
//     case Cons(x, xs) => x + sum(xs)
//   }

//   def product(ds: List[Double]): Double = ds match {
//     case Nil => 1.0
//     case Cons(0.0, _) => 0.0
//     case Cons(x,xs) => x * product(xs)
//   }

//   // Variadic funcoitn syntax
//   def apply[A](as: A*): List[A] = {
//     if (as.isEmpty) Nil
//     else Cons(as.head, apply(as.tail: _*))
//   }
// }
