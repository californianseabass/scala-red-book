package chp4
import Option._

def mean(xs: Seq[Double]): Option[Double] = {
  if (xs.isEmpty) then None
  else Some(xs.sum / xs.length)
}

// def variance(xs: Seq[Double]): Option[Double] = {
// }

def toIntOption(s: String): Option[Int] = {
  try Some(s.toInt)
  catch case _: NumberFormatException => None
}

def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = {
  // if (age < 0 || numberOfSpeedingTickets < 0) then None
  age / (numberOfSpeedingTickets + 1)
}

def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
  map2(toIntOption(age), toIntOption(numberOfSpeedingTickets))(insuranceRateQuote)
}


@main
def main(): Unit = {
  val x = Some(2)
  val y = x.map(_ + 5)
  val z = x.orElse(Some(6))
  val a = x.filter(_ % 2 == 1)

  println(s"${None.getOrElse(-1)}")
  println(s"${x.getOrElse(1)}")
  println(s"${y.getOrElse(1)}")
  println(s"$a")

  println(s"${lift(math.abs)(Some(-1))}")

  val quote = parseInsuranceRateQuote("40", "4")
  println(s"quote: ${quote}")

  val seqTest = sequence(List(Some(1), None, Some(2), Some(2)))
  println(s"Sequence ${seqTest}")

  // val n: Option[Int] = None
  // val q = map2(Some(1), n) {_ + _}
  // val w = sequence(List(Some(3), Some(7), Some(2), Some(7)))
  // println(s"$w")

  // val e = traverse(List(1, 2, 3, 4)) { a => Some(a) }
  // println(s"$e")

}
