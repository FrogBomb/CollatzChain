import scala.language.postfixOps

// class IntSingleOrList[T]
// object IntSingleOrList {
//   implicit object SingleWitness extends IntSingleOrList[Int]
//   implicit object ListWitness extends IntSingleOrList[List[Int]]
// }

class CollatzChain(val start: Int) extends Iterator[Int] {
  var current = start
  val halveEvens: PartialFunction[Int, Int] = new PartialFunction[Int, Int]{
    def isDefinedAt(x: Int) = x%2 == 0
    def apply(x: Int) = x / 2
  }
  def tripplePlusOne(x: Int) = 3*x + 1
  val collatzStep = halveEvens orElse PartialFunction[Int, Int](tripplePlusOne _)

  // def old_collatzChain[T: IntSingleOrList](start: T) : List[Int] = {
  //   return start match {
  //       case x: Int => old_collatzChain(List(x))
  //       case xs: List[Int] => xs match {
  //         case 1 :: tail => 1 :: tail
  //         case head :: _ => old_collatzChain(collatzStep(head) :: xs)
  //         case _ => List[Int]()
  //       }
  //     }
  // }

  def hasNext() : Boolean = {
    return !(-1 to 1 contains current)
  }
  def next() : Int = {
    current = collatzStep(current)
    return current
  }
}

object Collatz_main {
  def main(args: Array[String]){
    args match {
      case Array(h, _*) => println((new CollatzChain(h toInt)).toList)
      case _ => println((new CollatzChain(27)).toList)
    }
  }
}
