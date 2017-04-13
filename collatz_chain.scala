import scala.language.postfixOps

class IntSingleOrList[T]
object IntSingleOrList {
  implicit object SingleWitness extends IntSingleOrList[Int]
  implicit object ListWitness extends IntSingleOrList[List[Int]]
}
object Collatz {
  val halveEvens: PartialFunction[Int, Int] = new PartialFunction[Int, Int]{
    def isDefinedAt(x: Int) = x%2 == 0
    def apply(x: Int) = x / 2
  }
  def tripplePlusOne(x: Int) = 3*x + 1
  val colatzStep = halveEvens orElse PartialFunction[Int, Int](tripplePlusOne _)

    return start match {
        case x: Int => colatzChain(List(x))
        case xs: List[Int] => xs match {
          case 1 :: tail => 1 :: tail
          case head :: _ => colatzChain(colatzStep(head) :: xs)
          case _ => List[Int]()
        }
      }
  }

  def main(args: Array[String]){
    args match {
    }
  }
}
