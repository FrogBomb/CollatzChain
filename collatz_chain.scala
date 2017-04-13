import scala.language.postfixOps

object Collatz {
  val halveEvens: PartialFunction[Int, Int] = new PartialFunction[Int, Int]{
    def isDefinedAt(x: Int) = x%2 == 0
    def apply(x: Int) = x / 2
  }
  def tripplePlusOne(x: Int) = 3*x + 1
  val colatzStep = halveEvens orElse PartialFunction[Int, Int](tripplePlusOne _)

  def colatzChain(xs: List[Int]) : List[Int] = {
    return xs match {
        case 1 :: tail => 1 :: tail
        case head :: _ => colatzChain(colatzStep(head) :: xs)
        case _ => List()
      }
  }

  def main(args: Array[String]){
    args match {
      case Array(h, _*) => println(colatzChain(List(h toInt)))
      case _ => println(colatzChain(List(27)))
    }
  }
}
