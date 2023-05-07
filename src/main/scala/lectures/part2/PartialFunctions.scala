package lectures.part2

object PartialFunctions extends App {
  val aFunction = (x: Int) => x + 1

  val aFussyFunction = (x: Int) =>
    if x == 1 then 42
    else if x == 2 then 42
    else if x == 5 then 999
    else throw new FunctionNotApplicableException

  class FunctionNotApplicableException extends RuntimeException

  val aNicerFussyFunction = (x: Int) => x match
    case 1 => 42
    case 2 => 56
    case 3 => 999

  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  } // partial function value

  println(aPartialFunction(2))

  // PF utilities
  println(aPartialFunction.isDefinedAt(67))

  // lift
  val lifted = aPartialFunction.lift
  println(lifted(2))
  println(lifted(98))

  val pfChain = aPartialFunction.orElse[Int, Int] {
    case 45 => 67
  }

  println(pfChain(2))
  println(pfChain(45))

  val aTotalFunction: Int => Int = {
    case 1 => 99
  }

  // HOFs accept partial functions as well

  val aMappedList = List(1, 2, 3).map {
    case 1 => 42
    case 2 => 78
    case 3 => 1000
  }

  println(aMappedList)

  /*
    Note: PF can only have ONE parameter type
  */

  /**
    * Exercises
    *
    * 1 - construct  a PF instance yourself (anonymoys class)
    * 2 - dump chatbot as a PF
    */

  val manualFussyFunction = new PartialFunction[Int, Int] {
    override def apply(v1: Int): Int = v1 match
      case 1 => 42
      case 2 => 65
      case 5 => 999

    override def isDefinedAt(x: Int): Boolean =
      x == 1 || x == 2 || x == 5
  }

  val chatbot: PartialFunction[String, String] = {
    case "hi" => "hello fellas"
    case "pastel" => "hmmm i like it"
    case "call 911 now" => "vrummm rim vruuumm tank tank tank skrrrrr"
    case _ => "sorry i am dump"
  }

  scala.io.Source.stdin.getLines().map(chatbot).foreach(println)
}
