package lectures.part1

import scala.util.Try

object DarkSugars extends App {

  // #1 single arg methods
  def singleArgMethod(arg: Int): String = s"$arg little ducks"

  val description = singleArgMethod {
    // write some code
    43 // return the result at the end
  }

  val tryInstance = Try { // java's try {...}
    throw new RuntimeException
  }

  List(1, 2, 3, 4).map { x =>
    x + 1
  }

  // #2 single abstract method
  trait Action {
    def act(x: Int): Int
  }

  val instance: Action = new Action {
    override def act(x: Int): Int = x + 1
  }

  val funkyInstance: Action = (x: Int) => x + 1 // magic

  val simpleThread = new Thread(new Runnable {
    override def run(): Unit = println("Hello, Pedro!")
  })

  val sweeterThread = new Thread(() => println("sweet pedro"))

  abstract class AnAbstractType {
    def implemented: Int = 23

    def f(a: Int): Unit
  }

  val anAbstractInstance: AnAbstractType = (a: Int) => println(a)

  // #3 the :: and #:: methods are special

  val prependedList = 2 :: List(3, 4)
  // List(3, 4).::(2) ???

  // answer: scala spec => last char decides associativity of method
  1 :: 2 :: 3 :: List(4, 5)
  // ==
  List(4 ,5).::(3).::(2).::(1)

  class MyStream[T] {
    // note the ':' after arrow, it tells to the compile that the last expression will be evaluated first
    def -->:(value: T): MyStream[T] = this // actual implementation here
  }

  // new MyStream will be evaluated first
  val myStream = 1 -->: 2 -->: 3 -->: new MyStream[Int]

  // syntax sugar #4: multi-word method naming
  class ScoobyDoo(name: String) {
    def `e a perua falou`(gossip: String): Unit = println(s"E a perua falou $gossip")
  }

  val fred = new ScoobyDoo("Fred")
  fred `e a perua falou` "\"Ta afim?\" e eu disse \"agora não\""

  // #5: Infix types
  class Composite[A, B]
  val composite: Composite[Int, String] = ???
  val otherComposite: Int Composite String = ???

  class -->[A, B]
  val towards: Int --> String = ???

  // # 6: update() is very special, much like apply()
  val anArray = Array(1, 2, 3)
  anArray(2) = 7 // rewritten to anArray.update(2, 7)
  // used in mutable collections
  // remember apply() and update()

  // #7: setters for mutable containers
  class Mutable {
    private var internalMember: Int = 0 // private for OO encapsulation
    def member = internalMember // "getter"
    def member_=(value: Int):Unit =
      internalMember = value // "setter"
  }

  val mutableContainer = new Mutable

  mutableContainer.member = 42 // rewritten as  mutableContainer.member_=(42)
}
