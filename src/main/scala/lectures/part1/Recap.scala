package lectures.part1

import scala.annotation.tailrec

object Recap extends App {
  val condition: Boolean = false
  val conditionedVal = if condition then 43 else 65

  // instructions vs expressions

  // compiler infers types for us
  val codeBlock = {
    if condition then 64 else 53
  }

  // Unit = void
  val theUnit = println("Hello, Scala")

  // functions
  def function(x: Int): Int = x + 1

  // recursion: stack and tail
  @tailrec
  def factorial(n: Int, acc: Int): Int =
    if n <= 0 then acc else factorial(n - 1, acc)

  // object-oriented programming
  class Animal

  class Dog extends Animal

  val dog: Animal = new Dog

  trait Carnivore {
    def eat(a: Animal): Unit
  }

  class Crocodile extends Animal with Carnivore {
    override def eat(a: Animal): Unit = println("Crunch!")
  }

  // val notation
  val croc = Crocodile()
  croc.eat(dog)
  croc eat dog

  val newCroc = new Crocodile {
    override def eat(a: Animal): Unit = println("Roar!")
  }

  // generics
  abstract class MyList[+A] // variance and variance problems

  // singletons and companions
  object MyList

  // case classes
  case class Person(name: String, age: Int)

  // exceptions try/catch/finally

  val throwsException = throw new RuntimeException // nothing
  val potencialFailure = try {
    throw new RuntimeException
  } catch {
    case e: Exception => "I caught an exception!"
  } finally {
    println("Some logs")
  }

  // packaging and imports

  // functional programming
  var incrementer = new Function[Int, Int] {
    override def apply(v1: Int): Int = v1 + 1
  }

  incrementer(1)

  val anonymousIncrementer = (x: Int) => x + 1
  List(1, 2, 3).map(anonymousIncrementer) // HOF
  // map, flatMap, filter

  // for-comprehension
  val pars = for {
    num <- List(1, 2, 3) // if condition
    char <- List('a', 'b', 'c')
  } yield num + "-" + char

  // Scala collections:  Seqs, Arrays, Lists, Vectors, Maps, Tuples
  val map = Map(
    "Daniel" -> 789,
    "Pastel" -> 555
  )

  // "collections": Options and tries
  val anOption = Some(2)

  // Pattern Matching
  val x = 2
  val order = x match
    case 1 => "first"
    case 2 => "second"
    case 3 => "third"
    case _ => x + "th"

  val bob = Person("Bob", 22)

  val greeting = bob match
    case Person(n, _) => s"Hello, $n"

  // all the patterns
}
