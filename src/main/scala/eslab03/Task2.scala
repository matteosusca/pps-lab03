package eslab03

import u02.Modules.Person
import u02.Modules.Person.*
import u03.Sequences
import u03.Sequences.Sequence
import u03.Sequences.Sequence.*
import u03.Streams
import u03.Streams.Stream

object Task2 extends App:
  def getCourses(people: Sequence[Person]): Sequence[String] =
    flatMap(people)(p =>
      p match
        case Teacher(_, course) => Cons(course, Nil())
        case _ => Nil()
    )

  def foldLeft[A, B](seq: Sequence[A])(acc: B)(op: (B,A) => B): B = seq match
    case Cons(h, t) => foldLeft(t)(op(acc,h))(op)
    case _ => acc

  def countCourses(seq: Sequence[Person]): Int =
    foldLeft(map(getCourses(seq))(v => 1))(0)(_ + _)

// Task 3
object Task3 extends App:
  def fill[A](n: Int)(k: A): Sequence[A] = n match
    case n if n > 0 => Cons(k, fill(n - 1)(k))
    case _ => Nil()

  def getFibonacci(): Stream[Int] = {
    def fibFrom(a: Int, b: Int): Stream[Int] =
      Stream.cons(a, fibFrom(b, a + b))

    fibFrom(0, 1)
  }