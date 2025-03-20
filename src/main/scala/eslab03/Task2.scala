package eslab03

import u02.Modules.Person
import u02.Modules.Person.*
import u03.Sequences
import u03.Sequences.Sequence
import u03.Sequences.Sequence.*

object Task2 extends App:
  def getCourses(people: Sequence[Person]): Sequence[String] =
    flatMap(people)(p =>
      p match
        case Teacher(_, course) => Cons(course, Nil())
        case _ => Nil()
    )

  def foldLeft[A](seq: Sequence[A])(a: A)(op: (A,A) => A): A = seq match
    case Cons(h, t) => foldLeft(t)(op(a,h))(op)
    case _ => a