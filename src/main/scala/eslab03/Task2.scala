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