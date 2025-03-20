package eslab03

import org.junit.*
import org.junit.Assert.*
import u02.Modules.Person.*
import u03.Sequences.Sequence.*
import u03.Sequences.*

class Task2Test:

  @Test def testGetCourses() =
    val people = Cons(Teacher("John", "Math"), Cons(Student("Alice", 2021), Cons(Teacher("Bob", "Physics"), Nil())))
    val courses = Task2.getCourses(people)
    val expectedCourses = Cons("Math", Cons("Physics", Nil()))
    assertEquals(expectedCourses, courses)