package astrac.effunit

import cats.effect.{IO, Resource}
import scala.collection.mutable.Stack

class StackExampleSpec extends MutableEffectfulSuite[IO] {

  var stack: Stack[Int] = Stack.empty

  def withStackElem(i: Int): Resource[IO, Int] =
    Resource.make(IO.delay {
      stack.push(i)
      i
    }) { i =>
      IO.suspend {
        if (stack.pop() != i) {
          IO.raiseError(new RuntimeException("Unexpected resource status"))
        } else
          IO.unit
      }
    }

  sharingResource("acquire-stack-elem-a", withStackElem(10)) { elemA =>
    test("stack-elem-a-value") {
      elemA.get.map(assertEquals(_, 10))
    }

    testPure("stack-state-a") {
      assertEquals(stack.toList, List(10))
    }

    sharingResource("acquire-stack-elem-b", withStackElem(20)) { elemB =>
      test("stack-elem-b-value") {
        elemB.get.map(assertEquals(_, 20))
      }

      testPure("stack-state-b") {
        assertEquals(stack.toList, List(20, 10))
      }
    }

    testPure("stack-state-post-b") {
      assertEquals(stack.toList, List(10))
    }
  }

  testPure("stack-state-post-a") {
    assertEquals(stack.toList, Nil)
  }

  testPure("munit-tags-example".ignore) {
    throw new RuntimeException("This should not be reached")
  }

}

