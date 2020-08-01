# EffUnit

This is an experiment on building a junit test runner based on `cats-effect`. It begun with trying to bring together `cats.effect.Resource` with `munit.Fixture`; during this effort I got interested learning a bit more about JUnit and I ended up reorganising the original munit code to deal with `IO` directly rather than trying to lift it in/out of the `GenericTest.body` function.

The result of this effort is a test DSL that allows nesting in order to delimitate areas that should share a resource. This is intended to be a replacement in place of using the classic inheritance-based `beforeAll`/`afterAll`. Failing to acquire a resource will fail as any other test and will also cause all the nested test to be marked as skipped. Failing to release a resource will add a test mark as skipped as well.

This is implemented by tricking the user of the DSL and giving him an `F[_]` of the type of the shared resource that will only be available once the tests are actually run. I did this project just to experiment so this is not battle-tested and the resource trick has a certain naughty feeling to it but it seems to be working.

Feedback and suggestions are appreciated.

## Example

This is an example where a mutable `Stack` is used to simulate resource acquisition:

```scala
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
```

This gives the following output when run:

```
astrac.effunit.StackExampleSpec:
  + acquire-stack-elem-a 0.012s
  + | stack-elem-a-value 0.001s
  + | stack-state-a 0.0s
  + | acquire-stack-elem-b 0.0s
  + | | stack-elem-b-value 0.0s
  + | | stack-state-b 0.0s
  + | | stack-state-post-b 0.0s
  + stack-state-post-a 0.0s 
==> i astrac.effunit.StackExampleSpec.munit-tags-example ignored 0.0s
```

## Dependencies and inspirations

`munit` is a dependency for the project: while I implemented a new suite trait and a new JUnit runner, this project still relies on `munit` for things like `Assertions` and `Location` and I aim to keep it consistent with the experience of using `munit` itself.

This project also depends on `cats` and `cats-effect` for several typeclasses and for `Resource`.

The test tree processing is implemented using recursion schemes powered by `droste`.


## TODOs

- Introduce a `Resource`-based equivalent of `beforeEach`/`afterEach`
- Make `Context` less invasive (possibly remove it)
- More extensive testing, esp. for resource error conditions
- Revisit the case for skipped tests (they are not evident e.g. on sbt)
- Publish on bintray

