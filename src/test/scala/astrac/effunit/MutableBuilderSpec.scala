package astrac.effunit

import munit.FunSuite

import utils._
import MetaTree.{leaf, node}

class MutableBuilderSpec extends FunSuite {

  val (builder, expectedMeta) = Examples.notExpectedToRun

  test("MutableBuilder.testTrees") {
    assertEquals(
      builder.testTrees.map(MetaTree(_)),
      List(
        node(
          expectedMeta("res1", 1),
          0,
          leaf(expectedMeta("res1.test1", 2)),
          leaf(expectedMeta("res1.test2".fail, 3)),
          node(
            expectedMeta("res1.res2", 4),
            1,
            leaf(expectedMeta("res1.res2.test1".ignore, 5)),
            leaf(
              expectedMeta("res1.res2.test2".tag(Examples.aTag), 6)
            )
          )
        ),
        leaf(expectedMeta("root-test-1", 9)),
        leaf(expectedMeta("root-test-2", 10))
      )
    )
  }

  test("MutableBuilder.testTreesFailOnNonEmptyStack".fail) {
    builder.sharingResource("push-to-stack", Examples.unitResource) { _ =>
      builder.testTrees
      ()
    }
  }
}
