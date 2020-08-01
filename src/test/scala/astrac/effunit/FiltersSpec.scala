package astrac.effunit

import cats.effect.IO
import cats.syntax.functor._
import munit.{FunSuite, Only}
import higherkindness.droste.{Trans, scheme}
import higherkindness.droste.data.AttrF
import higherkindness.droste.data.prelude._

import tree._
import filters.LabelledFilter
import utils._
import MetaTree.{leaf, node}

class FiltersSpec extends FunSuite {

  type DepthTreeF[A] = LabelledTreeF[IO, Int, A]
  type DepthTree = LabelledTree[IO, Int]

  val depthLabelTrans: Trans[TestTree[IO, *], DepthTreeF, DepthTree] =
    Trans {
      case t @ Test(_, _) => AttrF(0, t)
      case SharingResource(o, e, r, ts) =>
        AttrF(0, SharingResource(o, e, r, ts.map(_.map(_ + 1))))
    }

  def toDepthTree(tree: TestTree.Fixed[IO]): DepthTree =
    scheme.cata(depthLabelTrans.algebra).apply(tree)

  val depthFilter = LabelledFilter.ignoreIf[IO, Int](_.ask > 1)

  val res = Examples.unitResource

  test("depthFilterExample") {
    val (builder, expectedMeta) = Examples.notExpectedToRun
    val trees = builder.testTrees
    val filtered = trees.flatMap(t => depthFilter(toDepthTree(t)))

    assertEquals(
      filtered.map(t => MetaTree(t.forget)),
      List(
        node(
          expectedMeta("res1", 1),
          0,
          leaf(expectedMeta("res1.test1", 2)),
          leaf(expectedMeta("res1.test2".fail, 3)),
          node(expectedMeta("res1.res2", 4), 1)
        ),
        leaf(expectedMeta("root-test-1", 9)),
        leaf(expectedMeta("root-test-2", 10))
      )
    )
  }

  test("only") {
    val (builder, expectedMeta) = Examples.tagged(Only)

    assertEquals(
      builder.testTrees.flatMap(filters.only[IO]).map(MetaTree(_)),
      List(
        node(
          expectedMeta("untagged-root-res", 1),
          0,
          leaf(expectedMeta("tagged-root-res-test".only, 3)),
          node(
            expectedMeta("tagged-sub-res".only, 4),
            1,
            leaf(expectedMeta("untagged-sub-res-test", 5)),
            node(
              expectedMeta("untagged-sub-sub-res", 6),
              2,
              leaf(expectedMeta("untagged-sub-sub-res-test", 7))
            )
          )
        ),
        leaf(expectedMeta("tagged-root-test".only, 12))
      )
    )
  }

}
