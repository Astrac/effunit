package astrac.effunit
package utils

import munit.{Location, Tag, TestOptions}
import higherkindness.droste.{Trans, scheme}
import higherkindness.droste.data.Fix

import tree._

case class TestMeta(name: String, tags: Set[Tag], location: Location)
object TestMeta {
  def apply(o: TestOptions): TestMeta = TestMeta(o.name, o.tags, o.location)
}

sealed trait MetaTreeF[A]
case class LeafF[A](options: TestMeta) extends MetaTreeF[A]
case class NodeF[A](options: TestMeta, effectId: Int, children: A*)
    extends MetaTreeF[A]

object MetaTree {
  def leaf(options: TestMeta) = Fix(LeafF[MetaTree](options))
  def node(options: TestMeta, eventId: Int, children: MetaTree*) =
    Fix(NodeF(options, eventId, children: _*))

  def toMetaTrans[F[_]]: Trans[TestTree[F, *], MetaTreeF, MetaTree] =
    Trans {
      case Test(o, _)                   => LeafF(TestMeta(o))
      case SharingResource(o, e, _, ts) => NodeF(TestMeta(o), e, ts: _*)
    }

  def apply[F[_]](t: TestTree.Fixed[F]): MetaTree =
    scheme.cata(toMetaTrans[F].algebra).apply(t)
}
