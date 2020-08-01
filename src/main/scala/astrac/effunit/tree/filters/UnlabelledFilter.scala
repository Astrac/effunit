package astrac.effunit
package tree
package filters

import higherkindness.droste.data.{Attr, AttrF}
import higherkindness.droste.data.prelude._

class UnlabelledFilter[F[_]](
    matchPredicate: TestTree[F, _] => Boolean,
    ignorePredicate: TestTree[F, _] => Boolean
) extends (TestTree.Fixed[F] => Option[TestTree.Fixed[F]]) {

  private val internalFilter: LabelledFilter[F, None.type] =
    new LabelledFilter[F, None.type](
      matchPredicate.compose(AttrF.un(_)._2),
      ignorePredicate.compose(AttrF.un(_)._2)
    )

  def apply(tree: TestTree.Fixed[F]): Option[TestTree.Fixed[F]] = {
    internalFilter(Attr.ana(tree)(TestTree.unfix, _ => None)).map(_.forget)
  }
}

object UnlabelledFilter {

  def apply[F[_]](
      matchPredicate: TestTree[F, _] => Boolean,
      ignorePredicate: TestTree[F, _] => Boolean
  ): UnlabelledFilter[F] =
    new UnlabelledFilter[F](matchPredicate, ignorePredicate)

  def matchIf[F[_]](
      predicate: TestTree[F, _] => Boolean
  ): UnlabelledFilter[F] =
    UnlabelledFilter(predicate, _ => false)
}

