package astrac.effunit
package tree
package filters

import higherkindness.droste.{Algebra, Trans, scheme}
import higherkindness.droste.data.{Attr, AttrF}
import higherkindness.droste.data.prelude._
import cats.instances.option._
import cats.syntax.semigroup._
import cats.syntax.functor._
import FilterAttributes._

class LabelledFilter[F[_], L](
    matchPredicate: LabelledTreeF[F, L, _] => Boolean,
    ignorePredicate: LabelledTreeF[F, L, _] => Boolean
) extends (LabelledTree[F, L] => Option[LabelledTree[F, L]]) {

  type TreeF[A] = LabelledTreeF[F, L, A]
  type Tree = LabelledTree[F, L]
  type AttributedTreeF[A] = AttrF[TreeF, FilterAttributes, A]
  type AttributedTree = Attr[TreeF, FilterAttributes]

  private val computeFilterTrans
      : Trans[TreeF, AttributedTreeF, AttributedTree] =
    Trans { a =>
      a.lower match {
        case Test(_, _) =>
          val attributes = (Option(MatchThis).filter(_ => matchPredicate(a)) |+|
            Option(IgnoreThis).filter(_ => ignorePredicate(a)))
            .getOrElse(NoAttributes)

          AttrF(attributes, a)

        case SharingResource(_, _, _, ts) =>
          val matched = matchPredicate(a)
          val ignored = ignorePredicate(a)

          val thisAttributes =
            (Option(MatchThis).filter(_ => matched) |+|
              Option(IgnoreThis).filter(_ => ignored) |+|
              Option(MatchDescendant).filter(_ =>
                ts.exists(_.head.matchThisOrDescendant)
              ))
              .getOrElse(NoAttributes)

          val descendantAttributes =
            (Option(MatchAncestor).filter(_ => matched) |+|
              Option(IgnoreAncestor).filter(_ => ignored))
              .getOrElse(NoAttributes)

          AttrF(
            thisAttributes,
            if (matched) a.map(_.map(_ |+| descendantAttributes)) else a
          )
      }
    }

  private val applyFilterAlgebra: Algebra[AttributedTreeF, Option[Tree]] =
    Algebra { t =>
      t match {
        case AttrF(f, AttrF(l, Test(o, r))) if f.isMatched =>
          Some(Attr(l, Test(o, r)))

        case AttrF(f, AttrF(l, SharingResource(o, e, r, ts))) if f.isMatched =>
          Some(Attr(l, SharingResource(o, e, r, ts.flatten)))

        case _ => None
      }
    }

  def apply(tree: Tree): Option[Tree] =
    scheme
      .cata(computeFilterTrans.algebra)
      .andThen(scheme.cata(applyFilterAlgebra))
      .apply(tree)
}

object LabelledFilter {

  def apply[F[_], L](
      matchPredicate: AttrF[TestTree[F, *], L, _] => Boolean,
      ignorePredicate: AttrF[TestTree[F, *], L, _] => Boolean
  ): LabelledFilter[F, L] =
    new LabelledFilter[F, L](matchPredicate, ignorePredicate)

  def ignoreIf[F[_], L](
      predicate: AttrF[TestTree[F, *], L, _] => Boolean
  ): LabelledFilter[F, L] =
    LabelledFilter(_ => true, predicate)

  def matchIf[F[_], L](
      predicate: AttrF[TestTree[F, *], L, _] => Boolean
  ): LabelledFilter[F, L] =
    LabelledFilter(predicate, _ => false)
}

