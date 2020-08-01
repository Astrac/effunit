package astrac.effunit
package tree
package transforms

import cats.MonadError
import higherkindness.droste.{Trans, scheme}
import higherkindness.droste.data.AttrF
import higherkindness.droste.data.prelude._
import munit.Ignore

import description.{DescribedTree, DescribedTreeF}

class IgnoreTransform[F[_]](implicit
    F: MonadError[F, Throwable]
) extends Transform[F] {

  private def ignore(t: TestTree[F, _]): F[Unit] =
    F.raiseError(TestIgnoredException(t))

  private val ignoreTrans
      : Trans[DescribedTreeF[F, *], DescribedTreeF[F, *], DescribedTree[F]] =
    Trans {
      case AttrF(l, t @ Test(o, _)) if o.tags.contains(Ignore) =>
        AttrF(l, Test(o, ignore(t)))

      case t => t
    }

  def apply(tree: DescribedTree[F]): DescribedTree[F] =
    scheme.cata(ignoreTrans.algebra).apply(tree)
}

object IgnoreTransform {
  def apply[F[_]: MonadError[*[_], Throwable]]: IgnoreTransform[F] =
    new IgnoreTransform[F]
}

