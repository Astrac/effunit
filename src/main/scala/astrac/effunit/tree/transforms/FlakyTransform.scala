package astrac.effunit
package tree
package transforms

import cats.MonadError
import cats.syntax.applicativeError._
import higherkindness.droste.{Trans, scheme}
import higherkindness.droste.data.AttrF
import higherkindness.droste.data.prelude._
import munit.Flaky

import description.{DescribedTree, DescribedTreeF}

class FlakyTransform[F[_]](allowFlaky: Boolean)(implicit
    F: MonadError[F, Throwable]
) extends Transform[F] {

  private def flakyHandler(t: TestTree[F, _]): Throwable => F[Unit] =
    ex => F.raiseError(FlakyFailureException(t, ex))

  private val flakyTrans
      : Trans[DescribedTreeF[F, *], DescribedTreeF[F, *], DescribedTree[F]] =
    Trans {
      case AttrF(l, t @ Test(o, r)) if o.tags.contains(Flaky) && allowFlaky =>
        AttrF(l, Test(o, r.handleErrorWith(flakyHandler(t))))

      case t => t
    }

  def apply(tree: DescribedTree[F]): DescribedTree[F] =
    scheme.cata(flakyTrans.algebra).apply(tree)
}

object FlakyTransform {
  def apply[F[_]: MonadError[*[_], Throwable]](
      allowFlaky: Boolean
  ): FlakyTransform[F] =
    new FlakyTransform[F](allowFlaky)
}
