package astrac.effunit
package tree
package transforms

import cats.MonadError
import cats.syntax.applicativeError._
import cats.syntax.flatMap._
import higherkindness.droste.{Trans, scheme}
import higherkindness.droste.data.AttrF
import higherkindness.droste.data.prelude._
import munit.{Assertions, Fail, FailException}

import description.{DescribedTree, DescribedTreeF}

class FailTransform[F[_]](implicit
    F: MonadError[F, Throwable]
) extends Transform[F] {

  private def raiseExpectedFailure(t: TestTree[F, _]): F[Unit] =
    F.raiseError(
      new FailException(
        Assertions.munitLines.formatLine(
          t.options.location,
          "expected failure but test passed"
        ),
        t.options.location
      )
    )

  private val failTrans
      : Trans[DescribedTreeF[F, *], DescribedTreeF[F, *], DescribedTree[F]] =
    Trans {
      case AttrF(l, t @ Test(o, r)) if o.tags.contains(Fail) =>
        val modifiedRun =
          r.attempt.flatMap { outcome =>
            outcome.fold(_ => F.unit, _ => raiseExpectedFailure(t))
          }

        AttrF(l, Test(o, modifiedRun))

      case t =>
        t

    }

  def apply(tree: DescribedTree[F]): DescribedTree[F] =
    scheme.cata(failTrans.algebra).apply(tree)
}

object FailTransform {
  def apply[F[_]: MonadError[*[_], Throwable]]: FailTransform[F] =
    new FailTransform[F]
}
