package astrac.effunit
package tree

import cats.effect.{Resource, Sync}
import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.applicativeError._
import cats.syntax.traverse._
import higherkindness.droste.{Algebra, RAlgebra, scheme}
import higherkindness.droste.data.prelude._
import munit.internal.console.StackTraces
import org.junit.runner.Description

import description.{DescribedTree, DescribedTreeF}

class Evaluator[F[_]](
    notifier: Notifier[F],
    addResource: (Int, Any) => F[Unit],
    removeResource: Int => F[Unit],
    trimStackTraces: Boolean
)(implicit
    F: Sync[F]
) extends (DescribedTree[F] => F[Unit]) {
  import notifier._

  private def trimStackTrace(ex: Throwable): Unit = {
    if (trimStackTraces) {
      StackTraces.trimStackTrace(ex)
      ()
    }
  }

  private def testErrorHandler(
      description: Description,
      ex: Throwable
  ): F[Unit] =
    ex match {
      case TestIgnoredException(_) =>
        testIgnored(description)
      case FlakyFailureException(_, ex) =>
        trimStackTrace(ex)
        testAssumptionFailed(description, ex)
      case ex =>
        testFailure(description, ex)
    }

  // TODO: Fix swallowing of exceptions in EffectsCache
  private def resourceErrorHandler(
      description: Description,
      dependents: List[DescribedTree[F]],
      ex: Throwable
  ): F[Unit] =
    testErrorHandler(description, ex).void >>
      dependents
        .traverse(t => ignoreDependent(t, ex))
        .void

  private def dependentFailuresAlgebra(
      ex: Throwable
  ): Algebra[DescribedTreeF[F, *], F[Unit]] =
    Algebra { tree =>
      val description = tree.ask.unwrap
      tree.lower match {
        case Test(_, _) =>
          testAssumptionFailed(description, ex)
        case SharingResource(_, _, _, deps) =>
          testAssumptionFailed(description, ex) >> deps.sequence.void
      }
    }

  private def ignoreDependent(
      t: DescribedTree[F],
      ex: Throwable
  ) =
    scheme
      .cata(dependentFailuresAlgebra(ex))
      .apply(t)

  private def evaluationAlgebra
      : RAlgebra[DescribedTree[F], DescribedTreeF[F, *], F[Unit]] =
    RAlgebra { tree =>
      val description = tree.ask.unwrap
      tree.lower match {

        case Test(_, run) =>
          (testStarted(description) >>
            run
              .handleErrorWith(testErrorHandler(description, _))) >>
            testFinished(description)

        case SharingResource(_, id, resource, trees) =>
          resourceAcquisitionStart(description) >>
            resource
              .flatTap(_ =>
                Resource.make(F.unit)(_ => resourceReleased(description))
              )
              .use(a =>
                addResource(id, a) >>
                  resourceAcquired(description) >>
                  trees.traverse(_._2).void >>
                  removeResource(id)
              )
              .handleErrorWith(
                resourceErrorHandler(
                  description,
                  trees.map(_._1),
                  _
                )
              )
      }
    }

  def apply(tree: DescribedTree[F]): F[Unit] =
    scheme.zoo.para(evaluationAlgebra).apply(tree)
}
