package astrac.effunit

import cats.effect.Sync
import org.junit.runner.notification.{Failure, RunNotifier}
import org.junit.runner.Description

// $COVERAGE-OFF$
class JUnitNotifier[F[_]](notifier: RunNotifier)(implicit F: Sync[F])
    extends Notifier[F] {

  override def resourceAcquisitionFailed(
      d: Description,
      ex: Throwable
  ): F[Unit] =
    testFailure(d, ex)

  override def resourceReleaseFailed(d: Description, ex: Throwable): F[Unit] =
    testAssumptionFailed(d, ex)

  override def resourceReleased(d: Description): F[Unit] = F.unit

  override def resourceAcquired(d: Description): F[Unit] =
    testFinished(d)

  override def resourceAcquisitionStart(d: Description): F[Unit] =
    testStarted(d)

  override def testFailure(d: Description, ex: Throwable): F[Unit] =
    F.delay(notifier.fireTestFailure(new Failure(d, ex)))

  override def testStarted(d: Description): F[Unit] =
    F.delay(notifier.fireTestStarted(d))

  override def testFinished(d: Description): F[Unit] =
    F.delay(notifier.fireTestFinished(d))

  override def testAssumptionFailed(
      d: Description,
      ex: Throwable
  ): F[Unit] =
    F.delay(notifier.fireTestAssumptionFailed(new Failure(d, ex)))

  override def testIgnored(d: Description): F[Unit] =
    F.delay(notifier.fireTestIgnored(d))

}

object JUnitNotifier {
  def apply[F[_]: Sync](
      notifier: RunNotifier
  ): JUnitNotifier[F] =
    new JUnitNotifier[F](notifier)
}
// $COVERAGE-ON$
