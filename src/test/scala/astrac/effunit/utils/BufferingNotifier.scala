package astrac.effunit
package utils

import cats.effect.Sync
import cats.syntax.functor._
import org.junit.runner.Description
import scala.collection.mutable.ArrayBuffer

sealed trait Notification
case class TestStarted(description: String) extends Notification
case class TestFailure(description: String, ex: Throwable) extends Notification
case class TestFinished(description: String) extends Notification
case class TestAssumptionFailed(description: String, ex: Throwable)
    extends Notification
case class TestIgnored(description: String) extends Notification
case class ResourceAcquisitionStart(description: String) extends Notification
case class ResourceAcquired(description: String) extends Notification
case class ResourceReleased(description: String) extends Notification
case class ResourceAcquisitionFailed(description: String, ex: Throwable)
    extends Notification
case class ResourceReleaseFailed(description: String, ex: Throwable)
    extends Notification

class BufferingNotifier[F[_]: Sync] extends Notifier[F] {
  private val F = Sync[F]
  private var internalBuffer: ArrayBuffer[Notification] = ArrayBuffer.empty

  def notifications = internalBuffer.toList

  override def resourceAcquisitionStart(d: Description): F[Unit] =
    F.delay(internalBuffer.addOne(ResourceAcquisitionStart(d.getDisplayName())))
      .void

  override def resourceReleaseFailed(d: Description, ex: Throwable): F[Unit] =
    F.delay(internalBuffer.addOne(ResourceReleaseFailed(d.getDisplayName, ex)))
      .void

  override def resourceAcquisitionFailed(
      d: Description,
      ex: Throwable
  ): F[Unit] =
    F.delay(
      internalBuffer.addOne(ResourceAcquisitionFailed(d.getDisplayName, ex))
    ).void

  override def resourceAcquired(d: Description): F[Unit] =
    F.delay(internalBuffer.addOne(ResourceAcquired(d.getDisplayName))).void

  override def resourceReleased(d: Description): F[Unit] =
    F.delay(internalBuffer.addOne(ResourceReleased(d.getDisplayName))).void

  override def testFailure(d: Description, ex: Throwable): F[Unit] =
    F.delay(internalBuffer.addOne(TestFailure(d.getDisplayName, ex))).void

  override def testStarted(d: Description): F[Unit] =
    F.delay(internalBuffer.addOne(TestStarted(d.getDisplayName))).void

  override def testFinished(d: Description): F[Unit] =
    F.delay(internalBuffer.addOne(TestFinished(d.getDisplayName))).void

  override def testAssumptionFailed(d: Description, ex: Throwable): F[Unit] =
    F.delay(internalBuffer.addOne(TestAssumptionFailed(d.getDisplayName, ex)))
      .void

  override def testIgnored(d: Description): F[Unit] =
    F.delay(internalBuffer.addOne(TestIgnored(d.getDisplayName))).void

}
