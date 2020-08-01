package astrac.effunit

import org.junit.runner.Description

// TODO: Decouple from Description?
trait Notifier[F[_]] {
  def resourceReleaseFailed(d: Description, ex: Throwable): F[Unit]
  def resourceAcquisitionFailed(d: Description, ex: Throwable): F[Unit]
  def resourceAcquisitionStart(d: Description): F[Unit]
  def resourceAcquired(d: Description): F[Unit]
  def resourceReleased(d: Description): F[Unit]
  def testFailure(d: Description, ex: Throwable): F[Unit]
  def testStarted(d: Description): F[Unit]
  def testFinished(d: Description): F[Unit]
  def testAssumptionFailed(d: Description, ex: Throwable): F[Unit]
  def testIgnored(d: Description): F[Unit]
}

