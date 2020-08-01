package astrac.effunit
package mutable

import cats.syntax.functor._
import cats.effect.Sync
import scala.collection.mutable

class EffectsCache[F[_]] {
  private val resources: mutable.Map[Int, Any] =
    mutable.Map.empty

  def getResource[A](effectId: Int)(implicit F: Sync[F]): F[A] =
    F.delay(resources(effectId).asInstanceOf[A])

  def addResource(effectId: Int, v: Any)(implicit F: Sync[F]): F[Unit] =
    F.delay(resources.addOne((effectId, v)))

  def removeResource(effectId: Int)(implicit F: Sync[F]): F[Unit] =
    F.delay(resources.remove(effectId)).void
}
