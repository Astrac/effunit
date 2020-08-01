package astrac.effunit
package tree

import cats.{Applicative, Traverse}
import cats.effect.Resource
import cats.instances.list._
import cats.syntax.functor._
import cats.syntax.traverse._
import higherkindness.droste.data.Fix
import higherkindness.droste.util.DefaultTraverse
import munit.TestOptions
import java.lang.annotation.Annotation
import scala.collection.mutable

// TODO:
//   - Introduce EachUsingResource
//   - Try radical idea: The tree is just a tree, attributes are all added via Attr

case class Context[X[_], A](get: X[A])

sealed trait TestTree[F[_], R] {
  def options: TestOptions

  def annotations: Array[Annotation] = {
    val buf = new mutable.ArrayBuffer[Annotation](options.tags.size + 1)
    buf ++= options.tags
    buf += options.location
    buf.toArray
  }
}

case class SharingResource[F[_], A, R](
    options: TestOptions,
    effectId: Int,
    resource: Resource[F, A],
    trees: List[R]
) extends TestTree[F, R]

case class Test[F[_], R](options: TestOptions, run: F[Unit])
    extends TestTree[F, R]

object TestTree {
  type Fixed[F[_]] = Fix[TestTree[F, *]]

  def fix[F[_]](t: TestTree[F, TestTree.Fixed[F]]): TestTree.Fixed[F] =
    Fix(t)

  def unfix[F[_]](t: TestTree.Fixed[F]): TestTree[F, TestTree.Fixed[F]] =
    Fix.un(t)

  def test[F[_]](options: TestOptions, run: F[Unit]): TestTree.Fixed[F] =
    Fix(Test(options, run))

  implicit def traverseInstance[F[_]]: Traverse[TestTree[F, *]] =
    new DefaultTraverse[TestTree[F, *]] {
      override def traverse[G[_]: Applicative, A, B](fa: TestTree[F, A])(
          f: A => G[B]
      ): G[TestTree[F, B]] =
        fa match {
          case Test(opts, run) => Applicative[G].pure(Test(opts, run))
          case SharingResource(opts, effectId, resource, trees) =>
            trees.traverse(f).map(SharingResource(opts, effectId, resource, _))
        }
    }
}
