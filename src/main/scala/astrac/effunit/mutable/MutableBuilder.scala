package astrac.effunit
package mutable

import cats.effect.{Resource, Sync}
import cats.syntax.functor._
import scala.collection.mutable
import munit.TestOptions
import munit.TestOptionsConversions

import tree._

trait MutableBuilder[F[_]] extends TestOptionsConversions {

  private var resCounter: Int = 0

  private val stack: mutable.Stack[SharingResource[F, _, TestTree.Fixed[F]]] =
    mutable.Stack.empty

  private val treeBuffer: mutable.ListBuffer[TestTree.Fixed[F]] =
    mutable.ListBuffer.empty

  val effectsCache = new EffectsCache[F]

  def testTrees: List[TestTree.Fixed[F]] =
    if (stack.isEmpty) treeBuffer.toList
    else
      throw new RuntimeException(
        s"Stack must be empty when testTree is called. Stack was: $stack"
      )

  def test(
      options: TestOptions
  )(f: => F[_])(implicit F: Sync[F]): Unit =
    addTree(TestTree.test(options, Sync[F].defer(f).void))

  def testPure(
      options: TestOptions
  )(f: => Any)(implicit F: Sync[F]): Unit =
    test(options)(F.delay(f))

  def sharingResource[A](
      options: TestOptions,
      r: Resource[F, A]
  )(f: Context[F, A] => Unit)(implicit F: Sync[F]): Unit = {
    val id = resCounter
    resCounter = resCounter + 1

    stack.push(SharingResource(options, id, r, Nil))
    f(Context(effectsCache.getResource(id)))
    addTree(TestTree.fix(stack.pop()))
  }

  private def addTree(tree: TestTree.Fixed[F]): Unit = {
    if (stack.isEmpty) treeBuffer.addOne(tree)
    else {
      val top = stack.pop()
      val newTop = top.copy(trees = top.trees :+ tree)

      stack.push(newTop)
    }
  }
}

object MutableBuilder {
  def apply[F[_]]: MutableBuilder[F] = new MutableBuilder[F] {}
}
