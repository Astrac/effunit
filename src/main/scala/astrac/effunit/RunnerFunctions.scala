package astrac.effunit

import cats.effect.Sync
import cats.instances.list._
import cats.syntax.functor._
import cats.syntax.traverse._
import munit.Assertions
import org.junit.runner.manipulation.Filter

import mutable.EffectsCache
import tree._
import tree.description._

class RunnerFunctions[F[_]](
    cls: Class[_ <: EffectfulSuite],
    effectsCache: EffectsCache[F],
    filter: Filter = Filter.ALL,
    trimStackTraces: Boolean = true,
    allowOnly: Boolean = true,
    allowFlaky: Boolean = false
)(implicit F: Sync[F]) {

  def run(trees: List[DescribedTree[F]], notifier: Notifier[F]): F[Unit] = {
    val evaluator =
      new Evaluator(
        notifier,
        effectsCache.addResource _,
        effectsCache.removeResource _,
        trimStackTraces
      )

    trees
      .map(transforms.FlakyTransform[F](allowFlaky).apply(_))
      .map(transforms.FailTransform[F].apply(_))
      .map(transforms.IgnoreTransform[F].apply(_))
      .traverse(tree => evaluator(tree))
      .void
  }

  def filteredTrees(
      testTrees: List[TestTree.Fixed[F]]
  ): List[DescribedTree[F]] = {
    val labeller = ChildlessLabeller[F](cls)

    val onlySuite =
      testTrees.flatMap(filters.only)

    if (!onlySuite.isEmpty && !allowOnly)
      Assertions.fail("'Only' tag is not allowed when `isCI=true`")(
        Locations.testLocations(onlySuite.head).head
      )

    val childless = if (onlySuite.isEmpty) {
      testTrees
        .map(labeller)
        .flatMap(filters.junitFilter[F](filter).apply(_))
    } else onlySuite.map(labeller)

    childless.map(CompleteLabeller[F])
  }
}

object RunnerFunctions {
  def apply[F[_]](
      cls: Class[_ <: EffectfulSuite],
      effectsCache: EffectsCache[F],
      filter: Filter = Filter.ALL,
      trimStackTraces: Boolean = true,
      allowOnly: Boolean = true,
      allowFlaky: Boolean = false
  )(implicit F: Sync[F]): RunnerFunctions[F] =
    new RunnerFunctions[F](
      cls,
      effectsCache,
      filter,
      trimStackTraces = trimStackTraces,
      allowOnly = allowOnly,
      allowFlaky = allowFlaky
    )
}
