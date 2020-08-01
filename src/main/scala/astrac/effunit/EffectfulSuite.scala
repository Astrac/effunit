package astrac.effunit

import cats.effect.Effect
import munit.Assertions
import org.junit.runner.RunWith

import mutable.{EffectsCache, MutableBuilder}
import tree.TestTree

@RunWith(classOf[astrac.effunit.Runner])
trait EffectfulSuite extends Assertions {

  type Eff[_]

  implicit def Eff: Effect[Eff]

  def effectsCache: EffectsCache[Eff]
  def testTrees: List[TestTree.Fixed[Eff]]
}

abstract class MutableEffectfulSuite[F[_]](implicit val Eff: Effect[F])
    extends EffectfulSuite
    with MutableBuilder[F] {
  override final type Eff[A] = F[A]
}
