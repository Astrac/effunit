package astrac.effunit

import munit.internal.junitinterface.{CustomFingerprint, CustomRunners}
import sbt.testing.{Fingerprint, SubclassFingerprint}

class Framework extends munit.internal.junitinterface.JUnitFramework {
  val munitFingerprint: CustomFingerprint = CustomFingerprint.of(
    "astrac.effunit.EffectfulSuite",
    "astrac.effunit.Runner"
  )

  override val name = "munitEffect"

  override val fingerprints: Array[Fingerprint] = Array(
    munitFingerprint,
    new SubclassFingerprint {
      def isModule(): Boolean = true
      def superclassName(): String = "astrac.effunit.EffectfulSuite"
      // $COVERAGE-OFF$
      def requireNoArgConstructor(): Boolean = true
      // $COVERAGE-ON$
    }
  )

  override val customRunners: CustomRunners =
    CustomRunners.of(munitFingerprint)
}
