package astrac.effunit

import higherkindness.droste.data.prelude._
import java.lang.reflect.Modifier
import munit.internal.junitinterface.{Configurable, Settings}
import org.junit.runner.{Description, Runner => JUnitRunner}
import org.junit.runner.manipulation.{Filter, Filterable}
import org.junit.runner.notification.RunNotifier
import scala.annotation.nowarn

class Runner(
    cls: Class[_ <: EffectfulSuite],
    newInstance: () => EffectfulSuite
) extends JUnitRunner
    with Filterable
    with Configurable {

  @nowarn
  def this(cls: Class[_ <: EffectfulSuite]) =
    this(Runner.ensureEligibleConstructor(cls), () => cls.newInstance())

  @volatile private var filter: Filter = Filter.ALL
  @volatile private var settings: Settings = Settings.defaults()

  val suite: EffectfulSuite = newInstance()

  def isCI: Boolean = "true" == System.getenv("CI")
  def munitFlakyOK: Boolean = "true" == System.getenv("MUNIT_FLAKY_OK")

  lazy val functions = RunnerFunctions(
    cls,
    suite.effectsCache,
    filter,
    trimStackTraces = settings.trimStackTraces(),
    allowFlaky = munitFlakyOK,
    allowOnly = !isCI
  )(suite.Eff)

  // $COVERAGE-OFF$
  override def filter(filter: Filter): Unit =
    this.filter = filter
  // $COVERAGE-ON$

  override def configure(settings: Settings): Unit =
    this.settings = settings

  override lazy val getDescription: Description = {
    val suiteDescription = Description.createSuiteDescription(cls)
    filteredTrees.map(_.head.unwrap)
    suiteDescription
  }

  override def run(runNotifier: RunNotifier): Unit = {
    val notifier = JUnitNotifier[suite.Eff](runNotifier)(suite.Eff)

    runNotifier.fireTestSuiteStarted(getDescription)
    suite.Eff.toIO(functions.run(filteredTrees, notifier)).unsafeRunSync()
    runNotifier.fireTestSuiteFinished(getDescription)
  }

  private lazy val filteredTrees =
    functions.filteredTrees(suite.testTrees)
}

object Runner {
  private def ensureEligibleConstructor(
      cls: Class[_ <: EffectfulSuite]
  ): Class[_ <: EffectfulSuite] = {
    require(
      hasEligibleConstructor(cls),
      s"Class '${cls.getName}' is missing a public empty argument constructor"
    )
    cls
  }
  private def hasEligibleConstructor(
      cls: Class[_ <: EffectfulSuite]
  ): Boolean = {
    try {
      val constructor = cls.getConstructor(
        new Array[java.lang.Class[_]](0): _*
      )
      Modifier.isPublic(constructor.getModifiers)
    } catch {
      // $COVERAGE-OFF$
      case _: NoSuchMethodException => false
      // $COVERAGE-ON$
    }
  }
}
