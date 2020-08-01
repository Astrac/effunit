package astrac.effunit

import cats.effect.{IO, Resource}
import munit.FunSuite

import mutable.MutableBuilder
import utils._
import IntDb._

class EvaluatorSpec extends FunSuite {

  val cls = classOf[EffectfulSuite]
  val testException = new RuntimeException("Test failure")

  def testDescription(indent: Int, name: String) =
    s"${("| " * indent)}$name(${cls.getName()})"

  def makeTree() = {
    val intDb = new IntDb
    val connection: Resource[IO, Connection] =
      Resource.make(IO.delay(intDb.connect()))(c => IO.delay(intDb.close(c)))

    val notifier = new BufferingNotifier[IO]
    val b = MutableBuilder[IO]
    val functions = RunnerFunctions[IO](cls, b.effectsCache, allowFlaky = true)

    b.test("set(foo, 0)") {
      connection.use(c => IO.delay(intDb.set(c, "foo", 0)))
    }

    b.test("set(fox, 10)") {
      connection.use(c => IO.delay(intDb.set(c, "fox", 10)))
    }

    b.test("flaky".flaky) {
      throw testException
    }

    b.sharingResource("external-connection", connection) { external =>
      b.test("set(bar, 1)") {
        external.get.map(c => intDb.set(c, "bar", 1)).void
      }

      b.test("get(bar)") {
        external.get.map(c => assertEquals(intDb.get(c, "bar"), Some(1)))
      }

      b.test("get(bar).fail".fail) {
        external.get.map(c => assertEquals(intDb.get(c, "bar"), None))
      }

      b.sharingResource("internal-connection", connection) { internal =>
        b.test("set(baz, 2)") {
          internal.get.map(c => intDb.set(c, "baz", 2))
        }

        b.test("get(baz)[FAIL]") {
          throw testException
        }

        b.test("get(baz).ignore".ignore) {
          fail("This should not run")
        }

        b.test("ext.get(bar)") {
          external.get.map(c => assertEquals(intDb.get(c, "bar"), Some(1)))
        }
      }
    }

    (functions, b, intDb, notifier)
  }

  test("evaluationExample") {
    val (functions, builder, intDb, notifier) = makeTree()

    functions
      .run(functions.filteredTrees(builder.testTrees), notifier)
      .unsafeRunSync()

    assertEquals(
      intDb.internalMap.toMap,
      Map("foo" -> 0, "fox" -> 10, "bar" -> 1, "baz" -> 2)
    )

    assertEquals(
      intDb.commands.toMap,
      Map(
        Connection(0) -> Vector(DbSet("foo", 0)),
        Connection(1) -> Vector(DbSet("fox", 10)),
        Connection(2) -> Vector(
          DbSet("bar", 1),
          DbGet("bar"),
          DbGet("bar"),
          DbGet("bar")
        ),
        Connection(3) -> Vector(DbSet("baz", 2))
      )
    )

    assert(intDb.connections.isEmpty)

    assertEquals(
      notifier.notifications,
      List(
        TestStarted(testDescription(0, "set(foo, 0)")),
        TestFinished(testDescription(0, "set(foo, 0)")),
        TestStarted(testDescription(0, "set(fox, 10)")),
        TestFinished(testDescription(0, "set(fox, 10)")),
        TestStarted(testDescription(0, "flaky")),
        TestAssumptionFailed(testDescription(0, "flaky"), testException),
        TestFinished(testDescription(0, "flaky")),
        ResourceAcquisitionStart(testDescription(0, "external-connection")),
        ResourceAcquired(testDescription(0, "external-connection")),
        TestStarted(testDescription(1, "set(bar, 1)")),
        TestFinished(testDescription(1, "set(bar, 1)")),
        TestStarted(testDescription(1, "get(bar)")),
        TestFinished(testDescription(1, "get(bar)")),
        TestStarted(testDescription(1, "get(bar).fail")),
        TestFinished(testDescription(1, "get(bar).fail")),
        ResourceAcquisitionStart(testDescription(1, "internal-connection")),
        ResourceAcquired(testDescription(1, "internal-connection")),
        TestStarted(testDescription(2, "set(baz, 2)")),
        TestFinished(testDescription(2, "set(baz, 2)")),
        TestStarted(testDescription(2, "get(baz)[FAIL]")),
        TestFailure(testDescription(2, "get(baz)[FAIL]"), testException),
        TestFinished(testDescription(2, "get(baz)[FAIL]")),
        TestStarted(testDescription(2, "get(baz).ignore")),
        TestIgnored(testDescription(2, "get(baz).ignore")),
        TestFinished(testDescription(2, "get(baz).ignore")),
        TestStarted(testDescription(2, "ext.get(bar)")),
        TestFinished(testDescription(2, "ext.get(bar)")),
        ResourceReleased(testDescription(1, "internal-connection")),
        ResourceReleased(testDescription(0, "external-connection"))
      )
    )
  }

}
