package astrac.effunit

import tree.TestTree

case class FlakyFailureException[F[_]](test: TestTree[F, _], cause: Throwable)
    extends Exception(
      s"A test marked as flaky failed: ${test.options.name}",
      cause
    )

case class TestIgnoredException[F[_]](test: TestTree[F, _])
    extends Exception(s"This test was ignored: ${test.options.name}")

case class DependencyFailedException[F[_]](t: TestTree[F, _], cause: Throwable)
    extends Exception(
      s"Failed dependency: ${t.options.name}" +
        cause
    )
