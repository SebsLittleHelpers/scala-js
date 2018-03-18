package build

import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
  )

  val Tools = Seq(
      // private[linker], not an issue
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.linker.StandardLinker#Config.outputMode"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.linker.StandardLinker#Config.withOutputMode")
  )

  val JSEnvs = Seq(
  )

  val JSEnvsTestKit = Seq(
  )

  val SbtPlugin = Seq(
  )

  val TestCommon = Seq(
  )

  val TestAdapter = TestCommon ++ Seq(
  )

  val CLI = Seq(
  )

  val Library = Seq(
  )

  val TestInterface = TestCommon ++ Seq(
  )
}
