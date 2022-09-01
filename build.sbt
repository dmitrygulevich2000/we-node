import com.typesafe.sbt.git.JGit
import sbt.Tests.Group

import scala.jdk.CollectionConverters._

name := "waves-enterprise"

enablePlugins(GitVersioning)
disablePlugins(AssemblyPlugin)

Global / onChangedBuildSource := ReloadOnSourceChanges
Global / concurrentRestrictions := {
  val processorsCount = java.lang.Runtime.getRuntime.availableProcessors
  val parallelism     = Option(System.getenv("SBT_THREAD_NUMBER")).fold(processorsCount)(_.toInt)
  println(s"[info] sbt parallelism – $parallelism")

  Seq(
    Tags.limit(Tags.CPU, parallelism),
    Tags.limit(Tags.Test, parallelism),
    Tags.limitAll(parallelism)
  )
}

initialize := {
  val _        = initialize.value // run the previous initialization
  val required = 11
  val current  = sys.props("java.specification.version").toDouble
  assert(current >= required, s"Unsupported JDK version: required '$required' or above, but current is '$current'")
}

inThisBuild {
  Seq(
    scalaVersion := "2.12.10",
    organization := "com.wavesenterprise",
    crossPaths := false,
    scalafmtOnCompile := true,
    fork := true,
    Test / parallelExecution := true,
    testOptions += Tests.Argument("-oIDOF", "-u", "target/test-reports"),
    scalacOptions ++= Seq(
      "-feature",
      "-deprecation",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-Ywarn-unused:-implicits",
      "-Xlint",
      "-Yresolve-term-conflict:object",
      "-Ypartial-unification",
      "-language:postfixOps"
    )
  )
}

lazy val branchName = Def.setting[String](sys.env.getOrElse("CI_COMMIT_REF_NAME", git.gitCurrentBranch.value))

/**
  * The version is generated by the first possible method in the following order:
  *   Release version – {Tag}[-DIRTY]. When the tag corresponding to the version pattern is set on the last commit;
  *   Snapshot version – {Tag}-{Commits-ahead}-{Branch-name}-{Commit-hash}[-DIRTY]-SNAPSHOT. When the `git describe --tags` is worked;
  *   Fallback version – {Current-date}-SNAPSHOT.
  */
ThisBuild / version := {
  if (git.gitUncommittedChanges.value) {
    val changes = JGit(baseDirectory.value).porcelain
      .status()
      .call()
      .getUncommittedChanges
      .asScala
      .mkString("\n")

    println(s"Uncommitted changes detected:\n$changes")
  }

  val uncommittedChangesSuffix = git.makeUncommittedSignifierSuffix(git.gitUncommittedChanges.value, Some("DIRTY"))
  val snapshotSuffix           = "SNAPSHOT"

  val releaseVersion = git.releaseVersion(git.gitCurrentTags.value, git.gitTagToVersionNumber.value, uncommittedChangesSuffix)

  lazy val snapshotVersion = git.gitDescribedVersion.value.map { described =>
    val commitHashLength                          = 7
    val (tagVersionWithoutCommitHash, commitHash) = described.splitAt(described.length - commitHashLength)
    val tagVersionWithCommitsAhead                = tagVersionWithoutCommitHash.dropRight(2)
    val branchSuffix                              = branchName.value
    s"$tagVersionWithCommitsAhead-$branchSuffix-$commitHash$uncommittedChangesSuffix-$snapshotSuffix"
  }

  lazy val fallbackVersion = s"${git.formattedDateVersion.value}-$snapshotSuffix"

  (releaseVersion orElse snapshotVersion) getOrElse fallbackVersion
}

/**
  * You have to put your credentials in a local file ~/.sbt/.credentials
  *
  * File structure:
  * realm=Sonatype Nexus Repository Manager
  * host=artifacts.wavesenterprise.com
  * username={YOUR_LDAP_USERNAME}
  * password={YOUR_LDAP_PASSWORD}
  */
ThisBuild / credentials += {
  val envUsernameOpt = sys.env.get("nexusUser")
  val envPasswordOpt = sys.env.get("nexusPassword")

  (envUsernameOpt, envPasswordOpt) match {
    case (Some(username), Some(password)) =>
      println("Using credentials from environment for artifacts.wavesenterprise.com")
      Credentials("Sonatype Nexus Repository Manager", "artifacts.wavesenterprise.com", username, password)

    case _ =>
      val localCredentialsFile = Path.userHome / ".sbt" / ".credentials"
      println(s"Going to use ${localCredentialsFile.getAbsolutePath} as credentials for artifacts.wavesenterprise.com")
      Credentials(localCredentialsFile)
  }
}

lazy val testGroupingConfig: Def.Setting[_] =
  testGrouping := {
    val javaHomeValue    = javaHome.value
    val envVarsValue     = envVars.value
    val javaOptionsValue = javaOptions.value
    for {
      group <- testGrouping.value
      test  <- group.tests
    } yield {
      Group(
        name = test.name,
        tests = Seq(test),
        runPolicy = Tests.SubProcess {
          ForkOptions(
            javaHome = javaHomeValue,
            outputStrategy = outputStrategy.value,
            bootJars = Vector.empty[java.io.File],
            workingDirectory = Option(baseDirectory.value),
            runJVMOptions = javaOptionsValue.toVector,
            connectInput = false,
            envVars = envVarsValue
          )
        }
      )
    }
  }

lazy val testParallelizationSettings: Seq[Def.Setting[_]] =
  inConfig(Test) {
    inTask(test)(testGroupingConfig) ++
      inTask(testOnly)(testGroupingConfig)
  }

/* ********************************************************* */

lazy val node = (project in file("node"))
  .settings(testParallelizationSettings)

lazy val generator = (project in file("generator"))
  .dependsOn(node)
  .dependsOn(node % "test->test")

lazy val transactionsSigner = (project in file("transactions-signer"))
  .dependsOn(node)
