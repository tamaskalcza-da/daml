// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.testing.postgresql

import java.io.StringWriter
import java.net.InetAddress
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.UUID
import java.util.concurrent.atomic.AtomicBoolean

import com.daml.testing.postgresql.PostgresAround._
import org.apache.commons.io.{FileUtils, IOUtils}
import org.slf4j.LoggerFactory

import scala.collection.JavaConverters.asScalaBufferConverter
import scala.util.control.NonFatal

trait PostgresAround {
  @volatile
  private var fixture: PostgresFixture = _

  private val started: AtomicBoolean = new AtomicBoolean(false)

  protected def startEphemeralPostgres(): Unit = {
    logger.info("Starting an ephemeral PostgreSQL instance...")
    val tempDir = Files.createTempDirectory("postgres_test")
    val dataDir = tempDir.resolve("data")
    val confFile = dataDir.resolve("postgresql.conf")
    val logFile = Files.createFile(tempDir.resolve("postgresql.log"))
    val lockedPort = FreePort.find()
    val port = lockedPort.port
    fixture = PostgresFixture(port, tempDir, dataDir, confFile, logFile)

    try {
      initializeDatabase()
      createConfigFile()
      startPostgres()
      lockedPort.unlock()
      logger.info(s"PostgreSQL has started on port $port.")
    } catch {
      case NonFatal(e) =>
        lockedPort.unlock()
        stopPostgres()
        deleteRecursively(tempDir)
        fixture = null
        throw e
    }
  }

  protected def stopAndCleanUpPostgres(): Unit = {
    logger.info("Stopping and cleaning up PostgreSQL...")
    stopPostgres()
    deleteRecursively(fixture.tempDir)
    logger.info("PostgreSQL has stopped, and the data directory has been deleted.")
    fixture = null
  }

  protected def startPostgres(): Unit = {
    if (!started.compareAndSet(false, true)) {
      throw new IllegalStateException(
        "Attempted to start PostgreSQL, but it has already been started.",
      )
    }
    try {
      run(
        "start PostgreSQL",
        Tool.pg_ctl,
        "-w",
        "-D",
        fixture.dataDir.toString,
        "-l",
        fixture.logFile.toString,
        "start",
      )
    } catch {
      case NonFatal(e) =>
        logger.error("Starting PostgreSQL failed.", e)
        started.set(false)
        throw e
    }
  }

  protected def stopPostgres(): Unit = {
    if (started.compareAndSet(true, false)) {
      logger.info("Stopping PostgreSQL...")
      run(
        "stop PostgreSQL",
        Tool.pg_ctl,
        "-w",
        "-D",
        fixture.dataDir.toString,
        "-m",
        "immediate",
        "stop",
      )
      logger.info("PostgreSQL has stopped.")
    }
  }

  protected def createNewRandomDatabase(): PostgresDatabase =
    createNewDatabase(UUID.randomUUID().toString)

  protected def createNewDatabase(name: String): PostgresDatabase = {
    val database = PostgresDatabase(hostName, fixture.port, userName, name)
    createDatabase(database)
    database
  }

  private def initializeDatabase(): Unit = run(
    "initialize the PostgreSQL database",
    Tool.initdb,
    s"--username=$userName",
    if (isWindows) "--locale=English_United States" else "--locale=en_US.UTF-8",
    "-E",
    "UNICODE",
    "-A",
    "trust",
    fixture.dataDir.toString.replaceAllLiterally("\\", "/"),
  )

  private def createConfigFile(): Unit = {
    // taken from here: https://bitbucket.org/eradman/ephemeralpg/src/1b5a3c6be81c69a860b7bd540a16b1249d3e50e2/pg_tmp.sh?at=default&fileviewer=file-view-default#pg_tmp.sh-54
    // We set unix_socket_directories to /tmp rather than tempDir
    // since the latter will refer to a temporary directory set by
    // Bazel which is too long (there is a limit on the length of unix domain
    // sockets). On Windows, unix domain sockets do not exist and
    // this option is ignored.
    val configText =
      s"""|unix_socket_directories = '/tmp'
          |shared_buffers = 12MB
          |fsync = off
          |synchronous_commit = off
          |full_page_writes = off
          |log_min_duration_statement = 0
          |log_connections = on
          |listen_addresses = '$hostName'
          |port = ${fixture.port}
          """.stripMargin
    Files.write(fixture.confFile, configText.getBytes(StandardCharsets.UTF_8))
    ()
  }

  private def createDatabase(database: PostgresDatabase): Unit = run(
    "create the database",
    Tool.createdb,
    "--host",
    database.hostName,
    "--port",
    database.port.toString,
    "--username",
    database.userName,
    database.databaseName,
  )

  protected def dropDatabase(database: PostgresDatabase): Unit = run(
    "drop a database",
    Tool.dropdb,
    "--host",
    database.hostName,
    "--port",
    database.port.toString,
    "--username",
    database.userName,
    database.databaseName,
  )

  private def run(description: String, tool: Tool, args: String*): Unit = {
    val command = tool.path.toString +: args
    logger.debug(s"Running: ${command.mkString(" ")}")
    try {
      val process = Runtime.getRuntime.exec(command.toArray)
      if (process.waitFor() != 0) {
        val stdout = new StringWriter
        IOUtils.copy(process.getInputStream, stdout, StandardCharsets.UTF_8)
        val stderr = new StringWriter
        IOUtils.copy(process.getErrorStream, stderr, StandardCharsets.UTF_8)
        val logs = Files.readAllLines(fixture.logFile).asScala
        throw new ProcessFailedException(
          description = description,
          command = command,
          stdout = stdout.toString,
          stderr = stderr.toString,
          logs = logs,
        )
      }
    } catch {
      case e: ProcessFailedException =>
        throw e
      case NonFatal(e) =>
        val logs = Files.readAllLines(fixture.logFile).asScala
        throw new ProcessFailedException(
          description = description,
          command = command,
          logs = logs,
          cause = e,
        )
    }
  }

  private def deleteRecursively(tempDir: Path): Unit =
    FileUtils.deleteDirectory(tempDir.toFile)
}

object PostgresAround {
  private val logger = LoggerFactory.getLogger(getClass)

  private val hostName = InetAddress.getLoopbackAddress.getHostName

  val userName = "test"
  val password = ""

  private class ProcessFailedException(
      description: String,
      command: Seq[String],
      stdout: String = "<none>",
      stderr: String = "<none>",
      logs: Seq[String] = Seq.empty,
      cause: Throwable = null,
  ) extends RuntimeException(
        Seq(
          s"Failed to $description.",
          s"Command:",
          command.mkString("\n"),
          s"\nSTDOUT:\n$stdout",
          s"\nSTDERR:\n$stderr",
          if (logs.isEmpty) "\nLogs: <none>" else s"\nLogs:\n${logs.mkString("\n")}",
        ).mkString("\n"),
        cause,
      )
}
