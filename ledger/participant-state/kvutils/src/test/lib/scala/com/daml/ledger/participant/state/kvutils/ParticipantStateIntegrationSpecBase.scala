// Copyright (c) 2020 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.ledger.participant.state.kvutils

import java.io.File
import java.time.Duration
import java.util.UUID
import java.util.concurrent.TimeUnit

import akka.stream.scaladsl.Sink
import com.daml.ledger.participant.state.kvutils.ParticipantStateIntegrationSpecBase._
import com.daml.ledger.participant.state.v1.Update._
import com.daml.ledger.participant.state.v1._
import com.digitalasset.daml.bazeltools.BazelRunfiles._
import com.digitalasset.daml.lf.archive.DarReader
import com.digitalasset.daml.lf.data.Ref.LedgerString
import com.digitalasset.daml.lf.data.Time.Timestamp
import com.digitalasset.daml.lf.data.{ImmArray, InsertOrdSet, Ref}
import com.digitalasset.daml.lf.transaction.GenTransaction
import com.digitalasset.daml_lf_dev.DamlLf
import com.digitalasset.ledger.api.testing.utils.AkkaBeforeAndAfterAll
import com.digitalasset.resources.{Resource, ResourceOwner}
import org.scalatest.Inside._
import org.scalatest.Matchers._
import org.scalatest.{Assertion, AsyncWordSpec, BeforeAndAfterEach}

import scala.collection.immutable.HashMap
import scala.compat.java8.FutureConverters._
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Try

//noinspection DuplicatedCode
abstract class ParticipantStateIntegrationSpecBase(implementationName: String)
    extends AsyncWordSpec
    with BeforeAndAfterEach
    with AkkaBeforeAndAfterAll {

  private implicit val ec: ExecutionContext = ExecutionContext.global

  private var ledgerId: LedgerString = _
  private var participantStateResource: Resource[ParticipantState] = _
  private var ps: ParticipantState = _
  private var rt: Timestamp = _

  val startIndex: Long = 0

  def participantStateFactory(
      participantId: ParticipantId,
      ledgerId: LedgerString,
  ): ResourceOwner[ParticipantState]

  def currentRecordTime(): Timestamp

  override protected def beforeEach(): Unit = {
    super.beforeEach()
    ledgerId = Ref.LedgerString.assertFromString(s"ledger-${UUID.randomUUID()}")
    participantStateResource = participantStateFactory(participantId, ledgerId).acquire()
    ps = Await.result(participantStateResource.asFuture, 10.seconds)
    rt = currentRecordTime()
  }

  override protected def afterEach(): Unit = {
    if (participantStateResource != null) {
      Await.result(participantStateResource.release(), 10.seconds)
    }
    super.afterEach()
  }

  // TODO(BH): Many of these tests for transformation from DamlLogEntry to Update better belong as
  // a KeyValueConsumptionSpec as the heart of the logic is there

  implementationName should {
    "return initial conditions" in {
      for {
        conditions <- ps
          .getLedgerInitialConditions()
          .runWith(Sink.head)
      } yield {
        conditions.ledgerId should be(ledgerId)
      }
    }

    "provide update after uploadPackages" in {
      val submissionId = randomLedgerString()
      for {
        _ <- ps.uploadPackages(submissionId, List(archives.head), sourceDescription).toScala
        (offset, update) <- ps.stateUpdates(beginAfter = None).runWith(Sink.head)
      } yield {
        offset should be(theOffset(0, 0))
        matchPackageUpload(update, rt, submissionId, List(archives.head))
      }
    }

    "provide two updates after uploadPackages with two archives" in {
      val submissionId = randomLedgerString()
      for {
        _ <- ps.uploadPackages(submissionId, archives, sourceDescription).toScala
        (offset, update) <- ps.stateUpdates(beginAfter = None).runWith(Sink.head)
      } yield {
        offset should be(theOffset(0, 0))
        matchPackageUpload(update, rt, submissionId, archives)
      }
    }

    "remove duplicate package from update after uploadPackages" in {
      val archive1 :: archive2 :: _ = archives
      val (subId1, subId2, subId3) =
        (randomLedgerString(), randomLedgerString(), randomLedgerString())

      for {
        _ <- ps.uploadPackages(subId1, List(archive1), sourceDescription).toScala
        _ <- ps.uploadPackages(subId2, List(archive1), sourceDescription).toScala
        _ <- ps.uploadPackages(subId3, List(archive2), sourceDescription).toScala
        Seq((offset1, update1), (offset2, update2), (offset3, update3)) <- ps
          .stateUpdates(beginAfter = None)
          .take(3)
          .runWith(Sink.seq)
      } yield {
        // first upload arrives as head update:
        offset1 should be(theOffset(0, 0))
        matchPackageUpload(update1, rt, subId1, List(archive1))
        offset2 should be(theOffset(1, 0))
        matchPackageUpload(update2, rt, subId2, List())
        offset3 should be(theOffset(2, 0))
        matchPackageUpload(update3, rt, subId3, List(archive2))
      }
    }

    "reject uploadPackages when archive is empty" in {

      val badArchive = DamlLf.Archive.newBuilder
        .setHash("asdf")
        .build

      val submissionId = randomLedgerString()

      for {
        _ <- ps.uploadPackages(submissionId, List(badArchive), sourceDescription).toScala
        (offset, update) <- ps
          .stateUpdates(beginAfter = None)
          .idleTimeout(DefaultIdleTimeout)
          .runWith(Sink.head)
      } yield {
        offset should be(theOffset(0, 0))
        inside(update) {
          case PublicPackageUploadRejected(actualSubmissionId, recordTime, _) =>
            actualSubmissionId should be(submissionId)
            recordTime should be >= rt
        }
      }
    }

    "reject duplicate submission in uploadPackage" in {
      val submissionIds = (randomLedgerString(), randomLedgerString())
      val archive1 :: archive2 :: _ = archives

      for {
        result1 <- ps.uploadPackages(submissionIds._1, List(archive1), sourceDescription).toScala
        result2 <- ps.uploadPackages(submissionIds._1, List(archive1), sourceDescription).toScala
        result3 <- ps.uploadPackages(submissionIds._2, List(archive2), sourceDescription).toScala
        results = Seq(result1, result2, result3)
        // second submission is a duplicate, it fails silently
        Seq(_, (offset2, update2)) <- ps
          .stateUpdates(beginAfter = None)
          .take(2)
          .runWith(Sink.seq)
      } yield {
        all(results) should be(SubmissionResult.Acknowledged)
        offset2 should be(theOffset(2, 0))
        inside(update2) {
          case PublicPackageUpload(_, _, _, Some(submissionId)) =>
            submissionId should be(submissionIds._2)
        }
      }
    }

    "provide update after allocateParty" in {
      val partyHint = Ref.Party.assertFromString("Alice")
      val displayName = "Alice Cooper"

      for {
        result <- ps
          .allocateParty(Some(partyHint), Some(displayName), randomLedgerString())
          .toScala
        (offset, update) <- ps.stateUpdates(beginAfter = None).runWith(Sink.head)
      } yield {
        result should be(SubmissionResult.Acknowledged)
        offset should be(theOffset(0, 0))
        inside(update) {
          case PartyAddedToParticipant(
              party,
              actualDisplayName,
              actualParticipantId,
              recordTime,
              _,
              ) =>
            party should be(partyHint)
            actualDisplayName should be(displayName)
            actualParticipantId should be(participantId)
            recordTime should be >= rt
        }
      }
    }

    "accept allocateParty when hint is empty" in {
      val displayName = "Alice Cooper"

      for {
        result <- ps.allocateParty(hint = None, Some(displayName), randomLedgerString()).toScala
        (offset, update) <- ps.stateUpdates(beginAfter = None).runWith(Sink.head)
      } yield {
        result should be(SubmissionResult.Acknowledged)
        offset should be(theOffset(0, 0))
        inside(update) {
          case PartyAddedToParticipant(
              party,
              actualDisplayName,
              actualParticipantId,
              recordTime,
              _,
              ) =>
            party should not be empty
            actualDisplayName should be(displayName)
            actualParticipantId should be(participantId)
            recordTime should be >= rt
        }
      }
    }

    "reject duplicate submission in allocateParty" in {
      val hints =
        (Some(Ref.Party.assertFromString("Alice")), Some(Ref.Party.assertFromString("Bob")))
      val displayNames = ("Alice Cooper", "Bob de Boumaa")

      val submissionIds = (randomLedgerString(), randomLedgerString())

      for {
        result1 <- ps.allocateParty(hints._1, Some(displayNames._1), submissionIds._1).toScala
        result2 <- ps.allocateParty(hints._2, Some(displayNames._2), submissionIds._1).toScala
        result3 <- ps.allocateParty(hints._2, Some(displayNames._2), submissionIds._2).toScala
        results = Seq(result1, result2, result3)
        // second submission is a duplicate, it fails silently
        Seq(_, (offset2, update2)) <- ps
          .stateUpdates(beginAfter = None)
          .take(2)
          .runWith(Sink.seq)
      } yield {
        all(results) should be(SubmissionResult.Acknowledged)
        offset2 should be(theOffset(2, 0))
        inside(update2) {
          case PartyAddedToParticipant(_, displayName, _, _, Some(submissionId)) =>
            displayName should be(displayNames._2)
            submissionId should be(submissionIds._2)
        }
      }
    }

    "reject duplicate party in allocateParty" in {
      val hint = Some(Ref.Party.assertFromString("Alice"))
      val displayName = Some("Alice Cooper")

      for {
        result1 <- ps.allocateParty(hint, displayName, randomLedgerString()).toScala
        result2 <- ps.allocateParty(hint, displayName, randomLedgerString()).toScala
        results = Seq(result1, result2)
        Seq(_, (offset2, update2)) <- ps
          .stateUpdates(beginAfter = None)
          .take(2)
          .runWith(Sink.seq)
      } yield {
        all(results) should be(SubmissionResult.Acknowledged)
        offset2 should be(theOffset(1, 0))
        inside(update2) {
          case PartyAllocationRejected(_, _, _, rejectionReason) =>
            rejectionReason should be("Party already exists")
        }
      }
    }

    "provide update after transaction submission" in {
      for {
        _ <- ps.allocateParty(hint = Some(alice), None, randomLedgerString()).toScala
        _ <- ps
          .submitTransaction(submitterInfo(rt, alice), transactionMeta(rt), emptyTransaction)
          .toScala
        (offset, _) <- ps.stateUpdates(beginAfter = None).drop(1).runWith(Sink.head)
      } yield {
        offset should be(theOffset(1, 0))
      }
    }

    "reject duplicate commands" in {
      val commandIds = ("X1", "X2")

      for {
        _ <- ps.allocateParty(hint = Some(alice), None, randomLedgerString()).toScala
        _ <- ps
          .submitTransaction(
            submitterInfo(rt, alice, commandIds._1),
            transactionMeta(rt),
            emptyTransaction,
          )
          .toScala
        _ <- ps
          .submitTransaction(
            submitterInfo(rt, alice, commandIds._1),
            transactionMeta(rt),
            emptyTransaction,
          )
          .toScala
        _ <- ps
          .submitTransaction(
            submitterInfo(rt, alice, commandIds._2),
            transactionMeta(rt),
            emptyTransaction,
          )
          .toScala
        Seq((offset1, update1), (offset2, update2), (offset3, update3)) <- ps
          .stateUpdates(beginAfter = None)
          .take(3)
          .runWith(Sink.seq)
      } yield {
        offset1 should be(theOffset(0, 0))
        update1 should be(a[PartyAddedToParticipant])
        offset2 should be(theOffset(1, 0))
        matchTransaction(update2, rt, commandIds._1)
        offset3 should be(theOffset(3, 0))
        matchTransaction(update3, rt, commandIds._2)
      }
    }

    "return second update with beginAfter=0" in {
      for {
        _ <- ps
          .allocateParty(hint = Some(alice), None, randomLedgerString())
          .toScala // offset now at [1,0]
        _ <- ps
          .submitTransaction(submitterInfo(rt, alice, "X1"), transactionMeta(rt), emptyTransaction)
          .toScala
        _ <- ps
          .submitTransaction(submitterInfo(rt, alice, "X2"), transactionMeta(rt), emptyTransaction)
          .toScala
        (offset, update) <- ps
          .stateUpdates(beginAfter = Some(theOffset(1, 0)))
          .runWith(Sink.head)
      } yield {
        offset should be(theOffset(2, 0))
        update should be(a[TransactionAccepted])
      }
    }

    "correctly implements tx submission authorization" in {

      val unallocatedParty = Ref.Party.assertFromString("nobody")

      for {
        lic <- ps.getLedgerInitialConditions().runWith(Sink.head)

        _ <- ps
          .submitConfiguration(
            maxRecordTime = rt.addMicros(1000000),
            submissionId = randomLedgerString(),
            config = lic.config.copy(
              generation = lic.config.generation + 1,
            ),
          )
          .toScala

        // Submit without allocation
        _ <- ps
          .submitTransaction(
            submitterInfo(rt, unallocatedParty),
            transactionMeta(rt),
            emptyTransaction,
          )
          .toScala

        // Allocate a party and try the submission again with an allocated party.
        result <- ps
          .allocateParty(
            None /* no name hint, implementation decides party name */,
            Some("Somebody"),
            randomLedgerString(),
          )
          .toScala
        _ = result should be(a[SubmissionResult])

        //get the new party off state updates
        newParty <- ps
          .stateUpdates(beginAfter = Some(theOffset(1, 0)))
          .runWith(Sink.head)
          .map(_._2.asInstanceOf[PartyAddedToParticipant].party)
        _ <- ps
          .submitTransaction(
            submitterInfo(rt, party = newParty),
            transactionMeta(rt),
            emptyTransaction,
          )
          .toScala

        Seq((offset1, update1), (offset2, update2), (offset3, update3), (offset4, update4)) <- ps
          .stateUpdates(beginAfter = None)
          .take(4)
          .runWith(Sink.seq)

      } yield {
        offset1 should be(theOffset(0, 0))
        update1 should be(a[ConfigurationChanged])

        offset2 should be(theOffset(1, 0))
        inside(update2) {
          case CommandRejected(_, _, reason) =>
            reason should be(RejectionReason.PartyNotKnownOnLedger)
        }

        offset3 should be(theOffset(2, 0))
        update3 should be(a[PartyAddedToParticipant])

        offset4 should be(theOffset(3, 0))
        update4 should be(a[TransactionAccepted])
      }
    }

    "allow an administrator to submit new configuration" in {
      for {
        lic <- ps.getLedgerInitialConditions().runWith(Sink.head)

        // Submit an initial configuration change
        _ <- ps
          .submitConfiguration(
            maxRecordTime = rt.addMicros(1000000),
            submissionId = randomLedgerString(),
            config = lic.config.copy(
              generation = lic.config.generation + 1,
            ),
          )
          .toScala

        // Submit another configuration change that uses stale "current config".
        _ <- ps
          .submitConfiguration(
            maxRecordTime = rt.addMicros(1000000),
            submissionId = randomLedgerString(),
            config = lic.config.copy(
              generation = lic.config.generation + 1,
              timeModel = TimeModel(
                Duration.ofSeconds(123),
                Duration.ofSeconds(123),
                Duration.ofSeconds(123),
              ).get,
            ),
          )
          .toScala

        Seq((_, update1), (_, update2)) <- ps.stateUpdates(None).take(2).runWith(Sink.seq)
      } yield {
        // The first submission should change the config.
        inside(update1) {
          case ConfigurationChanged(_, _, _, newConfiguration) =>
            newConfiguration should not be lic.config
        }

        // The second submission should get rejected.
        update2 should be(a[ConfigurationChangeRejected])
      }
    }

    "reject duplicate submission in new configuration" in {
      val submissionIds = (randomLedgerString(), randomLedgerString())

      for {

        lic <- ps.getLedgerInitialConditions().runWith(Sink.head)

        // Submit an initial configuration change
        result1 <- ps
          .submitConfiguration(
            maxRecordTime = rt.addMicros(1000000),
            submissionId = submissionIds._1,
            config = lic.config.copy(
              generation = lic.config.generation + 1,
            ),
          )
          .toScala
        result2 <- ps
          .submitConfiguration(
            maxRecordTime = rt.addMicros(2000000),
            submissionId = submissionIds._1,
            config = lic.config.copy(
              generation = lic.config.generation + 2,
            ),
          )
          .toScala
        result3 <- ps
          .submitConfiguration(
            maxRecordTime = rt.addMicros(2000000),
            submissionId = submissionIds._2,
            config = lic.config.copy(
              generation = lic.config.generation + 2,
            ),
          )
          .toScala
        results = Seq(result1, result2, result3)
        // second submission is a duplicate, it fails silently
        Seq(_, (offset2, update2)) <- ps.stateUpdates(beginAfter = None).take(2).runWith(Sink.seq)
      } yield {
        all(results) should be(SubmissionResult.Acknowledged)
        offset2 should be(theOffset(2, 0))
        inside(update2) {
          case ConfigurationChanged(_, submissionId, _, _) =>
            submissionId should be(submissionIds._2)
        }
      }
    }

    "process commits serially" in {
      val partyCount = 1000L
      val partyIds = 1L to partyCount
      val partyIdDigits = partyCount.toString.length
      val partyNames =
        partyIds
          .map(i => Ref.Party.assertFromString(s"party-%0${partyIdDigits}d".format(i)))
          .toVector

      val updatesF = ps.stateUpdates(beginAfter = None).take(partyCount).runWith(Sink.seq)
      for {
        results <- Future.sequence(
          partyNames.map(name =>
            ps.allocateParty(Some(name), Some(name), randomLedgerString()).toScala),
        )
        updates <- updatesF
      } yield {
        all(results) should be(SubmissionResult.Acknowledged)

        val expectedOffsets = partyIds.map(i => theOffset(i - 1, 0)).toVector
        val actualOffsets = updates.map(_._1).sorted.toVector
        actualOffsets should be(expectedOffsets)

        val actualNames =
          updates.map(_._2.asInstanceOf[PartyAddedToParticipant].displayName).sorted.toVector
        actualNames should be(partyNames)
      }
    }
  }

  private def theOffset(first: Long, rest: Long*): Offset =
    Offset(Array(first + startIndex, rest: _*))
}

object ParticipantStateIntegrationSpecBase {
  type ParticipantState = ReadService with WriteService

  private val DefaultIdleTimeout = FiniteDuration(5, TimeUnit.SECONDS)
  private val emptyTransaction: SubmittedTransaction =
    GenTransaction(HashMap.empty, ImmArray.empty, Some(InsertOrdSet.empty))

  private val participantId: ParticipantId =
    Ref.LedgerString.assertFromString("in-memory-participant")
  private val sourceDescription = Some("provided by test")

  private val darReader = DarReader { case (_, is) => Try(DamlLf.Archive.parseFrom(is)) }
  private val archives =
    darReader.readArchiveFromFile(new File(rlocation("ledger/test-common/Test-stable.dar"))).get.all

  private val alice = Ref.Party.assertFromString("alice")

  private def randomLedgerString(): Ref.LedgerString =
    Ref.LedgerString.assertFromString(UUID.randomUUID().toString)

  private def submitterInfo(rt: Timestamp, party: Ref.Party, commandId: String = "X") =
    SubmitterInfo(
      submitter = party,
      applicationId = Ref.LedgerString.assertFromString("tests"),
      commandId = Ref.LedgerString.assertFromString(commandId),
      maxRecordTime = rt.addMicros(Duration.ofSeconds(10).toNanos / 1000),
    )

  private def transactionMeta(let: Timestamp) = TransactionMeta(
    ledgerEffectiveTime = let,
    workflowId = Some(Ref.LedgerString.assertFromString("tests")),
  )

  private def matchPackageUpload(
      update: Update,
      expectedRecordTime: Timestamp,
      expectedSubmissionId: SubmissionId,
      expectedArchives: List[DamlLf.Archive],
  ): Assertion =
    inside(update) {
      case PublicPackageUpload(
          actualArchives,
          actualSourceDescription,
          actualRecordTime,
          Some(actualSubmissionId),
          ) =>
        actualRecordTime should be >= expectedRecordTime
        actualArchives.map(_.getHash).toSet should be(expectedArchives.map(_.getHash).toSet)
        actualSourceDescription should be(sourceDescription)
        actualSubmissionId should be(expectedSubmissionId)
    }

  private def matchTransaction(
      update: Update,
      expectedRecordTime: Timestamp,
      expectedCommandId: String,
  ): Assertion =
    inside(update) {
      case TransactionAccepted(
          Some(SubmitterInfo(_, _, actualCommandId, _)),
          _,
          _,
          _,
          actualRecordTime,
          _,
          ) =>
        actualCommandId should be(expectedCommandId)
        actualRecordTime should be >= expectedRecordTime
    }
}
