package com.wavesenterprise.state.diffs

import com.wavesenterprise.account.PrivateKeyAccount
import com.wavesenterprise.features.BlockchainFeature
import com.wavesenterprise.lagonaki.mocks.TestBlock.{create => block}
import com.wavesenterprise.settings.TestFunctionalitySettings
import com.wavesenterprise.state.{BinaryDataEntry, BooleanDataEntry, ByteStr, DataEntry, IntegerDataEntry}
import com.wavesenterprise.utils.EitherUtils.EitherExt
import com.wavesenterprise.transaction.{DataTransactionV1, GenesisTransaction}
import com.wavesenterprise.{NoShrink, TransactionGen}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

class DataTransactionDiffTest extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers with TransactionGen with NoShrink {

  val fs = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(BlockchainFeature.DataTransaction.id -> 0))

  val baseSetup: Gen[(GenesisTransaction, PrivateKeyAccount, Long)] = for {
    master <- accountGen
    ts     <- positiveLongGen
    genesis: GenesisTransaction = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
  } yield (genesis, master, ts)

  def data(sender: PrivateKeyAccount, data: List[DataEntry[_]], fee: Long, timestamp: Long): DataTransactionV1 =
    DataTransactionV1.selfSigned(sender, sender, data, timestamp, fee).explicitGet()

  property("state invariants hold") {
    val setup = for {
      (genesis, master, ts) <- baseSetup

      key1   <- validAliasStringGen
      value1 <- positiveLongGen
      item1 = IntegerDataEntry(key1, value1)
      fee1 <- smallFeeGen
      dataTx1 = data(master, List(item1), fee1, ts + 10000)

      key2   <- validAliasStringGen
      value2 <- Arbitrary.arbitrary[Boolean]
      item2 = BooleanDataEntry(key2, value2)
      fee2 <- smallFeeGen
      dataTx2 = data(master, List(item2), fee2, ts + 20000)

      value3 <- positiveLongGen
      item3 = IntegerDataEntry(key1, value3)
      fee3 <- smallFeeGen
      dataTx3 = data(master, List(item3), fee3, ts + 30000)
    } yield (genesis, Seq(item1, item2, item3), Seq(dataTx1, dataTx2, dataTx3))

    forAll(setup) {
      case (genesisTx, items, txs) =>
        val sender  = txs.head.sender
        val genesis = block(Seq(genesisTx))
        val blocks  = txs.map(tx => block(Seq(tx)))

        val item1 = items.head
        assertDiffAndState(Seq(genesis), blocks(0), fs) {
          case (totalDiff, state) =>
            assertBalanceInvariant(totalDiff)
            state.addressBalance(sender.toAddress) shouldBe (ENOUGH_AMT - txs(0).fee)
            state.accountData(sender.toAddress, item1.key) shouldBe Some(item1)
            state.accountData(sender.toAddress).data.get(item1.key) shouldBe Some(item1)
        }

        val item2 = items(1)
        assertDiffAndState(Seq(genesis, blocks(0)), blocks(1), fs) {
          case (totalDiff, state) =>
            assertBalanceInvariant(totalDiff)
            state.addressBalance(sender.toAddress) shouldBe (ENOUGH_AMT - txs.take(2).map(_.fee).sum)
            state.accountData(sender.toAddress, item1.key) shouldBe Some(item1)
            state.accountData(sender.toAddress).data.get(item1.key) shouldBe Some(item1)
            state.accountData(sender.toAddress, item2.key) shouldBe Some(item2)
            state.accountData(sender.toAddress).data.get(item2.key) shouldBe Some(item2)
        }

        val item3 = items(2)
        assertDiffAndState(Seq(genesis, blocks(0), blocks(1)), blocks(2), fs) {
          case (totalDiff, state) =>
            assertBalanceInvariant(totalDiff)
            state.addressBalance(sender.toAddress) shouldBe (ENOUGH_AMT - txs.map(_.fee).sum)
            state.accountData(sender.toAddress, item1.key) shouldBe Some(item3)
            state.accountData(sender.toAddress).data.get(item1.key) shouldBe Some(item3)
            state.accountData(sender.toAddress, item2.key) shouldBe Some(item2)
            state.accountData(sender.toAddress).data.get(item2.key) shouldBe Some(item2)
        }
    }
  }

  property("cannot overspend funds") {
    val setup = for {
      (genesis, master, ts) <- baseSetup
      key                   <- validAliasStringGen
      value                 <- bytes64gen
      feeOverhead           <- Gen.choose[Long](1, ENOUGH_AMT)
      dataTx = data(master, List(BinaryDataEntry(key, ByteStr(value))), ENOUGH_AMT + feeOverhead, ts + 10000)
    } yield (genesis, dataTx)

    forAll(setup) {
      case (genesis, dataTx) =>
        assertDiffEither(Seq(block(Seq(genesis))), block(Seq(dataTx)), fs) { blockDiffEi =>
          blockDiffEi should produce("negative WEST balance")
        }
    }
  }

  property("validation fails prior to feature activation") {
    val setup = for {
      (genesis, master, ts) <- baseSetup
      fee                   <- smallFeeGen
      dataTx = data(master, List(), fee, ts + 10000)
    } yield (genesis, dataTx)
    val settings = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(BlockchainFeature.DataTransaction.id -> 10))

    forAll(setup) {
      case (genesis, data) =>
        assertDiffEither(Seq(block(Seq(genesis))), block(Seq(data)), settings) { blockDiffEi =>
          blockDiffEi should produce("has not been activated")
        }
    }
  }
}
