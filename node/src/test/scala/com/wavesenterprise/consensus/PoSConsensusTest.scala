package com.wavesenterprise.consensus

import cats.data.NonEmptyList
import cats.implicits._
import com.wavesenterprise.account.PrivateKeyAccount
import com.wavesenterprise.CryptoHelpers

import scala.util.Random
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

class PoSConsensusTest extends AnyPropSpec with Matchers {

  case class Block(height: Int, baseTarget: Long, miner: PrivateKeyAccount, timestamp: Long, delay: Long)

  def generationSignature: Array[Byte] = {
    val arr = new Array[Byte](32)
    Random.nextBytes(arr)
    arr
  }

  def minerAccount: PrivateKeyAccount = CryptoHelpers.generatePrivateKey

  val balance           = 50000000L * 100000000L
  val blockDelaySeconds = 60
  val defaultBaseTarget = 100L

  ignore("Correct consensus parameters distribution of blocks generated with FairPoS") {

    val miners = mkMiners
    val first  = Block(0, defaultBaseTarget, minerAccount, System.currentTimeMillis(), 0)

    val chain = (1 to 100000 foldLeft NonEmptyList.of(first))((acc, _) => {
      val gg     = acc.tail.lift(1)
      val blocks = miners.map(mineBlock(acc.head, gg, _))

      val next = blocks.minBy(_.delay)

      next :: acc
    }).reverse.tail

    val avgBT    = chain.map(_.baseTarget).sum / chain.length
    val avgDelay = chain.tail.map(_.delay).sum / (chain.length - 1)

    val minersPerformance = calcPerfomance(chain, miners)

    assert(minersPerformance.forall(p => p._2 < 1.1 && p._2 > 0.9))
    assert(avgDelay < 80000 && avgDelay > 40000)
    assert(avgBT < 200 && avgBT > 20)
  }

  def mineBlock(prev: Block, grand: Option[Block], minerWithBalance: (PrivateKeyAccount, Long)): Block = {
    val (miner, balance) = minerWithBalance
    val gs               = PoSConsensus.generatorSignature(generationSignature, minerAccount.publicKey.getEncoded)
    val h                = PoSConsensus.hit(gs)
    val delay            = PoSConsensus.calculateDelay(h, prev.baseTarget, balance, 0)
    val bt = PoSConsensus.calculateBaseTarget(
      prev.height + 1,
      prev.baseTarget,
      prev.timestamp,
      grand.map(_.timestamp),
      blockDelaySeconds,
      prev.timestamp + delay
    )

    Block(
      prev.height + 1,
      bt,
      miner,
      prev.timestamp + delay,
      delay
    )
  }

  def calcPerfomance(chain: List[Block], miners: Map[PrivateKeyAccount, Long]): Map[Long, Double] = {
    val balanceSum  = miners.values.sum
    val blocksCount = chain.length

    chain
      .groupBy(_.miner)
      .map(mbs => {
        val (miner, blocks) = mbs

        val minerBalance   = miners(miner)
        val expectedBlocks = ((minerBalance.toDouble / balanceSum) * blocksCount).toLong
        val perfomance     = blocks.length.toDouble / expectedBlocks

        minerBalance -> perfomance
      })
  }

  def mkMiners: Map[PrivateKeyAccount, Long] =
    List(
      CryptoHelpers.generatePrivateKey -> 200000000000000L,
      CryptoHelpers.generatePrivateKey -> 500000000000000L,
      CryptoHelpers.generatePrivateKey -> 1000000000000000L,
      CryptoHelpers.generatePrivateKey -> 1500000000000000L,
      CryptoHelpers.generatePrivateKey -> 2000000000000000L,
      CryptoHelpers.generatePrivateKey -> 2500000000000000L
    ).toMap
}
