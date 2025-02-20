package com.wavesenterprise.wasm

import com.google.common.primitives.Longs

import java.nio.charset.StandardCharsets.UTF_8
import scala.collection.mutable
import com.wavesenterprise.account.{Address, AddressOrAlias, AddressScheme, Alias}
import com.wavesenterprise.crypto.DigestSize
import com.wavesenterprise.docker.{ContractExecution, ContractExecutionSuccessV2}
import com.wavesenterprise.state.AssetHolder.AddressExt
import com.wavesenterprise.state.ContractBlockchain.ContractReadingContext
import com.wavesenterprise.state.{Account, AssetHolder, Blockchain, ByteStr, Contract, ContractId, DataEntry, LeaseId}
import com.wavesenterprise.transaction.AssetId
import com.wavesenterprise.transaction.docker.ContractTransactionEntryOps.{parse, toBytes}
import com.wavesenterprise.transaction.docker.ExecutableTransaction
import com.wavesenterprise.transaction.docker.assets.ContractAssetOperation.{
  ContractBurnV1,
  ContractCancelLeaseV1,
  ContractIssueV1,
  ContractLeaseV1,
  ContractPaymentV1,
  ContractReissueV1,
  ContractTransferOutV1
}
import com.wavesenterprise.transaction.docker.assets.ContractAssetOperation
import com.wavesenterprise.utils.Base58
import com.wavesenterprise.wasm.WASMServiceImpl.{
  BytecodeNotFound,
  DataIsMissing,
  DiffAction,
  GetAction,
  InvalidArgument,
  InvalidTransfer,
  SetAction,
  WEVMExecutionException,
  getTransfers
}
import com.wavesenterprise.wasm.core.WASMService
import com.wavesenterprise.{crypto, getWasmContract}

import java.nio.ByteBuffer

class WASMServiceImpl(
    currentContractId: ContractId,
    tx: ExecutableTransaction,
    blockchain: Blockchain
) extends WASMService {

  private val results = mutable.Map[(ByteStr, String), DiffAction]()

  private val assetOperations = mutable.Map.empty[ContractId, mutable.ArrayBuffer[ContractAssetOperation]]

  private val readingContext = ContractReadingContext.TransactionExecution(tx.id.value())

  private val assetNonce = mutable.Map.empty[ContractId, Int].withDefaultValue(1)

  private val leaseNonce = mutable.Map.empty[ContractId, Int].withDefaultValue(1)

  private val holderBalances = mutable.Map[(AssetHolder, Option[AssetId]), Long]()

  private val contractPayments: mutable.Map[
    ContractId,
    mutable.Map[Int, (ByteStr, List[ContractPaymentV1])]
  ] = {
    if (tx.payments.isEmpty) {
      mutable.Map.empty
    } else {
      val holder = tx.sender.toAddress

      mutable.Map(currentContractId -> mutable.Map(
        0 -> (currentContractId.byteStr -> tx.payments.map(p => {
          val bal = holderBalances.getOrElseUpdate(holder.toAssetHolder -> p.assetId, blockchain.addressBalance(holder, p.assetId))
          if (bal - p.amount < 0)
            throwException(503, s"Balance $bal is less then expected send amount")
          holderBalances.put(holder.toAssetHolder -> p.assetId, bal - p.amount)
          val cBal = holderBalances.getOrElseUpdate(
            Contract(currentContractId) -> p.assetId,
            blockchain.contractBalance(currentContractId, p.assetId, readingContext)
          )
          holderBalances.put(Contract(currentContractId) -> p.assetId, cBal + p.amount)
          ContractPaymentV1(
            p.assetId,
            currentContractId.byteStr,
            p.amount)
        }))))
    }
  }

  private def throwException(code: Int, message: String) = throw WEVMExecutionException(code, message)

  private def asString(value: Array[Byte]): String = new String(value, UTF_8)

  private def toDataEntry(bytes: Array[Byte]): DataEntry[_] = {
    parse(bytes, 0)._1
  }

  private def toContractId(value: Array[Byte]): ContractId = ContractId {
    ByteStr(value)
  }

  private def assetIdOpt(assetId: Array[Byte]): Option[ByteStr] = {
    if (assetId.isEmpty) None else Some(ByteStr(assetId))
  }

  private def addAssetOp(contractId: ContractId, operation: ContractAssetOperation): Unit = {
    assetOperations.getOrElseUpdate(contractId, mutable.ArrayBuffer.empty[ContractAssetOperation]) += operation
  }

  private def dataGetter(contractId: ByteStr, key: Array[Byte], get: () => DataEntry[_]): Array[Byte] = {
    val keyStr = asString(key)
    results.get(contractId -> keyStr) match {
      case Some(value) => value.getBytes
      case None =>
        val got = get()
        results.put(contractId -> keyStr, GetAction(got))
        toBytes(got)
    }
  }

  private def dataSetter(contractId: ContractId, value: DataEntry[_]): Array[Byte] = {
    val action = SetAction(value)
    results.put(contractId.byteStr -> value.key, action)
    action.getBytes
  }

  def getContractExecution: ContractExecution = {
    val finalResults = results.filter(_._2.isSet)
      .toList
      .groupBy(_._1._1)
      .mapValues(_.map(_._2.getEntry))
    // drop attached payment since it is already in transaction
    if (tx.payments.nonEmpty) {
      contractPayments(currentContractId).remove(0)
    }

    val paymentsByCaller: mutable.Map[ByteStr, mutable.ArrayBuilder[List[ContractPaymentV1]]] = mutable.Map.empty

    contractPayments.values.foreach {
      map =>
        map.foreach {
          case (_, (cid, paymentList)) =>
            paymentsByCaller.getOrElseUpdate(cid, mutable.ArrayBuilder.make()) += paymentList
        }
    }

    val allKeys = paymentsByCaller.keys.map(ContractId) ++ assetOperations.keys

    val finalOperations = allKeys.map { cid =>
      val payments = paymentsByCaller.getOrElse(cid.byteStr, mutable.ArrayBuilder.make()).result().flatten.toList
      val assetOps = assetOperations.getOrElse(cid, mutable.ArrayBuffer.empty).result.toList
      cid.byteStr -> (payments ++ assetOps)
    }

    ContractExecutionSuccessV2(
      results = finalResults,
      assetOperations = finalOperations.toMap
    )
  }

  /**
    * @param contractId ID of a contract
    * @return Bytecode contract
    */
  override def getBytecode(contractId: Array[Byte]): Array[Byte] = {
    val cid = toContractId(contractId)
    try {
      blockchain.contract(cid).map(getWasmContract _ andThen (_.bytecode))
        .getOrElse(throwException(BytecodeNotFound, s"bytecode for $cid is missing"))
    } catch {
      // if StoredContract isInstanceOf DockerContract
      case _: IllegalArgumentException => throwException(BytecodeNotFound, s"bytecode for $cid is missing")
    }
  }

  /**
    * @param contractId ID of a contract (optional field, array can be empty)
    * @param key        Record key
    * @return Record value
    */
  override def getStorage(contractId: Array[Byte], key: Array[Byte]): Array[Byte] = {
    val cid    = toContractId(contractId)
    val keyStr = asString(key)

    val get = () => {
      val contractData = blockchain.contractData(cid.byteStr, keyStr, readingContext)
      contractData.getOrElse(throwException(DataIsMissing, s"data entry of contract $cid ($keyStr) is missing"))
    }

    dataGetter(cid.byteStr, key, get)
  }

  /**
    * @param value      Record value
    * @return Execution result (true/false)
    */
  def setStorage(contractId: Array[Byte], value: Array[Byte]): Unit = {
    dataSetter(
      toContractId(contractId),
      toDataEntry(value)
    )
  }

  /**
    * @param assetId ID of a token (optional field, array can be empty). Base58 bytes
    * @param assetHolder AssetHolder the token
    * @return Amount of tokens
    */
  override def getBalance(assetId: Array[Byte], assetHolder: Array[Byte]): Long = {
    val asset = assetIdOpt(assetId)
    getRecipient(assetHolder) match {
      case acc: Account =>
        holderBalances.getOrElse(acc -> asset, blockchain.addressBalance(acc.address, asset))
      case contract: Contract =>
        holderBalances.getOrElse(
          contract -> asset,
          blockchain.contractBalance(contract.contractId, asset, readingContext)
        )
    }
  }

  /**
    * @param contractId ID of a contract called this function. Base58 bytes
    * @param assetId    ID of a token to be transferred (optional field, array can be empty)
    * @param recipient  AssetHolder of recipient of tokens
    * @param amount     Amount of tokens
    * @return Execution result (true/false)
    */
  override def transfer(contractId: Array[Byte], assetId: Array[Byte], recipient: Array[Byte], amount: Long): Unit = {

    if (amount < 0) {
      throw WEVMExecutionException(InvalidTransfer, s"cant transfer negative amount $amount")
    }

    val cid   = toContractId(contractId)
    val asset = assetIdOpt(assetId)
    val recip = getRecipient(recipient)

    val (newAmount, op) = recip match {

      case acc: Account =>
        val aBalance = holderBalances.getOrElse(acc -> asset, blockchain.addressBalance(acc.address, asset))
        (aBalance + amount) -> ContractTransferOutV1(
          assetId = asset,
          recipient = acc.address,
          amount = amount)

      case contract: Contract =>
        val cBalance = holderBalances.getOrElse(
          contract -> asset,
          blockchain.contractBalance(contract.contractId, asset, readingContext)
        )
        (cBalance + amount) -> ContractPaymentV1(
          assetId = asset,
          recipient = contract.contractId.byteStr,
          amount = amount)

    }
    holderBalances.put(recip -> asset, newAmount)
    addAssetOp(cid, op)
  }

  /**
    * @param contractId   ID of a contract called this function. Base58 bytes
    * @param name         An arbitrary name of asset
    * @param description  An arbitrary description of a asset
    * @param quantity     Number of tokens to be issued
    * @param decimals     Digit capacity of a token in use
    * @param isReissuable Re-issuability of a token
    * @return assetId
    */
  override def issue(
      contractId: Array[Byte],
      name: Array[Byte],
      description: Array[Byte],
      quantity: Long,
      decimals: Long,
      isReissuable: Boolean
  ): Array[Byte] = {
    val cid = toContractId(contractId)

    var nextAssetNonce = assetNonce(cid)
    var assetId        = ByteStr.empty

    if (quantity < 0 || decimals < 0) {
      throwException(InvalidTransfer, s"unexpected issue quantity $quantity and decimals $decimals for tx ${tx.id.value()}")
    }

    do {
      assetId = ByteStr(crypto.fastHash(tx.id.value().arr :+ nextAssetNonce.toByte))
      if (nextAssetNonce == Byte.MaxValue)
        throwException(InvalidTransfer, s"asset id overflow for tx ${tx.id.value()} and asset ${asString(name)}")
      nextAssetNonce += 1
    } while (blockchain.asset(assetId).isDefined)

    val op = ContractIssueV1(
      assetId = assetId,
      name = asString(name),
      description = asString(description),
      quantity = quantity,
      decimals = decimals.toByte,
      isReissuable = isReissuable,
      nonce = (nextAssetNonce - 1).toByte
    )
    addAssetOp(cid, op)
    holderBalances.put(Contract(cid) -> Some(assetId), quantity)
    assetNonce(cid) = nextAssetNonce
    assetId.arr
  }

  /**
    * @param contractId ID of a contract called this function. Base58 bytes
    * @param assetId    ID of a token to be burned
    * @param amount     Amount of tokens
    */
  override def burn(contractId: Array[Byte], assetId: Array[Byte], amount: Long): Unit = {
    val asset = assetIdOpt(assetId)
    val cid   = toContractId(contractId)

    if (asset.isEmpty) {
      throw WEVMExecutionException(InvalidTransfer, s"cant burn system token")
    }
    if (amount < 0) {
      throw WEVMExecutionException(InvalidTransfer, s"cant burn negative amount $amount")
    }

    addAssetOp(cid, ContractBurnV1(asset, amount))
    withdrawBalance(cid, asset, amount)
  }

  /**
    * @param contractId   ID of a contract called this function. Base58 bytes
    * @param assetId      ID of a token to be reissued
    * @param amount       Amount of tokens
    * @param isReissuable Re-issuability of a token
    */
  override def reissue(contractId: Array[Byte], assetId: Array[Byte], amount: Long, isReissuable: Boolean): Unit = {
    val cid = toContractId(contractId)
    if (amount <= 0) {
      throwException(InvalidTransfer, s"can't reissue $amount amount asset")
    } else {
      val op = ContractReissueV1(
        assetId = assetIdOpt(assetId).getOrElse(
          throwException(InvalidArgument, s"incorrect assetId ${asString(assetId)}")
        ),
        quantity = amount,
        isReissuable = isReissuable
      )
      addAssetOp(cid, op)
    }
  }

  /**
    * @param contractId ID of a contract called this function. Base58 bytes
    * @param recipient  AssetHolder of recipient of tokens
    * @param amount     Number of tokens for leasing
    * @return leaseId of a leasing transaction
    */
  override def lease(contractId: Array[Byte], recipient: Array[Byte], amount: Long): Array[Byte] = {

    if (amount < 0) {
      throw WEVMExecutionException(InvalidTransfer, s"cant lease negative amount $amount")
    }

    val cid   = toContractId(contractId)
    val recip = getRecipient(recipient)

    var leaseId        = LeaseId(ByteStr.empty)
    var nextLeaseNonce = leaseNonce(cid)

    do {
      leaseId = LeaseId(ByteStr(crypto.fastHash(tx.id.value().arr :+ nextLeaseNonce.toByte)))
      if (nextLeaseNonce == Byte.MaxValue)
        throwException(InvalidTransfer, s"leaseId overflow for tx ${tx.id.value()} and recipient ${asString(recipient)}")
      nextLeaseNonce += 1
    } while (blockchain.leaseDetails(leaseId).isDefined)

    recip match {

      case Account(address) =>
        val op = ContractLeaseV1(
          leaseId = leaseId.byteStr,
          nonce = (nextLeaseNonce - 1).toByte,
          recipient = address,
          amount = amount
        )
        addAssetOp(cid, op)
        leaseNonce(cid) = nextLeaseNonce
        leaseId.arr

      case Contract(_) => throw WEVMExecutionException(
          400,
          "lease to contract is not supported yet"
        )
    }
  }

  /**
    * @param contractId ID of a contract called this function. Base58 bytes
    * @param leaseId    ID of a leasing transaction
    * @return Execution result (true/false)
    */
  override def cancelLease(contractId: Array[Byte], leaseId: Array[Byte]): Unit = {
    val cid = toContractId(contractId)
    val id  = ByteStr(leaseId)
    addAssetOp(cid, ContractCancelLeaseV1(id))
  }

  override def getBlockTimestamp: Long = {
    blockchain.lastBlockTimestamp.getOrElse(0)
  }

  override def getBlockHeight: Long = {
    blockchain.height
  }

  /**
    * @return Address calling contract
    */
  override def tx(field: Array[Byte]): Array[Byte] = {
    asString(field) match {
      case "txId"   => tx.id().arr
      case "sender" => tx.sender.publicKey.getEncoded
    }
  }

  private def getPaymentNum(paymentId: Array[Byte]) = ByteBuffer.wrap(paymentId.drop(DigestSize)).getLong.toInt

  private def getPayment(paymentId: Array[Byte]): List[ContractPaymentV1] = {
    val cid        = toContractId(paymentId.take(DigestSize))
    val paymentNum = getPaymentNum(paymentId)

    val payments = contractPayments.getOrElse(
      cid,
      throwException(InvalidTransfer, s"payments for contract $cid not found")
    )
    payments.getOrElse(
      paymentNum,
      throwException(InvalidTransfer, s"$paymentNum not found in $payments for contract $cid")
    )._2
  }

  /**
    * @param  paymentId – Unique payment identifier. Represents the concatenation of contractId bytes and unique 8 bytes
    * @return Number of attached payments
    */
  override def getTxPayments(paymentId: Array[Byte]): Long = {
    getPayment(paymentId).size
  }

  /**
    * @param paymentId – Unique payment identifier. Represents the concatenation of contractId bytes and unique 8 bytes
    * @param number Attached payment number
    * @return assetId of a token (optional field, array can be empty)
    */
  override def getTxPaymentAssetId(paymentId: Array[Byte], number: Long): Array[Byte] = {
    val payment = getPayment(paymentId)
    if (number >= payment.size) {
      throwException(InvalidTransfer, s"$number exceeds ${payment.size} for payment list $payment")
    } else {
      payment(number.toInt).assetId.map(_.arr).getOrElse(Array.emptyByteArray)
    }
  }

  /**
   * @param paymentId Unique payment identifier. Represents the concatenation of contractId bytes and unique 8 bytes
   * @param number    Attached payment number
   * @return Amount of tokens
   */
  override def getTxPaymentAmount(paymentId: Array[Byte], number: Long): Long = {
    val payment = getPayment(paymentId)
    if (number >= payment.size) {
      throwException(InvalidTransfer, s"$number exceeds ${payment.size} for payment list $payment")
    } else {
      payment(number.toInt).amount
    }
  }

  /**
   * @param contractId ID of a contract called this function. Base58 bytes
   * @param paymentId  of a contract. Base58 bytes concatenated with payment nonce
   * @param payments   Serialized list assetId and amount
   */
  override def addPayments(contractId: Array[Byte], paymentId: Array[Byte], payments: Array[Byte]): Unit = {
    if (payments.length > 2) { // ignore empty payments

      val cid        = toContractId(contractId)
      val recipient  = toContractId(paymentId.take(DigestSize))
      val paymentNum = getPaymentNum(paymentId)

      val transfers = getTransfers(recipient.byteStr, payments)

      transfers.foreach { transfer =>
        val recipient = ContractId(transfer.recipient)
        val balance = holderBalances.getOrElse(
          Contract(recipient) -> transfer.assetId,
          blockchain.contractBalance(recipient, transfer.assetId, readingContext)
        )
        holderBalances.update(Contract(recipient) -> transfer.assetId, balance + transfer.amount)

        withdrawBalance(cid, transfer.assetId, transfer.amount)
      }

      contractPayments.getOrElseUpdate(recipient, mutable.Map.empty).put(paymentNum, cid.byteStr -> transfers)
    }
  }

  override def getChainId(): Byte = AddressScheme.getAddressSchema.chainId

  override def fastHash(bytes: Array[Byte]): Array[Byte] = crypto.fastHash(bytes)

  override def secureHash(bytes: Array[Byte]): Array[Byte] = crypto.secureHash(bytes)

  override def sigVerify(message: Array[Byte], signature: Array[Byte], publicKey: Array[Byte]): Boolean = {
    crypto.verify(signature = signature, message = message, publicKey = publicKey)
  }

  private def getRecipient(recipient: Array[Byte]): AssetHolder = {
    recipient(0) match {
      case Account.binaryHeader =>
        val addrOrAlias = AddressOrAlias.fromBytes(recipient, 1).getOrElse(
          throw WEVMExecutionException(InvalidArgument, "wrong addressOrAlias bytes")
        )._1
        Account(
          addrOrAlias match {
            case address: Address => address
            case alias: Alias => blockchain.resolveAlias(alias).getOrElse(
                throw WEVMExecutionException(InvalidArgument, s"could not resolve alias $alias")
              )
          }
        )
      case Contract.binaryHeader =>
        val contractBytes = recipient.slice(1, com.wavesenterprise.crypto.DigestSize + 2)
        Contract(ContractId(ByteStr(contractBytes)))
    }
  }

  private def withdrawBalance(
      contractId: ContractId,
      asset: Option[AssetId],
      amount: Long) = {
    val balance = holderBalances.getOrElse(
      Contract(contractId) -> asset,
      blockchain.contractBalance(contractId, asset, readingContext)
    )
    val newBalance = balance - amount
    if (newBalance < 0) {
      throw WEVMExecutionException(InvalidTransfer, s"Contract $contractId reached negative balance $newBalance for asset $asset")
    }
    holderBalances.put(Contract(contractId) -> asset, newBalance)
  }

}

object WASMServiceImpl {

  trait DiffAction {

    def isSet: Boolean

    def getEntry: DataEntry[_]

    def getBytes: Array[Byte]
  }

  case class SetAction(entry: DataEntry[_]) extends DiffAction {
    override def isSet: Boolean = true

    override def getEntry: DataEntry[_] = entry

    override def getBytes: Array[Byte] = toBytes(entry)
  }

  case class GetAction(entry: DataEntry[_]) extends DiffAction {

    override def isSet: Boolean = false

    override def getEntry: DataEntry[_] = entry

    override def getBytes: Array[Byte] = toBytes(entry)
  }

  def getTransfers(recipient: ByteStr, payments: Array[Byte]): List[ContractPaymentV1] = {
    val assetLength = 32
    var pos         = 2
    val count: Int  = ((payments(0) & 0xff) << 8) | (payments(1) & 0xff)

    val result = mutable.ArrayBuilder.make[ContractPaymentV1]()

    for (_ <- 0 until count) {
      var assetId: Option[AssetId] = None

      if (payments(pos) == 1) {
        val assetStr = Base58.encode(payments.slice(pos + 1, pos + 1 + assetLength))
        assetId = Some(
          ByteStr
            .decodeBase58(assetStr)
            .getOrElse(throw WEVMExecutionException(InvalidArgument, s"String is not base58: $assetStr"))
        )
        pos += 1 + assetLength
      } else if (payments(pos) == 0) {
        pos += 1
      } else {
        throw WEVMExecutionException(
          InvalidArgument,
          s"expecting 0 (assetId not defined) or 1 (assetId is defined). But got ${payments(pos)}"
        )
      }
      val amount = Longs.fromByteArray(payments.slice(pos, pos + Longs.BYTES))
      if (amount < 0) {
        throw WEVMExecutionException(InvalidTransfer, s"amount $amount < 0")
      }
      pos += Longs.BYTES
      result += ContractPaymentV1(assetId, recipient, amount)
    }
    result.result().toList
  }

  case class WEVMExecutionException(code: Int, message: String) extends Throwable

  val DataIsMissing   = 501
  val InvalidArgument = 502
  val InvalidTransfer = 503

  val BytecodeNotFound = 401
}
