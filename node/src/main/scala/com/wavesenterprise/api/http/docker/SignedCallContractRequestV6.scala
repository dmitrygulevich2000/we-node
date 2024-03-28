package com.wavesenterprise.api.http.docker

import cats.implicits._
import com.wavesenterprise.account.PublicKeyAccount
import com.wavesenterprise.api.http.{BroadcastRequest, UnsignedTxRequest}
import com.wavesenterprise.crypto.internals.confidentialcontracts.Commitment
import com.wavesenterprise.state.{ByteStr, DataEntry}
import com.wavesenterprise.transaction.ValidationError.InvalidContractId
import com.wavesenterprise.transaction.docker.assets.ContractTransferInV1
import com.wavesenterprise.transaction.docker.{CallContractTransaction, CallContractTransactionV6}
import com.wavesenterprise.transaction.{AssetIdStringLength, AtomicBadge, Proofs, ValidationError}
import play.api.libs.json.{JsNumber, JsObject, Json, OFormat}

/**
 * Signed [[CallContractTransactionV6]] request
 */
case class SignedCallContractRequestV6(version: Int,
                                       sender: String,
                                       senderPublicKey: String,
                                       contractId: String,
                                       contractVersion: Int,
                                       params: List[DataEntry[_]],
                                       payments: List[ContractTransferInV1],
                                       fee: Long,
                                       feeAssetId: Option[String],
                                       timestamp: Option[Long],
                                       atomicBadge: Option[AtomicBadge],
                                       proofs: List[String],
                                       inputCommitment: Commitment,
                                       password: Option[String] = None)
    extends UnsignedTxRequest with BroadcastRequest {

  def toTx: Either[ValidationError, CallContractTransactionV6] =
    for {
      sender     <- PublicKeyAccount.fromBase58String(senderPublicKey).leftMap(ValidationError.fromCryptoError)
      feeAssetId <- parseBase58ToOption(feeAssetId.filter(_.nonEmpty), "invalid feeAssetId", AssetIdStringLength)
      proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      proofs     <- Proofs.create(proofBytes)
      contractId <- ByteStr.decodeBase58(contractId).fold(_ => Left(InvalidContractId(contractId)), Right(_))
      tx <- CallContractTransactionV6.create(
        sender,
        contractId,
        params,
        fee,
        timestamp.getOrElse(System.currentTimeMillis()),
        contractVersion,
        feeAssetId,
        atomicBadge,
        payments,
        proofs,
        inputCommitment
      )
    } yield tx

  def toJson: JsObject = Json.toJsObject(this) + ("type" -> JsNumber(CallContractTransaction.typeId.toInt))
}

object SignedCallContractRequestV6 {

  implicit val format: OFormat[SignedCallContractRequestV6] = Json.format

}
