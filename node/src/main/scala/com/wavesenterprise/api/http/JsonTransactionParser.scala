package com.wavesenterprise.api.http

import cats.syntax.either._
import com.wavesenterprise.account.PublicKeyAccount
import com.wavesenterprise.api.http.ApiError.WrongJson
import com.wavesenterprise.api.http.acl.{PermitRequestV1, PermitRequestV2}
import com.wavesenterprise.api.http.alias.{CreateAliasV2Request, CreateAliasV3Request, CreateAliasV4Request}
import com.wavesenterprise.api.http.assets.{
  BurnV2Request,
  BurnV3Request,
  IssueV2Request,
  IssueV3Request,
  MassTransferRequestV1,
  MassTransferRequestV2,
  MassTransferRequestV3,
  ReissueV2Request,
  ReissueV3Request,
  SetAssetScriptRequest,
  SetScriptRequest,
  SponsorFeeRequest,
  SponsorFeeRequestV2,
  TransferV2Request,
  TransferV3Request
}
import com.wavesenterprise.api.http.docker._
import com.wavesenterprise.api.http.leasing.{LeaseCancelV2Request, LeaseCancelV3Request, LeaseV2Request, LeaseV3Request}
import com.wavesenterprise.api.http.privacy.{
  CreatePolicyRequestV1,
  CreatePolicyRequestV2,
  CreatePolicyRequestV3,
  PolicyDataHashRequestV3,
  UpdatePolicyRequestV1,
  UpdatePolicyRequestV2,
  UpdatePolicyRequestV3
}
import com.wavesenterprise.api.http.wasm.{CallContractRequestV7, CreateContractRequestV7, UpdateContractRequestV6}
import com.wavesenterprise.transaction.ValidationError.GenericError
import com.wavesenterprise.transaction._
import com.wavesenterprise.transaction.acl.{PermitTransactionV1, PermitTransactionV2}
import com.wavesenterprise.transaction.assets._
import com.wavesenterprise.transaction.docker._
import com.wavesenterprise.transaction.lease.{LeaseCancelTransactionV2, LeaseCancelTransactionV3, LeaseTransactionV2, LeaseTransactionV3}
import com.wavesenterprise.transaction.smart.SetScriptTransactionV1
import com.wavesenterprise.transaction.transfer.{
  MassTransferTransactionV1,
  MassTransferTransactionV2,
  MassTransferTransactionV3,
  TransferTransactionV2,
  TransferTransactionV3
}
import com.wavesenterprise.utils.Time
import com.wavesenterprise.wallet.Wallet
import play.api.libs.json.{JsError, JsObject, JsSuccess, Json}

class JsonTransactionParser {

  def parseTransaction(senderPk: String, typeId: Byte, version: Byte, txJson: JsObject): Either[ValidationError, Transaction] = {
    PublicKeyAccount
      .fromBase58String(senderPk)
      .leftMap(ValidationError.fromCryptoError)
      .flatMap { senderPk =>
        TransactionParsers.by(typeId, version) match {
          case None => Left(GenericError(s"Bad transaction type ($typeId) and version ($version)"))
          case Some(x) =>
            x match {
              // token txs
              case IssueTransactionV2        => TransactionFactory.issueAssetV2(txJson.as[IssueV2Request], senderPk)
              case IssueTransactionV3        => TransactionFactory.issueAssetV3(txJson.as[IssueV3Request], senderPk)
              case TransferTransactionV2     => TransactionFactory.transferAssetV2(txJson.as[TransferV2Request], senderPk)
              case TransferTransactionV3     => TransactionFactory.transferAssetV3(txJson.as[TransferV3Request], senderPk)
              case ReissueTransactionV2      => TransactionFactory.reissueAssetV2(txJson.as[ReissueV2Request], senderPk)
              case ReissueTransactionV3      => TransactionFactory.reissueAssetV3(txJson.as[ReissueV3Request], senderPk)
              case BurnTransactionV2         => TransactionFactory.burnAssetV2(txJson.as[BurnV2Request], senderPk)
              case BurnTransactionV3         => TransactionFactory.burnAssetV3(txJson.as[BurnV3Request], senderPk)
              case MassTransferTransactionV1 => TransactionFactory.massTransferAssetV1(txJson.as[MassTransferRequestV1], senderPk)
              case MassTransferTransactionV2 => TransactionFactory.massTransferAssetV2(txJson.as[MassTransferRequestV2], senderPk)
              case MassTransferTransactionV3 => TransactionFactory.massTransferAssetV3(txJson.as[MassTransferRequestV3], senderPk)
              case LeaseTransactionV2        => TransactionFactory.leaseV2(txJson.as[LeaseV2Request], senderPk)
              case LeaseTransactionV3        => TransactionFactory.leaseV3(txJson.as[LeaseV3Request], senderPk)
              case LeaseCancelTransactionV2  => TransactionFactory.leaseCancelV2(txJson.as[LeaseCancelV2Request], senderPk)
              case LeaseCancelTransactionV3  => TransactionFactory.leaseCancelV3(txJson.as[LeaseCancelV3Request], senderPk)
              // alias
              case CreateAliasTransactionV2 => TransactionFactory.aliasV2(txJson.as[CreateAliasV2Request], senderPk)
              case CreateAliasTransactionV3 => TransactionFactory.aliasV3(txJson.as[CreateAliasV3Request], senderPk)
              case CreateAliasTransactionV4 => TransactionFactory.aliasV4(txJson.as[CreateAliasV4Request], senderPk)
              // data
              case DataTransactionV1 => TransactionFactory.dataV1(txJson.as[DataRequestV1], senderPk, senderPk)
              case DataTransactionV2 => TransactionFactory.dataV2(txJson.as[DataRequestV2], senderPk, senderPk)
              case DataTransactionV3 => TransactionFactory.dataV3(txJson.as[DataRequestV3], senderPk, senderPk)
              // ride smart-contracts
              case SetScriptTransactionV1      => TransactionFactory.setScript(txJson.as[SetScriptRequest], senderPk)
              case SetAssetScriptTransactionV1 => TransactionFactory.setAssetScript(txJson.as[SetAssetScriptRequest], senderPk)
              case SponsorFeeTransactionV1     => TransactionFactory.sponsor(txJson.as[SponsorFeeRequest], senderPk)
              case SponsorFeeTransactionV2     => TransactionFactory.sponsorV2(txJson.as[SponsorFeeRequestV2], senderPk)
              // acl
              case PermitTransactionV1       => TransactionFactory.permitTransactionV1(txJson.as[PermitRequestV1], senderPk)
              case PermitTransactionV2       => TransactionFactory.permitTransactionV2(txJson.as[PermitRequestV2], senderPk)
              case RegisterNodeTransactionV1 => TransactionFactory.registerNodeTransaction(txJson.as[RegisterNodeRequest], senderPk)
              case RegisterNodeTransactionV2 => TransactionFactory.registerNodeTransactionV2(txJson.as[RegisterNodeRequestV2], senderPk)
              // docker smart-contracts
              case CreateContractTransactionV1  => TransactionFactory.createContractTransactionV1(txJson.as[CreateContractRequestV1], senderPk)
              case CreateContractTransactionV2  => TransactionFactory.createContractTransactionV2(txJson.as[CreateContractRequestV2], senderPk)
              case CreateContractTransactionV3  => TransactionFactory.createContractTransactionV3(txJson.as[CreateContractRequestV3], senderPk)
              case CreateContractTransactionV4  => TransactionFactory.createContractTransactionV4(txJson.as[CreateContractRequestV4], senderPk)
              case CreateContractTransactionV5  => TransactionFactory.createContractTransactionV5(txJson.as[CreateContractRequestV5], senderPk)
              case CreateContractTransactionV6  => TransactionFactory.createContractTransactionV6(txJson.as[CreateContractRequestV6], senderPk)
              case CreateContractTransactionV7  => TransactionFactory.createContractTransactionV7(txJson.as[CreateContractRequestV7], senderPk)
              case CallContractTransactionV1    => TransactionFactory.callContractTransactionV1(txJson.as[CallContractRequestV1], senderPk)
              case CallContractTransactionV2    => TransactionFactory.callContractTransactionV2(txJson.as[CallContractRequestV2], senderPk)
              case CallContractTransactionV3    => TransactionFactory.callContractTransactionV3(txJson.as[CallContractRequestV3], senderPk)
              case CallContractTransactionV4    => TransactionFactory.callContractTransactionV4(txJson.as[CallContractRequestV4], senderPk)
              case CallContractTransactionV5    => TransactionFactory.callContractTransactionV5(txJson.as[CallContractRequestV5], senderPk)
              case CallContractTransactionV6    => ??? // TODO
              case CallContractTransactionV7    => TransactionFactory.callContractTransactionV7(txJson.as[CallContractRequestV7], senderPk)
              case DisableContractTransactionV1 => TransactionFactory.disableContractTransactionV1(txJson.as[DisableContractRequestV1], senderPk)
              case DisableContractTransactionV2 => TransactionFactory.disableContractTransactionV2(txJson.as[DisableContractRequestV2], senderPk)
              case DisableContractTransactionV3 => TransactionFactory.disableContractTransactionV3(txJson.as[DisableContractRequestV3], senderPk)
              case UpdateContractTransactionV1  => TransactionFactory.updateContractTransactionV1(txJson.as[UpdateContractRequestV1], senderPk)
              case UpdateContractTransactionV2  => TransactionFactory.updateContractTransactionV2(txJson.as[UpdateContractRequestV2], senderPk)
              case UpdateContractTransactionV3  => TransactionFactory.updateContractTransactionV3(txJson.as[UpdateContractRequestV3], senderPk)
              case UpdateContractTransactionV4  => TransactionFactory.updateContractTransactionV4(txJson.as[UpdateContractRequestV4], senderPk)
              case UpdateContractTransactionV5  => TransactionFactory.updateContractTransactionV5(txJson.as[UpdateContractRequestV5], senderPk)
              case UpdateContractTransactionV6  => TransactionFactory.updateContractTransactionV6(txJson.as[UpdateContractRequestV6], senderPk)
              // privacy txs
              case CreatePolicyTransactionV1   => TransactionFactory.createPolicyTransactionV1(txJson.as[CreatePolicyRequestV1], senderPk)
              case CreatePolicyTransactionV2   => TransactionFactory.createPolicyTransactionV2(txJson.as[CreatePolicyRequestV2], senderPk)
              case CreatePolicyTransactionV3   => TransactionFactory.createPolicyTransactionV3(txJson.as[CreatePolicyRequestV3], senderPk)
              case UpdatePolicyTransactionV1   => TransactionFactory.updatePolicyTransactionV1(txJson.as[UpdatePolicyRequestV1], senderPk)
              case UpdatePolicyTransactionV2   => TransactionFactory.updatePolicyTransactionV2(txJson.as[UpdatePolicyRequestV2], senderPk)
              case UpdatePolicyTransactionV3   => TransactionFactory.updatePolicyTransactionV3(txJson.as[UpdatePolicyRequestV3], senderPk)
              case PolicyDataHashTransactionV3 => TransactionFactory.policyDataHashTransactionV3(txJson.as[PolicyDataHashRequestV3], senderPk)
              // atomic
              case AtomicTransactionV1 => TransactionFactory.atomicTransactionV1(txJson.as[AtomicTransactionRequestV1], senderPk)
            }
        }
      }
  }

  protected def parseAndSignTransaction(typeId: Byte,
                                        version: Byte,
                                        txJson: JsObject,
                                        wallet: Wallet,
                                        time: Time,
                                        checkCerts: Boolean): Either[ValidationError, ProvenTransaction] = {

    TransactionParsers.by(typeId, version) match {
      case None => Left(GenericError(s"Bad transaction type ($typeId) and version ($version)"))
      case Some(txParser) =>
        txParser match {
          // token txs
          case IssueTransactionV2        => TransactionFactory.issueAssetV2(txJson.as[IssueV2Request], wallet, time)
          case IssueTransactionV3        => TransactionFactory.issueAssetV3(txJson.as[IssueV3Request], wallet, time)
          case TransferTransactionV2     => TransactionFactory.transferAssetV2(txJson.as[TransferV2Request], wallet, time)
          case TransferTransactionV3     => TransactionFactory.transferAssetV3(txJson.as[TransferV3Request], wallet, time)
          case ReissueTransactionV2      => TransactionFactory.reissueAssetV2(txJson.as[ReissueV2Request], wallet, time)
          case ReissueTransactionV3      => TransactionFactory.reissueAssetV3(txJson.as[ReissueV3Request], wallet, time)
          case BurnTransactionV2         => TransactionFactory.burnAssetV2(txJson.as[BurnV2Request], wallet, time)
          case BurnTransactionV3         => TransactionFactory.burnAssetV3(txJson.as[BurnV3Request], wallet, time)
          case MassTransferTransactionV1 => TransactionFactory.massTransferAssetV1(txJson.as[MassTransferRequestV1], wallet, time)
          case MassTransferTransactionV2 => TransactionFactory.massTransferAssetV2(txJson.as[MassTransferRequestV2], wallet, time)
          case MassTransferTransactionV3 => TransactionFactory.massTransferAssetV3(txJson.as[MassTransferRequestV3], wallet, time)
          case LeaseTransactionV2        => TransactionFactory.leaseV2(txJson.as[LeaseV2Request], wallet, time)
          case LeaseTransactionV3        => TransactionFactory.leaseV3(txJson.as[LeaseV3Request], wallet, time)
          case LeaseCancelTransactionV2  => TransactionFactory.leaseCancelV2(txJson.as[LeaseCancelV2Request], wallet, time)
          case LeaseCancelTransactionV3  => TransactionFactory.leaseCancelV3(txJson.as[LeaseCancelV3Request], wallet, time)
          // alias
          case CreateAliasTransactionV2 => TransactionFactory.aliasV2(txJson.as[CreateAliasV2Request], wallet, time)
          case CreateAliasTransactionV3 => TransactionFactory.aliasV3(txJson.as[CreateAliasV3Request], wallet, time)
          case CreateAliasTransactionV4 => TransactionFactory.aliasV4(txJson.as[CreateAliasV4Request], wallet, time)
          // data
          case DataTransactionV1 => TransactionFactory.dataV1(txJson.as[DataRequestV1], wallet, time)
          case DataTransactionV2 => TransactionFactory.dataV2(txJson.as[DataRequestV2], wallet, time)
          case DataTransactionV3 => TransactionFactory.dataV3(txJson.as[DataRequestV3], wallet, time)
          // ride smart-contracts
          case SetScriptTransactionV1      => TransactionFactory.setScript(txJson.as[SetScriptRequest], wallet, time)
          case SetAssetScriptTransactionV1 => TransactionFactory.setAssetScript(txJson.as[SetAssetScriptRequest], wallet, time)
          case SponsorFeeTransactionV1     => TransactionFactory.sponsor(txJson.as[SponsorFeeRequest], wallet, time)
          case SponsorFeeTransactionV2     => TransactionFactory.sponsorV2(txJson.as[SponsorFeeRequestV2], wallet, time)
          // acl
          case PermitTransactionV1       => TransactionFactory.permitTransactionV1(txJson.as[PermitRequestV1], wallet, time)
          case PermitTransactionV2       => TransactionFactory.permitTransactionV2(txJson.as[PermitRequestV2], wallet, time)
          case RegisterNodeTransactionV1 => TransactionFactory.registerNodeTransaction(txJson.as[RegisterNodeRequest], wallet, time)
          case RegisterNodeTransactionV2 => TransactionFactory.registerNodeTransactionV2(txJson.as[RegisterNodeRequestV2], wallet, time)
          // docker smart-contract txs
          case CreateContractTransactionV1  => TransactionFactory.createContractTransactionV1(txJson.as[CreateContractRequestV1], wallet, time)
          case CreateContractTransactionV2  => TransactionFactory.createContractTransactionV2(txJson.as[CreateContractRequestV2], wallet, time)
          case CreateContractTransactionV3  => TransactionFactory.createContractTransactionV3(txJson.as[CreateContractRequestV3], wallet, time)
          case CreateContractTransactionV4  => TransactionFactory.createContractTransactionV4(txJson.as[CreateContractRequestV4], wallet, time)
          case CreateContractTransactionV5  => TransactionFactory.createContractTransactionV5(txJson.as[CreateContractRequestV5], wallet, time)
          case CreateContractTransactionV6  => TransactionFactory.createContractTransactionV6(txJson.as[CreateContractRequestV6], wallet, time)
          case CreateContractTransactionV7  => TransactionFactory.createContractTransactionV7(txJson.as[CreateContractRequestV7], wallet, time)
          case CallContractTransactionV1    => TransactionFactory.callContractTransactionV1(txJson.as[CallContractRequestV1], wallet, time)
          case CallContractTransactionV2    => TransactionFactory.callContractTransactionV2(txJson.as[CallContractRequestV2], wallet, time)
          case CallContractTransactionV3    => TransactionFactory.callContractTransactionV3(txJson.as[CallContractRequestV3], wallet, time)
          case CallContractTransactionV4    => TransactionFactory.callContractTransactionV4(txJson.as[CallContractRequestV4], wallet, time)
          case CallContractTransactionV5    => TransactionFactory.callContractTransactionV5(txJson.as[CallContractRequestV5], wallet, time)
          case CallContractTransactionV7    => TransactionFactory.callContractTransactionV7(txJson.as[CallContractRequestV7], wallet, time)
          case UpdateContractTransactionV1  => TransactionFactory.updateContractTransactionV1(txJson.as[UpdateContractRequestV1], wallet, time)
          case UpdateContractTransactionV2  => TransactionFactory.updateContractTransactionV2(txJson.as[UpdateContractRequestV2], wallet, time)
          case UpdateContractTransactionV3  => TransactionFactory.updateContractTransactionV3(txJson.as[UpdateContractRequestV3], wallet, time)
          case UpdateContractTransactionV4  => TransactionFactory.updateContractTransactionV4(txJson.as[UpdateContractRequestV4], wallet, time)
          case UpdateContractTransactionV5  => TransactionFactory.updateContractTransactionV5(txJson.as[UpdateContractRequestV5], wallet, time)
          case UpdateContractTransactionV6  => TransactionFactory.updateContractTransactionV6(txJson.as[UpdateContractRequestV6], wallet, time)
          case DisableContractTransactionV1 => TransactionFactory.disableContractTransactionV1(txJson.as[DisableContractRequestV1], wallet, time)
          case DisableContractTransactionV2 => TransactionFactory.disableContractTransactionV2(txJson.as[DisableContractRequestV2], wallet, time)
          case DisableContractTransactionV3 => TransactionFactory.disableContractTransactionV3(txJson.as[DisableContractRequestV3], wallet, time)
          // privacy txs
          case CreatePolicyTransactionV1   => TransactionFactory.createPolicyTransactionV1(txJson.as[CreatePolicyRequestV1], wallet, time)
          case CreatePolicyTransactionV2   => TransactionFactory.createPolicyTransactionV2(txJson.as[CreatePolicyRequestV2], wallet, time)
          case CreatePolicyTransactionV3   => TransactionFactory.createPolicyTransactionV3(txJson.as[CreatePolicyRequestV3], wallet, time)
          case UpdatePolicyTransactionV1   => TransactionFactory.updatePolicyTransactionV1(txJson.as[UpdatePolicyRequestV1], wallet, time)
          case UpdatePolicyTransactionV2   => TransactionFactory.updatePolicyTransactionV2(txJson.as[UpdatePolicyRequestV2], wallet, time)
          case UpdatePolicyTransactionV3   => TransactionFactory.updatePolicyTransactionV3(txJson.as[UpdatePolicyRequestV3], wallet, time)
          case PolicyDataHashTransactionV3 => TransactionFactory.policyDataHashTransactionV3(txJson.as[PolicyDataHashRequestV3], wallet, time)
          // atomic
          case AtomicTransactionV1 => TransactionFactory.atomicTransactionV1(txJson.as[AtomicTransactionRequestV1], wallet, time)
          case _                   => Left(ValidationError.UnsupportedTransactionType)
        }
    }
  }

  def createTransaction(senderPk: String, jsv: JsObject): Either[ApiError, Transaction] = {
    getFilledTransaction(jsv) match {
      case Left(parseErrors) => Left(WrongJson(None, parseErrors.errors))
      case Right(txJsonWithMeta) =>
        val typeId  = txJsonWithMeta.typeId
        val version = txJsonWithMeta.version
        val txJson  = txJsonWithMeta.txJson

        parseTransaction(senderPk, typeId, version, txJson)
          .leftMap(ApiError.fromValidationError)
    }
  }

  def signTransaction(jsv: JsObject, wallet: Wallet, time: Time, checkCerts: Boolean): Either[ApiError, ProvenTransaction] = {
    getFilledTransaction(jsv) match {
      case Left(parseErrors) => Left(WrongJson(None, parseErrors.errors))
      case Right(txJsonWithMeta) =>
        val typeId  = txJsonWithMeta.typeId
        val version = txJsonWithMeta.version
        val txJson  = txJsonWithMeta.txJson

        parseAndSignTransaction(typeId, version, txJson, wallet, time, checkCerts).leftMap(ApiError.fromValidationError)
    }
  }

  private def getFilledTransaction(jsv: JsObject): Either[ParseErrors, TxJsonWithMeta] = {
    val typeId = (jsv \ "type").as[Byte]

    (jsv \ "version").validateOpt[Byte](versionReads) match {
      case JsError(errors) => Left(ParseErrors(errors))
      case JsSuccess(value, _) =>
        val version = value getOrElse (1: Byte)
        val txJson  = jsv ++ Json.obj("version" -> version)
        Right(TxJsonWithMeta(txJson, typeId, version))
    }
  }
}
