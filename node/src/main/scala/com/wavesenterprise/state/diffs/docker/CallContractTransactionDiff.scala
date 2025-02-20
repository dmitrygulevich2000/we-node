package com.wavesenterprise.state.diffs.docker

import cats.implicits._
import com.wavesenterprise.docker.ContractInfo
import com.wavesenterprise.features.BlockchainFeature
import com.wavesenterprise.state.AssetHolder._
import com.wavesenterprise.state.diffs.TransferOpsSupport
import com.wavesenterprise.state.{Blockchain, ContractId, Diff}
import com.wavesenterprise.transaction.ValidationError.{ContractIsDisabled, ContractNotFound, ContractVersionMatchError, UnexpectedTransactionError}
import com.wavesenterprise.transaction.docker._
import com.wavesenterprise.transaction.{Signed, ValidationError}

/**
  * Creates [[Diff]] for [[CallContractTransaction]]
  */
case class CallContractTransactionDiff(blockchain: Blockchain, blockOpt: Option[Signed], height: Int)
    extends ValidatorsValidator with BytecodeValidator
    with TransferOpsSupport {

  def apply(tx: CallContractTransaction): Either[ValidationError, Diff] = {
    blockOpt match {
      case Some(_) => Left(UnexpectedTransactionError(tx))
      case None =>
        lazy val baseCallContractDiff = for {
          contractInfo <- blockchain.contract(ContractId(tx.contractId)).toRight(ContractNotFound(tx.contractId))
          _            <- checkContractIsNotLegacy(tx)
          _            <- checkContractVersion(tx, contractInfo)
          _            <- checkCallFunc(tx)
          _            <- checkValidators(contractInfo.validationPolicy)
          _            <- Either.cond(contractInfo.active, (), ContractIsDisabled(tx.contractId))
        } yield Diff(height = height, tx = tx, portfolios = Diff.feeAssetIdPortfolio(tx, tx.sender.toAddress.toAssetHolder, blockchain))

        tx match {
          case ctx @ (_: CallContractTransactionV5 | _: CallContractTransactionV6 | _: CallContractTransactionV7) if ctx.payments.nonEmpty =>
            for {
              baseDiff      <- baseCallContractDiff
              transfersDiff <- contractTransfersDiff(blockchain, tx, ctx.payments, height)
            } yield baseDiff |+| transfersDiff
          case _ => baseCallContractDiff
        }
    }
  }

  private def checkContractVersion(tx: CallContractTransaction, ci: ContractInfo): Either[ValidationError, Unit] = {
    tx match {
      case _: CallContractTransactionV1 =>
        Either.cond(ci.version == ContractInfo.FirstVersion, (), ContractVersionMatchError(ci, ContractInfo.FirstVersion))
      case ctxV2 =>
        Either.cond(ci.version == ctxV2.contractVersion, (), ContractVersionMatchError(ci, ctxV2.contractVersion))
    }
  }

  private def checkContractIsNotLegacy(tx: CallContractTransaction): Either[ValidationError, Unit] = {
    import com.wavesenterprise.features.FeatureProvider._

    if (blockchain.isFeatureActivated(BlockchainFeature.ContractNativeTokenSupportAndPkiV1Support, height)) {
      blockchain
        .transactionInfo(tx.contractId)
        .collectFirst {
          case (_, tx: CreateContractTransaction) => tx
        }
        .toRight {
          ValidationError.GenericError(s"Failed to validate the transaction '${tx.id()}'. Create contract tx '${tx.contractId}' not found.")
        }
        .flatMap {
          case _: CreateContractTransactionV1 =>
            Left {
              ValidationError.GenericError(
                "Not allowed to call contract which was created with CreateContractTransactionV1." +
                  s"Since feature '${BlockchainFeature.ContractNativeTokenSupportAndPkiV1Support.id}' activation," +
                  "  REST-based smart-contracts are deprecated and cannot be created anymore.")
            }
          case _ =>
            Right(())
        }
    } else {
      Right(())
    }
  }

  private def checkCallFunc(tx: CallContractTransaction): Either[ValidationError, Unit] = {
    tx match {
      case ctx: CallContractTransactionV7 =>
        Either.cond(
          ctx.contractEngine == "docker" || (ctx.callFunc.nonEmpty && ctx.callFunc.get != "_constructor"),
          (),
          ValidationError.GenericError(s"tx ${tx.id.value()}: Unexpected callFunc ${ctx.callFunc} for contract ${tx.contractId}")
        )
      case _ => Right(())
    }

  }

}
