package com.wavesenterprise.utx

import cats.implicits.catsSyntaxOptionId
import com.wavesenterprise.account.Address
import com.wavesenterprise.docker.ContractInfo
import com.wavesenterprise.network.contracts.{ConfidentialDataId, ConfidentialDataType}
import com.wavesenterprise.state.{Blockchain, ContractId}
import com.wavesenterprise.transaction.docker.{ConfidentialDataInCallContractSupported, CreateContractTransactionV6, ExecutableTransaction}
import com.wavesenterprise.transaction.{AtomicTransaction, ConfidentialContractDataUpdate, Transaction}
import com.wavesenterprise.utils.ScorexLogging
import monix.execution.Scheduler
import monix.reactive.subjects.ConcurrentSubject

trait ConfidentialOps extends ScorexLogging {

  def blockchain: Blockchain

  def nodeOwner: Address

  def confidentialScheduler: Scheduler

  protected val confidentialTransactionsInternal: ConcurrentSubject[ConfidentialContractDataUpdate, ConfidentialContractDataUpdate] =
    ConcurrentSubject.publish[ConfidentialContractDataUpdate](confidentialScheduler)

  protected def addToConfidentialTransactions(tx: Transaction): Unit = tx match {
    case tx: ConfidentialDataInCallContractSupported =>
      contractInfo(tx, Set.empty[ContractInfo]) match {
        case Some(contractInfo)
            if nodeIsContractValidator() && contractInfo.groupParticipants.contains(nodeOwner) =>
          confidentialTransactionsInternal.onNext {
            log.trace(s"Confidential tx '${tx.id()}' has been added to the stream")
            ConfidentialContractDataUpdate(
              ConfidentialDataId(ContractId(contractInfo.contractId), tx.inputCommitment, ConfidentialDataType.Input),
              tx.timestamp.some
            )
          }
        case _ => ()
      }
    case atomic: AtomicTransaction =>
      atomic.transactions.foldLeft(Set.empty[ContractInfo]) {
        case (state, tx) => (contractInfo(tx, state), tx) match {
            case (Some(contractInfo), tx: ConfidentialDataInCallContractSupported)
                if nodeIsContractValidator() && contractInfo.groupParticipants.contains(nodeOwner) =>
              {

                confidentialTransactionsInternal.onNext {
                  log.trace(s"Confidential tx '${tx.id()}' has been added to the stream")
                  ConfidentialContractDataUpdate(
                    ConfidentialDataId(ContractId(contractInfo.contractId), tx.inputCommitment, ConfidentialDataType.Input, Some(contractInfo)),
                    tx.timestamp.some
                  )
                }
              }
              state
            case (None, tx: CreateContractTransactionV6) =>
              state + ContractInfo(tx)
            case _ => state
          }
      }
    case _ => ()
  }

  private def nodeIsContractValidator(): Boolean = blockchain.lastBlockContractValidators.contains(nodeOwner)

  private def contractInfo(tx: Transaction, state: Set[ContractInfo]): Option[ContractInfo] = tx match {
    case executableTx: ExecutableTransaction =>
      blockchain.contract(ContractId(executableTx.contractId))
        .orElse(state.find(_.contractId == executableTx.contractId))
        .filter(_.isConfidential)
    case _ => None
  }

}
