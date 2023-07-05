package com.wavesenterprise.docker

import akka.actor.ActorSystem
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}
import com.wavesenterprise.account.PrivateKeyAccount
import com.wavesenterprise.api.http.service.{AddressApiService, ContractsApiService, PermissionApiService}
import com.wavesenterprise.block.Block.BlockId
import com.wavesenterprise.docker.grpc.GrpcDockerContractExecutor
import com.wavesenterprise.docker.grpc.service._
import com.wavesenterprise.docker.validator.{ContractValidatorResultsStore, ExecutableTransactionsValidator}
import com.wavesenterprise.mining.{TransactionsAccumulator, TransactionsAccumulatorProvider}
import com.wavesenterprise.network.peers.ActivePeerConnections
import com.wavesenterprise.protobuf.service.contract._
import com.wavesenterprise.settings.WESettings
import com.wavesenterprise.state.reader.DelegatingBlockchain
import com.wavesenterprise.state.{Blockchain, ByteStr, MiningConstraintsHolder, NG}
import com.wavesenterprise.transaction.BlockchainUpdater
import com.wavesenterprise.utils.{NTP, ScorexLogging}
import com.wavesenterprise.utx.UtxPool
import com.wavesenterprise.wallet.Wallet
import monix.execution.Scheduler

import scala.concurrent.Future

case class ContractExecutionComponents(
    settings: WESettings,
    nodeOwnerAccount: PrivateKeyAccount,
    dockerEngine: DockerEngine,
    time: NTP,
    utx: UtxPool,
    blockchain: Blockchain with NG with MiningConstraintsHolder,
    activePeerConnections: ActivePeerConnections,
    contractReusedContainers: ContractReusedContainers,
    grpcContractExecutor: GrpcDockerContractExecutor,
    contractExecutionMessagesCache: ContractExecutionMessagesCache,
    contractValidatorResultsStore: ContractValidatorResultsStore,
    contractAuthTokenService: ContractAuthTokenService,
    dockerExecutorScheduler: Scheduler,
    partialHandlers: Seq[PartialFunction[HttpRequest, Future[HttpResponse]]],
    private val delegatingState: DelegatingBlockchain
) extends AutoCloseable
    with ScorexLogging {

  def setDelegatingState(blockchain: Blockchain): Unit =
    delegatingState.setState(blockchain)

  def createMinerExecutor(transactionsAccumulator: TransactionsAccumulator, keyBlockId: BlockId): MinerTransactionsExecutor = {
    new MinerTransactionsExecutor(
      messagesCache = contractExecutionMessagesCache,
      transactionsAccumulator = transactionsAccumulator,
      nodeOwnerAccount = nodeOwnerAccount,
      utx = utx,
      blockchain = blockchain,
      time = time,
      grpcContractExecutor = grpcContractExecutor,
      contractValidatorResultsStore = contractValidatorResultsStore,
      keyBlockId = keyBlockId,
      parallelism = settings.dockerEngine.contractsParallelism.value
    )(dockerExecutorScheduler)
  }

  def createTransactionValidator(transactionsAccumulatorProvider: TransactionsAccumulatorProvider): ExecutableTransactionsValidator = {
    new ExecutableTransactionsValidator(
      ownerKey = nodeOwnerAccount,
      transactionsAccumulatorProvider = transactionsAccumulatorProvider,
      contractExecutionComponentsOpt = Some(this),
      utx = utx,
      blockchain = blockchain,
      time = time,
      pullingBufferSize = settings.miner.pullingBufferSize,
      utxCheckDelay = settings.miner.utxCheckDelay
    )(dockerExecutorScheduler)
  }

  def createValidatorExecutor(transactionsAccumulator: TransactionsAccumulator, keyBlockId: ByteStr): ValidatorTransactionsExecutor = {
    new ValidatorTransactionsExecutor(
      nodeOwnerAccount = nodeOwnerAccount,
      transactionsAccumulator = transactionsAccumulator,
      messagesCache = contractExecutionMessagesCache,
      utx = utx,
      blockchain = blockchain,
      time = time,
      activePeerConnections = activePeerConnections,
      grpcContractExecutor = grpcContractExecutor,
      keyBlockId = keyBlockId,
      parallelism = settings.dockerEngine.contractsParallelism.value
    )(dockerExecutorScheduler)
  }

  override def close(): Unit = {
    log.info("Stopping running docker containers with contracts")
    contractReusedContainers.close()
    dockerEngine.stopAllRunningContainers()
    log.info("Stopping contract execution messages cache")
    contractExecutionMessagesCache.close()
  }
}

object ContractExecutionComponents extends ScorexLogging {

  def apply(
      settings: WESettings,
      dockerExecutorScheduler: Scheduler,
      contractExecutionMessagesCache: ContractExecutionMessagesCache,
      blockchainUpdater: BlockchainUpdater with NG with MiningConstraintsHolder,
      nodeOwnerAccount: PrivateKeyAccount,
      utx: UtxPool,
      time: NTP,
      wallet: Wallet,
      privacyServiceImpl: PrivacyServiceImpl,
      activePeerConnections: ActivePeerConnections,
      grpcContractExecutor: GrpcDockerContractExecutor,
      dockerEngine: DockerEngine,
      contractAuthTokenService: ContractAuthTokenService,
      contractReusedContainers: ContractReusedContainers,
  )(implicit grpcSystem: ActorSystem): ContractExecutionComponents = {
    val contractValidatorResultsStore = new ContractValidatorResultsStore
    val delegatingState               = new DelegatingBlockchain(blockchainUpdater)
    val contractsApiService           = new ContractsApiService(delegatingState, contractExecutionMessagesCache)

    val partialHandlers =
      Seq(
        AddressServicePowerApiHandler.partial(
          new AddressServiceImpl(new AddressApiService(delegatingState, wallet), contractAuthTokenService, dockerExecutorScheduler)),
        BlockServicePowerApiHandler.partial(new BlockServiceImpl(blockchainUpdater, contractAuthTokenService, dockerExecutorScheduler)),
        ContractServicePowerApiHandler.partial(
          new ContractServiceImpl(grpcContractExecutor, contractsApiService, contractAuthTokenService, dockerExecutorScheduler)),
        PermissionServicePowerApiHandler.partial(
          new PermissionServiceImpl(time, new PermissionApiService(blockchainUpdater), contractAuthTokenService, dockerExecutorScheduler)),
        PrivacyServicePowerApiHandler.partial(privacyServiceImpl),
        UtilServicePowerApiHandler.partial(new UtilServiceImpl(time, contractAuthTokenService, dockerExecutorScheduler)),
        TransactionServicePowerApiHandler.partial(new TransactionServiceImpl(delegatingState, contractAuthTokenService, dockerExecutorScheduler))
      )

    new ContractExecutionComponents(
      settings,
      nodeOwnerAccount,
      dockerEngine,
      time,
      utx,
      blockchainUpdater,
      activePeerConnections,
      contractReusedContainers,
      grpcContractExecutor,
      contractExecutionMessagesCache,
      contractValidatorResultsStore,
      contractAuthTokenService,
      dockerExecutorScheduler,
      partialHandlers,
      delegatingState
    )
  }
}
