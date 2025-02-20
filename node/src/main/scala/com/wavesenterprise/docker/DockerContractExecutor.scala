package com.wavesenterprise.docker

import com.wavesenterprise.docker.ContractExecutionError.{NodeFailure, RecoverableErrorCode}
import com.wavesenterprise.docker.StoredContract.DockerContract
import com.wavesenterprise.{ContractExecutor, getDockerContract}
import com.wavesenterprise.docker.exceptions.FatalExceptionsMatchers._
import com.wavesenterprise.metrics.docker.{ContractExecutionMetrics, UpdateContractTx}
import com.wavesenterprise.settings.dockerengine.{CircuitBreakerSettings, DockerEngineSettings}
import com.wavesenterprise.state.ByteStr
import com.wavesenterprise.state.contracts.confidential.ConfidentialInput
import com.wavesenterprise.transaction.docker.{CallContractTransaction, CreateContractTransaction, ExecutableTransaction, UpdateContractTransaction}
import com.wavesenterprise.utils.ScorexLogging
import monix.eval.Task
import monix.execution.Scheduler
import play.api.libs.json.{JsValue, Json, OFormat}

import scala.concurrent.Future

trait DockerContractExecutor extends ContractExecutor with ScorexLogging with CircuitBreakerSupport {

  import DockerContractExecutor._

  def dockerEngine: DockerEngine

  def dockerEngineSettings: DockerEngineSettings

  def scheduler: Scheduler

  def contractReusedContainers: ContractReusedContainers

  override protected def circuitBreakerSettings: CircuitBreakerSettings = dockerEngineSettings.circuitBreaker

  def contractStarted(contract: ContractInfo): Task[Boolean] = Task {
    contractReusedContainers.isStarted(ContainerKey(contract))
  }

  def startContract(contract: ContractInfo, metrics: ContractExecutionMetrics): Future[String] = startOrReuseContainer(contract, metrics)

  private def startOrReuseContainer(contract: ContractInfo, metrics: ContractExecutionMetrics): Future[String] = {
    val setup = StartContractSetup(ContainerKey(contract), startContractInternal(contract, metrics), invalidateContainer)
    contractReusedContainers.startOrReuse(setup)
  }

  private def startContractInternal(contract: ContractInfo, metrics: ContractExecutionMetrics): Task[String] =
    Task(log.debug(s"Starting contract '${contract.contractId}' container")) *>
      protect(contract, executionExceptionsMatcher) {
        for {
          containerId <- startContainer(contract, metrics)
          _           <- waitConnection(containerId, contract, metrics).onErrorHandleWith(onConnectionFailed(containerId))
        } yield containerId
      }

  private def onConnectionFailed(containerId: String)(throwable: Throwable): Task[String] =
    Task {
      if (dockerEngineSettings.removeContainerOnFail) removeContainer(containerId)
    } >> Task.raiseError(throwable)

  protected def startContainer(contract: ContractInfo, metrics: ContractExecutionMetrics): Task[String]

  protected def waitConnection(containerId: String, contract: ContractInfo, metrics: ContractExecutionMetrics): Task[Unit] = Task.unit

  def executeTransaction(contract: ContractInfo,
                         tx: ExecutableTransaction,
                         maybeConfidentialInput: Option[ConfidentialInput],
                         metrics: ContractExecutionMetrics): Task[ContractExecution] =
    protect(contract, tx.id(), executionExceptionsMatcher) {
      tx match {
        case create: CreateContractTransaction =>
          executeWithContainer(contract, containerId => executeCreate(containerId, contract, create, metrics))
        case call: CallContractTransaction =>
          executeWithContainer(contract, containerId => executeCall(containerId, contract, call, maybeConfidentialInput, metrics))
        case _: UpdateContractTransaction => Task.pure(ContractUpdateSuccess)
      }
    }.doOnCancel {
      Task.eval(log.trace(s"Contract '${contract.contractId}' execution was cancelled"))
    }.onErrorRecover {
      case err: ContractExecutionException
          if err.code.contains(RecoverableErrorCode) =>
        log.trace(s"Tx $tx got error $err")
        ContractExecutionError(RecoverableErrorCode, err.getMessage)
      case err =>
        log.trace(s"Tx $tx got error $err")
        ContractExecutionError(NodeFailure, err.getMessage)
    }

  private def executeWithContainer(contract: ContractInfo, executeFunction: String => Task[ContractExecution]): Task[ContractExecution] = {
    val storedContract = getDockerContract(contract)
    contractReusedContainers.getStarted(ContainerKey(storedContract)).flatMap { containerId =>
      executeFunction(containerId)
        .timeoutTo(
          dockerEngineSettings.executionLimits.timeout,
          Task.raiseError(new ContractExecutionException(
            s"Contract '${contract.contractId}' execution timeout, container '$containerId'",
            Some(RecoverableErrorCode)
          ))
        )
    }
  }

  protected def executeCreate(containerId: String,
                              contract: ContractInfo,
                              tx: CreateContractTransaction,
                              metrics: ContractExecutionMetrics): Task[ContractExecution]

  def contractExists(contract: ContractInfo): Task[Boolean] =
    protect(contract, executionExceptionsMatcher) {
      deferEither(dockerEngine.imageExists(contract))
    }

  /**
    * Pulls a new image and checks it's digest against given imageHash.
    * Doesn't actually run a contract.
    */
  def inspectOrPullContract(contract: ContractInfo, metrics: ContractExecutionMetrics): Future[Unit] =
    protect(contract, prepareExecutionExceptionsMatcher) {
      log.trace(s"inspectOrPullContract $contract")
      metrics.measureTask(
        UpdateContractTx,
        deferEither(dockerEngine.inspectContractImage(contract, metrics)).void
      )
    }.executeAsync.runToFuture(scheduler)

  protected def invalidateContainer(containerKey: ContainerKey, containerId: String): Unit = {
    log.debug(s"Removing container for image '${containerKey.image}'")
    removeContainer(containerId)
  }

  private def removeContainer(containerId: String): Unit = {
    scheduler.executeAsync(() => dockerEngine.removeContainer(containerId))
  }
}

object DockerContractExecutor {

  case class ContainerKey(imageHash: String, image: String)

  object ContainerKey {
    def apply(ci: DockerContract): ContainerKey = new ContainerKey(ci.imageHash, ci.image)
    def apply(ci: ContractInfo): ContainerKey = {
      val image = ci.storedContract.asInstanceOf[DockerContract]
      ContainerKey(image)
    }
  }

  case class ContractTxClaimContent(txId: ByteStr, contractId: ByteStr, executionId: String = "") extends ClaimContent {
    override def toJson: JsValue = Json.toJson(this)
  }

  object ContractTxClaimContent {
    implicit val format: OFormat[ContractTxClaimContent] = Json.format
  }

  case class StartContractSetup(containerKey: ContainerKey, startTask: Task[String], onInvalidate: (ContainerKey, String) => Unit)
}
