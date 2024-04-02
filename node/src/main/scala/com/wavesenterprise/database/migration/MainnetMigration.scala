package com.wavesenterprise.database.migration

import com.google.common.primitives.Shorts
import com.wavesenterprise.database.KeyHelpers.hBytes
import com.wavesenterprise.database.keys.ContractCFKeys.{ContractIdsPrefix, ContractPrefix}
import com.wavesenterprise.database.rocksdb.MainDBColumnFamily.ContractCF
import com.wavesenterprise.database.rocksdb.{MainDBColumnFamily, MainReadWriteDB}
import com.wavesenterprise.database.{InternalRocksDBSet, MainDBKey, WEKeys}
import com.wavesenterprise.docker.StoredContract.DockerContract
import com.wavesenterprise.docker.{ContractApiVersion, ContractInfo}
import com.wavesenterprise.docker.validator.ValidationPolicy
import com.wavesenterprise.state.ByteStr

object MainnetMigration {

  object KeysInfo {
    def legacyContractInfoKey(contractId: ByteStr)(height: Int): MainDBKey[Option[ContractInfo]] =
      MainDBKey.opt("contract", ContractCF, hBytes(ContractPrefix, height, contractId.arr), ContractInfo.fromBytes, ContractInfo.toBytes)

    def modernContractInfoKey(contractId: ByteStr)(height: Int): MainDBKey[Option[ContractInfo]] =
      MainDBKey.opt("contract", ContractCF, hBytes(ContractPrefix, height, contractId.arr), ContractInfo.fromBytes, ContractInfo.toBytes)
  }

  private val ContractsIdSet = new InternalRocksDBSet[ByteStr, MainDBColumnFamily](
    name = "contract-ids",
    columnFamily = ContractCF,
    prefix = Shorts.toByteArray(ContractIdsPrefix),
    itemEncoder = (_: ByteStr).arr,
    itemDecoder = ByteStr(_),
    keyConstructors = MainDBKey
  )

  def apply(rw: MainReadWriteDB): Unit = {
    for {
      contractId <- ContractsIdSet.members(rw)
      contractHistory = rw.get(WEKeys.contractHistory(contractId))
      height          <- contractHistory
      oldContractInfo <- rw.get(KeysInfo.legacyContractInfoKey(contractId)(height)).toSeq
    } yield {
      oldContractInfo.storedContract match {
        case storedContract: DockerContract => {
          val newStoredContract = storedContract.copy(apiVersion = ContractApiVersion.Initial)
          val newContractInfo = oldContractInfo.copy(
            storedContract = newStoredContract,
            validationPolicy = ValidationPolicy.Default,
          )

          rw.put(KeysInfo.modernContractInfoKey(contractId)(height), Some(newContractInfo))
        }
      }
    }
  }
}
