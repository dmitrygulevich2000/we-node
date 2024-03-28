package com.wavesenterprise.network.contracts

import com.wavesenterprise.crypto.internals.confidentialcontracts.Commitment
import com.wavesenterprise.docker.ContractInfo
import com.wavesenterprise.state.ContractId

case class ConfidentialDataId(contractId: ContractId,
                              commitment: Commitment,
                              dataType: ConfidentialDataType,
                              contractInfo: Option[ContractInfo] = None) {
  override def toString: String = s"ConfidentialDataId(contractId: '$contractId', commitment: '${commitment.hash}', dataType: '$dataType')"
}
