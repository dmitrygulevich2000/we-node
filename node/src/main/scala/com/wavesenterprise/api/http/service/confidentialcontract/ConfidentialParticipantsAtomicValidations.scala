package com.wavesenterprise.api.http.service.confidentialcontract

import cats.implicits._
import com.wavesenterprise.account.Address
import com.wavesenterprise.api.http.ApiError
import com.wavesenterprise.docker.ContractInfo
import com.wavesenterprise.transaction.{AtomicBadge, ValidationError}

trait ConfidentialParticipantsAtomicValidations {

  protected def getConfidentialDataRecipients(confidentialContractCallRequest: ConfidentialContractCallRequest,
                                              contractInfo: Option[ContractInfo]): Either[ApiError, (Set[Address], String)] = {
    val participantsForAtomic: Either[ApiError, List[Address]] =
      confidentialContractCallRequest.participantsForAtomic.traverse(Address.fromString).leftMap(cryptoError =>
        ApiError.fromValidationError(ValidationError.fromCryptoError(cryptoError)))

    participantsForAtomic.flatMap(participants => variantsForParticipants(participants, contractInfo, confidentialContractCallRequest.atomicBadge))

  }

  private def variantsForParticipants(addresses: List[Address],
                                      contractInfo: Option[ContractInfo],
                                      atomicBadge: Option[AtomicBadge]): Either[ApiError, (Set[Address], String)] = {
    (atomicBadge, contractInfo) match {
      case (None, Some(contractInfo: ContractInfo)) =>
        (contractInfo.groupParticipants, "All right, for non-atomic transaction got participants from contractInfo on blockchain").asRight
      case (None, None)                       => ApiError.CustomValidationError("There is no such contract on blockchain for non-atomic tx").asLeft
      case (Some(_), _) if addresses.size < 3 => ApiError.CustomValidationError("Provide not less than 3 participants for atomic for call tx").asLeft
      case (Some(_), None) =>
        (addresses.toSet,
         s"You've provided participants '${addressesToString(addresses.toSet)}', " +
           s"please ensure that in your atomic before that call tx will be exactly one create tx and zero or more update txs with cumulative participants you've just provided")
          .asRight
      case (Some(_), Some(contractInfo: ContractInfo)) =>
        (addresses.toSet,
         s"You've provided participants '${addressesToString(addresses.toSet)}'" +
           s"and there is contract for that tx on blockchain with participants: '${addressesToString(contractInfo.groupParticipants)}', " +
           s"please ensure that in your atomic before that call tx will be no create tx and zero or more update tx with next cumulative effect ${deltaAtomicParticipants(addresses.toSet, contractInfo.groupParticipants)}")
          .asRight
    }
  }

  private def deltaAtomicParticipants(before: Set[Address], after: Set[Address]): String = {
    val afterDifferenceBefore = after -- before
    if (afterDifferenceBefore.nonEmpty) {
      s"Add next participants: '${addressesToString(afterDifferenceBefore)}'"
    } else {
      val beforeDifferenceAfter = before -- after
      if (beforeDifferenceAfter.nonEmpty) {
        s"Remove next participants: '${addressesToString(beforeDifferenceAfter)}'"
      } else {
        s"Participants should be remain same"
      }
    }
  }

  private def addressesToString(setOfAddresses: Set[Address]): String = {
    setOfAddresses.map(_.toString).mkString("[", ",", "]")
  }
}
