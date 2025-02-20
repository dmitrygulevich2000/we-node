package com.wavesenterprise.network.peers

import com.wavesenterprise.acl.{Permissions, Role}
import com.wavesenterprise.network.Attributes._
import io.netty.channel.Channel

trait MinerOrValidatorAttrSet {
  def setMinerOrValidatorAttr(channel: Channel, permissions: Permissions, timeStamp: Long): Unit = {
    val isValidator = permissions.contains(Role.ContractValidator, timeStamp)
    val isMiner     = permissions.contains(Role.Miner, timeStamp)
    if (isMiner) channel.setAttrWithLogging(MinerAttribute, ()) else channel.removeAttrWithLogging(MinerAttribute)
    if (isValidator) channel.setAttrWithLogging(ValidatorAttribute, ()) else channel.removeAttrWithLogging(ValidatorAttribute)
  }
}
