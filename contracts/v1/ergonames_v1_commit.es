{
    // ===== Contract Description ===== //
    // Name: ErgoNames Commit Contract
    // Description: User commits their intention to register an ErgoName.
    // Version: 1.0.0
    // Author: Luca D'Angelo (ldgaetano@protonmail.com)

    // ===== Box Contents ===== //
    // Tokens
    // None
    // Registers
    // R4: Coll[Byte]       CommitHash
    // R5: GroupElement     UserPKGroupElement
    // R6: Long             MinerFee

    // ===== Relevant Transactions ===== //
    // 1. Mint ErgoName
    // Inputs: Reveal, Registry, Commit
    // Data Inputs: SigUsdOracleDatapoint, ?ErgoDexErg2Token, ?Config
    // Outputs: ErgoNameIssuance, Registry, SubNameRegistry, ErgoNameFee, MinerFee, TxOperatorFee
    // Context Variables: None
    // 2. Refund
    // Inputs: Commit
    // Data Inputs: None
    // Outputs: UserPK, MinerFee
    // Context Variables: None

    // ===== Compile Time Constants ($) ===== //
    // $registrySingletonTokenId: Coll[Byte]
    // $maxCommitBoxAge: Int

    // ===== Context Variables (_) ===== //
    // None

    // ===== User Defined Functions ===== //
    // def isSigmaPropEqualToBoxProp: ((SigmaProp, Box) => Boolean)

    def isSigmaPropEqualToBoxProp(propAndBox: (SigmaProp, Box)): Boolean = {

        val prop: SigmaProp = propAndBox._1
        val box: Box = propAndBox._2

        val propBytes: Coll[Byte] = prop.propBytes
        val treeBytes: Coll[Byte] = box.propositionBytes

        if (treeBytes(0) == 0) {

            (treeBytes == propBytes)

        } else {

            // offset = 1 + <number of VLQ encoded bytes to store propositionBytes.size>
            val offset = if (treeBytes.size > 127) 3 else 2
            (propBytes.slice(1, propBytes.size) == treeBytes.slice(offset, treeBytes.size)) 

        }        

    }

    // ===== Relevant Variables ===== //
    val minerFeeErgoTreeHash = fromBase16("e540cceffd3b8dd0f401193576cc413467039695969427df94454193dddfb375")
    val userPKGroupElement: GroupElement = SELF.R5[GroupElement].get
    val userPKSigmaProp: SigmaProp = proveDlog(userPKGroupElement)
    val minerFee: Long = SELF.R6[Long].get
    val creationHeight: Int = SELF.creationInfo._1
    val isOldEnough: Boolean = HEIGHT - creationHeight >= $maxCommitBoxAge
    val isRefund: Boolean = (OUTPUTS.size == 2) && isOldEnough

    if (!isRefund) {

        // ===== Mint ErgoName Tx ===== //
        val validMintErgoNameTx: Boolean = {

            // Inputs
            val registryBoxIn: Box = INPUTS(1)


            val validSingletonTokenId: Boolean = {

                (registryBoxIn.tokens(0)._1 == $registrySingletonTokenId)

            }

            allOf(Coll(
                validSingletonTokenId
            ))

        }

        sigmaProp(validMintErgoNameTx)

    } else {

        // ===== Refund Tx ===== //
        val validRefundTx: Boolean = {

            // Outputs
            val userPKBoxOut: Box = OUTPUTS(0)
            val minerFeeBoxOut: Box = OUTPUTS(1)

            val validUser: Boolean = {

                val propAndBox: (SigmaProp, Box) = (userPKSigmaProp, userPKBoxOut)

                allOf(Coll(
                    (userPKBoxOut.value == SELF.value - minerFee),
                    isSigmaPropEqualToBoxProp(propAndBox)
                ))

            }

            val validMinerFee: Boolean = {

                allOf(Coll(
                    (minerFeeBoxOut.value == minerFee),
                    (blake2b256(minerFeeBoxOut.propositionBytes) == minerFeeErgoTreeHash)
                ))

            }

            allOf(Coll(
                validUser,
                validMinerFee
            ))

        }

        sigmaProp(validRefundTx)

    }

}