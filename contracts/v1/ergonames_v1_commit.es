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
    // R7: Long             TxOperatorFee

    // ===== Relevant Transactions ===== //
    // 1. Mint ErgoName
    // Inputs: Reveal, Registry, Commit
    // Data Inputs: SigUsdOracleDatapoint, ?ErgoDexErg2Token, ?Config
    // Outputs: ErgoNameIssuance, Registry, SubNameRegistry, ErgoNameFee, MinerFee, TxOperatorFee
    // Context Variables: None
    // 2. Refund
    // Inputs: Commit
    // Data Inputs: None
    // Outputs: UserPK, MinerFee, TxOperatorFee
    // Context Variables: None

    // ===== Compile Time Constants ($) ===== //
    // $registrySingletonTokenId: Coll[Byte]
    // $maxCommitBoxAge: Int

    // ===== Context Variables (_) ===== //
    // None

    val minerFeeErgoTreeHash = fromBase16("e540cceffd3b8dd0f401193576cc413467039695969427df94454193dddfb375")

    // ===== Relevant Variables ===== //
    val userPKGroupElement: GroupElement = SELF.R5[GroupElement].get
    val userPKSigmaProp: SigmaProp = proveDlog(userPKGroupElement)
    val minerFee: Long = SELF.R6[Long].get
    val txOperatorFee: Long = SELF.R7[Long].get
    val creationHeight: Int = SELF.creationInfo._1
    val isOldEnough: Boolean = HEIGHT - creationHeight >= $maxCommitBoxAge
    val isRefund: Boolean = (OUTPUTS.size == 3) && isOldEnough

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
            val txOperatorFeeBoxOut: Box = OUTPUTS(2)

            val validUser: Boolean = {

                allOf(Coll(
                    (userPKBoxOut.value == SELF.value - minerFee - txOperatorFee),
                    (userPKBoxOut.propositionBytes == userPKSigmaProp.propBytes)
                ))

            }

            val validMinerFee: Boolean = {

                allOf(Coll(
                    (minerFeeBoxOut.value == minerFee),
                    (blake2b256(minerFeeBoxOut.propositionBytes) == minerFeeErgoTreeHash)
                ))

            }

            val validTxOperatorFee: Boolean = {

                (txOperatorFeeBoxOut.value == txOperatorFee)

            }

            allOf(Coll(
                validUser,
                validMinerFee,
                validTxOperatorFee
            ))

        }

        sigmaProp(validRefundTx)

    }

}