{

    // ===== Contract Description ===== //
    // Name: ErgoName Collection Contract
    // Description: Cntract holding the ErgoName collecion tokens.
    // Version: 1.0.0
    // Author: Luca D'Angelo (ldgaetano@protonmail.com)

    // ===== Box Contents ===== //
    // Tokens
    // 1. (ErgoNameCollectionTokenId, Long.MaxValue)
    // Registers
    // None

    // ===== Relevant Transactions ===== //
    // 1. Reveal
    // Inputs: ErgoNameCollection, UserPK
    // Data Inputs: None
    // Outputs: ErgoNameCollection, Reveal, MinerFee
    // Context Variables: None
    // 2. Refund Reveal
    // Inputs: ErgoNameCollection, Reveal
    // Data Inputs: None
    // Outputs: ErgoNameCollection, UserPK, MinerFee, TxOperatorFee
    // Context Variables: None

    // ===== Compile Time Constants ($) ===== //
    // $revealErgoTreeBytesHash: Coll[Byte]

    // ===== Context Variables (_) ===== //
    // None

    // ===== Relevant Variables ===== //
    val minerFeeErgoTreeHash: Coll[Byte] = fromBase16("e540cceffd3b8dd0f401193576cc413467039695969427df94454193dddfb375")
    val ergonameCollectionTokenId: Coll[Byte] = SELF.tokens(0)._1
    val ergonameCollectionTokenAmount: Long = SELF.tokens(0)._2
    val isRefund: Boolean = (OUTPUTS.size == 4)

    if (!isRefund) {

        val validRevealTx: Boolean = {

            // Outputs
            val ergonameCollectionBoxOut: Box = OUTPUTS(0)
            val revealBoxOut: Box = OUTPUTS(1)
            val minerFeeBoxOut: Box = OUTPUTS(2)

            val validSelfRecreation: Boolean = {

                allOf(Coll(
                    (ergonameCollectionBoxOut.value == SELF.value),
                    (ergonameCollectionBoxOut.propositionBytes == SELF.propositionBytes),
                    (ergonameCollectionBoxOut.tokens(0) == (ergonameCollectionTokenId, ergonameCollectionTokenAmount - 1L))
                ))

            }

            val validReveal: Boolean = {

                allOf(Coll(
                    (blake2b256(revealBoxOut.propositionBytes) == $revealErgoTreeBytesHash),
                    (revealBoxOut.tokens(0) == (ergonameCollectionTokenId, 1L)),
                    (revealBoxOut.R7[Coll[Byte]].get == ergonameCollectionTokenId) // For artwork standard v2 (EIP-24).
                ))

            }

            val validMinerFee: Boolean = (blake2b256(minerFeeBoxOut.propositionBytes) == minerFeeErgoTreeHash)

            allOf(Coll(
                validSelfRecreation,
                validReveal,
                validMinerFee
            ))

        }

        sigmaProp(validRevealTx)

    } else {

        val validRefundTx: Boolean = {

            // Inputs
            val revealBoxIn: Box = INPUTS(1)

            // Outputs
            val ergonameCollectionBoxOut: Box = OUTPUTS(0)
            val userPKBoxOut: Box = OUTPUTS(1)
            val minerFeeBoxOut: Box = OUTPUTS(2)
            val txOperatorFeeBoxOut: Box = OUTPUTS(3)

            val validSelfRecreation: Boolean = {

                allOf(Coll(
                    (ergonameCollectionBoxOut.value == SELF.value),
                    (ergonameCollectionBoxOut.propositionBytes == SELF.propositionBytes),
                    (ergonameCollectionBoxOut.tokens(0) == (ergoNameCollectionTokenId, ergonameCollectionTokenAmount + 1L))
                ))

            }

            val validRevealInput: Boolean = {

                allOf(Coll(
                    (blake2b256(revealBoxIn.propositionBytes) == $revealErgoTreeBytesHash),
                    (revealBoxIn.tokens(0) == (ergoNameCollectionTokenId, 1L))
                ))

            }

            allOf(Coll(
                validSelfRecreation,
                validRevealInput
            ))

        }

        sigmaProp(validRefundTx)

    }

}