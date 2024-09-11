{

    // ===== Contract Description ===== //
    // Name: ErgoNames Collection Contract
    // Description: Contract holding the ErgoName collecion tokens.
    // Version: 1.0.0
    // Author: Luca D'Angelo (ldgaetano@protonmail.com)

    // ===== Box Contents ===== //
    // Tokens
    // 1. (ErgoNameCollectionSingletonTokenId, 1L)
    // 2. (ErgoNameCollectionTokenId, Long.MaxValue)
    // Registers
    // None

    // ===== Relevant Transactions ===== //
    // 1. Reveal
    // Inputs: ErgoNameCollection, RevealProxy
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
    // $revealProxyErgoTreeBytesHash: Coll[Byte]

    // ===== Context Variables (_) ===== //
    // None

    // ===== Relevant Variables ===== //
    val minerFeeErgoTreeHash: Coll[Byte] = fromBase16("e540cceffd3b8dd0f401193576cc413467039695969427df94454193dddfb375")
    val ergonameCollectionTokenId: Coll[Byte] = SELF.tokens(1)._1
    val ergonameCollectionTokenAmount: Long = SELF.tokens(1)._2
    val isRefund: Boolean = (OUTPUTS.size == 4)

    if (!isRefund) {

        val validRevealTx: Boolean = {

            // Inputs
            val revealProxyBoxIn: Box = INPUTS(1)

            // Outputs
            val ergonameCollectionBoxOut: Box = OUTPUTS(0)
            val revealBoxOut: Box = OUTPUTS(1)
            val minerFeeBoxOut: Box = OUTPUTS(2)

            // Relevant Variables
            val minerFee: Long = revealProxyBoxIn.R4[Long].get
            val revealBoxHash: Coll[Byte] = revealProxyBoxIn.R5[Coll[Byte]].get

            val validSelfRecreation: Boolean = {

                allOf(Coll(
                    (ergonameCollectionBoxOut.value == SELF.value),
                    (ergonameCollectionBoxOut.propositionBytes == SELF.propositionBytes),
                    (ergonameCollectionBoxOut.tokens(0) == SELF.tokens(0)),
                    (ergonameCollectionBoxOut.tokens(1) == (ergonameCollectionTokenId, ergonameCollectionTokenAmount - 1L))
                ))

            }

            val validRevealProxy: Boolean = (blake2b256(revealBoxIn.propositionBytes) == $revealProxyErgoTreeBytesHash)

            val validReveal: Boolean = {
                               
                allOf(Coll(
                    (blake2b256(revealBoxOut.propositionBytes) == $revealErgoTreeBytesHash),
                    (revealBoxOut.tokens(0) == (ergonameCollectionTokenId, 1L)),
                    (revealBoxOut.R7[Coll[Byte]].get == ergonameCollectionTokenId), // For artwork standard v2 (EIP-24).
                ))

            }

            allOf(Coll(
                validSelfRecreation,
                validRevealProxy,
                validReveal
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
                    (ergonameCollectionBoxOut.tokens(0) == SELF.tokens(0)),
                    (ergonameCollectionBoxOut.tokens(1) == (ergonameCollectionTokenId, ergonameCollectionTokenAmount + 1L))
                ))

            }

            val validRevealInput: Boolean = {

                allOf(Coll(
                    (blake2b256(revealBoxIn.propositionBytes) == $revealErgoTreeBytesHash),
                    (revealBoxIn.tokens(0) == (ergonameCollectionTokenId, 1L))
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