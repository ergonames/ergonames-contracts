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
    // $ergonameCollectionSingletonTokenId: Coll[Byte]
    // $ergonameCollectionTokenId: Coll[Byte]

    // ===== Context Variables (_) ===== //
    // _action: Int

    // ===== User Defined Functions ===== //
    // None

    // ===== Relevant Variables ===== //
    val minerFeeErgoTreeHash: Coll[Byte] = fromBase16("e540cceffd3b8dd0f401193576cc413467039695969427df94454193dddfb375")
    val ergonameCollectionTokenAmount = SELF.tokens.fold(0L, { (sum: Long, t: (Coll[Byte], Long)) =>
        if (t._1 == $ergonameCollectionTokenId) sum + t._2 else sum
    })

    val _action: Int = if (getVar[Int](0).isDefined) getVar[Int](0).get else 0

    if (_action == 1) {

        val validRevealTx: Boolean = {

            // Inputs
            val revealProxyBoxIn: Box = INPUTS(1)

            // Outputs
            val ergonameCollectionBoxOut: Box = OUTPUTS(0)
            val revealBoxOut: Box = OUTPUTS(1)
            val minerFeeBoxOut: Box = OUTPUTS(2)
            val txOperatorFeeBoxOut: Box = OUTPUTS(3)

            val validSelfRecreation: Boolean = {

                val validSingletonTransfer: Boolean = {

                    ergonameCollectionBoxOut.tokens.exists({ (t: (Coll[Byte], Long)) =>
                        t._1 == $ergonameCollectionSingletonTokenId
                    })

                }

                val validCollectionTokensTransfer: Boolean = {

                    ergonameCollectionBoxOut.tokens.exists({ (t: (Coll[Byte], Long)) =>
                        t == ($ergonameCollectionTokenId, ergonameCollectionTokenAmount - 1L)
                    })

                }

                allOf(Coll(
                    (ergonameCollectionBoxOut.value == SELF.value),
                    (ergonameCollectionBoxOut.propositionBytes == SELF.propositionBytes),
                    validSingletonTransfer,
                    validCollectionTokensTransfer
                ))

            }

            val validRevealProxy: Boolean = (blake2b256(revealProxyBoxIn.propositionBytes) == $revealProxyErgoTreeBytesHash)

            val validReveal: Boolean = {

                allOf(Coll(
                    (blake2b256(revealBoxOut.propositionBytes) == $revealErgoTreeBytesHash),
                    (revealBoxOut.tokens(0) == ($ergonameCollectionTokenId, 1L)),
                    (revealBoxOut.R7[Coll[Byte]].get == $ergonameCollectionTokenId), // For artwork standard v2 (EIP-24).
                ))

            }

            allOf(Coll(
                validSelfRecreation,
                validRevealProxy,
                validReveal
            ))

        }

        sigmaProp(validRevealTx)

    } else if (_action == 2) {

        val validRefundTx: Boolean = {

            // Inputs
            val revealBoxIn: Box = INPUTS(1)

            // Outputs
            val ergonameCollectionBoxOut: Box = OUTPUTS(0)
            val userPKBoxOut: Box = OUTPUTS(1)
            val minerFeeBoxOut: Box = OUTPUTS(2)

            val validSelfRecreation: Boolean = {

                allOf(Coll(
                    (ergonameCollectionBoxOut.value == SELF.value),
                    (ergonameCollectionBoxOut.propositionBytes == SELF.propositionBytes),
                    (ergonameCollectionBoxOut.tokens(0) == SELF.tokens(0)),
                    (ergonameCollectionBoxOut.tokens(1) == ($ergonameCollectionTokenId, ergonameCollectionTokenAmount + 1L))
                ))

            }

            val validRevealInput: Boolean = {

                allOf(Coll(
                    (blake2b256(revealBoxIn.propositionBytes) == $revealErgoTreeBytesHash),
                    (revealBoxIn.tokens(0) == ($ergonameCollectionTokenId, 1L))
                ))

            }

            allOf(Coll(
                validSelfRecreation,
                validRevealInput
            ))

        }

        sigmaProp(validRefundTx)

    } else {

        val adoo = PK("3WvubspBMttcKU97e6oAKdjgaXmoVUDDi6aKdt3in9zTvzSUTxto")
        val lgd = PK("3WxJrwDLXgGE53KpdJ2nSjSMRdXaDWh7Fdz9MY2Zh37UAwfLXzBU")
        val balb = PK("3WvubspBMttcKU97e6oAKdjgaXmoVUDDi6aKdt3in9zTvzSUTxto")
        val mgpai = PK("3WxJrwDLXgGE53KpdJ2nSjSMRdXaDWh7Fdz9MY2Zh37UAwfLXzBU")
        val addresses: Coll[SigmaProp] = Coll(adoo, lgd, balb, mgpai)
        val minRequiredSignatures = 2
        val ergonameMultiSigSigmaProp = atLeast(minRequiredSignatures, addresses)

        ergonameMultiSigSigmaProp

    }

}