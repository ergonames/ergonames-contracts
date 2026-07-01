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
    // None

    // ===== User Defined Functions ===== //
    // None

    // ===== Relevant Variables ===== //
    val minerFeeErgoTreeHash: Coll[Byte] = fromBase16("e540cceffd3b8dd0f401193576cc413467039695969427df94454193dddfb375")
    val ergonameCollectionTokenAmount = SELF.tokens.fold(0L, { (sum: Long, t: (Coll[Byte], Long)) =>
        if (t._1 == $ergonameCollectionTokenId) sum + t._2 else sum
    })
    // Genesis 2-of-4 governance multisig (Minotaur, validated 2026-06-29).
    // Replaces the operator-derived 1-of-2 placeholder (H3 key replacement).
    val govA = PK("9g5yzitxX53B4RVi1DHLjrMx7iwTQn38kLG2XVVkhvHvcB1TcEz")
    val govB = PK("9gCJDv78SUUes6sNo81KqbP4yu3vHU6GtcatvmySiBsRoU1k4T8")
    val govC = PK("9hdYFtdV8JLXJho4wAzy6dB6DHQn4gvZsmf4vf1kbkggJ59Wn3Z")
    val govD = PK("9iJV2D1gzvWeBbSXHPgTai3S41CjoBBodxMB9DB2Dwt1kaRA9z2")
    val $ergonameMultiSigSigmaProp = atLeast(2, Coll(govA, govB, govC, govD))

    // Shape guards: reveal txs have 4 outputs, refunds 3; anything else falls
    // through to the multisig escape hatch (migrations use 2 outputs) instead
    // of throwing inside a branch and locking the collection box forever.
    val isRevealShaped: Boolean = (OUTPUTS.size == 4) && (INPUTS.size >= 2)
    val isRefundShaped: Boolean = (OUTPUTS.size == 3) && (INPUTS.size >= 2)

    if (isRevealShaped) {

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

    } else if (isRefundShaped) {

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

        $ergonameMultiSigSigmaProp

    }

}