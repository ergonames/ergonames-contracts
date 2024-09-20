{

    // ===== Contract Description ===== //
    // Name: ErgoNames Reveal Proxy
    // Description: Contract holding the ErgoName collecion tokens.
    // Version: 1.0.0
    // Author: Luca D'Angelo (ldgaetano@protonmail.com)

    // ===== Box Contents ===== //
    // Tokens
    // 1. (PaymentTokenId, PaymentTokenAmount) // If ErgoName can be purchased with a custom token.
    // Registers
    // R4: Coll[Byte]       RevealBoxHash
    // R5: Long             MinerFee
    // R6: Long             TxOperatorFee
    // R7: GroupElement     UserPKGroupElement

    // ===== Relevant Transactions ===== //
    // 1. Reveal
    // Inputs: ErgoNameCollection, RevealProxy
    // Data Inputs: None
    // Outputs: ErgoNameCollection, Reveal, MinerFee, TxOperatorFee
    // Context Variables: None
    // 2. Refund Reveal Proxy
    // Inputs: RevealProxy
    // DataInputs: None
    // Outputs: UserPKBoxOut, MinerFee
    // Context Variables: None

    // ===== Compile Time Constants ($) ===== //
    // $ergoNameCollectionSingletonTokenId: Coll[Byte]

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
    val minerFeeErgoTreeHash: Coll[Byte] = fromBase16("e540cceffd3b8dd0f401193576cc413467039695969427df94454193dddfb375")
    val revealBoxHash: Coll[Byte] = SELF.R4[Coll[Byte]].get
    val minerFee: Long = SELF.R5[Long].get
    val txOperatorFee: Long = SELF.R6[Long].get
    val userPKGroupElement: GroupElement = SELF.R7[GroupElement].get
    val userPKSigmaProp: SigmaProp = proveDlog(userPKGroupElement)
    val isPayingWithToken: Boolean = (SELF.tokens.size == 1)
    val isRefund: Boolean = (OUTPUTS.size == 2)

    if (!isRefund) {

        val validRevealTx: Boolean = {

                // Inputs
                val ergonameCollectionBoxIn: Box = INPUTS(0)

                // Outputs
                val ergonameCollectionBoxOut: Box = OUTPUTS(0)
                val revealBoxOut: Box = OUTPUTS(1)
                val minerFeeBoxOut: Box = OUTPUTS(2)
                val txOperatorFeeBoxOut: Box = OUTPUTS(3)

                val validCollection: Boolean = {

                    ergonameCollectionBoxIn.tokens.exists({ (t: (Coll[Byte], Long)) =>
                        t._1 == $ergoNameCollectionSingletonTokenId
                    })

                }

                val validReveal: Boolean = {

                    val revealHash: Coll[Byte] = blake2b256(revealBoxOut.bytesWithoutRef) // Bytes of box contents without transaction id and output index.
                    val validPaymentToken: Boolean = if (isPayingWithToken) (revealBoxOut.tokens(1) == SELF.tokens(0)) else true

                    allOf(Coll(
                        (revealBoxOut.value == SELF.value - minerFee - txOperatorFee),
                        validPaymentToken,
                        (revealHash == revealBoxHash)
                    ))

                }

                val validMinerFee: Boolean = {

                    allOf(Coll(
                        (minerFeeBoxOut.value == minerFee),
                        (blake2b256(minerFeeBoxOut.propositionBytes) == minerFeeErgoTreeHash)
                    ))

                }

                val validTxOperatorFee: Boolean = (txOperatorFeeBoxOut.value == txOperatorFee)

                allOf(Coll(
                    validCollection,
                    validReveal,
                    validMinerFee,
                    validTxOperatorFee,
                    (OUTPUTS.size == 4)
                ))

            }

            sigmaProp(validRevealTx)

    } else {

        val validRefundTx: Boolean = {

            // Outputs
            val userPKBoxOut: Box = OUTPUTS(0)
            val minerFeeBoxOut: Box = OUTPUTS(1)

            val validUser: Boolean = {

                val propAndBox: (SigmaProp, Box) = (userPKSigmaProp, userPKBoxOut)

                allOf(Coll(
                    (userPKBoxOut.value == SELF.value - minerFee),
                    isSigmaPropEqualToBoxProp(propAndBox),
                    (userPKBoxOut.tokens == SELF.tokens)
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

        sigmaProp(validRefundTx) && userPKSigmaProp

    }

}