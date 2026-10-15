{

    // ===== Contract Description ===== //
    // Name: ErgoNames Reveal Proxy
    // Description: Proxy contract used to properly create the reveal box.
    // Version: 1.0.0
    // Author: Luca D'Angelo (ldgaetano@protonmail.com)

    // ===== Box Contents ===== //
    // Tokens
    // 1. (PaymentTokenId, PaymentTokenAmount) // If ErgoName can be purchased with a custom token.
    // Registers
    // R4: Coll[Byte]       RevealR9Hash
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
    // _action: Int

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
    val revealR9Hash: Coll[Byte] = SELF.R4[Coll[Byte]].get
    val minerFee: Long = SELF.R5[Long].get
    val txOperatorFee: Long = SELF.R6[Long].get
    val userPKGroupElement: GroupElement = SELF.R7[GroupElement].get
    val userPKSigmaProp: SigmaProp = proveDlog(userPKGroupElement)

    val _action: Int = if (getVar[Int](0).isDefined) getVar[Int](0).get else 0

    if (_action == 1) {

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

                    val validR9Hash: Boolean = {
                        val r9 = revealBoxOut.R9[(GroupElement, (Coll[Coll[Byte]], Coll[Long]))]
                        if (r9.isDefined) {
                            val data = r9.get
                            val ge = data._1
                            val secrets = data._2._1
                            val fees = data._2._2
                            
                            val ge_bytes = ge.getEncoded
                            val secrets_bytes = secrets.flatMap({ (secret: Coll[Byte]) => longToByteArray(secret.size.toLong) ++ secret})
                            val fees_bytes = fees.flatMap({(fee: Long) => longToByteArray(fee)})
                            val bytes = ge_bytes ++ longToByteArray(secrets.size.toLong) ++ secrets_bytes ++ longToByteArray(fees.size.toLong) ++ fees_bytes
                            val bytes_hash = blake2b256(bytes)

                            (bytes_hash == revealR9Hash)
                        
                        } else {
                            false
                        }
                    }

                    allOf(Coll(
                        (revealBoxOut.value == SELF.value - minerFee - txOperatorFee), // Reveal box must have the miner fee + ergoname fee + min box value
                        (revealBoxOut.tokens.size == 1), // For now, since payment tokens are disabled. 
                        validR9Hash
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
                    (OUTPUTS.size == 4),
                    (SELF.tokens.size == 0)
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