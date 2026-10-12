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
    // $ergoNameFeeContractBytesHash: Coll[Byte]

    // ===== Context Variables (_) ===== //
    // _action: Int

    // ===== User Defined Functions ===== //
    // None

    // ===== Relevant Variables ===== //
    val minerFeeErgoTreeHash: Coll[Byte] = fromBase16("e540cceffd3b8dd0f401193576cc413467039695969427df94454193dddfb375")
    val singletonTokenId: Coll[Byte]    = SELF.tokens(0)._1
    val collectionTokenId: Coll[Byte]   = SELF.tokens(1)._1
    val collectionTokenAmount: Long     = SELF.tokens(1)._2

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

                allOf(Coll(
                    (ergonameCollectionBoxOut.value == SELF.value),
                    (ergonameCollectionBoxOut.propositionBytes == SELF.propositionBytes),
                    (ergonameCollectionBoxOut.tokens(0) == SELF.tokens(0)),
                    (ergonameCollectionBoxOut.tokens(1) == (collectionTokenId, collectionTokenAmount - 1L))
                ))

            }

            val validRevealProxy: Boolean = (blake2b256(revealProxyBoxIn.propositionBytes) == $revealProxyErgoTreeBytesHash)

            val validReveal: Boolean = {

                val validEIP24Registers: Boolean = {

                    // Ensure royalty must be programmatically enforced.
                    val validRoyalty: Boolean = {
                        if (revealBoxOut.R5[Coll[(Coll[Byte], Int)]].get.size == 1) {
                            val royalties = revealBoxOut.R5[Coll[(Coll[Byte], Int)]].get
                            val royalty = royalties(0)
                            allOf(Coll(
                                (blake2b256(royalty._1) == $ergoNameFeeContractBytesHash),
                                (royalty._2 == 25)
                            ))
                        } else {
                            false
                        }
                    }
                    
                    allOf(Coll(
                        (revealBoxOut.R4[Int].get == 2),
                        validRoyalty,
                        (revealBoxOut.R6[(Coll[(Coll[Byte], Coll[Byte])], (Coll[(Coll[Byte], (Int, Int))], Coll[(Coll[Byte], (Int, Int))]))].get == (Coll[(Coll[Byte], Coll[Byte])](), (Coll[(Coll[Byte], (Int, Int))](), Coll[(Coll[Byte], (Int, Int))]()))),
                        (revealBoxOut.R7[Coll[Byte]].get == collectionTokenId),
                        (revealBoxOut.R8[Coll[(Coll[Byte], Coll[Byte])]].get == Coll[(Coll[Byte], Coll[Byte])]())
                    ))

                }

                val validR9Data: Boolean = {
                    if (revealBoxOut.R9[(GroupElement, (Coll[Coll[Byte]], Coll[Long]))].isDefined) {
                        
                        val data = revealBoxOut.R9[(GroupElement, (Coll[Coll[Byte]], Coll[Long]))].get
                        val fees = data._2._2
                        val secrets = data._2._1
                        if (fees.size == 3 && secrets.size == 4) {
                            allOf(Coll(
                                (fees(0) >= 10000000L), // MinerFee
                                (fees(1) >= 20000000L), // TxOperatorFee
                                (fees(2) >= 10000000L)  // MinBoxValue
                            ))
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                }

                allOf(Coll(
                    (blake2b256(revealBoxOut.propositionBytes) == $revealErgoTreeBytesHash),
                    (revealBoxOut.tokens(0) == (collectionTokenId, 1L)),
                    validEIP24Registers,
                    validR9Data
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
                    (ergonameCollectionBoxOut.tokens(1) == (collectionTokenId, collectionTokenAmount + 1L))
                ))

            }

            val validRevealInput: Boolean = {

                allOf(Coll(
                    (blake2b256(revealBoxIn.propositionBytes) == $revealErgoTreeBytesHash),
                    (revealBoxIn.tokens(0) == (collectionTokenId, 1L))
                ))

            }

            allOf(Coll(
                validSelfRecreation,
                validRevealInput
            ))

        }

        sigmaProp(validRefundTx)

    } else {

        val adoo = PK("9iJV2D1gzvWeBbSXHPgTai3S41CjoBBodxMB9DB2Dwt1kaRA9z2")
        val lgd = PK("9hdYFtdV8JLXJho4wAzy6dB6DHQn4gvZsmf4vf1kbkggJ59Wn3Z")
        val balb = PK("9gCJDv78SUUes6sNo81KqbP4yu3vHU6GtcatvmySiBsRoU1k4T8")
        val mgpai = PK("9g5yzitxX53B4RVi1DHLjrMx7iwTQn38kLG2XVVkhvHvcB1TcEz")
        val msig_addresses: Coll[SigmaProp] = Coll(adoo, lgd, balb, mgpai)
        val min_required_signatures: Int = 2
        val ergoname_msig_sigmaprop: SigmaProp = atLeast(min_required_signatures, msig_addresses)

        ergoname_msig_sigmaprop

    }

}