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
    // Inputs: ErgoNameCollection, UserPK
    // Data Inputs: Commit
    // Outputs: ErgoNameCollection, Reveal, MinerFee
    // Context Variables: None
    // 2. Refund Reveal
    // Inputs: ErgoNameCollection, Reveal
    // Data Inputs: None
    // Outputs: ErgoNameCollection, UserPK, MinerFee
    // Context Variables: None

    // ===== Compile Time Constants ($) ===== //
    // $commitContractBytesHash: Coll[Byte]
    // $revealErgoTreeBytesHash: Coll[Byte]
    // $ergoNameFeeContractBytesHash: Coll[Byte]
    // $collectionUpdateNFT: Coll[Byte]

    // ===== Context Variables (_) ===== //
    // _action: Int

    // ===== User Defined Functions ===== //
    // None

    // ===== Relevant Variables ===== //
    val minerFeeErgoTreeHash: Coll[Byte] = fromBase16("e540cceffd3b8dd0f401193576cc413467039695969427df94454193dddfb375")
    val singletonTokenId: Coll[Byte]    = SELF.tokens(0)._1
    val collectionTokenId: Coll[Byte]   = SELF.tokens(1)._1
    val collectionTokenAmount: Long     = SELF.tokens(1)._2
    val minTxOperatorFee: Long = 2000000L
    val minMinerFee: Long = 1000000L
    val maxNameLength = 20
    val maxSvgBytes = 2000

    val _action: Int = if (getVar[Int](0).isDefined) getVar[Int](0).get else 0

    if (_action == 1) {

        // Data-Inputs
        val commit: Box = CONTEXT.dataInputs(0)

        // Outputs
        val ergonameCollectionBoxOut: Box = OUTPUTS(0)
        val revealBoxOut: Box = OUTPUTS(1)
        val minerFeeBoxOut: Box = OUTPUTS(2)

        val onlyOne = (INPUTS(0).id == SELF.id)

        val validSelfRecreation: Boolean = {

            allOf(Coll(
                (ergonameCollectionBoxOut.value == SELF.value),
                (ergonameCollectionBoxOut.propositionBytes == SELF.propositionBytes),
                (ergonameCollectionBoxOut.tokens(0) == SELF.tokens(0)),
                (ergonameCollectionBoxOut.tokens(1) == (collectionTokenId, collectionTokenAmount - 1L))
            ))

        }

        val validCommit: Boolean = (blake2b256(commit.propositionBytes) == $commitContractBytesHash)

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

                val emptyR6 = (Coll[(Coll[Byte], Coll[Byte])](), (Coll[(Coll[Byte], (Int, Int))](), Coll[(Coll[Byte], (Int, Int))]()))
                val emptyR8 = Coll[(Coll[Byte], Coll[Byte])]()
                allOf(Coll(
                    (revealBoxOut.R4[Int].get == 2),
                    validRoyalty,
                    (revealBoxOut.R6[(Coll[(Coll[Byte], Coll[Byte])], (Coll[(Coll[Byte], (Int, Int))], Coll[(Coll[Byte], (Int, Int))]))].get == emptyR6),
                    (revealBoxOut.R7[Coll[Byte]].get == collectionTokenId),
                    (revealBoxOut.R8[Coll[(Coll[Byte], Coll[Byte])]].get == emptyR8)
                ))

            }

            val validR9Data: Boolean = {
                if (revealBoxOut.R9[(GroupElement, (Coll[Coll[Byte]], Coll[Long]))].isDefined) {
                    
                    val data = revealBoxOut.R9[(GroupElement, (Coll[Coll[Byte]], Coll[Long]))].get
                    val fees = data._2._2
                    val secrets = data._2._1
                    if (fees.size == 3 && secrets.size == 5) {
                        
                        val validFees = {
                            (fees(0) >= minMinerFee) &&
                            (fees(1) >= minTxOperatorFee) &&
                            (fees(2) >= minBoxValue) && // Enough to leave it at this since with 360 nanoERG per byte, 0.01 ERG implies ~2.7 kB, which is way more than expected.
                            (fees(1) == commit.value) &&
                            (revealBoxOut.value > fees(0) + fees(2)) // The remaining amount is the ergoname cost.
                        }

                        val validSecrets = {
                            (secrets(0).size > 0) &&
                            (secrets(0).size <= maxNameLength) &&
                            (secrets(1).size == 32) &&
                            (secrets(2).size == 32) &&
                            (secrets(3).size == 32) &&
                            (secrets(4).size > 0) &&
                            (secrets(4).size <= maxSvgBytes) &&
                            (secrets(4).slice(0,4) == fromBase16("3c737667")) && // raw svg prefix
                            (secrets(2) == commit.id) &&
                            (secrets(0) == commit.R5[GroupElement].get)
                        }

                        val validCommitHash = {
                            commit.R4[Coll[Byte]].get == blake2b256(
                                longToByteArray(1L) ++                      // version
                                secrets(1) ++                               // commit secret
                                proveDlog(secrets(0)).propBytes ++          // user pk proposition bytes
                                longToByteArray(secrets(0).size.toLong) ++  // ergoname length
                                secrets(0) ++                               // ergoname
                                secrets(3)                                  // image data hash
                            )
                        }

                        allOf(Coll(
                            validFees,
                            validSecrets,
                            validCommitHash
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

        val validMiner: Boolean = {
            minerFeeBoxOut.value >= minMinerFee &&
            blake2b256(minerFeeBoxOut.propositionBytes) == minerFeeErgoTreeHash &&
            minerFeeBoxOut.tokens.size == 0
        }

        val validRevealTx = {
            allOf(Coll(
                onlyOne,
                validSelfRecreation,
                validCommit,
                validRevealProxy,
                validReveal,
                validMiner
            ))
        }

        sigmaProp(validRevealTx) && proveDlog(commit.R5[GroupElement].get)

    } else if (_action == 2) {

        val validRefundTx: Boolean = {

            // Inputs
            val revealBoxIn: Box = INPUTS(1)

            // Outputs
            val ergonameCollectionBoxOut: Box = OUTPUTS(0)
            val userPKBoxOut: Box = OUTPUTS(1)
            val minerFeeBoxOut: Box = OUTPUTS(2)

            val onlyOne = (INPUTS(0).id == SELF.id)

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
                onlyOne,
                validSelfRecreation,
                validRevealInput
            ))

        }

        sigmaProp(validRefundTx)

    } else {

        // Other than the collection contract itself, this can allow for new reveal and reveal proxy contracts.
        val validUpdate = (INPUTS(0).tokens(0)._1 == $collectionUpdateNFT)
        sigmaProp(validUpdate)

    }

}