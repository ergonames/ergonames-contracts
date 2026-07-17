{

    // ===== Contract Description ===== //
    // Name: ErgoNames Reveal Contract
    // Description: User reveals their ErgoName registration secret.
    // Version: 1.0.0
    // Author: Luca D'Angelo (ldgaetano@protonmail.com)

    // ===== Box Contents ===== //
    // Tokens
    // 1. (ErgoNameCollectionTokenId, 1L)
    // 2. (PaymentTokenId, PaymentTokenAmont) // If ErgoName can be purchased with a custom token.
    // Registers
    // R4: Int                                                                                                  ArtworkStandardVersion
    // R5: Coll[(Coll[Byte], Int)]                                                                              ArtworkRoyaltyRecipients
    // R6: (Coll[(Coll[Byte], Coll[Byte])], (Coll[(Coll[Byte], (Int, Int))], Coll[(Coll[Byte], (Int, Int))]))   ArtworkTraits
    // R7: Coll[Byte]                                                                                           ArtworkCollectionTokenId
    // R8: Coll[(Coll[Byte], Coll[Byte])]                                                                       ArtworkAdditionalInformation
    // R9: (GroupElement, (Coll[Coll[Byte]], Coll[Long]))                                                       RevealData: (UserPKGroupElement, (Coll[ErgoNameBytes, CommitSecret, CommitBoxId], Coll[MinerFee, TxOperatorFee, MinBoxValue]))

    // ===== Relevant Transactions ===== //
    // 1. Mint ErgoName
    // Inputs: Reveal, Registry Commit
    // Data Inputs: USDv2OracleDatapoint, ?ErgoDexErg2Token, ?Config
    // Outputs: Registry, SubNameRegistry, ErgoNameIssuer, ErgoNameFee, MinerFee, TxOperatorFee
    // Context Variables: ErgoNameCollectionIssuerBox
    // 2. Refund Reveal
    // Inputs: ErgoNameCollection, Reveal
    // Data Inputs: None
    // Outputs: ErgoNameCollection, UserPK, MinerFee, TxOperatorFee
    // Context Variables: None

    // ===== Compile Time Constants ($) ===== //
    // $commitContractBytesHash: Coll[Byte]
    // $ergoNameCollectionSingletonTokenId: Coll[Byte]
    // $ergoNameCollectionTokenId: Coll[Byte]

    // ===== Context Variables (_) ===== //
    // _action: Int
    // _ergoNameCollectionIssuerBox: Box

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
    val revealData: (GroupElement, (Coll[Coll[Byte]], Coll[Long])) = SELF.R9[(GroupElement, (Coll[Coll[Byte]], Coll[Long]))].get
    val userPKGroupElement: GroupElement = revealData._1
    val userPKSigmaProp: SigmaProp = proveDlog(userPKGroupElement)
    val ergonameBytes: Coll[Byte] = revealData._2._1(0)
    val commitSecret: Coll[Byte] = revealData._2._1(1)
    val commitBoxId: Coll[Byte] = revealData._2._1(2)
    val minerFee: Long = revealData._2._2(0)
    val txOperatorFee: Long = revealData._2._2(1)
    val minBoxValue: Long = revealData._2._2(2)
    val minerFeeErgoTreeHash: Coll[Byte] = fromBase16("e540cceffd3b8dd0f401193576cc413467039695969427df94454193dddfb375")
    val isRefund: Boolean = (OUTPUTS.size == 3)
    val collectionTokenId: Coll[Byte] = SELF.tokens(0)._1
    val artworkCollectionTokenId: Coll[Byte] = SELF.R7[Coll[Byte]].get

    val _action: Int = if (getVar[Int](0).isDefined) getVar[Int](0).get else 0

    if (_action == 1) {

        // ===== Mint ErgoName Tx ===== //
        val validMintErgoNameTx: Boolean = {

            // Inputs
            val registryBoxIn: Box = INPUTS(1)
            val commitBoxIn: Box = INPUTS(2)

            // Ouputs
            val ergonameIssuanceBoxOut: Box = OUTPUTS(0)
            val subNameRegistryBoxOut: Box = OUTPUTS(2)
            val ergoNameFeeBoxOut: Box = OUTPUTS(3)
            val minerFeeBoxOut: Box = OUTPUTS(4)
            val txOperatorFeeBoxOut: Box = OUTPUTS(5)

            val _ergoNameCollectionIssuerBox: Box = getVar[Box](1).get

            val validErgoNameCollection: Boolean = {

                val validCollection: Boolean = ($ergoNameCollectionTokenId == _ergoNameCollectionIssuerBox.id)
                val validSelection: Boolean = (artworkCollectionTokenId == $ergoNameCollectionTokenId)
                val validExistence: Boolean = (collectionTokenId, 1L) == (artworkCollectionTokenId, 1L)

                allOf(Coll(
                    validCollection,
                    validSelection,
                    validExistence
                ))

            }

            val validCollectionTokenBurn = {

                OUTPUTS.forall { (output: Box) =>
                    output.tokens.forall { (token: (Coll[Byte], Long)) =>
                        token._1 != collectionTokenId
                    }
                }

            }

            // Reveal box validates correct commit box.
            // Commit box validates correct registry.
            // Registry handles propery payment check.
            val validCommitBoxIn: Boolean = {

                allOf(Coll(
                    (commitBoxIn.id == commitBoxId),
                    (blake2b256(commitBoxIn.propositionBytes) == $commitContractBytesHash),
                    (commitBoxIn.R5[GroupElement].get == userPKGroupElement)
                ))

            }
            
            val validErgonameIssuanceAmount: Boolean = (ergonameIssuanceBoxOut.value >= minBoxValue) // It would equal this if the price had maximum slippage, i.e. no change for the user.

            val validSubNameRegistryAmount: Boolean = (subNameRegistryBoxOut.value == minBoxValue)

            val validMinerFeeBoxOut: Boolean = {

                allOf(Coll(
                    (minerFeeBoxOut.value == minerFee),
                    (blake2b256(minerFeeBoxOut.propositionBytes) == minerFeeErgoTreeHash),
                    (minerFeeBoxOut.tokens.size == 0)
                ))

            }

            val validTxOperatorFeeBoxOut: Boolean = {

                allOf(Coll(
                    (txOperatorFeeBoxOut.value == txOperatorFee),
                    (txOperatorFee == commitBoxIn.value)
                ))

            }

            allOf(Coll(
                validRevealBoxInValue,
                validCommitBoxIn,
                validSubNameRegistryAmount,
                validErgonameIssuanceAmount,
                validErgoNameMint,
                validCollectionTokenBurn,
                validMinerFeeBoxOut,
                validTxOperatorFeeBoxOut,
                (OUTPUTS.size == 6)
            ))

        }

        sigmaProp(validMintErgoNameTx)

    } else {

        // ===== Refund Tx ===== //
        val validRefundTx: Boolean = {

            // Inputs
            val ergonameCollectionBoxIn: Box = INPUTS(0)

            // Outputs
            val ergonameCollectionBoxOut: Box = OUTPUTS(0)
            val userPKBoxOut: Box = OUTPUTS(1)
            val minerFeeBoxOut: Box = OUTPUTS(2)

            val validErgoNameCollection: Boolean = (ergonameCollectionBoxIn.tokens(0)._1 == $ergoNameCollectionSingletonTokenId) // If ErgoName collection token amount is Long.MaxValue, we can just burn the token, no need for this check.

            val validUser: Boolean = {

                val propAndBox: (SigmaProp, Box) = (userPKSigmaProp, userPKBoxOut)
                val validPaymentTokenTransfer: Boolean = (userPKBoxOut.tokens() == SELF.tokens())

                allOf(Coll(
                    (userPKBoxOut.value == SELF.value - minerFee),
                    isSigmaPropEqualToBoxProp(propAndBox),
                    validPaymentTokenTransfer
                ))

            }

            val validMinerFee: Boolean = {

                allOf(Coll(
                    (minerFeeBoxOut.value == minerFee),
                    (blake2b256(minerFeeBoxOut.propositionBytes) == minerFeeErgoTreeHash),
                    (minerFeeBoxOut.tokens.size == 0)
                ))

            }

            allOf(Coll(
                validErgoNameCollection,
                validUser,
                validMinerFee
            ))

        }

        sigmaProp(validRefundTx) && userPKSigmaProp

    }

}