{
    // ===== Contract Description ===== //
    // Name: ErgoNames Commit Contract
    // Description: User commits their intention to register an ErgoName.
    // Version: 1.0.0
    // Author: Luca D'Angelo (ldgaetano@protonmail.com)

    // ===== Box Contents ===== //
    // Value: The value is used to pay the tx operator fee for the mint, value >= 2_000_000 nanoERG.
    // Tokens
    // None
    // Registers
    // R4: Coll[Byte]       CommitHash
    // R5: GroupElement     UserPKGroupElement

    // ===== Relevant Transactions ===== //
    // 1. Mint ErgoName
    // Inputs: Reveal, Registry, Commit
    // Data Inputs: USDv2OracleDatapoint, ?Config
    // Outputs: ErgoNameIssuance, Registry, SubNameRegistry, ErgoNameFee, MinerFee, TxOperatorFee
    // Context Variables: None
    // 2. Refund
    // Inputs: Commit
    // Data Inputs: None
    // Outputs: UserPK, MinerFee
    // Context Variables: None

    // ===== Compile Time Constants ($) ===== //
    // $registrySingletonTokenId: Coll[Byte]
    // $maxCommitBoxAge: Int

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
    val minerFeeErgoTreeHash = fromBase16("e540cceffd3b8dd0f401193576cc413467039695969427df94454193dddfb375")
    val userPKGroupElement: GroupElement = SELF.R5[GroupElement].get
    val userPKSigmaProp: SigmaProp = proveDlog(userPKGroupElement)
    val minerFee: Long = 1000000L
    val creationHeight: Int = SELF.creationInfo._1
    val isOldEnough: Boolean = HEIGHT - creationHeight >= $maxCommitBoxAge

    val _action: Int = if (getVar[Int](0).isDefined) getVar[Int](0).get else 0
    val isRefund: Boolean = (_action == 2) && isOldEnough


    if (_action == 1) {

        // ===== Mint ErgoName Tx ===== //
        val validMintErgoNameTx: Boolean = {

            // Inputs
            val registryBoxIn: Box = INPUTS(1)
            
            val onlyOne: Boolean = (INPUTS(2).id == SELF.id)

            val validRegistry: Boolean = {

                (registryBoxIn.tokens(0)._1 == $registrySingletonTokenId)

            }

            allOf(Coll(
                onlyOne,
                validRegistry,
                (SELF.tokens.size == 0) // Force refund if there are extra tokens in this box by accident. This is needed for the mint to ensure extra tokens are not stolen.
            ))

        }

        sigmaProp(validMintErgoNameTx)

    } else if (isRefund) {

        // ===== Refund Tx ===== //
        val validRefundTx: Boolean = {

            // Outputs
            val userPKBoxOut: Box = OUTPUTS(0)
            val minerFeeBoxOut: Box = OUTPUTS(1)

            val onlyOne = (INPUTS(0).id == SELF.id)

            val validUser: Boolean = {

                val propAndBox: (SigmaProp, Box) = (userPKSigmaProp, userPKBoxOut)

                allOf(Coll(
                    (userPKBoxOut.value == SELF.value - minerFee),
                    (userPKBoxOut.tokens == SELF.tokens),
                    isSigmaPropEqualToBoxProp(propAndBox)
                ))

            }

            val validMinerFee: Boolean = {

                allOf(Coll(
                    (minerFeeBoxOut.value == minerFee),
                    (blake2b256(minerFeeBoxOut.propositionBytes) == minerFeeErgoTreeHash),
                    (minerFeeBoxOut.tokens.size == 0)
                ))

            }

            val validSize: Boolean = {
                (OUTPUTS.size == 2) &&
                (INPUTS.size == 1)
            }

            allOf(Coll(
                onlyOne,
                validUser,
                validMinerFee,
                validSize
            ))

        }

        sigmaProp(validRefundTx) && userPKSigmaProp

    } else {
        sigmaProp(false) // No update path, commit can only change if the registry contract changes.
    }

}