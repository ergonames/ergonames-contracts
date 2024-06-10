{
    // ===== Contract Description ===== //
    // Name: ErgoNames Commit Contract
    // Description: User commits their intention to register an ErgoName.
    // Version: 1.0.0
    // Author: Luca D'Angelo (ldgaetano@protonmail.com)

    // ===== Box Contents ===== //
    // Tokens
    // None
    // Registers
    // R4: Coll[Byte]       CommitHash
    // R5: GroupElement     BuyerPKGroupElement
    // R6: Long             MinerFee

    // ===== Relevant Transactions ===== //
    // 1. Mint ErgoName
    // Inputs: Registry, Reveal, Commit
    // Data Inputs: ErgoDexErg2SigUsd, ?ErgoDexErg2Token, ?Config
    // Outputs: Registry, SubNameRegistry, ErgoNameIssuer, ErgoNameFee, MinerFee, TxOperatorFee
    // Context Variables: None
    // 2. Refund
    // Inputs: Commit
    // Data Inputs: None
    // Outputs: BuyerPK, MinerFee
    // Context Variables: None

    // ===== Compile Time Constants ($) ===== //
    // $registrySingletonTokenId: Coll[Byte]

    // ===== Context Variables (_) ===== //
    // None

    val minerFeeErgoTreeHash = fromBase16("e540cceffd3b8dd0f401193576cc413467039695969427df94454193dddfb375")

    // ===== Relevant Variables ===== //
    val buyerPKGroupElement: GroupElement = SELF.R5[GroupElement].get
    val buyerPKSigmaProp: SigmaProp = proveDlog(buyerPKGroupElement)
    val minerFee: Long = SELF.R6[Long].get
    val isRefund: Boolean = (OUTPUTS.size == 2)

    if (!isRefund) {

        // ===== Mint ErgoName Tx ===== //
        val validMintErgoNameTx: Boolean = {

            // Inputs
            val registryBoxIn: Box = INPUTS(0)


            val validSingletonTokenId: Boolean = {

                (registryBoxIn.tokens(0)._1 == $registrySingletonTokenId)

            }

            allOf(Coll(
                validSingletonTokenId
            ))

        }

        sigmaProp(validMintErgoNameTx)

    } else {

        // ===== Refund Tx ===== //
        val validRefundTx: Boolean = {

            // Inputs

            // Outputs
            val buyerPKBoxOut: Box = OUTPUTS(0)
            val minerFeeBoxOut: Box = OUTPUTS(1)

            val validBuyerPKBoxOut: Boolean = {

                allOf(Coll(
                    (buyerPKBoxOut.value == SELF.value - minerFee),
                    (buyerPKBoxOut.propositionBytes == buyerPKSigmaProp.propBytes)
                ))

            }

            val validMinerFeeBoxOut: Boolean = {

                allOf(Coll(
                    (minerFeeBoxOut.value == minerFee),
                    (blake2b256(minerFeeBoxOut.propositionBytes) == minerFeeErgoTreeHash)
                ))

            }

            allOf(Coll(
                validBuyerPKBoxOut,
                validMinerFeeBoxOut,
                (OUTPUTS.size == 2)
            ))

        }

        sigmaProp(validRefundTx) && buyerPKSigmaProp

    }

}