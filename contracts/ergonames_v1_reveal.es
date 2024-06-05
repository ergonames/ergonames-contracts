{

    // ===== Contract Description ===== //
    // Name: ErgoNames Reveal Contract
    // Description: User reveals their ErgoName registration secret.
    // Version: 1.0.0
    // Author: Luca D'Angelo (ldgaetano@protonmail.com)

    // ===== Box Contents ===== //
    // Tokens
    // 1. (PaymentTokenId, PaymentTokenAmount) // If ErgoName can be purchased with a custom token.
    // Registers
    // R4: Coll[Byte]       ErgoNameBytes
    // R5: GroupElement     BuyerPKGroupElement
    // R6: Coll[Byte]       CommitSecret
    // R7: Coll[Byte]       CommitBoxId
    // R8: Coll[Long]       Coll(MinerFee, TxOperatorFee, MinBoxValue)

    // ===== Relevant Transactions ===== //
    // 1. Mint ErgoName
    // Inputs: Registry, Reveal, Commit
    // Data Inputs: ErgoDexErg2SigUsd, ?ErgoDexErg2Token, ?Config
    // Outputs: Registry, SubNameRegistry, ErgoNameIssuer, ErgoNameFee, MinerFee, TxOperatorFee
    // Context Variables: None
    // 2. Refund
    // Inputs: Reveal
    // Data Inputs: None
    // Outputs: BuyerPK, MinerFee
    // Context Variables: None

    // ===== Compile Time Constants ($) ===== //
    // $commitContractBytes: Coll[Byte]

    // ===== Context Variables (_) ===== //
    // None

    // ===== Relevant Variables ===== //
    val buyerPKGroupElement: GroupElement = SELF.R5[GroupElement].get
    val buyerPKSigmaProp: SigmaProp = proveDlog(buyerPKGroupElement)
    val commitBoxId: Coll[Byte] = SELF.R7[Coll[Byte]].get
    val minerFee: Long = SELF.R8[Coll[Long]].get(0)
    val txOperatorFee: Long = SELF.R8[Coll[Long]].get(1)
    val minBoxValue: Long = SELF.R8[Coll[Long]].get(2)
    val minerFeeErgoTreeHash: Coll[Byte] = fromBase16("e540cceffd3b8dd0f401193576cc413467039695969427df94454193dddfb375")
    val isPayingWithToken: Boolean = (SELF.tokens.size != 0)
    val isRefund: Boolean = (OUTPUTS.size == 2)

    if (!isRefund) {

        // ===== Mint ErgoName Tx ===== //
        val validMintErgoNameTx: Boolean = {

            // Inputs
            val registryBoxIn: Box = INPUTS(0)
            val commitBoxIn: Box = INPUTS(2)

            // Ouputs
            val subNameRegistryBoxOut: Box = OUTPUTS(1)
            val ergoNameIssuerBoxOut: Box = OUTPUTS(2)
            val ergoNameFeeBoxOut: Box = OUTPUTS(3)
            val minerFeeBoxOut: Box = OUTPUTS(4)
            val txOperatorFeeBoxOut: Box = OUTPUTS(5)

            // Relevant Variables
            val subNameRegistryAmount: Long = subNameRegistryBoxOut.value
            val ergoNameIssuerAmount: Long = ergoNameIssuerBoxOut.value
            val ergoNameFeeErgAmount: Long = if (!isPayingWithToken) ergoNameFeeBoxOut.value else 0L
            val ergoNameFeeTokenAmount: Long  = if (isPayingWithToken) ergoNameFeeBoxOut.tokens(0)._2 else 0L
            val minerFeeAmount: Long = minerFeeBoxOut.value
            val txOperatorFeeAmount: Long = txOperatorFeeBoxOut.value - commitBoxIn.value

            val validRevealBoxInValue: Boolean = {

                val validErgValue: Boolean = (SELF.value == subNameRegistryAmount + ergoNameIssuerAmount + ergoNameFeeErgAmount + minerFeeAmount + txOperatorFeeAmount)
                val validTokenValue: Boolean = {

                    if (isPayingWithToken) {
                        (SELF.tokens(0)._2 == ergoNameFeeTokenAmount)
                    } else {
                        true
                    }

                }

                allOf(Coll(
                    validErgValue,
                    validTokenValue
                ))

            }

            val validCommitBoxIn: Boolean = {

                allOf(Coll(
                    (commitBoxIn.id == commitBoxId),
                    (commitBoxIn.propositionBytes == $commitContractBytes),
                    (commitBoxIn.R5[GroupElement].get == buyerPKGroupElement)
                ))

            }

            val validSubNameRegistryAmount: Boolean = (subNameRegistryAmount == minBoxValue)

            val validErgonameIssuerAmount: Boolean = (ergoNameIssuerAmount == 2 * minerFee)

            val validMinerFeeBoxOut: Boolean = {

                allOf(Coll(
                    (minerFeeBoxOut.value == minerFee),
                    (blake2b256(minerFeeBoxOut.propositionBytes) == minerFeeErgoTreeHash),
                    (minerFeeBoxOut.tokens.size == 0)
                ))

            }

            val validTxOperatorFeeBoxOut: Boolean = (txOperatorFeeBoxOut.value == txOperatorFeeAmount + commitBoxIn.value)

            allOf(Coll(
                validRevealBoxInValue,
                validCommitBoxIn,
                validSubNameRegistryAmount,
                validErgonameIssuerAmount,
                validMinerFeeBoxOut,
                validTxOperatorFeeBoxOut,
                validErgonameIssuerAmount,
                (OUTPUTS.size == 6)
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