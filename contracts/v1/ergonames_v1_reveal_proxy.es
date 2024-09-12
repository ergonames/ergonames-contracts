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

    // ===== Relevant Transactions ===== //
    // 1. Reveal
    // Inputs: ErgoNameCollection, RevealProxy
    // Data Inputs: None
    // Outputs: ErgoNameCollection, Reveal, MinerFee, TxOperatorFee
    // Context Variables: None

    // ===== Compile Time Constants ($) ===== //
    // $ergoNameCollectionSingletonTokenId: Coll[Byte]

    // ===== Context Variables (_) ===== //
    // None

    // ===== User Defined Functions ===== //
    // None

    // ===== Relevant Variables ===== //
    val minerFeeErgoTreeHash: Coll[Byte] = fromBase16("e540cceffd3b8dd0f401193576cc413467039695969427df94454193dddfb375")
    val revealBoxHash: Coll[Byte] = SELF.R4[Coll[Byte]].get
    val minerFee: Long = SELF.R5[Long].get
    val txOperatorFee: Long = SELF.R6[Long].get
    val isPayingWithToken: Boolean = (SELF.tokens.size == 1)

    val validRevealTx: Boolean = {

        // Inputs
        val ergonameCollectionBoxIn: Box = INPUTS(0)

        // Outputs
        val ergonameCollectionBoxOut: Box = OUTPUTS(0)
        val revealBoxOut: Box = OUTPUTS(1)
        val minerFeeBoxOut: Box = OUTPUTS(2)

        val validCollection: Boolean = (ergonameCollectionBoxIn.tokens(0)._1 == $ergoNameCollectionSingletonTokenId)

        val validReveal: Boolean = {
            
            val revealHash: Coll[Byte] = blake2b256(revealBoxOut.bytesWithoutRef) // Bytes of box contents without transaction id and output index.
            val validPaymentToken: Boolean = isPayingWithToken ? (revealBoxOut.tokens(0) == SELF.tokens(0) : true)    

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

}