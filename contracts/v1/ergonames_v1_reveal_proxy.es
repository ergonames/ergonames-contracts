{

    // ===== Contract Description ===== //
    // Name: ErgoNames Reveal Proxy
    // Description: Contract holding the ErgoName collecion tokens.
    // Version: 1.0.0
    // Author: Luca D'Angelo (ldgaetano@protonmail.com)

    // ===== Box Contents ===== //
    // Tokens
    // None
    // Registers
    // R4: Long             MinerFee
    // R5: Coll[Byte]       RevealBoxHash

    // ===== Relevant Transactions ===== //
    // 1. Reveal
    // Inputs: ErgoNameCollection, RevealProxy
    // Data Inputs: None
    // Outputs: ErgoNameCollection, Reveal, MinerFee
    // Context Variables: None

    // ===== Compile Time Constants ($) ===== //
    // $ergonameCollectionSingletonTokenId: Coll[Byte]

    // ===== Context Variables (_) ===== //
    // None

    // ===== Relevant Variables ===== //
    val minerFeeErgoTreeHash: Coll[Byte] = fromBase16("e540cceffd3b8dd0f401193576cc413467039695969427df94454193dddfb375")
    val minerFee: Long = SELF.R4[Long].get
    val revealBoxHash: Coll[Byte] = SELF.R5[Coll[Byte]].get

    val validRevealTx: Boolean = {

        // Inputs
        val ergonameCollectionBoxIn: Box = INPUTS(0)

        // Outputs
        val ergonameCollectionBoxOut: Box = OUTPUTS(0)
        val revealBoxOut: Box = OUTPUTS(1)
        val minerFeeBoxOut: Box = OUTPUTS(2)

        val validCollection: Boolean = (ergonameCollectionBoxIn.tokens(0)._1 == $ergonameCollectionSingletonTokenId)

        val validReveal: Boolean = {
            
            val revealHash: Coll[Byte] = blake2b256(revealBoxOut.bytesWithoutRef) // Bytes of box contents without transaction id and output index.
            
            allOf(Coll(
                (revealBoxOut.value == SELF.value - minerFee),
                (revealHash == revealBoxHash)
            ))

        }

        val validMinerFee: Boolean = {

            allOf(Coll(
                (minerFeeBoxOut.value == minerFee),
                (blake2b256(minerFeeBoxOut.propositionBytes) == minerFeeErgoTreeHash)
            ))

        }

        allOf(Coll(
            validCollection,
            validReveal,
            validMinerFee
        ))

    }

    sigmaProp(validRevealTx)

}