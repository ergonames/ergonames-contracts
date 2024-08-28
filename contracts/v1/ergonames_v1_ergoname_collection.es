{

    // ===== Contract Description ===== //
    // Name: ErgoName Collection Contract
    // Description: Cntract holding the ErgoName collecion tokens.
    // Version: 1.0.0
    // Author: Luca D'Angelo (ldgaetano@protonmail.com)

    // ===== Box Contents ===== //
    // Tokens
    // 1. (ErgoNameCollectionTokenId, Long.MaxValue)
    // Registers
    // None

    // ===== Relevant Transactions ===== //
    // 1. Commit
    // Inputs: ErgoNameCollection
    // Data Inputs: None
    // Outputs: ErgoNameCollection, Commit, MinerFee
    // Context Variables: ErgoNameCollectionIssuerBox

    // ===== Compile Time Constants ($) ===== //
    // $commitErgoTreeBytes: Coll[Byte]

    // ===== Context Variables (_) ===== //
    // None

    // ===== Relevant Variables ===== //
    val minerFeeErgoTreeHash: Coll[Byte] = fromBase16("e540cceffd3b8dd0f401193576cc413467039695969427df94454193dddfb375")
    val ergonameCollectionTokenId: Coll[Byte] = SELF.tokens(0)._1
    val ergonameCollectionTokenAmount: Long = SELF.tokens(0)._2


    val validCommitTx: Boolean = {

        // Outputs
        val ergonameCollectionBoxOut: Box = OUTPUTS(0)
        val commitBoxOut: Box = OUTPUTS(1)
        val minerFeeBoxOut: Box = OUTPUTS(2)

        val validSelfRecreation: Boolean = {

            allOf(Coll(
                (ergonameCollectionBoxOut.value == SELF.value),
                (ergonameCollectionBoxOut.propositionBytes == SELF.propositionBytes),
                (ergonameCollectionBoxOut.tokens(0) == (ergonameCollectionTokenId, ergonameCollectionTokenAmount - 1L))
            ))

        }

        val validCommitBoxOut: Boolean = {

            allOf(Coll(
                (commitBoxOut.propositionBytes == $commitErgoTreeBytes),
                (commitBoxOut.tokens(0) == (ergonameCollectionTokenId, 1L)),
                (commitBoxOut.R7[Coll[Byte]].get == ergonameCollectionTokenId) // For artwork standard v2 (EIP-24).
            ))

        }

        val validMinerFeeBoxOut: Boolean = (blake2b256(minerFeeBoxOut.propositionBytes) == minerFeeErgoTreeHash)

        allOf(Coll(
            validSelfRecreation,
            validCommitBoxOut,
            validMinerFeeBoxOut
        ))

    }

    sigmaProp(validCommitTx)

}