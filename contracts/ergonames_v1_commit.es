{
    // ===== Contract Description ===== //
    // Name: ErgoNames Commit Contract
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
    // Data Inputs: Config, SigUSDOracleDataPoint
    // Outputs: Registry, SubNameRegistry, ErgoNameIssuer, ErgoNameFee, MinerFee, TxOperatorFee
    // Context Variables: None
    // 2. Refund
    // Inputs: 
    // Data Inputs:
    // Outputs:
    // Context Variables: None

    // ===== Compile Time Constants ($) ===== //
    // $registrySingletonTokenId: Coll[Byte]
    // $ergoNameIssuerContractBytes: Coll[Byte]

    // ===== Context Variables (_) ===== //
    // None

    // ===== Relevant Variables ===== //
    val buyerPKGroupElement: GroupElement = SELF.R5[GroupElement].get
    val buyerPKSigmaProp: SigmaProp = proveDlog(buyerPKGroupElement)
    val minerFee: Long = SELF.R6[Long].get
    val minerFeeErgoTreeBytes: Coll[Byte] = fromBase16("e540cceffd3b8dd0f401193576cc413467039695969427df94454193dddfb375")

    // ===== Mint ErgoName Tx ===== //
    val validMintErgoNameTx: Boolean = {

        // Inputs
        val registryBoxIn: Box = INPUTS(0)

        // Ouputs
        val ergoNameIssuerBoxOut: Box = OUTPUTS(2)


        val validSingletonTokenId: Boolean = {

            (registryBoxIn.tokens(0)._1 == $registrySingletonTokenId)

        }

        val validErgonameIssuerBoxOut: Boolean = {

            val validValue: Boolean = (ergoNameIssuerBoxOut.value >= SELF.value - minerFee)
            val validContract: Boolean = (ergoNameIssuerBoxOut.propositionBytes = $ergoNameIssuerContractBytes)
            val validBuyerPKGroupElement: Boolean = (ergoNameIssuerBoxOut.R4[GroupElement].get == buyerPKGroupElement)

            allOf(Coll(
                validValue,
                validContract,
                validBuyerPKGroupElement
            ))

        }

        allOf(Coll(
            validSingletonTokenId,
            validErgonameIssuerBoxOut
        ))

    }

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

        val validMinerFee: Boolean = {

            allOf(Coll(
                (minerFeeBoxOut.value == minerFee),
                (minerFeeBoxOut.propositionBytes == minerFeeErgoTreeBytes)
            ))   

        }

        allOf(Coll(
            validBuyerPKBoxOut,
            validMinerFee
        ))

    }

    sigmaProp(validMintErgoNameTx) || (sigmaProp(validRefundTx) && buyerPKSigmaProp)

}