{
    // ===== Contract Description ===== //
    // Name: ErgoNames Reveal Contract
    // Description: 
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

{
    // ===== Contract Description ===== //
    // Name: ErgoNames Proxy Contract
    // Description: This contract is a proxy contract and ensures funds are used properly
    // Version: 1.0.0
    // Author: zackbalbin@github.com
    // Auditor: mgpai22@github.com

    // ===== Box Registers ===== //
    // R4: Coll[Byte] => name to register
    // R5: SigmaProp => receiver sigmaProp
    // R6: Coll[Byte] => commitment secret
    // R7: Coll[Byte] => commitment box id

    // ===== Compile Time Constants ===== //
    // _singletonToken: Coll[Byte]
    // _minerFee: Long //miner fee in nano ergs

    // ===== Context Extension Variables ===== //
    // None



    val isRefund: Boolean = (INPUTS.size == 1)
    val buyerPK: SigmaProp = SELF.R5[SigmaProp].get

    if (!isRefund) {

        val validTx: Boolean = {

            // inputs
            val registryInputBox = INPUTS(0)

            // outputs
            val tokenReceiverBox = OUTPUTS(0)

            val commitmentBoxId: Coll[Byte] = SELF.R7[Coll[Byte]].get


            val validRecipient: Boolean = {
                tokenReceiverBox.propositionBytes == buyerPK.propBytes
            }

//            val validAmount: Boolean = {
//                tokenReceiverBox.value == INPUTS(0).value
//            }

            val validRegistryBox: Boolean = {
                (registryInputBox.tokens(0)._1 == _singletonToken)
            }

            val validCommitmentBox: Boolean = {
                (INPUTS(2).id == commitmentBoxId)
            }

            allOf(Coll(
                validRecipient,
                validRegistryBox,
                validCommitmentBox
            ))

        }

        sigmaProp(validTx)

    } else {

        val validRefundTx: Boolean = {

            // outputs
            val refundBoxOUT: Boolean = OUTPUTS(0)
            val minerBoxOUT: Box = OUTPUTS(1)

            val validRefundBox: Boolean = {

                allOf(Coll(
                    (refundBoxOUT.value == SELF.value - _minerFee),
                    (refundBoxOUT.propositionBytes == buyerPK.propBytes)
                ))

            }

            val validMinerFee: Boolean = (minerBoxOUT.value == _minerFee)

            allOf(Coll(
                validRefundBox,
                validMinerFee,
                (OUTPUTS.size == 2)
            ))

        }

        sigmaProp(validRefundTx) && buyerPK // buyer must sign tx themself as well

    }

}