{

    // ===== Contract Description ===== //
    // Name: ErgoName Issuer Contract
    // Description: Used as the issuer box for minting the ErgoName NFT of the user.
    // Version: 1.0.0
    // Author: Luca D'Angelo (ldgaetano@protonmail.com)

    // ===== Box Contents ===== //
    // Tokens
    // 1. (ErgoNameCollectionTokenId, 1L)
    // Registers
    // R4: Int                                                                                                  ArtworkStandardVersion
    // R5: Coll[(Coll[Byte], Int)]                                                                              ArtworkRoyaltyRecipients
    // R6: (Coll[(Coll[Byte], Coll[Byte])], (Coll[(Coll[Byte], (Int, Int))], Coll[(Coll[Byte], (Int, Int))]))   ArtworkTraits
    // R7: Coll[Byte]                                                                                           ArtworkCollectionTokenId        
    // R8: Coll[(Coll[Byte], Coll[Byte])]                                                                       ArtworkAdditionalInformation
    // R9: (GroupElement, Coll[Byte])                                                                           ReceiverData

    // ===== Relevant Transactions ===== //
    // 1. Mint ErgoName NFT
    // Inputs: ErgoNameIssuer
    // Data Inputs: None
    // Outputs: ErgoNameIssuance, MinerFee
    // Context Variables: ErgoNameCollectionIssuerBox

    // ===== Compile Time Constants ($) ===== //
    // $ergoNameCollectionTokenId

    // ===== Context Variables (_) ===== //
    // _ergoNameCollectionIssuerBox

    // ===== Relevant Variables ===== //
    val minerFeeErgoTreeHash: Coll[Byte] = fromBase16("e540cceffd3b8dd0f401193576cc413467039695969427df94454193dddfb375")
    val artworkCollectionTokenId: Coll[Byte] = SELF.R7[Coll[Byte]].get
    val receiverData: (GroupElement, Coll[Byte]) = SELF.R9[(GroupElement, Coll[Byte])].get
    val receiverGE: GroupElement = receiverData._1
    val receiverSigmaProp = proveDlog(receiverGE)
    val receiverErgoNameBytes = receiverData._2

    val validMintErgoNameNftTx: Boolean = {

        // Outputs
        val receiverBoxOut: Box = OUTPUTS(0)
        val minerFeeBoxOut: Box = OUTPUTS(1)

        val validErgoNameCollection: Boolean = {
            
            val validCollection: Boolean = ($ergoNameCollectionTokenId == _ergoNameCollectionIssuerBox.id)
            val validSelection: Boolean = (artworkCollectionTokenId == $ergoNameCollectionTokenId)
            val validExistence: Boolean = SELF.tokens(0) == (artworkCollectionTokenId, 1L)

            allOf(Coll(
                validCollection,
                validSelection,
                validExistence
            ))
            
        }

        val validErgoNameMint: Boolean = {

            allOf(Coll(
                (SELF.value == receiverBoxOut.value * 2),
                (receiverBoxOut.propositionBytes == receiverSigmaProp.propBytes),
                (receiverBoxOut.tokens(0) == (SELF.id, 1L))
            ))

        }

        val validMinerFeeBoxOut: Boolean = {

            allOf(Coll(
                (SELF.value == minerFeeBoxOut.value * 2),
                (blake2b256(minerFeeBoxOut.propositionBytes) == minerFeeErgoTreeHash),
                (minerFeeBoxOut.tokens.size == 0)
            ))

        }

        allOf(Coll(
            validErgoNameCollection,
            validErgoNameMint,
            validMinerFee
        ))

    }

    sigmaProp(validMintErgoNameNftTx)

}