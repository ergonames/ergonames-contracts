{

    // ===== Contract Information ===== //
    // Name: ErgoNames Config Contract
    // Description: Contract for the config box of the ergonames protocol, used for determining payment options.
    // Version: 1.0.0
    // Author: Luca D'Angelo

    // ===== Box Contents ===== //
    // Tokens
    // 1. (ConfigSingleton, 1L)
    // Registers
    // R4: AvlTree  ConfigAvlTree

    // ===== Relevant Transactions ===== //
    // 1. Mint ErgoName
    // Inputs: Reveal, Registry, Commit
    // Data Inputs: USDv2OracleDatapoint, ?Config
    // Outputs: Registry, SubNameRegistry, ErgoNameIssuer, ErgoNameFee, MinerFee, TxOperatorFee
    // Context Variables: None

    // ===== Compile Time Constants ($) ===== //
    // None

    // ===== Context Variables (_) ===== //
    // None

    // ===== User Defined Functions ===== //
    // None

    // ===== Relevant Variables ===== //
    val validUpdate = (INPUTS(0).tokens(0)._1 == $updateNFT)
    sigmaProp(validUpdate)

}