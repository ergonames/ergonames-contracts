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
    // Data Inputs: USDv2OracleDatapoint, ?ErgoDexErg2Token, ?Config
    // Outputs: Registry, SubNameRegistry, ErgoNameIssuer, ErgoNameFee, MinerFee, TxOperatorFee
    // Context Variables: None

    // ===== Compile Time Constants ($) ===== //
    // None

    // ===== Context Variables (_) ===== //
    // None

    // ===== User Defined Functions ===== //
    // None

    // ===== Relevant Variables ===== //
    val address1 = PK("3WvubspBMttcKU97e6oAKdjgaXmoVUDDi6aKdt3in9zTvzSUTxto")
    val address2 = PK("3WxJrwDLXgGE53KpdJ2nSjSMRdXaDWh7Fdz9MY2Zh37UAwfLXzBU")
    //val address3 = PK("3WvubspBMttcKU97e6oAKdjgaXmoVUDDi6aKdt3in9zTvzSUTxto")
    //val address4 = PK("3WxJrwDLXgGE53KpdJ2nSjSMRdXaDWh7Fdz9MY2Zh37UAwfLXzBU")

    val addresses = Coll(address1, address2)
    val minRequiredSignatures = 1

    atLeast(minRequiredSignatures, addresses)

}