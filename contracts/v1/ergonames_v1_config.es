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
    // Inputs: Registry, Reveal, Commit
    // Data Inputs: ErgoDexErg2SigUsd, ?ErgoDexErg2Token, ?Config
    // Outputs: Registry, SubNameRegistry, ErgoNameIssuer, ErgoNameFee, MinerFee, TxOperatorFee
    // Context Variables: None

    // ===== Compile Time Constants ($) ===== //
    // $ergonameMultiSigSigmaProp: SigmaProp

    // ===== Context Variables (_) ===== //
    // None

    // ===== Relevant Variables ===== //
    // None

    // ===== User Defined Functions ===== //
    // None

    val address1 = PK("3Wy7pdHGCRjfZJT3gHFSd6sHqa1Vyn4Xkh93VcLy7TH5CEi6jxVC")
    val address2 = PK("3WwcNTWMqbJBVHpHGoxPdbc8stHCPbSNFD6zhBfYgFLKcS7gwKiT")

    val $addresses = Coll(address1, address2)
    val $minRequiredSignatures = 1

    atLeast($minRequiredSignatures, $addresses)

}