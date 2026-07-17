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
    val adoo = PK("9iJV2D1gzvWeBbSXHPgTai3S41CjoBBodxMB9DB2Dwt1kaRA9z2")
    val lgd = PK("9hdYFtdV8JLXJho4wAzy6dB6DHQn4gvZsmf4vf1kbkggJ59Wn3Z")
    val balb = PK("9gCJDv78SUUes6sNo81KqbP4yu3vHU6GtcatvmySiBsRoU1k4T8")
    val mgpai = PK("9g5yzitxX53B4RVi1DHLjrMx7iwTQn38kLG2XVVkhvHvcB1TcEz")
    val msig_addresses: Coll[SigmaProp] = Coll(adoo, lgd, balb, mgpai)
    val min_required_signatures: Int = 2
    val ergoname_msig_sigmaprop: SigmaProp = atLeast(min_required_signatures, msig_addresses)

    ergoname_msig_sigmaprop

}