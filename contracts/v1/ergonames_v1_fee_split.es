{
    // ===== Contract Description ===== //
    // Name: ErgoNames Fee Split Contract
    // Description: Accumulates ErgoName minting fees, spendable only by a
    //              distribution transaction that pays every configured
    //              recipient at least its per-mille share of the distributed
    //              value.
    // Version: 1.0.0
    // Author: Luca D'Angelo (ldgaetano@protonmail.com)

    // ===== Box Contents ===== //
    // Tokens: None expected (ERG fees only in default payment mode).
    // Registers: None.

    // ===== Relevant Transactions ===== //
    // 1. Distribute Fees
    // Inputs: One or more FeeSplit boxes
    // Outputs: One box per recipient (same order as $recipientPropBytes),
    //          then the miner fee box.
    // Context Variables: 

    // ===== Compile Time Constants ($) ===== //
    // None

    // ===== Context Variables (_) ===== //
    // _action: Int

    // ===== User Defined Functions ===== //
    // None

    val minerFeeErgoTreeHash: Coll[Byte] = fromBase16("e540cceffd3b8dd0f401193576cc413467039695969427df94454193dddfb375")
    val multisigErgoTreeHash: Coll[Byte] = fromBase16("9b17f3d89673488ce95e049f551712b755b5575cbbc4b818f4bc7342b7f7b95c")
    val adoo = PK("9iJV2D1gzvWeBbSXHPgTai3S41CjoBBodxMB9DB2Dwt1kaRA9z2")
    val lgd = PK("9hdYFtdV8JLXJho4wAzy6dB6DHQn4gvZsmf4vf1kbkggJ59Wn3Z")
    val balb = PK("9gCJDv78SUUes6sNo81KqbP4yu3vHU6GtcatvmySiBsRoU1k4T8")
    val mgpai = PK("9g5yzitxX53B4RVi1DHLjrMx7iwTQn38kLG2XVVkhvHvcB1TcEz")
    val addresses: Coll[SigmaProp] = Coll(adoo, lgd, balb, mgpai)

    val shares: Coll[Long] = Coll(500L, 200L, 100L, 50L, 150L)
    val recipientCount: Int = addresses.size
    val miner_fee: Long = 1000000 // The update path could change this value, for example, if Ergo blockchain parameters change.

    val _action: Int = if (getVar[Int](0).isDefined) getVar[Int](0).get else 0

    if (_action == 1) {

        val validDistribution: Boolean = {

            // Total value entering from fee-split boxes (allows sweeping
            // several accumulated fee boxes in one distribution).
            val totalIn: Long = INPUTS.fold(0L, { (sum: Long, box: Box) =>
                if (box.propositionBytes == SELF.propositionBytes) sum + box.value else sum
            })
            val distributable: Long = totalIn - miner_fee

            val minerFeeBoxOut: Box = OUTPUTS(recipientCount+1)

            val indices: Coll[Int] = addresses.indices

            val validRecipients: Boolean = indices.forall { (i: Int) =>
                val out: Box = OUTPUTS(i)
                val owed: Long = (distributable * shares(i)) / 1000L
                val address: SigmaProp = addresses(i)
                (out.propositionBytes == address.propBytes) && (out.value >= owed)
            }

            val validMsigRecipient: Boolean = {
                val out: Box = OUTPUTS(4)
                val owed: Long = (distributable * shares(4)) / 1000L
                (blake2b256(out.propositionBytes) == multisigErgoTreeHash)
            }

            val validMinerFee: Boolean = {
                allOf(Coll(
                    (minerFeeBoxOut.value == miner_fee),
                    (blake2b256(minerFeeBoxOut.propositionBytes) == minerFeeErgoTreeHash)
                ))
            }

            (distributable > 0) &&
            validRecipients &&
            validMsigRecipient && 
            validMinerFee

        }

        sigmaProp(validDistribution)

    } else {

        sigmaProp(false) // No update, this contract is fine, a new contract can be set when the registry contract is updated.

    }

}