{
    // ===== Contract Description ===== //
    // Name: ErgoNames Fee Split Contract
    // Description: Accumulates ErgoName minting fees, spendable only by a
    //              distribution transaction that pays every configured
    //              recipient at least its per-mille share of the distributed
    //              value, or by the multisig escape hatch.
    // Version: 1.0.0
    // Author: Luca D'Angelo (ldgaetano@protonmail.com)

    // ===== Box Contents ===== //
    // Tokens: None expected (ERG fees only in default payment mode)
    // Registers: None

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

    //val msig = PK("91JepDHYPLhuwyCqsmu528NTRbLN2xCNRiSqz7ZsZSrGJYvCFbV3XAmdjMrUzEG8Ah84cJzCVVMp4FCDXDfsxo1cbdu7zaadwKZhLpZwRpFJnwRhDwUefx9BgoavK98QYtjCP4oL734AGB8fUfhDiewSCQngoZREvRfvMNggauzmf4iw7WZwSUEdavwJzApfzZmVsRPC5492rWjT2Die3pQdUYruNr")
    //val addresses: Coll[SigmaProp] = Coll(adoo, lgd, balb, mgpai, msig)
    val adoo = PK("9iJV2D1gzvWeBbSXHPgTai3S41CjoBBodxMB9DB2Dwt1kaRA9z2")
    val lgd = PK("9hdYFtdV8JLXJho4wAzy6dB6DHQn4gvZsmf4vf1kbkggJ59Wn3Z")
    val balb = PK("9gCJDv78SUUes6sNo81KqbP4yu3vHU6GtcatvmySiBsRoU1k4T8")
    val mgpai = PK("9g5yzitxX53B4RVi1DHLjrMx7iwTQn38kLG2XVVkhvHvcB1TcEz")
    val msig_addresses: Coll[SigmaProp] = Coll(adoo, lgd, balb, mgpai)
    val min_required_signatures: Int = 2
    val ergoname_msig_sigmaprop: SigmaProp = atLeast(min_required_signatures, msig_addresses)
    val addresses: Coll[SigmaProp] = msig_addresses ++ ergoname_msig_sigmaprop

    val shares: Coll[Long] = Coll(500L, 200L, 100L, 50L, 150L)
    val recipientCount: Int = addresses.size

    val _action: Int = if (getVar[Int](0).isDefined) getVar[Int](0).get else 0

    if (_action == 1) {

        val validDistribution: Boolean = {

            // Total value entering from fee-split boxes (allows sweeping
            // several accumulated fee boxes in one distribution).
            val totalIn: Long = INPUTS.fold(0L, { (sum: Long, box: Box) =>
                if (box.propositionBytes == SELF.propositionBytes) sum + box.value else sum
            })

            val minerFeeBoxOut: Box = OUTPUTS(recipientCount)
            val distributable: Long = totalIn - minerFeeBoxOut.value

            val indices: Coll[Int] = addresses.indices

            val validRecipients: Boolean = indices.forall { (i: Int) =>
                val out: Box = OUTPUTS(i)
                val owed: Long = (distributable * shares(i)) / 1000L
                val address: SigmaProp = addresses(i)
                (out.propositionBytes == address.propBytes) && (out.value >= owed)
            }

            val minerFeeErgoTreeHash: Coll[Byte] = fromBase16("e540cceffd3b8dd0f401193576cc413467039695969427df94454193dddfb375")
            val validMinerFee: Boolean = (blake2b256(minerFeeBoxOut.propositionBytes) == minerFeeErgoTreeHash)

            validRecipients && validMinerFee

        }

        sigmaProp(validDistribution)

    } else {

        ergoname_msig_sigmaprop

    }

}