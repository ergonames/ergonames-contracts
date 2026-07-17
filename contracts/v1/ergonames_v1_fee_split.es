{

    // ===== Contract Description ===== //
    // Name: ErgoNames Fee Split Contract
    // Description: Accumulates ErgoName minting fees; spendable only by a
    //              distribution transaction that pays every configured
    //              recipient at least its per-mille share of the distributed
    //              value, or by the multisig escape hatch.
    // Version: 1.0.0

    // ===== Box Contents ===== //
    // Tokens: none expected (ERG fees only in default payment mode)
    // Registers: none

    // ===== Relevant Transactions ===== //
    // 1. Distribute Fees
    // Inputs: one or more FeeSplit boxes
    // Outputs: one box per recipient (same order as $recipientPropBytes),
    //          then the miner fee box
    // Context Variables: none

    // ===== Compile Time Constants ($) ===== //
    // $recipientPropBytes: Coll[Coll[Byte]] - propositionBytes per recipient
    // $recipientShares: Coll[Long]          - per-mille share per recipient

    // Genesis 2-of-4 governance multisig (Minotaur, validated 2026-06-29).
    // Replaces the operator-derived 1-of-2 placeholder (H3 key replacement).
    val govA = PK("9g5yzitxX53B4RVi1DHLjrMx7iwTQn38kLG2XVVkhvHvcB1TcEz")
    val govB = PK("9gCJDv78SUUes6sNo81KqbP4yu3vHU6GtcatvmySiBsRoU1k4T8")
    val govC = PK("9hdYFtdV8JLXJho4wAzy6dB6DHQn4gvZsmf4vf1kbkggJ59Wn3Z")
    val govD = PK("9iJV2D1gzvWeBbSXHPgTai3S41CjoBBodxMB9DB2Dwt1kaRA9z2")
    val $ergonameMultiSigSigmaProp = atLeast(2, Coll(govA, govB, govC, govD))

    val recipientCount: Int = $recipientPropBytes.size

    val isDistributionShaped: Boolean = (OUTPUTS.size == recipientCount + 1)

    if (isDistributionShaped) {

        val validDistribution: Boolean = {

            // Total value entering from fee-split boxes (allows sweeping
            // several accumulated fee boxes in one distribution).
            val totalIn: Long = INPUTS.fold(0L, { (sum: Long, box: Box) =>
                if (box.propositionBytes == SELF.propositionBytes) sum + box.value else sum
            })

            val minerFeeBoxOut: Box = OUTPUTS(recipientCount)
            val distributable: Long = totalIn - minerFeeBoxOut.value

            val indices: Coll[Int] = $recipientPropBytes.indices

            val validRecipients: Boolean = indices.forall { (i: Int) =>
                val out: Box = OUTPUTS(i)
                val owed: Long = (distributable * $recipientShares(i)) / 1000L
                (out.propositionBytes == $recipientPropBytes(i)) && (out.value >= owed)
            }

            val minerFeeErgoTreeHash: Coll[Byte] = fromBase16("e540cceffd3b8dd0f401193576cc413467039695969427df94454193dddfb375")
            val validMinerFee: Boolean = (blake2b256(minerFeeBoxOut.propositionBytes) == minerFeeErgoTreeHash)

            validRecipients && validMinerFee

        }

        // H3: multisig only in the `else` branch; a distribution-shaped tx must
        // satisfy validDistribution (no multisig override).
        sigmaProp(validDistribution)

    } else {

        $ergonameMultiSigSigmaProp

    }

}
