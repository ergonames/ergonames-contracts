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

    val address1 = PK("9h2g3WsPy5Ty2Ekq4w9tKMVrco4d5iu9dAxYLoTFoDggwSCW5yk")
    val address2 = PK("9fXDsjy38dyu1bzRbe6tp6Ltw4m2u6je98ujv82pbGw78uExcd9")
    val $ergonameMultiSigSigmaProp = atLeast(1, Coll(address1, address2))

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

        sigmaProp(validDistribution) || $ergonameMultiSigSigmaProp

    } else {

        $ergonameMultiSigSigmaProp

    }

}
