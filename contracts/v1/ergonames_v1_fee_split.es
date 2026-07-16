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

    val adoo = PK("3WvubspBMttcKU97e6oAKdjgaXmoVUDDi6aKdt3in9zTvzSUTxto")
    val lgd = PK("3WxJrwDLXgGE53KpdJ2nSjSMRdXaDWh7Fdz9MY2Zh37UAwfLXzBU")
    val balb = PK("3WvubspBMttcKU97e6oAKdjgaXmoVUDDi6aKdt3in9zTvzSUTxto")
    val mgpai = PK("3WxJrwDLXgGE53KpdJ2nSjSMRdXaDWh7Fdz9MY2Zh37UAwfLXzBU")
    val addresses: Coll[SigmaProp] = Coll(adoo, lgd, balb, mgpai)
    val minRequiredSignatures = 2
    val ergonameMultiSigSigmaProp = atLeast(minRequiredSignatures, addresses)

    val shares: Coll[Long] = Coll(250L, 250L, 250L, 250L)
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

        ergonameMultiSigSigmaProp

    }

}