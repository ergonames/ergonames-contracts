{
    // ===== DRAFT: Burn ErgoName branch for ergonames_v1_registry.es =====
    // Faithful, compilable reproduction of the proposed "Burn ErgoName" branch
    // (the exact reads/AVL ops it adds to the registry). Mirrors the subname
    // registry's self-burn (action 2). See BURN_DESIGN.md for integration.
    //
    // Burn ErgoName Tx
    //   Inputs:  Registry (SELF, INPUTS(0)), ErgoNameNFT (INPUTS(1), owner-signed)
    //   Outputs: Registry' (OUTPUTS(0)); owner refund + miner fee follow.
    //            The ErgoName token appears in NO output (burned).
    //   Context: getVar(0)=ErgoNameHash, getVar(1)=LookUpProof,
    //            getVar(2)=RemoveProof, getVar(3)=burn action byte (==1)

    // ---- SELF (registry) registers — identical types to the mint branch ----
    val previousRegistry: AvlTree           = SELF.R4[AvlTree].get
    val previousState: (Coll[Byte], Long)   = SELF.R5[(Coll[Byte], Long)].get
    val ageThreshold: (Int, Int)            = SELF.R6[(Int, Int)].get
    val priceMap: Coll[BigInt]              = SELF.R7[Coll[BigInt]].get

    val _ergoNameHash: Coll[Byte]   = getVar[Coll[Byte]](0).get
    val _lookupProof: Coll[Byte]    = getVar[Coll[Byte]](1).get
    val _removeProof: Coll[Byte]    = getVar[Coll[Byte]](2).get

    val ergoNameNftBoxIn: Box       = INPUTS(1)
    val registryBoxOut: Box         = OUTPUTS(0)

    val burnedTokenId: Coll[Byte]   = ergoNameNftBoxIn.tokens(0)._1

    // The registry maps this name to exactly the token being burned. Spending
    // the NFT box requires the owner's signature, so providing it IS the
    // authorisation; this binds the removed name to that owned token (you cannot
    // burn token A while removing name B, nor remove a name you do not hold).
    val validErgoName: Boolean = {
        val tokenId: Option[Coll[Byte]] = previousRegistry.get(_ergoNameHash, _lookupProof)
        if (tokenId.isDefined) {
            (tokenId.get == burnedTokenId)
        } else {
            false
        }
    }

    // The name's entry is removed from the registry AVL tree. REQUIRES the tree
    // to have been created with removeAllowed = true (genesis config — see H2).
    val validRemoval: Boolean = {
        val newRegistry: AvlTree = previousRegistry.remove(Coll(_ergoNameHash), _removeProof).get
        (registryBoxOut.R4[AvlTree].get.digest == newRegistry.digest)
    }

    // The ErgoName token is destroyed: it appears in no output.
    val validBurn: Boolean = {
        OUTPUTS.forall { (o: Box) =>
            o.tokens.forall { (t: (Coll[Byte], Long)) => (t._1 != burnedTokenId) }
        }
    }

    // Registry recreated unchanged except its (now-smaller) AVL tree: the
    // cumulative state counter, age threshold and price map are preserved.
    val validSelfRecreation: Boolean = {
        allOf(Coll(
            (registryBoxOut.value == SELF.value),
            (registryBoxOut.propositionBytes == SELF.propositionBytes),
            (registryBoxOut.tokens(0) == SELF.tokens(0)),
            (registryBoxOut.R5[(Coll[Byte], Long)].get == previousState),
            (registryBoxOut.R6[(Int, Int)].get == ageThreshold),
            (registryBoxOut.R7[Coll[BigInt]].get == priceMap)
        ))
    }

    sigmaProp(allOf(Coll(
        validErgoName,
        validRemoval,
        validBurn,
        validSelfRecreation
    )))

}
