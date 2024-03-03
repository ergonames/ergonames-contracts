{
    // ===== Contract Description ===== //
    // Name: ErgoNames Minting Contract
    // Description: This contract ensures ErgoNames tokens are distributed properly
    // Version: 1.0.0
    // Author: zackbalbin@github.com
    // Auditor: mgpai22@github.com

    // ===== AVL Box Registers ===== //
    // R4: AvlTree

    // ===== Commitment Box Registers ===== //
    // R4: Commitment Hash

    // ===== Compile Time Constants ===== //
    // _singletonToken: Coll[Byte]
    // _subnameContractBytes: Coll[Byte]

    // ===== Context Extension Variables ===== //
    // ergonameHash: Coll[Byte] - hash of the name to register
    // proof: Coll[Byte] - proof ergonameHash and tokenId insertion into the registry

    val registryInputBox = SELF
    val userInputBox = INPUTS(1) // proxy input
    val commitmentBox = INPUTS(2) // commitment input

    val registry = registryInputBox.R4[AvlTree].get

    val tokenIdToRegister = registryInputBox.id

    val ergonameHash = getVar[Coll[Byte]](0).get
    val proof = getVar[Coll[Byte]](1).get

    val nameToRegister = userInputBox.R4[Coll[Byte]].get
    val receiverAddress = userInputBox.R5[SigmaProp].get

    val mintBox = OUTPUTS(0)
    val updatedRegistryBox = OUTPUTS(1)
    val subnameRegistryBox = OUTPUTS(2)

    val newErgoNameToken = mintBox.tokens(0)

    val currentIndex = registryInputBox.R5[(Coll[Byte], Long)].get._2
    val newIndex = updatedRegistryBox.R5[(Coll[Byte], Long)].get._2
    val newErgoNameTokenInRegistry = updatedRegistryBox.R5[(Coll[Byte], Long)].get._1

    val minCommitmentBoxAge = 3
    val maxCommitmentBoxAge = 300

    val mintNewErgoName = {
        val validToken = newErgoNameToken == (tokenIdToRegister, 1L)
        val validErgoNameName = mintBox.R4[Coll[Byte]].get == nameToRegister
        val validReceiver = mintBox.propositionBytes == receiverAddress.propBytes

         allOf(Coll(
            validToken,
            validErgoNameName,
            validReceiver
         ))
    }

    val updateRegistry = {
        val updatedRegistry = registry.insert(Coll((ergonameHash, tokenIdToRegister)), proof).get
        val validRegistryUpdate = updatedRegistryBox.R4[AvlTree].get.digest == updatedRegistry.digest
        val validScript = updatedRegistryBox.propositionBytes == registryInputBox.propositionBytes
        val validIndex = currentIndex + 1L == newIndex
        val validErgoNamesTokenInRegister = newErgoNameTokenInRegistry == tokenIdToRegister

        allOf(Coll(
            validRegistryUpdate,
            validScript,
            validIndex,
            validErgoNamesTokenInRegister
        ))
    }

    val transferToken: Boolean = {
         allOf(Coll(
            (updatedRegistryBox.tokens(0) == (SELF.tokens(0)._1, 1L)),
            (updatedRegistryBox.tokens(0)._1 == _singletonToken)
         ))
    }

    val validCommitmentRequirements = {
        val commitAge = HEIGHT - commitmentBox.creationInfo._1
        val validCommitmentAge = commitAge >= minCommitmentBoxAge && commitAge <= maxCommitmentBoxAge

        val commitmentSecret = userInputBox.R6[Coll[Byte]].get
        val expectedCommitmentHash = commitmentBox.R4[Coll[Byte]].get
        val calculatedHash = blake2b256(
            commitmentSecret ++
            receiverAddress.propBytes ++ // attacker cannot replicate this, making frontrunning impossible :)
            nameToRegister
        )

        val validCommitment = calculatedHash == expectedCommitmentHash

        allOf(Coll(
            validCommitmentAge,
            validCommitment
        ))
    }

    val validSubnameTreeCreation = {

        val emptyDigest = fromBase16("4ec61f485b98eb87153f7c57db4f5ecd75556fddbc403b41acf8441fde8e160900")
        val validSubnameAddress = subnameRegistryBox.propositionBytes == _subnameContractBytes
        val validSubnameRegistryCreation = subnameRegistryBox.R4[AvlTree].get.digest == emptyDigest
        val validParentIdentifier = subnameRegistryBox.R5[Coll[Byte]].get == tokenIdToRegister

        allOf(Coll(
            validSubnameAddress,
            validSubnameRegistryCreation,
            validParentIdentifier
        ))
    }

    val validRegistration: Boolean = {

         allOf(Coll(
            mintNewErgoName,
            updateRegistry,
            transferToken,
            validCommitmentRequirements,
            validSubnameTreeCreation
         ))
    }

    sigmaProp(validRegistration)

}