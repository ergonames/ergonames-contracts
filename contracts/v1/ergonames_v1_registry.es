{
    // ===== Contract Description ===== //
    // Name: ErgoNames Registry Contract
    // Description: Contract managing the minting and price calculation of user ErgoName.
    // Version: 1.0.0
    // Author: Luca D'Angelo (ldgaetano@protonmail.com)

    // ===== Box Contents ===== //
    // Tokens
    // 1. (RegistrySingletonTokenId, 1L)
    // Registers
    // R4: AvlTree              RegistryAvlTree
    // R5: (Coll[Byte], Long)   PreviousState
    // R6: (Int, Int)           AgeThreshold
    // R7: Coll[BigInt]         PriceMap

    // ===== Relevant Transactions ===== //
    // 1. Mint ErgoName
    // Inputs: Reveal, Registry, Commit
    // Data Inputs: SigUsdOracleDatapoint, ?ErgoDexErg2Token, ?Config
    // Outputs: ErgoNameIssuance, Registry, SubNameRegistry, ErgoNameFee, MinerFee, TxOperatorFee
    // Context Variables: ErgoNameHash, InsertionProof, LookUpProof

    // ===== Compile Time Constants ($) ===== //
    // $subNameContractBytes: Coll[Byte]
    // $ergoNameFeeContractBytes: Coll[Byte]
    // $configSingletonTokenId: Coll[Byte]
    // $sigUsdOracleSingletonTokenId: Coll[Byte]
    // $ergonameMultiSigSigmaProp: SigmaProp

    // ===== Context Variables (_) ===== //
    // _ergoNameHash: Coll[Byte]    - Hash of the ErgoName to register.
    // _insertionProof: Coll[Byte]  - Proof that the ErgoNameHash and ErgoNameTokenId were inserted into the registry avl tree.
    // _lookupProof: Coll[Byte]     - Proof for getting a value from the config avl tree.

    // ===== User-Defined Functions ===== //
    // def calcUsdPriceInCents: (Coll[Byte] => BigInt)
    // def isValidAscii: (Coll[Byte] => Boolean)

    // ===== Relevant Variables ===== //
    val previousRegistry: AvlTree           = SELF.R4[AvlTree].get
    val previousState: (Coll[Byte], Long)   = SELF.R5[(Coll[Byte], Long)].get
    val ageThreshold: (Int, Int)            = SELF.R6[(Int, Int)].get
    val priceMap: Coll[BigInt]              = SELF.R7[Coll[BigInt]].get
    val minCommitBoxAge: Int                = ageThreshold._1
    val maxCommitBoxAge: Int                = ageThreshold._2

    val _ergoNameHash: Coll[Byte]   = getVar[Coll[Byte]](0).get
    val _insertionProof: Coll[Byte] = getVar[Coll[Byte]](1).get

    val isDefaultPaymentMode: Boolean = (CONTEXT.dataInputs.size == 1)

    // ===== User-Defined Functions ===== //
    def calcUsdPrice(charsAndMap: (Coll[Byte], Coll[BigInt])): BigInt = {

        // We assume the input can be interpreted as a valid ascii char byte collection.

        // USD price map in dollars, price map collection index is the amount of chars.
        val chars: Coll[Byte]               = charsAndMap._1
        val priceMapInner: Coll[BigInt]     = charsAndMap._2
        val supremum: Int                   = (priceMap.size - 1)

        if (chars.size <= supremum) {
            priceMapInner(chars.size)
        } else {
            priceMapInner(supremum)
        }

    }

    def isValidAscii(chars: Coll[Byte]): Boolean = {
        // Allowed ASCII characters (based on x.com handle format)
        val zero: Byte          = 48         // Numbers lower-bound
        val nine: Byte          = 57         // Numbers upper-bound
        val A: Byte             = 65         // Upper-case letters lower-bound
        val Z: Byte             = 90         // Upper-case letters upper-bound
        val a: Byte             = 97         // Lower-case letters lower-bound
        val z: Byte             = 122        // Lower-case letters upper-bound
        val underscore: Byte    = 95         // The only non-alphanumeric character allowed

        // All characters must be a digit, an uppercase letter, a lowercase letter, an underscore, or any combination thereof.
        chars.forall { (char: Byte) =>
            val isDigit: Boolean            = char >= zero && char <= nine
            val isUpperCaseLetter: Boolean  = char >= A && char <= Z
            val isLowerCaseLetter: Boolean  = char >= a && char <= z
            val isUnderscore: Boolean       = char == underscore

            isDigit || isUpperCaseLetter || isLowerCaseLetter || isUnderscore
        }
    }

    // ===== Mint ErgoName Tx ===== //
    val validMintErgoNameTx: Boolean = {

        // Inputs
        val revealBoxIn: Box = INPUTS(0)
        val commitBoxIn: Box = INPUTS(2)

        // Outputs
        val ergoNameIssuanceBoxOut: Box = OUTPUTS(0)
        val registryBoxOut: Box         = OUTPUTS(1)
        val subNameRegistryBoxOut: Box  = OUTPUTS(2)
        val ergoNameFeeBoxOut: Box      = OUTPUTS(3)
        val minerFeeBoxOut: Box         = OUTPUTS(4)
        val txOperatorFeeBoxOut: Box    = OUTPUTS(5)

        // Relevant Variables
        val ergoNameTokenId: Coll[Byte]             = revealBoxIn.id // Thus all ErgoName token ids will be unique.

        val commitAge: Int                          = (HEIGHT - commitBoxIn.creationInfo._1)
        val commitHash: Coll[Byte]                  = commitBoxIn.R4[Coll[Byte]].get

        val revealData: (GroupElement, (Coll[Coll[Byte]], Coll[Long]))      = revealBoxIn.R9[(GroupElement, (Coll[Coll[Byte]], Coll[Long]))].get
        val ergoNameBytes: Coll[Byte]                                       = revealData._2._1(0)
        val receiverPKGroupElement: GroupElement                            = revealData._1
        val receiverPKSigmaProp: SigmaProp                                  = proveDlog(receiverPKGroupElement)
        val commitSecret: Coll[Byte]                                        = revealData._2._1(1)

        val validErgoNameFormat: Boolean = {

            allOf(Coll(
                (ergoNameBytes.size > 0),
                isValidAscii(ergoNameBytes)
            ))

        }

        val validCommit: Boolean = {

            val calculatedHash: Coll[Byte] = blake2b256(
                commitSecret ++
                receiverPKSigmaProp.propBytes ++
                ergoNameBytes
            )

            val validCommitAge: Boolean         = (commitAge >= minCommitBoxAge) && (commitAge <= maxCommitBoxAge)
            val validCalculatedHash: Boolean    = (calculatedHash == commitHash)

            allOf(Coll(
                validCommitAge,
                validCalculatedHash
            ))

        }

        val validRegistryUpdate: Boolean = {

            val validStateUpdate: Boolean = {

                val newState: (Coll[Byte], Long)        = registryBoxOut.R5[(Coll[Byte], Long)].get

                val validErgoNameTokenIdUpdate: Boolean = (newState._1 == ergoNameTokenId)
                val validIndexIncrement: Boolean        = (newState._2 == previousState._2 + 1L)

                allOf(Coll(
                    validErgoNameTokenIdUpdate,
                    validIndexIncrement
                ))

            }

            val validErgoNameInsertion: Boolean = {

                val newRegistry: AvlTree = previousRegistry.insert(Coll((_ergoNameHash, ergoNameTokenId)), _insertionProof).get

                allOf(Coll(
                    (registryBoxOut.R4[AvlTree].get.digest == newRegistry.digest),
                    (_ergoNameHash == blake2b256(ergoNameBytes))
                ))
              
            }

            val validSelfRecreation: Boolean = {

                allOf(Coll(
                    (registryBoxOut.value == SELF.value),
                    (registryBoxOut.propositionBytes == SELF.propositionBytes),
                    (registryBoxOut.tokens(0) == SELF.tokens(0))
                ))

            }

            allOf(Coll(
                validStateUpdate,
                validErgoNameInsertion,
                validSelfRecreation
            ))

        }

        val validSubNameRegistryBoxOut: Boolean = {

            val emptyDigest: Coll[Byte] = fromBase16("4ec61f485b98eb87153f7c57db4f5ecd75556fddbc403b41acf8441fde8e160900")

            allOf(Coll(
                (subNameRegistryBoxOut.propositionBytes == $subNameContractBytes),
                (subNameRegistryBoxOut.tokens(0) == (ergoNameTokenId, 1L)), // We mint a token without following the asset standard, just used for identification purposes. This will have the same token id as the user's ErgoName.
                (subNameRegistryBoxOut.R4[AvlTree].get.digest == emptyDigest),
                (subNameRegistryBoxOut.R5[(Coll[Byte], Long)].get == (Coll[Byte](), 0L)),
                (subNameRegistryBoxOut.R6[Coll[Byte]].get == ergoNameTokenId)
            ))

        }

        val validErgoNameFeeBoxOut: Boolean = {

            val sigUsdOracleBoxIn: Box                      = CONTEXT.dataInputs(0)
            val nanoErgPerUsd: Long                         = sigUsdOracleBoxIn.R4[Long].get
            val oracleHeight: Long                          = sigUsdOracleBoxIn.R5[Int].get
            val charsAndMap: (Coll[Byte], Coll[BigInt])     = (ergoNameBytes, priceMap)
            val price: BigInt                               = calcUsdPrice(charsAndMap)
            val equivalentNanoErg: BigInt                   = (nanoErgPerUsd * price)
            val validSigUsdOracle: Boolean                  = (sigUsdOracleBoxIn.tokens(0)._1 == $sigUsdOracleSingletonTokenId)

            if (isDefaultPaymentMode) {

                val validFeePayment: Boolean = (ergoNameFeeBoxOut.value.toBigInt >= equivalentNanoErg)
                val validFeeAddress: Boolean = (ergoNameFeeBoxOut.propositionBytes == $ergoNameFeeContractBytes)

                allOf(Coll(
                    validSigUsdOracle,
                    validFeePayment,
                    validFeeAddress
                ))

            } else {

                val ergoDexErg2TokenPoolBoxIn: Box              = CONTEXT.dataInputs(1)
                val configBoxIn: Box                            = CONTEXT.dataInputs(2)

                val paymentTokenId: Coll[Byte]                  = revealBoxIn.tokens(0)._1
                val configAvlTree: AvlTree                      = configBoxIn.R4[AvlTree].get
                val _lookupProof: Coll[Byte]                    = getVar[Coll[Byte]](2).get
                val configElement: Coll[Byte]                   = configAvlTree.get(paymentTokenId, _lookupProof)

                if (configElement.isEmpty) {
                    false
                } else {

                    val ergoDexErg2TokenPoolId: Coll[Byte]      = configElement.get.slice(0, 32)
                    val nanoErgVolume_2: BigInt                 = ergoDexErg2TokenPoolBoxIn.value
                    val tokenVolume_2: BigInt                   = ergoDexErg2TokenPoolBoxIn.tokens(2)._2
                    val equivalentPaymentTokenAmount: BigInt    = (tokenVolume_2 * equivalentNanoErg) / nanoErgVolume_2

                    val validConfigBoxIn: Boolean               = (configBoxIn.tokens(0)._1 == $configSingletonTokenId)
                    val validErgoDexErg2TokenPool: Boolean      = (ergoDexErg2TokenPoolBoxIn.tokens(0)._1 == ergoDexErg2TokenPoolId)
                    val validFeePayment: Boolean                = ((ergoNameFeeBoxOut.tokens(0)._1 == paymentTokenId) && (ergoNameFeeBoxOut.tokens(0)._2.toBigInt >= equivalentPaymentTokenAmount))
                    val validFeeAddress: Boolean                = (ergoNameFeeBoxOut.propositionBytes == $ergoNameFeeContractBytes)

                    allOf(Coll(
                        validSigUsdOracle,
                        validConfigBoxIn,
                        validErgoDexErg2TokenPool,
                        validFeePayment,
                        validFeeAddress
                    ))

                }

            }

        }

        allOf(Coll(
            validErgoNameFormat,
            validCommit,
            validRegistryUpdate,
            validSubNameRegistryBoxOut,
            validErgoNameFeeBoxOut
        ))

    }

    sigmaProp(validMintErgoNameTx) || $ergonameMultiSigSigmaProp

}
