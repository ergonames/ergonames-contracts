{
    // ===== Contract Description ===== //
    // Name: ErgoNames Registry Contract
    // Description: Contract managing the minting and price calculation of user ErgoName.
    // Version: 1.0.0
    // Author: Luca D'Angelo (ldgaetano@protonmail.com)

    // ===== Box Contents ===== //
    // Tokens
    // 1. (RegistrySingletonTokenId, 1L)
    // 2. (ErgoNameCollectionTokenId, Long.MaxValue)
    // Registers
    // R4: AvlTree              RegistryAvlTree
    // R5: (Coll[Byte], Long)   PreviousState
    // R6: (Int, Int)           AgeThreshold
    // R7: Coll[BigInt]         PriceMap

    // ===== Relevant Transactions ===== //
    // 1. Mint ErgoName
    // Inputs: Registry, Reveal, Commit
    // Data Inputs: ErgoDexErg2SigUsd, ?ErgoDexErg2Token, ?Config
    // Outputs: Registry, SubNameRegistry, ErgoNameIssuer, ErgoNameFee, MinerFee, TxOperatorFee
    // Context Variables: None

    // ===== Compile Time Constants ($) ===== //
    // $subNameContractBytes: Coll[Byte]
    // $ergoNameIssuerContractBytes: Coll[Byte]
    // $ergoNameFeeContractBytes: Coll[Byte]
    // $configSingletonTokenId: Coll[Byte]
    // $ergoDexErgSigUsdPoolId: Coll[Byte]

    // ===== Context Variables (_) ===== //
    // _ergoNameHash: Coll[Byte]    - Hash of the ErgoName to register
    // _insertionProof: Coll[Byte]  - Proof that the ErgoNameHash and ErgoNameTokenId were inserted into the registry avl tree.  
    // _lookupProof: Coll[Byte]     - Proof for getting a value from the config avl tree.

    // ===== User-Defined Functions ===== //
    // def calcUsdPriceInCents: (Coll[Byte] => BigInt)
    // def isValidAscii: (Coll[Byte] => Boolean)

    // ===== Relevant Variables ===== //
    val prevRegistry: AvlTree = SELF.R4[AvlTree].get
    val previousState: (Coll[Byte], Long) = SELF.R5[(Coll[Byte], Long)].get
    val ageThreshold: (Int, Int) = SELF.R6[(Int, Int)].get
    val priceMap: Coll[BigInt] = SELF.R7[Coll[BigInt]].get
    val minCommitBoxAge: Int = ageThreshold._1
    val maxCommitBoxAge: Int = ageThreshold._2

    val _ergoNameHash: Coll[Byte] = getVar[Coll[Byte]](0).get
    val _insertionProof: Coll[Byte] = getVar[Coll[Byte]](1).get

    val isDefaultPaymentMode: Boolean = (CONTEXT.dataInputs.size == 1)

    val pk1: SigmaProp = PK("")
    val pk2: SigmaProp = PK("")
    val pk3: SigmaProp = PK("")
    val pk4: SigmaProp = PK("")

    // ===== User-Defined Functions ===== //
    def calcUsdPriceInCents(charsAndMap: (Coll[Byte], Coll[BigInt])): BigInt = {
        
        // We assume the input can be interpreted as a valid ascii char byte collection.
        
        // USD price map in cents, collection index is the amount of chars.
        val chars: Coll[Byte] = charsAndMap._1
        val priceMap: Coll[BigInt] = charsAndMap._2
        val supremum: Int = (priceMap.size - 1)
        
        if (chars.size <= supremum) {
            priceMap(chars.size)
        } else {
            priceMap(supremum)
        }

    }

    def isValidAscii(chars: Coll[Byte]): Boolean = {

        // We assume the input can be interpreted as a valid ascii char byte collection.

        // Allowed ASCII characters (based on X.com handle format)
        val zero: Byte = 48         // Numbers lower-bound
        val nine: Byte = 57         // Numbers upper-bound
        val A: Byte = 65            // Upper-case letters lower-bound
        val Z: Byte = 90            // Upper-case letters upper-bound
        val a: Byte = 97            // Lower-case letters lower-bound
        val z: Byte = 122           // Lower-case letters upper-bound
        val underscore: Byte = 95   // The only non-alphanumeric character allowed

        // All characters must be a digit, an uppercase letter, a lowercase letter, an underscore, or any combination thereof.
        chars.forall((char: Byte) => {

            val isDigit: Boolean = (char >= zero && char <= nine)
            val isUpperCaseLetter: Boolean = (char >= A && char <= Z)
            val isLowerCaseLetter: Boolean = (char >= a && char <= z)
            val isUnderscore: Boolena = (char == underscore)

            (isDigit || isUpperCaseLetter || isLowerCaseLetter || isUnderscore)

        })

    }

    // ===== Mint ErgoName Tx ===== //
    val validMintErgoNameTx: Boolean = {

        // Inputs
        val revealBoxIn: Box = INPUTS(1)
        val commitBoxIn: Box = INPUTS(2)

        // Outputs
        val registryBoxOut: Box = OUTPUTS(0)
        val subNameRegistryBoxOut: Box = OUTPUTS(1)
        val ergoNameIssuerBoxOut: Box = OUTPUTS(2)
        val ergoNameFeeBoxOut: Box = OUTPUTS(3)
        val minerFeeBoxOut: Box = OUTPUTS(4)
        val txOperatorFeeBoxOut: Box = OUTPUTS(5)

        // Relevant Variables
        val ergoNameTokenId: Coll[Byte] = ergoNameIssuerBoxOut.id // Thus all ErgoName token ids will be unique.
        
        val commitAge: Int = (HEIGHT - commitBoxIn.creationInfo._1)
        val commitHash: Coll[Byte] = commitBoxIn.R4[Coll[Byte]].get

        val ergoNameBytes: Coll[Byte] = revealBoxIn.R4[Coll[Byte]].get
        val receiverPKGroupElement: GroupElement = revealBoxIn.R5[GroupElement].get
        val receiverPKSigmaProp: SigmaProp = proveDlog(receiverPKGroupElement)
        val commitSecret: Coll[Byte] = revealBoxIn.R6[Coll[Byte]].get

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

            val validCommitAge: Boolean = (commitAge >= minCommitBoxAge) && (commitAge <= maxCommitBoxAge)
            val validCalculatedHash: Boolean = (calculatedHash == commitHash)

            allOf(Coll(
                validCommitAge,
                validCalculatedHash
            ))

        }

        val validRegistryUpdate: Boolean = {

            val validStateUpdate: Boolean = {
                
                val newState: (Coll[Byte], Long) = registryBoxOut.R5[(Coll[Byte], Long)].get

                val validErgoNameTokenIdUpdate: Boolean = (newState._1 == ergoNameTokenId)
                val validIndexIncrement: Boolean = (newState._2 == previousState._2 + 1L)

                allOf(Coll(
                    validErgoNameTokenIdUpdate,
                    validIndexIncrement
                ))

            }

            val validErgoNameInsertion: Boolean = {

                val newRegistry: AvlTree = previousRegistry.insert(Coll((_ergoNameHash, ergoNameTokenId)), _insertionProof).get
                
                (registryBoxOut.R4[AvlTree].get.digest == newRegistry.digest)

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

        val validSubNameIssuerBoxOut: Boolean = {

            val emptyDigest: Coll[Byte] = fromBase16("4ec61f485b98eb87153f7c57db4f5ecd75556fddbc403b41acf8441fde8e160900")

            allOf(Coll(
                (subNameRegistryBoxOut.propositionBytes == $subNameContractBytes),
                (subNameRegistryBoxOut.R4[AvlTree].get.digest == emptyDigest),
                (subNameRegistryBoxOut.R5[Coll[Byte]] == ergoNameTokenId)
            ))

        }

        val validErgoNameIssuerBoxOut: Boolean = {

            val validErgoNameCollectionTokenTransfer: Boolean = {

                allOf(Coll(
                    (registryBoxOut.tokens(1) == (SELF.tokens(1)._1, SELF.tokens(1)._2 - 1L)),
                    (ergoNameIssuerBoxOut.tokens(0) == (SELF.tokens(1)._1, 1L))
                ))

            }

            allOf(Coll(
                (ergoNameIssuerBoxOut.propositionBytes == $ergoNameIssuerContractBytes),
                (ergoNameIssuerBoxOut.R4[GroupElement].get == receiverPKGroupElement),
                (ergoNameIssuerBoxOut.R5[Coll[Byte]].get == ergoNameBytes),
                validErgoNameCollectionTokenTransfer
            ))

        }

        val validErgoNameFeeBoxOut: Boolean = {

            val ergoDexErg2SigUsdPoolBoxIn: Box = CONTEXT.dataInputs(0)
            val nanoErgVolume: BigInt = ergoDexErgSigUsdPoolBoxIn.value
            val sigUsdVolume: BigInt = ergoDexErgSigUsdPoolBoxIn.tokens(2)._2
            val charsAndMap: (Coll[Byte], Coll[BigInt]) = (ergoNameBytes, priceMap)
            val price: BigInt = calcUsdPriceInCents(charsAndMap)
            val equivalentNanoErg: BigInt = (nanoErgVolume * price) / sigUsdVolume

            if (isDefaultPaymentMode) {

                val validErgoDexErgSigUsdPool: Boolean = (ergoDexErgSigUsdPoolBoxIn.tokens(0)._1 == $ergoDexErgSigUsdPoolId)
                val validFeePayment: Boolean = (ergoNameFeeBoxOut.value.toBigInt == equivalentNanoErg)
                val validFeeAddress: Boolean = (ergoNameFeeBoxOut.propositionBytes == $ergoNameFeeContractBytes)

                allOf(Coll(
                    validErgoDexErgSigUsdPool,
                    validFeePayment,
                    validFeeAddress
                ))

            } else {

                val ergoDexErg2TokenPoolBoxIn: Box = CONTEXT.dataInputs(1)
                val configBoxIn: Box = CONTEXT.dataInputs(2)

                val paymentTokenId: Coll[Byte] = revealBoxIn.tokens(0)._1 
                val configAvlTree: AvlTree = configBoxIn.R4[AvlTree].get
                val _lookupProof: Coll[Byte] = getVar[Coll[Byte]](1).get
                val configElement: Coll[Byte] = configAvlTree.get(paymentTokenId, _lookupProof)
                
                if (configElement.isEmpty) {
                    false
                } else {

                    val ergoDexErg2TokenPoolId: Coll[Byte] = configElement.get.slice(0, 32)
                    val nanoErgVolume_2: BigInt = ergoDexErg2TokenPoolBoxIn.value
                    val tokenVolume_2: BigInt = ergoDexErg2TokenPoolBoxIn.tokens(2)._2
                    val equivalentPaymentTokenAmount: BigInt = (tokenVolume_2 * equivalentNanoErg) / nanoErgVolume_2

                    val validConfigBoxIn: Boolean = (configBoxIn.tokens(0)._1 == $configSingletonTokenId)
                    val validErgoDexErgSigUsdPool: Boolean = (ergoDexErgSigUsdPoolBoxIn.tokens(0)._1 == $ergoDexErgSigUsdPoolId)
                    val validErgoDexErg2TokenPool: Boolean = (ergoDexErg2TokenPoolBoxIn.tokens(0)._1 == ergoDexErg2TokenPoolId)
                    val validFeePayment: Boolean = (ergoNameFeeBoxOut.tokens(0)._1, ergoNameFeeBoxOut.tokens(0)._2.toBigInt == (paymentTokenId, equivalentPaymentTokenAmount))
                    val validFeeAddress: Boolean = (ergoNameFeeBoxOut.propositionBytes == $ergoNameFeeContractBytes)                    

                    allOf(Coll(
                        validConfigBoxIn,
                        validErgoDexErgSigUsdPool,
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
            validSubNameIssuerBoxOut,
            validErgoNameIssuerBoxOut
        ))

    }

    sigmaProp(validMintErgoNameTx) || atLeast(3, Coll(pk1, pk2, pk3, pk4))
    
}