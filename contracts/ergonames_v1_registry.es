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

    // ===== Relevant Transactions ===== //
    // 1. Mint ErgoName
    // Inputs: Registry, Reveal, Commit
    // Data Inputs: Config, SigUSDOracleDataPoint
    // Outputs: Registry, SubNameRegistry, ErgoNameIssuer, ErgoNameFee, MinerFee, TxOperatorFee
    // Context Variables: None

    // ===== Compile Time Constants ($) ===== //
    // $ergoNameIssuerContractBytes: Coll[Byte]
    // $subNameContractBytes: Coll[Byte]
    // $configContractBytes: Coll[Byte]
    // $ergoDexErgSigUsdPoolId: Coll[Byte]

    // ===== Context Variables (_) ===== //
    // _ergoNameHash: Coll[Byte]    - Hash of the ErgoName to register
    // _insertionProof: Coll[Byte]  - Proof that the ErgoNameHash and ErgoNameTokenId were inserted into the registry avl tree.  

    // ===== User-Defined Functions ===== //
    // def calcUsdPriceInCents: (Coll[Byte] => BigInt)
    // def isValidAscii: (Coll[Byte] => Boolean)

    // ===== Relevant Variables ===== //
    val prevRegistry: AvlTree = SELF.R4[AvlTree].get
    val previousState: (Coll[Byte], Long) = SELF.R5[(Coll[Byte], Long)].get
    val ageThreshold: (Int, Int) = SELF.R6[(Int, Int)].get
    val minCommitBoxAge: Int = ageThreshold._1
    val maxCommitBoxAge: Int = ageThreshold._2
    val _ergoNameHash: Coll[Byte] = getVar[Coll[Byte]](0).get
    val _proof: Coll[Byte] = getVar[Coll[Byte]](1).get
    val isDefaultPaymentMode: Boolean = (CONTEXT.dataInputs.size == 1)

    // ===== User-Defined Functions ===== //
    def calcUsdPriceInCents(chars: Coll[Byte]): BigInt = {
        
        // We assume the input can be interpreted as a valid ascii char byte collection.
        
        // USD price map in cents, collection index is the amount of chars.
        val priceMap: Coll[BigInt] = Coll(0, 0, 0, 50000, 15000, 5000, 5000, 1500, 1500, 500)
        
        if (chars.size <= 8) {
            priceMap(chars.size)
        } else {
            priceMap(8)
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

        val validErgoNameFormat: Boolean = isValidAscii(ergoNameBytes)

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

                val newRegistry: AvlTree = previousRegistry.insert(Coll((_ergoNameHash, ergoNameTokenId)), _proof).get
                
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

            if (isDefaultPaymentMode) {

                val ergoDexErgSigUsdPool: Box = CONTEXT.dataInputs(0)
                val nanoErgVolume: BigInt = ergoDexErgSigUsdPool.value
                val sigUsdVolume: BigInt = ergoDexErgSigUsdPool.tokens(2)._2
                val price: BigInt = calcUsdPriceInCents(ergoNameBytes)
                val equivalentNanoErg: BigInt = (nanoErgVolume * price) / sigUsdVolume
                
                val validErgoDexErgSigUsdPool: Boolean = (ergoDexErgSigUsdPool.tokens(0)._1 == $ergoDexErgSigUsdPoolId)
                val validPayment: Boolean = (ergoNameFeeBoxOut.value.toBigInt == equivalentNanoErg)

                allOf(Coll(
                    validErgoDexErgSigUsdPool,
                    validPayment
                ))

            } else {



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

    sigmaProp(validMintErgoNameTx)
    
}