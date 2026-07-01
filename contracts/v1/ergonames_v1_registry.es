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
    // $revealContractBytesHash: Coll[Byte]
    // $subNameContractBytesHash: Coll[Byte]
    // $ergoNameFeeContractBytesHash: Coll[Byte]
    // $configSingletonTokenId: Coll[Byte]
    // $sigUsdOracleSingletonTokenId: Coll[Byte]
    // $ergonameMultiSigSigmaProp: SigmaProp

    // ===== Context Variables (_) ===== //
    // _ergoNameHash: Coll[Byte]    - Hash of the ErgoName to register.
    // _insertionProof: Coll[Byte]  - Proof that the ErgoNameHash and ErgoNameTokenId were inserted into the registry avl tree.
    // _lookupProof: Coll[Byte]     - Proof for getting a value from the config avl tree.

    // ===== User Defined Functions ===== //
    // def calcUsdPriceInCents: (Coll[Byte] => BigInt)
    // def isValidAscii: (Coll[Byte] => Boolean)

    def calcUsdPrice(charsAndMap: (Coll[Byte], Coll[BigInt])): BigInt = {

        // We assume the input can be interpreted as a valid ascii char byte collection.

        // USD price map in dollars, price map collection index is the amount of chars.
        val chars: Coll[Byte]               = charsAndMap._1
        val priceMapInner: Coll[BigInt]     = charsAndMap._2
        val supremum: Int                   = (priceMapInner.size - 1)

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

    // ===== Relevant Variables ===== //
    val previousRegistry: AvlTree           = SELF.R4[AvlTree].get
    val previousState: (Coll[Byte], Long)   = SELF.R5[(Coll[Byte], Long)].get
    val ageThreshold: (Int, Int)            = SELF.R6[(Int, Int)].get
    val priceMap: Coll[BigInt]              = SELF.R7[Coll[BigInt]].get
    val minCommitBoxAge: Int                = ageThreshold._1
    val maxCommitBoxAge: Int                = ageThreshold._2

    // Genesis 2-of-4 governance multisig (Minotaur, validated 2026-06-29).
    // Replaces the operator-derived 1-of-2 placeholder (H3 key replacement).
    val govA = PK("9g5yzitxX53B4RVi1DHLjrMx7iwTQn38kLG2XVVkhvHvcB1TcEz")
    val govB = PK("9gCJDv78SUUes6sNo81KqbP4yu3vHU6GtcatvmySiBsRoU1k4T8")
    val govC = PK("9hdYFtdV8JLXJho4wAzy6dB6DHQn4gvZsmf4vf1kbkggJ59Wn3Z")
    val govD = PK("9iJV2D1gzvWeBbSXHPgTai3S41CjoBBodxMB9DB2Dwt1kaRA9z2")
    val $ergonameMultiSigSigmaProp = atLeast(2, Coll(govA, govB, govC, govD))

    // The mint validation dereferences inputs, outputs, and context variables
    // that only exist in a well-formed mint tx; evaluating it in any other tx
    // shape throws, which would make the multisig escape hatch unreachable.
    // Guard it behind a structural check so non-mint spends fall through to
    // the multisig branch.
    val isMintShaped: Boolean = allOf(Coll(
        (INPUTS.size >= 3),
        (OUTPUTS.size == 6),
        (CONTEXT.dataInputs.size >= 1),
        getVar[Coll[Byte]](0).isDefined,
        getVar[Coll[Byte]](1).isDefined
    ))

    if (isMintShaped) {

    val _ergoNameHash: Coll[Byte]   = getVar[Coll[Byte]](0).get
    val _insertionProof: Coll[Byte] = getVar[Coll[Byte]](1).get

    val isDefaultPaymentMode: Boolean = (CONTEXT.dataInputs.size == 1)

    // ===== Mint ErgoName Tx ===== //
    val validMintErgoNameTx: Boolean = {

        // Inputs
        val revealBoxIn: Box = INPUTS(0)
        val commitBoxIn: Box = INPUTS(2)

        // Outputs
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
                    // H2: pin the FULL tree config (digest + insert flag + keyLength),
                    // not just the digest — else a mint-tx builder can recreate R4 with
                    // the same digest but insert DISABLED, permanently bricking every
                    // future mint (previousRegistry.insert returns None → .get throws).
                    // AvlTree `==` compares all fields; newRegistry carries previous-
                    // Registry's flags/keyLength, so this also keeps insert enabled.
                    (registryBoxOut.R4[AvlTree].get == newRegistry),
                    (_ergoNameHash == blake2b256(ergoNameBytes))
                ))

            }

            val validSelfRecreation: Boolean = {

                allOf(Coll(
                    (registryBoxOut.value == SELF.value),
                    (registryBoxOut.propositionBytes == SELF.propositionBytes),
                    (registryBoxOut.tokens(0) == SELF.tokens(0)),
                    (registryBoxOut.R6[(Int, Int)].get == ageThreshold),
                    (registryBoxOut.R7[Coll[BigInt]].get == priceMap)
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
                (blake2b256(subNameRegistryBoxOut.propositionBytes) == $subNameContractBytesHash),
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
            val equivalentNanoErg: BigInt                   = (nanoErgPerUsd * price) / 100.toBigInt // price map is in US cents
            val validSigUsdOracle: Boolean                  = (sigUsdOracleBoxIn.tokens(0)._1 == $sigUsdOracleSingletonTokenId)

            if (isDefaultPaymentMode) {

                val validFeePayment: Boolean = {

                    val amount: BigInt      = revealBoxIn.value.toBigInt // Reveal box contains target price when reveal was created + 5% slippage.
                    val target: BigInt      = (amount * 100.toBigInt) / 105.toBigInt // 5% slippage
                    val slippage: BigInt    = (amount - target)
                    val difference: BigInt  = (equivalentNanoErg - target)
                    val isWithin: Boolean   = (difference >= 0 && difference < slippage) || (difference <= 0 && difference > -1.toBigInt * slippage)
                    
                    // NOTE: the former validChange check (issuance >= amount - equivalentNanoErg)
                    // was unsatisfiable: reveal.es pins issuance == minBoxValue while the reveal
                    // value equation forces amount - E >= 2*minBoxValue + minerFee. Overpayment is
                    // already bounded by isWithin (~5% slippage) and flows to the fee box.
                    val validFee: Boolean       = (ergoNameFeeBoxOut.value.toBigInt >= equivalentNanoErg)

                    allOf(Coll(
                        isWithin,
                        validFee
                    ))
                    
                }

                val validFeeAddress: Boolean = (blake2b256(ergoNameFeeBoxOut.propositionBytes) == $ergoNameFeeContractBytesHash)

                allOf(Coll(
                    validSigUsdOracle,
                    validFeePayment,
                    validFeeAddress
                ))

            } else {

                // M2: token payments are DISABLED at genesis. The token path derived
                // the price from instantaneous ErgoDex pool reserves with NO slippage
                // bound or oracle anchoring (the ERG path enforces ±5% via isWithin),
                // so a pool imbalance could over/underprice a name. Any non-default
                // payment mode is rejected here; the token-payment path must be
                // rebuilt (SigUSD-oracle-anchored + a slippage window) and re-audited
                // before re-enabling. The genesis config tree also stays empty.
                false

            }

        }

        // M1: authenticate INPUTS(0) as the real reveal contract. Without this an
        // attacker can pass their own P2PK box with a crafted R9 as INPUTS(0),
        // bypassing reveal.es (no collection token burned) yet still inserting a
        // name into the authoritative AVL tree. reveal.es itself authenticates the
        // commit box, so this single binding secures the whole mint input chain.
        val validRevealBoxAuth: Boolean = (blake2b256(revealBoxIn.propositionBytes) == $revealContractBytesHash)

        allOf(Coll(
            validRevealBoxAuth,
            validErgoNameFormat,
            validCommit,
            validRegistryUpdate,
            validSubNameRegistryBoxOut,
            validErgoNameFeeBoxOut
        ))

    }

    // H3: the multisig escape hatch lives ONLY in the non-mint `else` branch
    // below. A mint-shaped tx must satisfy validMint — the multisig can no
    // longer override mint validation to seize the user's boxes.
    sigmaProp(validMintErgoNameTx)

    } else {

        $ergonameMultiSigSigmaProp

    }

}