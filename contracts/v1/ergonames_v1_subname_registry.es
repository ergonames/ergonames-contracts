{
    // ===== Contract Description ===== //
    // Name: ErgoNames Subname Registry Contract
    // Description: Contract managing the minting of the ErgoName subname.
    // Version: 1.0.0
    // Author: Luca D'Angelo (ldgaetano@protonmail.com)

    // ===== Box Contents ===== //
    // Tokens
    // 1. (SubNameRegistrySingletonTokenId, 1L)
    // Registers
    // R4: AvlTree              RegistryAvlTree
    // R5: (Coll[Byte], Long)   PreviousState
    // R6: Coll[Byte]           ParentErgoNameBytes

    // ===== Relevant Transactions ===== //
    // 1. Mint SubName
    // Inputs: ErgoNameNFT, ParentSubNameRegistry
    // Data Inputs: None
    // Outputs: SubNameNFT, ParentSubNameRegistry, ChildSubNameRegistry, ErgoNameNFT
    // Context Variables: Action, SubNameHash, InsertionProof
    // 2. SubName Self-Burn
    // Inputs: ParentSubNameRegistry, SubNameNFT
    // Data Inputs: None
    // Outputs: ParentSubNameRegistry
    // Context Variables: Action, SubNameHash, LookUpProof, RemoveProof
    // 3. SubName Parent Burns Child
    // Inputs: ParentSubNameRegistry, ParentSubNameNFT
    // Data Inputs: None
    // Outputs: ParentSubNameRegistry, ParentSubNameNFT
    // Context Variables: Action, SubNameHash, ContainsProof, RemoveProof

    // ===== Compile Time Constants ($) ===== //
    // None

    // ===== Context Variables (_) ===== //
    // _action: Int                 - Integer representing the transaction type.

    // ===== User Defined Functions ===== //
    // def isValidAscii: (Coll[Byte] => Boolean)

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
    val _action: Int = if (getVar[Int](0).isDefined) getVar[Int](0).get else 0
    val parentErgoNameTokenId: Coll[Byte]   = SELF.tokens(0)._1
    val previousRegistry: AvlTree           = SELF.R4[AvlTree].get
    val previousState: (Coll[Byte], Long)   = SELF.R5[(Coll[Byte], Long)].get
    val parentErgoNameBytes: Coll[Byte]     = SELF.R6[Coll[Byte]].get

    if (_action == 1) {

        // ===== Mint SubName Tx ===== //
        val validMintSubNameTx: Boolean = {

            // _insertionProof: Coll[Byte]  - Proof that the SubNameHash and SubNameTokenId were inserted into the registry avl tree.

            // Inputs
            val ergoNameNftBoxIn: Box           = INPUTS(0)

            // Outputs
            val subNameNftBoxOut: Box                       = OUTPUTS(0)
            val parentSubNameRegistryBoxOut: Box            = OUTPUTS(1)
            val childSubNameRegistryBoxOut: Box             = OUTPUTS(2)
            val ergoNameNftBoxOut: Box                      = OUTPUTS(3)
            val minerFee: Box                               = OUTPUTS(4)

            // Relevant Variables
            val _insertionProof: Coll[Byte] = getVar[Coll[Byte]](2).get
            
            val subNameTokenId: Coll[Byte]  = ergoNameNftBoxIn.id // Thus all ErgoName token ids will be unique.

            val childPath: Coll[Byte] = childSubNameRegistryBoxOut.R6[Coll[Byte]].get
            val labelSize: Int = childPath.size - parentErgoNameBytes.size - 1 // e.g. bob~alice - alice
            val subNameBytes: Coll[Byte] = childPath.slice(0, labelSize) // e.g. bob
            val namePrefix = fromBase16("7e") // "~"
            val descriptionPrefix = fromBase16("4572676f4e616d65207e") // "ErgoName ~"
            val zeroDecimals = fromBase16("30") // "0"
            val pictureArtworkNFT = fromBase16("0101") // Coll(1, 1)
            val svgPrefix = fromBase16("646174613a696d6167652f7376672b786d6c3b6261736536342c") // "data:image/svg+xml;base64,
            
            val onlyOne: Boolean = (INPUTS(1).id == SELF.id)

            val validParentErgoName: Boolean = {
                allOf(Coll(
                    (ergoNameNftBoxIn.tokens(0) == (parentErgoNameTokenId, 1L)),
                    (ergoNameNftBoxIn.id != SELF.id)
                ))
            }

            val validSubNameFormat: Boolean = {

                allOf(Coll(
                    (subNameBytes.size > 0),
                    isValidAscii(subNameBytes)
                ))

            }

            val validSubNameRegistryUpdate: Boolean = {

                val validStateUpdate: Boolean = {

                    val newState: (Coll[Byte], Long)        = parentSubNameRegistryBoxOut.R5[(Coll[Byte], Long)].get

                    val validSubNameTokenIdUpdate: Boolean  = (newState._1 == subNameTokenId)
                    val validIndexIncrement: Boolean        = (newState._2 == previousState._2 + 1L)

                    allOf(Coll(
                        validSubNameTokenIdUpdate,
                        validIndexIncrement
                    ))

                }

                val validSubNameInsertion: Boolean = {

                    val subNameHash = blake2b256(subNameBytes)
                    val newRegistry: AvlTree = previousRegistry.insert(Coll((subNameHash, subNameTokenId)), _insertionProof).get

                    (parentSubNameRegistryBoxOut.R4[AvlTree].get == newRegistry)

                }

                val validSelfRecreation: Boolean = {

                    allOf(Coll(
                        (parentSubNameRegistryBoxOut.value == SELF.value),
                        (parentSubNameRegistryBoxOut.propositionBytes == SELF.propositionBytes),
                        (parentSubNameRegistryBoxOut.tokens(0) == SELF.tokens(0)),
                        (parentSubNameRegistryBoxOut.R6[Coll[Byte]].get == parentErgoNameBytes)
                    ))

                }

                allOf(Coll(
                    validStateUpdate,
                    validSubNameInsertion,
                    validSelfRecreation
                ))

            }

            val validChildSubNameRegistryBoxOut: Boolean = {

                val validAvlTree: Boolean = {

                    val emptyDigest: Coll[Byte] = fromBase16("4ec61f485b98eb87153f7c57db4f5ecd75556fddbc403b41acf8441fde8e160900")

                    allOf(Coll(
                        (childSubNameRegistryBoxOut.R4[AvlTree].get.digest == emptyDigest),
                        (childSubNameRegistryBoxOut.R4[AvlTree].get.isInsertAllowed),
                        (childSubNameRegistryBoxOut.R4[AvlTree].get.isUpdateAllowed == false),
                        (childSubNameRegistryBoxOut.R4[AvlTree].get.isRemoveAllowed),
                        (childSubNameRegistryBoxOut.R4[AvlTree].get.keyLength == 32),
                        (childSubNameRegistryBoxOut.R4[AvlTree].get.valueLengthOpt.get == 32)
                    ))
    
                }

                val validChildPath: Boolean = {

                    allOf(Coll(
                        (labelSize > 0),
                        (childPath.slice(labelSize, labelSize + 1) == namePrefix),
                        (childPath.slice(labelSize + 1, childPath.size) == parentErgoNameBytes),
                    ))

                }

                allOf(Coll(
                    (childSubNameRegistryBoxOut.propositionBytes == SELF.propositionBytes),
                    (childSubNameRegistryBoxOut.tokens(0) == (subNameTokenId, 1L)),
                    validAvlTree,
                    (childSubNameRegistryBoxOut.R5[(Coll[Byte], Long)].get == (Coll[Byte](), 0L)),
                    validChildPath
                ))

            }

            val validSubNameNftBoxOut: Boolean = {

                val validEIP4Registers: Boolean = {

                    val expectedName = namePrefix ++ childPath
                    val expectedDescription = descriptionPrefix ++ childPath

                    allOf(Coll(
                        (subNameNftBoxOut.R4[Coll[Byte]].get == expectedName),
                        (subNameNftBoxOut.R5[Coll[Byte]].get == expectedDescription),
                        (subNameNftBoxOut.R6[Coll[Byte]].get == zeroDecimals),
                        (subNameNftBoxOut.R7[Coll[Byte]].get == pictureArtworkNFT),
                        (subNameNftBoxOut.R8[Coll[Byte]].get.size == 32), // Hash size.
                        (subNameNftBoxOut.R9[Coll[Byte]].isDefined),      // There will be link to display image in wallets, e.g. Nautilus.
                        (subNameNftBoxOut.R9[Coll[Byte]].get.slice(0, 26) == svgPrefix) // Enforce svg as best we can.
                    ))

                }

                allOf(Coll(
                    (subNameNftBoxOut.propositionBytes == ergoNameNftBoxIn.propositionBytes),
                    (subNameNftBoxOut.tokens(0) == (subNameTokenId, 1L)),
                    validEIP4Registers
                ))

            }

            val validErgoNameNftBoxOut: Boolean = {

                allOf(Coll(
                    (ergoNameNftBoxOut.propositionBytes == ergoNameNftBoxIn.propositionBytes),
                    (ergoNameNftBoxOut.tokens == ergoNameNftBoxIn.tokens)
                ))

            }

            val validTokenLimit: Boolean = {

                val outputs_sum: Long = OUTPUTS.fold(0L, 
                { (sum: Long, output: Box) =>
                    
                    val output_sum: Long = output.tokens.fold(0L, 
                    { (sum: Long, t: (Coll[Byte], Long)) =>
                        if (t._1 == subNameTokenId) sum + t._2 else sum
                    })

                    sum + output_sum

                })

                (outputs_sum == 2L)
            }

            allOf(Coll(
                onlyOne,
                validParentErgoName,
                validSubNameFormat,
                validSubNameRegistryUpdate,
                validChildSubNameRegistryBoxOut,
                validSubNameNftBoxOut,
                validErgoNameNftBoxOut,
                validTokenLimit
            ))

        }

        sigmaProp(validMintSubNameTx)

    } else if (_action == 2) {

        // ===== SubName Self-Burn Tx ===== //
        val validSubNameSelfBurnTx: Boolean = {

            // ===== Context Variables ===== //
            // _subNameHash: Coll[Byte]     - Hash of the ErgoName SubName to burn.
            // _lookupProof: Coll[Byte]     - Proof for retrieveing SubName from the avl tree.
            // _removeProof: Coll[Byte]     - Proof SubName was deleted from the avl tree.

            val _subNameHash: Coll[Byte]    = getVar[Coll[Byte]](1).get
            val _lookupProof: Coll[Byte]    = getVar[Coll[Byte]](2).get
            val _removeProof: Coll[Byte]    = getVar[Coll[Byte]](3).get

            // Inputs
            val userPkBoxIn: Box = INPUTS(1)

            // Outputs
            val parentSubNameRegistryBoxOut: Box = OUTPUTS(0)

            // Relevant Variables
            val subNameTokenId: Coll[Byte] = userPkBoxIn.tokens(0)._1

            val onlyOne: Boolean = (INPUTS(0).id == SELF.id)

            val validSubName: Boolean = {

                val tokenId: Option[Coll[Byte]] = previousRegistry.get(_subNameHash, _lookupProof)

                if (tokenId.isDefined) {

                    (tokenId.get == subNameTokenId)

                } else {
                    false
                }

            }

            val validSubNameRemoval: Boolean = {

                val newRegistry: AvlTree = previousRegistry.remove(Coll(_subNameHash), _removeProof).get

                (parentSubNameRegistryBoxOut.R4[AvlTree].get == newRegistry)

            }

            val validSelfRecreation: Boolean = {

                allOf(Coll(
                    (parentSubNameRegistryBoxOut.value == SELF.value),
                    (parentSubNameRegistryBoxOut.propositionBytes == SELF.propositionBytes),
                    (parentSubNameRegistryBoxOut.tokens(0) == SELF.tokens(0)),
                    (parentSubNameRegistryBoxOut.R5[(Coll[Byte], Long)].get == previousState),
                    (parentSubNameRegistryBoxOut.R6[Coll[Byte]].get == parentErgoNameBytes)
                ))

            }

            val validBurn: Boolean = {

                OUTPUTS.forall{ (output: Box) =>
                    output.tokens.forall{ (token: (Coll[Byte], Long)) =>
                        (token._1 != subNameTokenId)
                    }
                }

            }

            allOf(Coll(
                onlyOne,
                validSubName,
                validSubNameRemoval,
                validSelfRecreation,
                validBurn
            ))

        }

        sigmaProp(validSubNameSelfBurnTx)

    } else if (_action == 3) {

        // ===== SubName Parent Burns Child Tx ===== //
        val validSubNameParentBurnsChildTx: Boolean = {

            // ===== Context Variables ===== //
            // _subNameHash: Coll[Byte]     - Hash of the ErgoName SubName to delete.
            // _containsProof: Coll[Byte]   - Proof for checking if SubName exists in the avl tree.
            // _removeProof: Coll[Byte]     - Proof SubName was deleted from the avl tree.

            val _subNameHash: Coll[Byte]    = getVar[Coll[Byte]](1).get
            val _containsProof: Coll[Byte]  = getVar[Coll[Byte]](2).get
            val _removeProof: Coll[Byte]    = getVar[Coll[Byte]](3).get

            // Inputs
            val userPkBoxIn: Box = INPUTS(1)

            // Outputs
            val parentSubNameRegistryBoxOut: Box = OUTPUTS(0)
            val userPkBoxOut: Box = OUTPUTS(1)

            // Relevant Variables
            val parentSubNameTokenId: Coll[Byte] = userPkBoxIn.tokens(0)._1

            val validParentSubName: Boolean = (parentSubNameTokenId == parentErgoNameTokenId)

            val onlyOne: Boolean = (INPUTS(0).id == SELF.id)

            val validSubName: Boolean = {

                val tokenId: Option[Coll[Byte]] = previousRegistry.get(_subNameHash, _containsProof)

                tokenId.isDefined

            }

            val validSubNameRemoval: Boolean = {

                val newRegistry: AvlTree = previousRegistry.remove(Coll(_subNameHash), _removeProof).get

                (parentSubNameRegistryBoxOut.R4[AvlTree].get == newRegistry)

            }

            val validSelfRecreation: Boolean = {

                allOf(Coll(
                    (parentSubNameRegistryBoxOut.value == SELF.value),
                    (parentSubNameRegistryBoxOut.propositionBytes == SELF.propositionBytes),
                    (parentSubNameRegistryBoxOut.tokens(0) == SELF.tokens(0)),
                    (parentSubNameRegistryBoxOut.R5[(Coll[Byte], Long)].get == previousState),
                    (parentSubNameRegistryBoxOut.R6[Coll[Byte]].get == parentErgoNameBytes)
                ))
            }

            val validParentRecreation: Boolean = {

                allOf(Coll(
                    (userPkBoxOut.propositionBytes == userPkBoxIn.propositionBytes),
                    (userPkBoxOut.tokens == userPkBoxIn.tokens)
                ))
            }

            allOf(Coll(
                onlyOne,
                validParentSubName,
                validSubName,
                validSubNameRemoval,
                validSelfRecreation,
                validParentRecreation
            ))

        }

        sigmaProp(validSubNameParentBurnsChildTx)

    } else {

        val adoo = PK("9iJV2D1gzvWeBbSXHPgTai3S41CjoBBodxMB9DB2Dwt1kaRA9z2")
        val lgd = PK("9hdYFtdV8JLXJho4wAzy6dB6DHQn4gvZsmf4vf1kbkggJ59Wn3Z")
        val balb = PK("9gCJDv78SUUes6sNo81KqbP4yu3vHU6GtcatvmySiBsRoU1k4T8")
        val mgpai = PK("9g5yzitxX53B4RVi1DHLjrMx7iwTQn38kLG2XVVkhvHvcB1TcEz")
        val msig_addresses: Coll[SigmaProp] = Coll(adoo, lgd, balb, mgpai)
        val min_required_signatures: Int = 2
        val ergoname_msig_sigmaprop: SigmaProp = atLeast(min_required_signatures, msig_addresses)

        ergoname_msig_sigmaprop

    }

}