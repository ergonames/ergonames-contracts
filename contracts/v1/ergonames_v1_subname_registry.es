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
    // R6: Coll[Byte]           ParentErgoNameTokenId

    // ===== Relevant Transactions ===== //
    // 1. Mint SubName
    // Inputs: ErgoNameNFT, ParentSubNameRegistry
    // Data Inputs: None
    // Outputs: SubNameNFT, ParentSubNameRegistry, ChildSubNameRegistry, ErgoNameNFT, MinerFee, TxOperatorFee
    // Context Variables: Action, SubNameHash, InerstionProof
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
    // _action: Byte    - Byte representing the transaction type.

    // ===== User Defined Functions ===== //
    // def isValidAscii: (Coll[Byte] => Boolean)
    // isSigmaPropEqualToBoxProp: ((SigmaProp, Box) => Boolean)

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

    def isSigmaPropEqualToBoxProp(propAndBox: (SigmaProp, Box)): Boolean = {

        val prop: SigmaProp = propAndBox._1
        val box: Box = propAndBox._2

        val propBytes: Coll[Byte] = prop.propBytes
        val treeBytes: Coll[Byte] = box.propositionBytes

        if (treeBytes(0) == 0) {

            (treeBytes == propBytes)

        } else {

            // offset = 1 + <number of VLQ encoded bytes to store propositionBytes.size>
            val offset = if (treeBytes.size > 127) 3 else 2
            (propBytes.slice(1, propBytes.size) == treeBytes.slice(offset, treeBytes.size)) 

        }        

    }

    // ===== Relevant Variables ===== //
    val _action: Byte = getVar[Byte](0).get

    if (_action == 1.toByte) {

        // ===== Mint SubName Tx ===== //
        val validMintSubNameTx: Boolean = {

            // ===== Context Variables ===== //
            // _subNameHash: Coll[Byte]     - Hash of the ErgoName SubName to register.
            // _insertionProof: Coll[Byte]  - Proof that the SubNameHash and SubNameTokenId were inserted into the registry avl tree.

            // ===== Relevant Variables ===== //
            val previousRegistry: AvlTree           = SELF.R4[AvlTree].get
            val previousState: (Coll[Byte], Long)   = SELF.R5[(Coll[Byte], Long)].get
            val parentErgoNameTokenId: Coll[Byte]   = SELF.R6[Coll[Byte]].get

            val _subNameHash: Coll[Byte]    = getVar[Coll[Byte]](1).get
            val _insertionProof: Coll[Byte] = getVar[Coll[Byte]](2).get

            // Inputs
            val ergoNameNftBoxIn: Box           = INPUTS(0)
            val parentSubNameRegistryBoxIn: Box = INPUTS(1) // i.e. SELF

            // Outputs
            val subNameNftBoxOut: Box                       = OUTPUTS(0)
            val parentSubNameRegistryBoxOut: Box            = OUTPUTS(1)
            val childSubNameRegistryBoxOut: Box             = OUTPUTS(2)
            val ergoNameNftBoxOut: Box                      = OUTPUTS(3)
            val minerFeeBoxOut: Box                         = OUTPUTS(4)
            val txOperatorFeeBoxOut: Box                    = OUTPUTS(5)

            // Relevant Variables
            val subNameTokenId: Coll[Byte]              = ergoNameNftBoxIn.id // Thus all ErgoName token ids will be unique.

            val subNameBytes: Coll[Byte]                = childSubNameRegistryBoxOut.R4[Coll[Byte]].get

            val receiverPKGroupElement: GroupElement    = childSubNameRegistryBoxOut.R5[GroupElement].get
            val receiverPKSigmaProp: SigmaProp          = proveDlog(receiverPKGroupElement)

            val validParentErgoName: Boolean = (ergoNameNftBoxIn.tokens(0) == (parentErgoNameTokenId, 1L))

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

                    val newRegistry: AvlTree = previousRegistry.insert(Coll((_subNameHash, subNameTokenId)), _insertionProof).get

                    (parentSubNameRegistryBoxOut.R4[AvlTree].get.digest == newRegistry.digest)

                }

                val validSelfRecreation: Boolean = {

                    allOf(Coll(
                        (parentSubNameRegistryBoxOut.value == SELF.value),
                        (parentSubNameRegistryBoxOut.propositionBytes == SELF.propositionBytes),
                        (parentSubNameRegistryBoxOut.tokens(0) == SELF.tokens(0)),
                        (parentSubNameRegistryBoxOut.R6[Coll[Byte]].get == SELF.R6[Coll[Byte]].get)
                    ))

                }

                allOf(Coll(
                    validStateUpdate,
                    validSubNameInsertion,
                    validSelfRecreation
                ))

            }

            val validChildSubNameRegistryBoxOut: Boolean = {

                val emptyDigest: Coll[Byte] = fromBase16("4ec61f485b98eb87153f7c57db4f5ecd75556fddbc403b41acf8441fde8e160900")

                val validR5: Boolean = {
                    val r5 = childSubNameRegistryBoxOut.R5[(Coll[Byte], Long)].get
                    (r5._1.size == 0) && (r5._2 == 0L)
                }

                allOf(Coll(
                    (childSubNameRegistryBoxOut.propositionBytes == SELF.propositionBytes),
                    (childSubNameRegistryBoxOut.tokens(0) == (subNameTokenId, 1L)),
                    (childSubNameRegistryBoxOut.R4[AvlTree].get.digest == emptyDigest),
                    validR5,
                    (childSubNameRegistryBoxOut.R6[Coll[Byte]].get == subNameTokenId)
                ))

            }

            val validSubNameNftBoxOut: Boolean = {

                val propAndBox: (SigmaProp, Box) = (receiverPKSigmaProp, subNameNftBoxOut)

                allOf(Coll(
                    isSigmaPropEqualToBoxProp(propAndBox),
                    (subNameNftBoxOut.tokens(0) == (subNameTokenId, 1L))
                ))

            }

            val validErgoNameNftBoxOut: Boolean = {

                val propAndBox: (SigmaProp, Box) = (receiverPKSigmaProp, ergoNameNftBoxOut)

                allOf(Coll(
                    isSigmaPropEqualToBoxProp(propAndBox),
                    (ergoNameNftBoxOut.tokens(0) == ergoNameNftBoxIn.tokens(0))
                ))

            }

            allOf(Coll(
                validParentErgoName,
                validSubNameFormat,
                validSubNameRegistryUpdate,
                validChildSubNameRegistryBoxOut,
                validSubNameNftBoxOut,
                validErgoNameNftBoxOut
            ))

        }

        sigmaProp(validMintSubNameTx) || $ergonameMultiSigSigmaProp

    } else if (_txType == 2.toByte) {

        // ===== SubName Self-Burn Tx ===== //
        val validSubNameSelfBurnTx: Boolean = {

            // ===== Context Variables ===== //
            // _subNameHash: Coll[Byte]     - Hash of the ErgoName SubName to register.
            // _lookupProof: Coll[Byte]     - Proof for retrieveing SubName from the avl tree.
            // _removeProof: Coll[Byte]     - Proof SubName was deleted from the avl tree.

            // ===== Relevant Variables ===== //
            val previousRegistry: AvlTree           = SELF.R4[AvlTree].get
            val previousState: (Coll[Byte], Long)   = SELF.R5[(Coll[Byte], Long)].get
            val parentErgoNameTokenId: Coll[Byte]   = SELF.R6[Coll[Byte]].get

            val _subNameHash: Coll[Byte]    = getVar[Coll[Byte]](1).get
            val _lookupProof: Coll[Byte]    = getVar[Coll[Byte]](2).get
            val _removeProof: Coll[Byte]    = getVar[Coll[Byte]](3).get

            // Inputs
            val userPkBoxIn: Box = INPUTS(1)

            // Outputs
            val parentSubNameRegistryBoxOut: Box = OUTPUTS(0)

            // Relevant Variables
            val subnameTokenId: Coll[Byte] = userPkBoxIn.tokens(0)._1

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

                (parentSubNameRegistryBoxOut.R4[AvlTree].get.digest == newRegistry.digest)

            }

            val validSelfRecreation: Boolean = {

                allOf(Coll(
                    (parentSubNameRegistryBoxOut.value == SELF.value),
                    (parentSubNameRegistryBoxOut.propositionBytes == SELF.propositionBytes),
                    (parentSubNameRegistryBoxOut.tokens(0) == SELF.tokens(0)),
                    (parentSubNameRegistryBoxOut.R5[(Coll[Byte], Long)].get == SELF.R5[(Coll[Byte], Long)].get),
                    (parentSubNameRegistryBoxOut.R6[Coll[Byte]].get == SELF.R6[Coll[Byte]].get)
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
                validSubName,
                validSubNameRemoval,
                validSelfRecreation,
                validBurn
            ))

        }

        sigmaProp(validSubNameSelfBurnTx)

    } else if (_action == 3.toByte) {

        // ===== SubName Parent Burns Child Tx ===== //
        val validSubNameParentBurnsChildTx: Boolean = {

            // ===== Context Variables ===== //
            // _subNameHash: Coll[Byte]     - Hash of the ErgoName SubName to register.
            // _containsProof: Coll[Byte]   - Proof for checking if SubName exists in the avl tree.
            // _removeProof: Coll[Byte]     - Proof SubName was deleted from the avl tree.

            // ===== Relevant Variables ===== //
            val previousRegistry: AvlTree                   = SELF.R4[AvlTree].get
            val previousState: (Coll[Byte], Long)           = SELF.R5[(Coll[Byte], Long)].get
            val parentErgoNameTokenId: Coll[Byte]           = SELF.R6[Coll[Byte]].get
            val parentRegistrySingletonTokenId: Coll[Byte]  = SELF.tokens(0)._1

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

            val validParentSubName: Boolean = (parentSubNameTokenId == parentRegistrySingletonTokenId)

            val validSubName: Boolean = {

                val tokenId: Option[Coll[Byte]] = previousRegistry.get(_subNameHash, _lookupProof)

                tokenId.isDefined

            }

            val validSubNameRemoval: Boolean = {

                val newRegistry: AvlTree = previousRegistry.remove(Coll(_subNameHash), _removeProof).get

                (parentSubNameRegistryBoxOut.R4[AvlTree].get.digest == newRegistry.digest)

            }

            val validSelfRecreation: Boolean = {

                allOf(Coll(
                    (parentSubNameRegistryBoxOut.value == SELF.value),
                    (parentSubNameRegistryBoxOut.propositionBytes == SELF.propositionBytes),
                    (parentSubNameRegistryBoxOut.tokens(0) == SELF.tokens(0)),
                    (parentSubNameRegistryBoxOut.R5[(Coll[Byte], Long)].get == SELF.R5[(Coll[Byte], Long)].get),
                    (parentSubNameRegistryBoxOut.R6[Coll[Byte]].get == SELF.R6[Coll[Byte]].get)
                ))

            }

            val validParentRecreation: Boolean = {

                allOf(Coll(
                    (userPkBoxOut.propositionBytes = userPkBoxIn.propositionBytes),
                    (userPkBoxOut.tokens(0) == userPkBoxIn.tokens(0))
                ))

            }          

            allOf(Coll(
                validParentSubName,
                validSubName,
                validSubNameRemoval,
                validSelfRecreation,
                validParentRecreation
            ))

        }

        sigmaProp(validSubNameParentBurnsChildTx)

    } else {
        
        sigmaProp(false)

    }

}
