{ 
  // ===== Contract Description ===== //
  // Name: ErgoNames Vote Contract
  // Description: Contract representing a vote for a specific contract update.
  // Version: 1.0.0
  // Author: Taken from SigUSD/Dexy.

  // ===== Box Contents ===== //
  // Tokens
  // 1. (VoteTokenId, 1L)
  // Registers
  // R4 [GroupElement]: The group element of the owner of the vote token.
  // R5 [Coll[Byte]]: The box id of the update box.
  // R6 [Coll[Byte]]: The hash of the updated contract's proposition bytes.

  // ===== Relevant Transactions ===== //
  // 1. Update
  // Inputs: Update, ContractToUpdate, Vote Boxes
  // Data Inputs: None
  // Outputs: Update, UpdatedContract, Vote Boxes
  // Context Variables: None

  // ===== Compile Time Constants ($) ===== //
  // $collectionUpdateNFT: Coll[Byte]
  // $registryUpdateNFT: Coll[Byte]

  // ===== Context Variables (_) ===== //
  // None

  // ===== User Defined Functions ===== //
  // None

  val collectionUpdateNFT = $collectionUpdateNFT
  val registryUpdateNFT = $registryUpdateNFT

  val pubKey = SELF.R4[GroupElement].get

  val index = INPUTS.indexOf(SELF, 0)

  val output = OUTPUTS(index)

  val isBasicCopy = 
    (output.R4[GroupElement].get == pubKey) && // todo: change to .isDefined so pubkey rotation possible?
    (output.propositionBytes == SELF.propositionBytes) &&
    (output.tokens == SELF.tokens) &&
    (output.value >= SELF.value)

  val castVote = proveDlog(pubKey)

  val notVoted = (output.R5[Coll[Byte]].isDefined == false)
  val update = 
    (INPUTS(0).tokens(0)._1 == collectionUpdateNFT || registryUpdateNFT) && 
    notVoted

  // Note: We want that during an update, a new valid vote should not be generated
  // notVoted ensures that the update action does not generate a new vote.
  // This is already prevented by having R5 contain the id of the update box,
  // This guarantees that R5 can never contain this box Id (because it depends on the outputs)
  // However, notVoted is used for additional security

  val validUpdate = 
    isBasicCopy &&
    (castVote || update)

  sigmaProp(validUpdate)
}