{
  // ===== Contract Description ===== //
  // Name: ErgoNames Update Contract
  // Description: Contract allowing for contract updates.
  // Version: 1.0.0
  // Author: Taken from SigUSD/Dexy.

  // ===== Box Contents ===== //
  // Tokens
  // 1. (UpdateRegistryTokenId, 1L)
  // Registers
  // None

  // ===== Relevant Transactions ===== //
  // 1. Update
  // Inputs: Update, ContractToUpdate, Vote Boxes
  // Data Inputs: None
  // Outputs: Update, UpdatedContract, Vote Boxes
  // Context Variables: None

  // ===== Compile Time Constants ($) ===== //
  // $registryNFT: Coll[Byte]
  // $voteTokenId: Coll[Byte]

  // ===== Context Variables (_) ===== //
  // None

  // ===== User Defined Functions ===== //
  // isValidVote: Box => Boolean

  // contractToUpdateNFT is whether collectionNFT or registryNFT
  val contractToUpdateNFT = $registryNFT

  val voteTokenId = $voteTokenId

  // todo: consider proper value before deployment
  val minVotes = 2 // out of 4

  // collect and update in one step
  val updateBoxOut = OUTPUTS(0) // copy of this box is the 1st output

  val validUpdateIn = SELF.id == INPUTS(0).id // this is 1st input

  val contractToUpdateBoxIn = INPUTS(1) // contract to update box is 2nd input
  val contractToUpdateBoxOut = OUTPUTS(1) // copy of contract to update box is the 2nd output

  // compute the hash of the contract to update output box. This should be the value voted for
  val contractToUpdateBoxOutHash = blake2b256(contractToUpdateBoxOut.propositionBytes)

  val validContractToUpdateIn = 
    (contractToUpdateBoxIn.tokens.size >= 1) && 
    (contractToUpdateBoxIn.tokens(0)._1 == contractToUpdateNFT)

  val validContractToUpdateOut = 
    (contractToUpdateBoxIn.value == contractToUpdateBoxOut.value) &&
    (contractToUpdateBoxIn.tokens == contractToUpdateBoxOut.tokens) &&
    (contractToUpdateBoxIn.R4[AvlTree].get == contractToUpdateBoxOut.R4[AvlTree]) &&
    (contractToUpdateBoxIn.R5[(Coll[Byte], Long)].get == contractToUpdateBoxOut.R5[(Coll[Byte], Long)].get) &&
    (contractToUpdateBoxIn.R6[(Int, Int)].get == contractToUpdateBoxOut.R6[(Int, Int)].get) // R7 can change if we want to change prices in the future.

  // burning of update NFT token is not possible, but it is possible to update a contract to
  // not to rely to update NFT anymore
  val validUpdateOut = 
    (SELF.tokens == updateBoxOut.tokens) &&
    (SELF.propositionBytes == updateBoxOut.propositionBytes) &&
    (updateBoxOut.value >= SELF.value)

  def isValidVote(b:Box) = {
    (b.tokens.size > 0) &&
    (b.tokens(0)._1 == voteTokenId) &&
    (b.R5[Coll[Byte]].get == SELF.id) && // ensure vote corresponds to this box
    (b.R6[Coll[Byte]].get == contractToUpdateBoxOutHash) // check value voted for
  }

  val voteBoxes = INPUTS.filter(isValidVote)

  val votesCount = voteBoxes.fold(0L, {(accum: Long, b: Box) => accum + b.tokens(0)._2})

  val validVotes = votesCount >= minVotes

  val validUpdate =
    validContractToUpdateIn &&
    validContractToUpdateOut &&
    validUpdateIn &&
    validUpdateOut &&
    validVotes

  sigmaProp(validUpdate) 
}