package transaction

import org.scalacheck.Gen
import scorex.crypto.hash.Digest32
import scorex.testkit.generators.CoreGenerators

trait Generators extends CoreGenerators{

  val preimageProofGenerator: Gen[Sha256PreimageProof] = nonEmptyBytesGen
    .map(b => Sha256PreimageProof(Digest32Preimage @@ b))

  val preimagePropositionGenerator: Gen[Sha256PreimageProposition] = nonEmptyBytesGen
    .map(b => Sha256PreimageProposition(Digest32 @@ b))

  val txOutputGen: Gen[(Sha256PreimageProposition, Value)] = for {
    preimage <- preimagePropositionGenerator
    value <- positiveLongGen.map(b => Value @@ b)
  } yield preimage -> value

  val BDTransactionGenerator: Gen[BlockchainDevelopersTransaction] = for {
    inputs <- Gen.nonEmptyListOf(genBytesList(32).map(OutputId @@ _))
    outputs <- Gen.nonEmptyListOf(txOutputGen)
    signatures <- Gen.listOfN(inputs.length, preimageProofGenerator)
  } yield BlockchainDevelopersTransaction(inputs.toIndexedSeq,outputs.toIndexedSeq,signatures.toIndexedSeq)
}
