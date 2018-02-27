package transaction

import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.hash.{Digest32, Sha256}
import scorex.testkit.generators.CoreGenerators
import scorex.testkit.properties.mempool.MempoolTransactionsTest

class BDMempoolTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with CoreGenerators
  with MempoolTransactionsTest[Sha256PreimageProposition, BlockchainDevelopersTransaction, BlockchainDevelopersMempool] {

  override val memPool: BlockchainDevelopersMempool = new BlockchainDevelopersMempool(Map())
  override val transactionGenerator: Gen[BlockchainDevelopersTransaction] = for {
    nInputs <- smallInt.filter(_ > 0)
    inputs <- Gen.listOfN(nInputs, genBytesList(32).map(OutputId @@ _))
    nOutputs <- smallInt.filter(_ > 0)
    digests <- Gen.listOfN(nOutputs, genBytesList(32))
    txoVals <- Gen.listOfN(nOutputs, positiveLongGen)
    signatures: IndexedSeq[Sha256PreimageProof] = digests.toIndexedSeq.map(item => Sha256PreimageProof(Digest32Preimage @@ item))
    outputs: IndexedSeq[(Sha256PreimageProposition, Value)] = digests.zip(txoVals).toIndexedSeq.map{ case (digest, txoVal) => {
      (Sha256PreimageProposition(Sha256.hash(digest)),Value @@ txoVal)
    }}
  } yield BlockchainDevelopersTransaction(inputs.toIndexedSeq,outputs,signatures)

  property("Should add new TX"){
    forAll(transactionGenerator){ tx =>
      val newPool = memPool.put(tx)
      newPool.isSuccess shouldBe true
    }
  }

  property("Shouldn't add same TX"){
    forAll(transactionGenerator){ tx =>
      var newPool = memPool.put(tx)
      newPool = newPool.get.put(tx)
      newPool.isSuccess shouldBe false
    }
  }

  property("Should find previously added TX"){
    forAll(transactionGenerator){ tx =>
      var newPool = memPool.put(tx)
      newPool.get.contains(tx.id) shouldBe true
    }
  }
  property("Should serialize and deserialize TX"){
    forAll(transactionGenerator){ tx =>
      val serialized = tx.bytes
      val restored = tx.serializer.parseBytes(serialized)
      val restoredBytes = restored.get.bytes
      tx.bytes shouldEqual restoredBytes
    }
  }
}

