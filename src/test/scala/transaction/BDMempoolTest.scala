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
    inputs: IndexedSeq[OutputId] <- Gen.listOfN(nInputs, genBytesList(32))
    nOutputs <- smallInt.filter(_ > 0)
    sigs: IndexedSeq[Sha256PreimageProof] <- Gen.listOfN(nOutputs, genBytesList(32))
    txoVals: IndexedSeq[Value] <- Gen.listOfN(nOutputs, positiveLongGen)
  } yield BlockchainDevelopersTransaction(inputs,
    sigs.zip(txoVals).toIndexedSeq.map { case (sig, txoVal) =>
      (Sha256PreimageProposition(Digest32 !@@ Sha256(sig)), Value @@ txoVal)
    },
    sigs)

  property("Should add new TX"){
    forAll(transactionGenerator){ tx =>
      val newPool = memPool.put(tx)
      newPool.isSuccess shouldBe true
    }
  }
}

