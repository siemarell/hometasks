package nodeViewHolder

import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.testkit.properties.mempool.MempoolTransactionsTest
import transaction.{BlockchainDevelopersTransaction, Generators, Sha256PreimageProposition}

class BDMempoolTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with Generators
  with MempoolTransactionsTest[Sha256PreimageProposition, BlockchainDevelopersTransaction, BlockchainDevelopersMempool] {

  override val memPool: BlockchainDevelopersMempool = new BlockchainDevelopersMempool
  override val transactionGenerator: Gen[BlockchainDevelopersTransaction] = BDTransactionGenerator

  property("Should add new TX") {
    forAll(transactionGenerator) { tx =>
      val newPool = memPool.put(tx)
      newPool.isSuccess shouldBe true
    }
  }

  property("Should add sequence of new TX") {
    forAll(Gen.listOfN(3, transactionGenerator)) { txs =>
      val newPool = memPool.put(txs).get
      newPool.size shouldEqual txs.size
    }
  }

  property("Shouldn't add same TX") {
    forAll(transactionGenerator) { tx =>
      val pool1 = memPool.put(tx).get
      val pool2 = pool1.put(tx).get
      pool1.size shouldEqual pool2.size
    }
  }

  property("Should remove TX") {
    forAll(transactionGenerator) { tx =>
      var pool = memPool.put(tx).get
      pool.remove(tx).size shouldBe 0

    }
  }

  property("Should filter TX") {
    forAll(transactionGenerator, transactionGenerator) { (tx1, tx2) =>
      var pool = memPool.put(Seq(tx1, tx2)).get
      pool.filter(_.id != tx1.id).size shouldBe 1
      pool.filter(_.id == 0).size shouldBe 0
    }
  }

  property("Should take correct number of transaction") {
    forAll(Gen.listOfN(10, transactionGenerator)){ txs =>
      val pool = memPool.put(txs).get
      pool.take(8).size shouldBe 8
      pool.take(12).size shouldBe 10
    }
  }

  property("Shouldn't exceed limit") {
    var pool = (0 until BlockchainDevelopersMempool.Limit)
      .map(_ => transactionGenerator.sample.get)
      .foldLeft(memPool)((acc, tx) => acc.put(tx).get)

    pool.size shouldBe BlockchainDevelopersMempool.Limit

    forAll(transactionGenerator) { tx =>
      pool = pool.put(tx).get
      pool.size shouldBe BlockchainDevelopersMempool.Limit
    }
  }
  //
  property("Shouldn't add transaction with wrong number of signatures") {
    var pool = memPool
    forAll(transactionGenerator) { tx =>
      pool = pool.put(tx).get
      val initialSize = pool.size
      val badTx = tx.copy(signatures = tx.signatures.tail)
      pool.put(badTx).isSuccess shouldBe false
      pool.size shouldBe initialSize
    }
  }
}

