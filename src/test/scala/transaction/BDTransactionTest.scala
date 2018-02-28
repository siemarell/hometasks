package transaction

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

class BDTransactionTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with Generators {


  property("BDTransactions should be serialized and deserialized") {
    forAll(BDTransactionGenerator) { tx =>
      val recovered = tx.serializer.parseBytes(tx.serializer.toBytes(tx)).get
      tx.serializer.toBytes(tx) shouldEqual tx.serializer.toBytes(recovered)
    }
  }

  property("BDTransactions should be JSON serialized") {
    forAll(BDTransactionGenerator) { tx =>
      tx.json.\\("id").head.as[List[Byte]].right.get shouldEqual tx.id
    }
  }
}