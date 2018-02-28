package transaction

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.hash.Sha256
import scorex.testkit.generators.CoreGenerators

class Sha256PreimageProofTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with Generators {


  property("Sha256PreimageProof should be serialized and deserialized") {
    forAll(preimageProofGenerator) { proof =>
      val recovered = proof.serializer.parseBytes(proof.serializer.toBytes(proof)).get
      proof.serializer.toBytes(proof) shouldEqual proof.serializer.toBytes(recovered)
    }
  }

  property("Sha256PreimageProof should be valid for propositions") {
    forAll(preimageProofGenerator) { proof =>
      val proposition = Sha256PreimageProposition(Sha256(proof.preimage))
      proof.isValid(proposition, Array(0.toByte))
    }
  }

  property("Sha256PreimageProof should not be valid for wrong propositions") {
    forAll(preimageProofGenerator) { proof =>
      val proposition = Sha256PreimageProposition(Sha256(Array(0.toByte)))
      proof.isValid(proposition, Array(0.toByte))
    }
  }

}