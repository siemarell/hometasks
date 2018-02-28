package transaction

import com.google.common.primitives.{Ints, Longs}
import io.circe.syntax._
import io.circe._
import scorex.core.{ModifierId, ModifierTypeId}
import scorex.core.serialization.Serializer
import scorex.core.transaction.Transaction
import scorex.crypto.hash.Digest32
import supertagged.untag

import scala.util.Try

case class BlockchainDevelopersTransaction(inputs: IndexedSeq[OutputId],
                                           outputs: IndexedSeq[(Sha256PreimageProposition, Value)],
                                           signatures: IndexedSeq[Sha256PreimageProof]
                        ) extends Transaction[Sha256PreimageProposition] {
  override type M = BlockchainDevelopersTransaction

  override val messageToSign: Array[Byte] =
      Ints.toByteArray(inputs.length)        ++
      inputs.foldLeft(Array[Byte]())(_ ++ _) ++
      Ints.toByteArray(outputs.length)       ++
      outputs.foldLeft(Array[Byte]()){
        (arr, txo) => arr ++ txo._1.bytes ++ Longs.toByteArray(txo._2)
      }


  override def serializer: Serializer[BlockchainDevelopersTransaction] = BCTransactionSerializer

  implicit val modifierTypeIdEncoder: Encoder[ModifierTypeId] = (a: ModifierTypeId) => {
    untag(a).asJson
  }

  implicit val modifierIdEncoder: Encoder[ModifierId] = (a: ModifierId) => {
    untag(a).asJson
  }

  implicit val outputIdEncoder: Encoder[OutputId] = (a: OutputId) => {
    untag(a).asJson
  }

  implicit val propositionEncoder: Encoder[Sha256PreimageProposition] = (a: Sha256PreimageProposition) => {
    Json.obj("hash" -> untag(a.hash).asJson)
  }

  implicit val proofEncoder: Encoder[Sha256PreimageProof] = (a: Sha256PreimageProof) => {
    Json.obj("preimage" -> untag(a.preimage).asJson)
  }

  implicit val valueEncoder: Encoder[Value] = (a: Value) => {
    untag(a).asJson
  }

  implicit val txEncoder: Encoder[BlockchainDevelopersTransaction] = (a: BlockchainDevelopersTransaction) => {
    Json.obj(
      "modifierTypeId" -> a.modifierTypeId.asJson,
      "id" -> a.id.asJson,
      "inputs" -> a.inputs.asJson,
      "outputs" -> a.outputs.asJson,
      "signatures" -> a.signatures.asJson
    )
  }
  override def json: Json = this.asJson
}

object BCTransactionSerializer extends Serializer[BlockchainDevelopersTransaction] {
  override def toBytes(obj: BlockchainDevelopersTransaction): Array[Byte] =
    obj.messageToSign ++
    Ints.toByteArray(obj.signatures.length) ++
    obj.signatures.foldLeft(Array[Byte]())((b, sig) => b ++ Ints.toByteArray(sig.bytes.length) ++ sig.bytes)

  override def parseBytes(bytes: Array[Byte]): Try[BlockchainDevelopersTransaction] = Try {
    val inputsLenIndex = 0
    val inputsLen = Ints.fromByteArray(bytes.slice(inputsLenIndex, inputsLenIndex+4))

    val outputsLenIndex = inputsLenIndex + 32 * inputsLen + 4
    val outPutsLen = Ints.fromByteArray(bytes.slice(outputsLenIndex, outputsLenIndex+4))

    val signaturesLenIndex = outputsLenIndex + 40 * outPutsLen + 4
    val signaturesLen = Ints.fromByteArray(bytes.slice(signaturesLenIndex, signaturesLenIndex+4))

    val inputs = bytes.slice(inputsLenIndex + 4, outputsLenIndex)
      .sliding(32, 32)
      .map(OutputId @@ _)
    val outputs = bytes.slice(outputsLenIndex + 4, signaturesLenIndex)
      .sliding(40, 40)
      .map{bytes =>
        Sha256PreimageProposition(Digest32 @@ bytes.take(32)) -> Value @@ Longs.fromByteArray(bytes.takeRight(8))
      }

    var signatureStartIndex = signaturesLenIndex + 4
    var signatures: Vector[Sha256PreimageProof] = Vector()

    for (_ <- 0 until signaturesLen) {
      val signatureLength = Ints.fromByteArray(bytes.slice(signatureStartIndex, signatureStartIndex + 4))
      signatures = signatures :+ Sha256PreimageProof(Digest32Preimage @@ bytes.slice(signatureStartIndex + 4, signatureStartIndex + 4 + signatureLength))
      signatureStartIndex += signatureLength + 4
    }

    BlockchainDevelopersTransaction(inputs.toIndexedSeq, outputs.toIndexedSeq, signatures)
  }

}