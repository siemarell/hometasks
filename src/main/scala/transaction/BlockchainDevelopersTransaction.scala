package transaction

import io.circe.syntax._
import io.circe._
import scorex.core.serialization.Serializer
import scorex.core.transaction.Transaction
import scorex.crypto.hash.Digest32

import scala.util.Try

case class BlockchainDevelopersTransaction(inputs: IndexedSeq[OutputId],
                                           outputs: IndexedSeq[(Sha256PreimageProposition, Value)],
                                           signatures: IndexedSeq[Sha256PreimageProof]
                        ) extends Transaction[Sha256PreimageProposition] {
  override type M = BlockchainDevelopersTransaction

  override val messageToSign: Array[Byte] =
    Array(inputs.length.toByte) ++
      inputs.foldLeft(Array[Byte]())(_ ++ _) ++
    Array(outputs.length.toByte) ++
      outputs.foldLeft(Array[Byte]()){
        (arr, txo) => arr ++ txo._1.bytes ++ BigInt(txo._2).toByteArray.reverse.padTo(8,0.toByte)
      }

  override def serializer: Serializer[BlockchainDevelopersTransaction] = BCTransactionSerializer

  override def json: Json = ???
}

object BCTransactionSerializer extends Serializer[BlockchainDevelopersTransaction] {
  override def toBytes(obj: BlockchainDevelopersTransaction): Array[Byte] =
    obj.messageToSign ++ obj.signatures.foldLeft(Array[Byte]())(_ ++ _.bytes)

  override def parseBytes(bytes: Array[Byte]): Try[BlockchainDevelopersTransaction] = Try {
    val inputsLenIndex = 0
    val inputsLen = bytes(inputsLenIndex).toInt
    val outputsLenIndex = inputsLenIndex + 32 * inputsLen + 1
    val outPutsLen =  bytes(outputsLenIndex).toInt
    val signaturesLenIndex = outputsLenIndex + 40 * outPutsLen + 1
    val signaturesLen = bytes(signaturesLenIndex)

    val inputs = bytes.slice(inputsLenIndex + 1, outputsLenIndex).sliding(32, 32).map(OutputId @@ _).toIndexedSeq
    val outputs = bytes.slice(outputsLenIndex + 1, signaturesLenIndex).sliding(40, 40).map{bytes =>
      (Sha256PreimageProposition(Digest32 @@ bytes.take(32)), Value @@ BigInt(bytes.takeRight(32).reverse).toLong)
    }.toIndexedSeq
    val signatures = bytes.slice(signaturesLenIndex + 1, signaturesLenIndex + signaturesLen*32).sliding(32,32)
      .map(Digest32Preimage @@ _).map(Sha256PreimageProof).toIndexedSeq

    BlockchainDevelopersTransaction(inputs, outputs, signatures)
  }


  def main(args: Array[String]): Unit = {
    val digest: OutputId = OutputId @@ Array.fill(32)((-1).toByte)
    val trx = BlockchainDevelopersTransaction(
      Array.fill(3)(digest).toIndexedSeq,
      Array.fill(3)((Sha256PreimageProposition(Digest32 @@ Array.fill(32)((-1).toByte)),Value @@ 1l)).toIndexedSeq,
      Array.fill(3)(Sha256PreimageProof(Digest32Preimage @@ Array.fill(32)((-1).toByte)))
    )
    println(trx.json)
  }


}