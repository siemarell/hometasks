package crypto

import scorex.crypto.signatures.{Curve25519, Signature}

object Curve25519SignatureForger {

  def forgeSignature(signature: Signature): Signature = {
    val rBytes = signature.take(32)
    val sBytes = signature.takeRight(32)
    val n = BigInt("7237005577332262213973186563042994240857116359379907606001950938285454250989")
    Signature(
      rBytes ++
      (BigInt(sBytes reverse) + n).toByteArray.reverse
    )
  }


  def main(args: Array[String]): Unit = {
    val data = "123" getBytes
    val pair = Curve25519.createKeyPair(data)
    val correctSignature = Curve25519.sign(pair._1, data)
    val r = correctSignature.take(32)
    val s = correctSignature.takeRight(32) reverse
    val ss = BigInt(s)
    val n = BigInt("7237005577332262213973186563042994240857116359379907606001950938285454250989")
    //val newR = doWork(ss, BigInt(r)) toByteArray(32)
    val correctSignature2 = Curve25519.sign(pair._1, data)
    val correctSignature3 = Curve25519.sign(pair._1, data)

    println( BigInt(correctSignature takeRight(32) reverse) < n)
    println( BigInt(correctSignature2 takeRight(32) reverse) < n)
    println( BigInt(correctSignature3 takeRight(32) reverse) < n)
  }


}
