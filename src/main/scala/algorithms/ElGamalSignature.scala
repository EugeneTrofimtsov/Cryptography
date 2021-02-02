package algorithms

object ElGamalSignature {

  def getMessage(r: BigInt, s: BigInt, p: BigInt, g: BigInt, y: BigInt): BigInt = {
    var Q = 1
    while ((y.modPow(r, p) * r.modPow(s, p)) % p != g.modPow(Q, p)) Q += 1
    Q
  }

  def main(args: Array[String]): Unit = {
    println(getMessage(16588, 23462, 78101, 28204, 4629)) // 77524
  }

}
