package algorithms

object ElGamal {

  def getSecretMessage(p: BigInt, g: BigInt, y: BigInt, a: BigInt, b: BigInt): BigInt = {
    var x = 0
    while (g.modPow(x, p) != y) x += 1
    (b * a.modPow(x, p).modInverse(p)).mod(p)
  }

  def main(args: Array[String]): Unit = {
    println(Gamma.encrypt("ФСЙОЩГМПА",
      getSecretMessage(54709, 23113, 25058, 926, 16005).toString.map(_.asDigit).toArray)) // СПЕКТАКЛЬ
  }

}
