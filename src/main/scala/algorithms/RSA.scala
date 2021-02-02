package algorithms

object RSA {

  def getSecretMessage(rb: BigInt, b: BigInt, m1: BigInt): BigInt = {
    var m = BigInt(1)
    while (m.modPow(b, rb) != m1) m += 1
    m
  }

  def main(args: Array[String]): Unit = {
    println(Gamma.encrypt("ДМТЭЕСХ",
      getSecretMessage(71361259, 74671, 3942877).toString.map(_.asDigit).toArray)) // БЕЛФАСТ
  }

}
