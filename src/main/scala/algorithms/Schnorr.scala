package algorithms

object Schnorr {

  def findAnswer(r: BigInt, g: BigInt, y: BigInt, p: BigInt, q: BigInt, e: Int): BigInt = {
    var a = 0
    while (r != g.pow(a) % p) a += 1
    var k = 0
    while (y != g.modPow(k, p).modInverse(p)) k += 1
    (a + k * e) % q
  }

  def main(args: Array[String]): Unit = {
    val se = Array((15776, 9856), (490, 8108), (9987, 7309), (155, 1267))
    se.foreach {
      case (e, s) =>
        val answer = findAnswer(32607, 2902, 9107, 33107, 16553, e)
        println(s"$s $e => $answer ${if (answer == s) "Right" else "Wrong"}")
    }
  }

}
