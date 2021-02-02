package algorithms

object DiffieHellman {

  def getSecretKey(m: BigInt, q: BigInt, x: BigInt, y: BigInt): BigInt = {
    var a = 0
    var b = 0
    while (x != q.modPow(a, m)) a += 1
    while (y != q.modPow(b, m)) b += 1
    val k1 = y.modPow(a, m)
    val k2 = x.modPow(b, m)
    if (k1 % m == k2 % m) k1 else throw new Exception("Something wrong")
  }

  def main(args: Array[String]): Unit = {
    println(Gamma.encrypt("ВСУПНХЧХУАУСПЗ",
      getSecretKey(41903, 10109, 38421, 14092).toString.map(_.asDigit).toArray)) // БИОИНФОРМАТИКА
    println(Gamma.encrypt("БЪЧЧООЧСПЯ",
      getSecretKey(41903, 10109, 38421, 14092).toString.map(_.asDigit).toArray)) // АСТРОНОМИЯ
  }
}
