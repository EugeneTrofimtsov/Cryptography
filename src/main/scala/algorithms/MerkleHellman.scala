package algorithms

object MerkleHellman {

  def getSecretMessage(w: Array[Int], q: BigInt, r: BigInt, x: Array[Int]): String = {
    def makeArr(index: Int, a: Array[BigInt]): Array[(BigInt, Array[Int])] =  index match {
      case 1 => a.zipWithIndex.map(x => (x._1, Array(x._2)))
      case 2 => for (i <- a; j <- a) yield (i + j, Array(a.indexOf(i), a.indexOf(j)))
      case 3 => for (i <- a; j <- a; k <- a) yield (i + j + k, Array(a.indexOf(i), a.indexOf(j), a.indexOf(k)))
    }
    def getBinaryArray(s: Int, arr: Array[BigInt]): String = {
      var i = 1
      while (!makeArr(i, arr).map(_._1).contains(s)) i += 1
      val tmp = makeArr(i, arr)
      ("0" * arr.length).toCharArray.zipWithIndex.map {
        case (x, i) => if (tmp(tmp.map(_._1).indexOf(s))._2.contains(i)) '1' else x
      }.mkString
    }
    x.map(v => Gamma.ra(Integer.parseInt(getBinaryArray(v, w.map(_ * r % q)), 2) - 1)).mkString
  }

  def main(args: Array[String]): Unit = {
    println(getSecretMessage(Array(1, 2, 4, 9, 18, 35), 80, 29, Array(55, 97, 21, 79, 100, 155))) // АВГУСТ
  }

}
