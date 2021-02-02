package algorithms

object Gamma {

  val ra = "АБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ" // 32
  val ea = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"       // 26

  def chooseAlphabet(text: String): String = text match {
    case text if text.forall(ch => ea.contains(ch)) => ea
    case text if text.forall(ch => ra.contains(ch)) => ra
    case _ => throw new Exception("Unknown alphabet")
  }

  def encrypt(text: String, key: Array[Int]): String = {
    val alpha = chooseAlphabet(text)
    val len = alpha.length
    val newText = new StringBuilder()
    for (i <- 0 until text.length) {
      newText.append(alpha((alpha.indexOf(text(i)) + len - key(i % key.length) % len) % len))
    }
    newText.toString()
  }

  def decrypt(text: String, key: Array[Int]): String = {
    val alpha = chooseAlphabet(text)
    val len = alpha.length
    val newText = new StringBuilder()
    for (i <- 0 until text.length) {
      newText.append(alpha((alpha.indexOf(text(i)) + key(i % key.length)) % len))
    }
    newText.toString()
  }

  def main(args: Array[String]): Unit = {
    println(encrypt("JPFZLUNPJ", Array(5, 13, 17, 12, 23, 8))) // ECONOMICS
  }
}
