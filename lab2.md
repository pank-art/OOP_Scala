% Лабораторная работа № 2 «Введение в объетно-ориентированное
  программирование на языке Scala»
% 27 мая 2025 г.
% Артём Панкратов, ИУ9-61Б

# Цель работы
Целью данной работы является изучение базовых объектно-ориентированных возможностей языка Scala.

# Индивидуальный вариант
80-битовое целое число со знаком, представленное списом из пяти Short’ов, с операциями сложения,
вычитания, умножения и изменения знака (унарный минус).

# Реализация и тестирование

```scala
object MyBigInt {
  def apply(parts: List[Short]): MyBigInt = new MyBigInt(parts)
  def apply(value: Long): MyBigInt = new MyBigInt(fromLong(value))

  private def fromLong(value: Long): List[Short] = {
    val part0 = (value & 0xFFFF).toShort
    val part1 = ((value >>> 16) & 0xFFFF).toShort
    val part2 = ((value >>> 32) & 0xFFFF).toShort
    val part3 = ((value >>> 48) & 0xFFFF).toShort
    val part4 = if (value < 0) 0xFFFF.toShort else 0x0000.toShort
    List(part0, part1, part2, part3, part4)
  }

  private def fromBigInt(value: scala.math.BigInt): List[Short] = {
    val mod = scala.math.BigInt(1) << 80
    val normalized = value.mod(mod)
    (0 until 5).map { i =>
      val shift = i * 16
      val part = (normalized >> shift) & 0xFFFF
      part.toShort
    }.toList
  }

  private def toBigInt(parts: List[Short]): scala.math.BigInt = {
    val unsigned = parts.zipWithIndex.foldLeft(scala.math.BigInt(0)) { case (acc, (part, idx)) =>
      acc | (scala.math.BigInt(part.toInt & 0xFFFF) << (idx * 16))
    }
    if ((parts(4).toInt & 0x8000) != 0) {
      unsigned - (scala.math.BigInt(1) << 80)
    } else {
      unsigned
    }
  }
}

class MyBigInt private (private val parts: List[Short]) {
  require(parts.length == 5, "должно быть 5 Short'ов")

  def +(that: MyBigInt): MyBigInt = {
    val resultParts = new Array[Short](5)
    var carry = 0
    for (i <- 0 to 4) {
      val a = (this.parts(i).toInt & 0xFFFF)
      val b = (that.parts(i).toInt & 0xFFFF)
      val sum = a + b + carry
      resultParts(i) = (sum & 0xFFFF).toShort
      carry = (sum >>> 16) & 0x1
    }
    new MyBigInt(resultParts.toList)
  }

  def unary_- : MyBigInt = {
    val resultParts = new Array[Short](5)
    var carry = 1
    for (i <- 0 to 4) {
      val inv = (~this.parts(i)) & 0xFFFF
      val sum = inv + carry
      resultParts(i) = (sum & 0xFFFF).toShort
      carry = (sum >>> 16) & 0xFFFF
    }
    new MyBigInt(resultParts.toList)
  }

  def -(that: MyBigInt): MyBigInt = this + (-that)

  def *(that: MyBigInt): MyBigInt = {
    val a = MyBigInt.toBigInt(this.parts)
    val b = MyBigInt.toBigInt(that.parts)
    val product = a * b
    new MyBigInt(MyBigInt.fromBigInt(product))
  }

  override def toString: String = {
    val value = MyBigInt.toBigInt(parts)
    value.toString()
  }
}

val a = MyBigInt(123L)           // 123
val b = MyBigInt(-456L)          // -456
val c = MyBigInt(789L)           // 789
val d = MyBigInt(List(0xFFFF.toShort, 0xFFFF.toShort, 0xFFFF.toShort, 0xFFFF.toShort, 0xFFFF.toShort)) // -1

println("a = " + a)
println("b = " + b)
println("c = " + c)
println("d = " + d) 

println("b - a = " + (b - a)) // -456 - 123 = -579
println("b + c = " + (b + c)) // -456 + 789 = 333
println("a * b = " + (a * b)) // 123 * -456 = -56088
println("-c = " + (-c)) // -789
```

Вывод на `stdout`

```
Output:

a = 123
b = -456
c = 789
d = -1
b - a = -579
b + c = 333
a * b = -56088
-c = -789
```

# Вывод
При выполнении данной лабораторной работы были приобретены навыки реализации неизменяемой структуры 
данных для представления 80-битовых целых чисел и разработки арифметических операций над ними с 
использованием объектно-ориентированных и функциональных возможностей языка Scala.