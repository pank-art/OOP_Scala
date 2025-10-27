% Лабораторная работа № 3 «Обобщённые классы в Scala»
% 28 мая 2025 г.
% Артём Панкратов, ИУ9-61Б

# Цель работы
Целью данной работы является приобретение навыков разработки обобщённых классов на языке Scala с 
использованием неявных преобразований типов.

# Индивидуальный вариант
Класс Matrix[T], представляющий неизменяемую квадратную матрицу с элементами типа T, для которой 
реализованы две операции: получение значения элемента на i-й строке в j-м столбце, а также перестановка 
двух строк матрицы. В случае, если T — числовой тип, для Matrix[T] должна быть доступна дополнительная 
операция power, возвращающая результат возведения матрицы в указанную степень.

# Реализация

```scala
import scala.reflect.ClassTag

class Matrix[T: ClassTag] private (private val data: IndexedSeq[IndexedSeq[T]]) {
  val size: Int = data.length

  def apply(i: Int, j: Int): T = data(i)(j)

  def swapRows(i: Int, j: Int): Matrix[T] = {
    if (i == j || i < 0 || j < 0 || i >= size || j >= size) this
    else new Matrix(data.updated(i, data(j)).updated(j, data(i)))
  }

  def power(n: Int)(implicit num: Numeric[T] = null): Matrix[T] = {
    if (num == null)
      throw new UnsupportedOperationException("Возведение в степень только для числовых типов")

    def multiply(a: Matrix[T], b: Matrix[T]): Matrix[T] = {
      val newData = Array.ofDim[T](size, size)
      for (i <- 0 until size; j <- 0 until size) {
        newData(i)(j) = (0 until size).foldLeft(num.zero) { (sum, k) =>
          num.plus(sum, num.times(a(i, k), b(k, j)))
        }
      }
      new Matrix(IndexedSeq.from(newData.map(row => IndexedSeq.from(row))))
    }

    def pow(mat: Matrix[T], exp: Int): Matrix[T] = {
      if (exp <= 0) identityMatrix
      else if (exp % 2 == 0) pow(multiply(mat, mat), exp / 2)
      else multiply(mat, pow(multiply(mat, mat), exp / 2))
    }

    pow(this, n)
  }

  private def identityMatrix(implicit num: Numeric[T]): Matrix[T] = {
    val identity = Array.tabulate(size, size) { (i, j) =>
      if (i == j) num.one else num.zero
    }
    new Matrix(IndexedSeq.from(identity.map(row => IndexedSeq.from(row))))
  }
  
  override def toString: String =
    data.map(row => row.mkString("[", ", ", "]")).mkString("\n")
}

object Matrix {
  def apply[T: ClassTag](rows: Seq[Seq[T]]): Matrix[T] = {
    require(rows.forall(_.length == rows.size), "Матрица должна быть квадратной")
    new Matrix(rows.map(_.toIndexedSeq).toIndexedSeq)
  }
}


object Main extends App {
  val m1 = Matrix(Seq(
    Seq(1, 2, 3),
    Seq(3, 4, 6),
    Seq(7, 8, 9)
  ))
  println("Матрица:")
  println(m1)

  val mSwapped = m1.swapRows(0, 1)
  println("\nПерестановка строк 0 и 1:")
  println(mSwapped)

  val powered = m1.power(3)
  println("\nМатрица^3:")
  println(powered)

  val m2 = Matrix(Seq(
    Seq(1.5, 2.0),
    Seq(0.5, 1.0)
  ))
  val poweredDouble = m2.power(2)
  println("\nМатрица 2:")
  println(poweredDouble)

  val m3 = Matrix(Seq(
    Seq("a", "b"),
    Seq("c", "d")
  ))
  println("\nМатрица без чисел:")
  println(m3)

  // Следующая строчка вызовет исключение:
  // println(m3.power(2))
}
```

# Тестирование

Результат запуска программы:

```
Output:

Матрица:
[1, 2, 3]
[3, 4, 6]
[7, 8, 9]

Перестановка строк 0 и 1:
[3, 4, 6]
[1, 2, 3]
[7, 8, 9]

Матрица^3:
[424, 528, 666]
[876, 1090, 1374]
[1498, 1860, 2340]

Матрица 2:
[3.25, 5.0]
[1.25, 2.0]

Матрица без чисел:
[a, b]
[c, d]
```

# Вывод
При выполнении данной лабораторной работы были приобретены навыки разработки обобщённого класса 
Matrix[T], представляющего неизменяемую квадратную матрицу с элементами произвольного типа T.