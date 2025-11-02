% Лабораторная работа № 1 «Введение в функциональное
  программирование на языке Scala»
% 5 марта 2025 г.
% Артём Панкратов, ИУ9-61Б

# Цель работы
Целью данной работы является ознакомление с программированием на языке Scala на основе чистых функций.

# Индивидуальный вариант
Функция trim: (List[Int], Int => Boolean) => List[Int], выполняющая удаление из списка подряд идущих нулей, количество которых удовлетворяет предикату.

# Реализация и тестирование

Работа в REPL-интерпретаторе Scala:

```scala
val trim: (List[Int], Int => Boolean) => List[Int] = { (list, predicate) =>
  list match {
    case Nil => Nil
    case x :: xs =>
      // Вспомогательная функция для сбора группы одинаковых элементов
      def collectGroup(head: Int, tail: List[Int], acc: List[Int]): (List[Int], List[Int]) = tail match {
        case y :: ys if y == head => collectGroup(head, ys, y :: acc)
        case _ => (acc, tail)
      }

      // Собираем текущую группу
      val (currentGroup, rest) = collectGroup(x, xs, List(x))

      // Если группа нулей и удовлетворяет предикату, пропускаем её
      if (currentGroup.head == 0 && predicate(currentGroup.length)) trim(rest, predicate)
      else currentGroup ++ trim(rest, predicate) // Иначе добавляем группу к результату
  }
}

// Предикат: удалить группы нулей длиной >= 2
val predicate = (length: Int) => length >= 2

// Исходный список
val input = List(0, 0, 1, 0, 0, 0, 2, 0, 3, 0, 0, 4)

// Вызов функции
val result = trim(input, predicate)

// Вывод результата
println(result) // Output: List(1, 2, 0, 3, 4)
```

# Вывод
В ходе выполнения данной лабораторной работы я познакомился с основами функционального программирования на языке Scala.
