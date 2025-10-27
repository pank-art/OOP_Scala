% Лабораторная работа № 4 «Case-классы и сопоставление с образцом в Scala»
% 28 мая 2025 г.
% Артём Панкратов, ИУ9-61Б

# Цель работы
Целью данной работы является приобретение навыков разработки case-классов на языке Scala для 
представления абстрактных синтаксических деревьев.

# Индивидуальный вариант
Абстрактный синтаксис параметризованных выражений:

Expr → C(Expr, …, Expr) | VARIABLE | NUMBER
Здесь C(Expr, …, Expr) — конструктор данных. Список операндов конструктора может быть пустым.

Примеры выражений:

Cons(1, Cons(2, Cons(3, Nil()))) — список из трёх чисел 1, 2, 3, конструктор Cons имееет два операнда, 
конструктор Nil — ноль операндов.
Cons(X, Cons(Y, Nil())) — список из двух звеньев, головы списков неизвестные — заданы переменными.
Tree(Leaf(), 1, Tree(Leaf(), 2, Leaf())) — конструктор Tree с тремя операндами, конструктор Leaf — без 
операндов.
Унификация двух параметризованных выражений e1 и e2 — поиск таких подстановок σ1 и σ2, что e1/σ1 = e2/σ2 — 
подстановки переводят их в одно и то же выражение.

Если в выражениях e1 и e2 нет одноимённых переменных, т.е. vars(e1) ∩ vars(e2) = ∅, то можно искать общую 
унифицирующую подстановку σ, такую, что e1/σ = e2/σ.

Требуется написать функцию

unify : (Expr, Expr) => Option(Map[String, Expr])
которая ищет унифицирующую подстановку. Можно считать, что повторяющихся переменных в выражениях нет и 
одна и та же переменная не может входить одновременно в оба выражения.

Функция должна возвращать Some, если унифицирующая подстановка существует, и None, если подстановку найти 
невозможно.

# Реализация

```scala
object Main extends App {

  sealed trait Expr
  case class Var(name: String) extends Expr
  case class Num(value: Int) extends Expr
  case class App(constructor: String, args: Seq[Expr]) extends Expr {
    override def toString: String = {
      if (args.isEmpty) constructor
      else s"$constructor(${args.map(_.toString).mkString(", ")})"
    }
  }

  type Substitution = Map[String, Expr]

  def applySubst(expr: Expr, subst: Substitution): Expr = expr match {  // Подстановка
    case v @ Var(x) => subst.getOrElse(x, v)
    case Num(n) => Num(n)
    case App(c, args) => App(c, args.map(applySubst(_, subst)))
  }

  def occurs(varName: String, expr: Expr): Boolean = expr match {  // Проверка вхождения переменной
    case Var(name) => name == varName
    case Num(_) => false
    case App(_, args) => args.exists(occurs(varName, _))
  }

  def unify(e1: Expr, e2: Expr): Option[Substitution] = {
    def loop(e1: Expr, e2: Expr, subst: Substitution): Option[Substitution] = {
      val e1Sub = applySubst(e1, subst)
      val e2Sub = applySubst(e2, subst)

      (e1Sub, e2Sub) match {

        case (Var(x), Var(y)) if x == y =>
          Some(subst)

        case (Var(x), e) =>
          if (occurs(x, e)) None
          else Some(subst.updated(x, e))

        case (e, Var(x)) =>
          loop(Var(x), e, subst)

        case (Num(n1), Num(n2)) if n1 == n2 =>
          Some(subst)

        case (App(c1, args1), App(c2, args2)) if c1 == c2 && args1.length == args2.length =>
          (args1 zip args2).foldLeft(Some(subst): Option[Substitution]) { (acc, pair) =>
            acc.flatMap(s => loop(pair._1, pair._2, s))
          }

        case _ =>
          None
      }
    }

    loop(e1, e2, Map.empty)
  }


  val list1 = App("Cons", Seq(Num(1), App("Cons", Seq(Num(2), App("Nil", Seq())))))
  val list2 = App("Cons", Seq(Var("X"), App("Cons", Seq(Var("Y"), App("Nil", Seq())))))

  println("Expression 1: " + list1)
  println("Expression 2: " + list2)

  val result = unify(list1, list2)
  println("Unification result 1,2: " + result)
  println()
  
  val tree1 = App("Tree", Seq(App("Leaf", Seq()), Num(1), App("Tree", Seq(App("Leaf", Seq()), Num(2), 
  App("Leaf", Seq())))))
  val tree2 = App("Tree", Seq(App("Leaf", Seq()), Num(1), App("Tree", Seq(App("Leaf", Seq()), Var("X"), 
  App("Leaf", Seq())))))
  
  println("Tree 1: " + tree1)
  println("Tree 2: " + tree2)
  
  println("Unifying trees: " + unify(tree1, tree2))
  println()
  
  val expr3 = App("Cons", Seq(Num(1), App("Cons", Seq(Num(2), App("Nil", Seq())))))
  val expr4 = App("Cons", Seq(Var("X"), App("Nil", Seq())))
  
  println("Expression 3: " + expr3)
  println("Expression 4: " + expr4)
  
  val result2 = unify(expr3, expr4)
  println("Unification result 3,4: " + result2)
}

```

# Тестирование

```scala
Output:

Expression 1: Cons(Num(1), Cons(Num(2), Nil))
Expression 2: Cons(Var(X), Cons(Var(Y), Nil))
Unification result 1,2: Some(Map(X -> Num(1), Y -> Num(2)))

Tree 1: Tree(Leaf, Num(1), Tree(Leaf, Num(2), Leaf))
Tree 2: Tree(Leaf, Num(1), Tree(Leaf, Var(X), Leaf))
Unifying trees: Some(Map(X -> Num(2)))

Expression 3: Cons(Num(1), Cons(Num(2), Nil))
Expression 4: Cons(Var(X), Nil)
Unification result 3,4: None
```

# Вывод
В ходе лабораторной работы были освоены принципы реализации унификации параметризованных выражений в 
Scala с использованием case-классов и сопоставления с образцом.