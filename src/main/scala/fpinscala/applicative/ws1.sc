import fpinscala.applicative.{Applicative, Failure, Success, Validation}

type Error = String

def checkUserName(u: String): Validation[Error, String] =
  if (u.isEmpty) Failure("empty name", Vector.empty[Error])
  else Success(u)

def checkAge(a: Int): Validation[Error, Int] =
  if (a <= 0 || a > 150) Failure("wrong age", Vector.empty[Error])
  else Success(a)

def howOldUser(userName: String, userAge: Int) =
  s"$userName $userAge years old"


final case class User(userName: String, age: Int)

type Response = String

def handle(u: User): Response = {
  val validation = Applicative.validationApplicative[Error]
    .map2(
      checkUserName(u.userName),
      checkAge(u.age)
    )(howOldUser)

  validation match {
    case Success(a) => a
    case Failure(head, tail) => (head +: tail).foldLeft("wrong data:") { (d, msg) =>
      d + "\n" + msg
    }
  }
}

val wrongName = User("", 15)
val wrongAge  = User("Mike", -1)
val wrongAll  = User("", -3)
val allFine   = User("Mike", 15)

handle(wrongName)
handle(wrongAge)
handle(wrongAll)
handle(allFine)