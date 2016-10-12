


def Y[A,B](f: (A=>B) => (A=>B)) : A=>B = f(Y(f))(_)

//define factorial
def fact = Y[Int, Int](f => n => if (n < 2) 1 else n * f(n-1))

//define n'th fibonacci number
def fib = Y[Int, Int](f => n => n match {
    case 1 => 1
    case 2 => 1
    case _ => f(n-1) + f(n-2)
})