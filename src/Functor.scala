
trait Functor[F[_]] {
  def map[A, B](fa: F[A], f: A => B): F[B]
}

case class My[T](e: T){
  def map[B](f: T=>B): My[B] = My(f(e))
}

object run extends App {
  def testMap[A, B, M <: My[A]](m:M, f: A => B)(implicit functor: Functor[My]) = {
    functor.map(m, f)
  }

  implicit object MyFunctor extends Functor[My] {
    def map[A, B](fa: My[A], f: A=>B) = My(f(fa.e))
  }

  println(testMap(My(200), (x: Int) => x + " O.K."))

  println(My(200).map(_.toString))
}
