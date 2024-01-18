trait Option[+A] {
  def map[B](f: A => B): Option[B] = { None }
  def flatMap[B](f: A => Option[B]): Option[B] = { None }
  def getOrElse[B >: A](default: => B): B = { default }
  def orElse[B >: A](ob: => Option[B]): Option[B]  = { None }
  def filter(f: A => Boolean): Option[A] = { None }
}
object None extends Option[Nothing]
case class Some[+A](get: A) extends Option[A]