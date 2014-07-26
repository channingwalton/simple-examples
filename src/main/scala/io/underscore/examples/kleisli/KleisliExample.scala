package io.underscore.examples.kleisli

import scalaz.Kleisli
import scalaz.std.option._

/**
 * Note that this differs from the blog slightly as scalaz has changed.
 * See http://underscoreconsulting.com/blog/posts/2012/07/02/kleisli-arrows.html
 */
object KleisliExample extends App {

  // Some methods that take simple types
  // and return higher-kinded types
  def str(x: Int): Option[String] = Some(x.toString)

  def toInt(x: String): Option[Int] = Some(x.toInt)

  def double(x: Int): Option[Double] = Some(x * 2)

  // Lets compose those functions Ye Olde Way
  def oldSchool(i: Int) =
    for (x <- str(i);
         y <- toInt(x);
         z <- double(y))
    yield z

  // Kleisli!
  val strk = Kleisli((x: Int) => Option(x.toString))

  val toIntk = Kleisli((x: String) => Option(x.toInt))

  val doublek = Kleisli((x: Int) => Option(x * 2))

  val funky = strk >=> toIntk >=> doublek

  println(oldSchool(1)) // Some(2.0)
  println(funky(1)) // Some(2.0)

}
