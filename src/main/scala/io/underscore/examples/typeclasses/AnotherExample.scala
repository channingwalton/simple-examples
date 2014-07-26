package io.underscore.examples.typeclasses

/**
 * See http://underscoreconsulting.com/blog/posts/2012/07/09/another-typeclass.html
 */
object AnotherExample extends App {

  /* The type class pattern starts with a trait defining the
   * behaviour we need. In this case, a Zero for some type T,
   * that can provide the zero for that type.
   */
  trait Zero[T] { def zero: T }

  /* The other half of the pattern is the trait's companion
   * object which contains a set of implicit instances
   * for the type class. Whenever an implicit instance
   * of a Zero is required, the companion object is one of
   * the last places searched.
   *
   * In this case there is only an instance for Int but it
   * could contain instances for all the basic types.
   */
  object Zero {
    implicit object IntZero extends Zero[Int] { def zero = 0 }
  }

  /* So that's the type class, but scala's Option does not have
   * the method we need, so lets enrich the Option type with an
   * implicit conversion (view) to a new, anonymous type with
   * the method we need.
   */
  implicit def withTheZeroes[T: Zero](option: Option[T]) = new {
    /* Note that the T: Zero in the type parameter is called a context
     * bound. It says that this method wants an implicit instance
     * of Zero[T] which will be used below.
     */

    /* The method will use Option's getOrElse method to get the
     * value if the option is a Some, or else return the value
     * returned by the Zero[T] if the Option is a None.
     */
    def unary_~(): T = option.getOrElse( implicitly[Zero[T]].zero )

    /* The implicitly here is just a method in scala.Predef
     *
     *   def implicitly[T](implicit e: T) = e
     *
     *   The comment says:
     *     "for summoning implicit values from the nether world"
     *
     * In other words, it's a method that requires an implicit
     * instance of the supplied type, in this case Zero[T] and
     * returns it. Since withTheZeroes requires an implicit
     * instance of Zero[T] through the context bound discussed
     * above, implicitly[Zero[T]] will find it.
     */
  }

  // ok lets try it out with a standard scala Option
  val something = Some(1)
  val nada: Option[Int] = None

  println(~something) // returns 1
  println(~nada) // returns 0

  /* But there is more. Type classes are open to extension.
   * We can supply our own zero, either superseding those
   * supplied by the Zero companion object, or add new ones.
   *
   * Here is one for a String.
   */
  implicit object StringZero extends Zero[String] { def zero = "" }
  val noString: Option[String] = None
  println(~noString) // returns an empty string
}