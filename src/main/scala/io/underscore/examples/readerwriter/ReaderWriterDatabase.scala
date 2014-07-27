package io.underscore.examples.readerwriter

import scalaz._

/**
 * See http://underscoreconsulting.com/blog/posts/2014/07/08/readerwriterstate.html
 */
object TheProblem {

  type Key = String

  object Database {

    // Run a function in a transaction, rolling back on failure
    def run[T](f: => T): T =
      try {
        startTransaction()
        val result = f
        commit()
        result
      } catch {
        case whatever: Exception => rollback(); throw whatever
      }

    def startTransaction() = {}
    def commit() = {}
    def rollback() = {}

    def addPostCommit(f: () => Unit): Unit = {}

    def put[A](a: A): Unit = {}

    def find[A](key: String): Option[A] = None
  }

  val result: Option[String] = Database.run {
    Database.put("stuff")
    Database.addPostCommit(() => println("blah"))
    Database.find("foo")
  }
}

object ReaderToTheRescue {

  import scalaz.Reader

  type Key = String

  trait Transaction

  /* Work represents a unit of work to do against the Database
   * It is a type alias for a scalaz.Reader, which wraps
   * a Transaction => A
   */
  type Work[+A] = Reader[Transaction, A]

  object Database {

    object MyTransaction extends Transaction

    // Run now requires Work
    def run[T](work: Work[T]): T =
      try {
        startTransaction()
        val result = work.run(MyTransaction)
        commit()
        result
      } catch {
        case whatever: Exception => rollback(); throw whatever
      }

    def startTransaction() = {}
    def commit() = {}
    def rollback() = {}

    // lift operations into Work - they do nothing here
    def put[A](key: Key, a: A): Work[Unit] =
      Reader(Transaction => {})

    def find[A](key: Key): Work[Option[A]] =
      Reader(Transaction => None)
  }

  // the program
  val work: Work[Option[String]] =
    for {
      _ <- Database.put("foo", "Bar")
      found <- Database.find[String]("foo")
    } yield found

  // now run the program
  val result: Option[String] = Database.run(work)
}

object ReaderWriterForPostCommits {

  import scalaz.Scalaz._
  import scalaz._

  type Key = String

  trait Transaction

  // A class to hold the post commit function
  case class PostCommit(f: () => Unit)

  /* Work represents some work to do on the Database
   * It is a Reader that takes a Transaction and returns a result
   * It is a Writer that records post commit actions in a List
   * It is also a State which is ignored here
   * ReaderWriterState's type args are:
   *   the Reader type, Writer type, State type and A
   */
  type Work[+A] =
    ReaderWriterState[Transaction, List[PostCommit], Unit, A]

  // helper to create Work for some Transaction => T
  def work[T](f: Transaction => T): Work[T] =
    ReaderWriterState {
      (trans, ignored) => (Nil, f(trans), ())
    }

  // helper to create Work for a post commit,
  // PostCommits are added to the written value
  def postCommit(f: () => Unit): Work[Unit] =
    ReaderWriterState {
      (trans, ignored) => (List(PostCommit(f)), (), ())
    }

  object Database {

    object MyTransaction extends Transaction

    // a convenient method to drop the state part of the result
    // and also could be used in tests to check post commits
    def runWork[T](work: Work[T]): (List[PostCommit], T) = {
      val results = work.run(MyTransaction, ())
      val (postCommits, result, ignoredState) = results

      (postCommits, result)
    }

    def run[T](work: Work[T]): \/[Throwable, T] =
      \/.fromTryCatch{
        startTransaction()
        val (postCommits, result) = runWork(work)
        postCommits foreach addPostCommit
        commit()
        result
      }.leftMap(err => {rollback(); err})

    def addPostCommit(pc: PostCommit): Unit = {}
    def startTransaction() = {}
    def commit() = {}
    def rollback() = {}

    def put[A](key: Key, a: A): Work[Unit] = work(Transaction => {})

    def find[A](key: Key): Work[Option[A]] = work(Transaction => None)
  }

  // The program with a post commit
  val work2: Work[Option[String]] =
    for {
      _ <- Database.put("foo", "Bar")
      _ <- postCommit(() => println("wahey"))
      found <- Database.find[String]("foo")
    } yield found

  // note that the result type is now \/
  val result2: \/[Throwable, Option[String]] = Database.run(work2)
}

/**
 * Bonus, sometimes we want to work with the option returned from
 * a method as we would normally in a for-comprehension. At the moment
 * the option and not its value ends up in values in the for-comprehension.
 * To achieve this we need a Monad Transformer, OptionT to the rescue
 */
object CopeWithOptions {

  import scalaz.Scalaz._
  import scalaz._
  import ReaderWriterForPostCommits._

  // now we can work with options but we need to lift existing result into OptionT
  val optional: OptionT[Work, String] =
    for {
      _ <- Database.put("foo", "Bar").liftM[OptionT]
      _ <- postCommit(() => println("wahey")).liftM[OptionT]
      found <- OptionT[Work, String](Database.find[String]("foo"))
    } yield found

  val optionalResult: \/[Throwable, Option[String]] = Database.run(optional.run)
}