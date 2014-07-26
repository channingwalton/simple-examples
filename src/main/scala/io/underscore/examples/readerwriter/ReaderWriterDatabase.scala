package io.underscore.examples.readerwriter

import scalaz._

/**
 * The problem is working with a database.
 * Operations should run in a transaction/connection/context
 * Errors need to be handled without exceptions
 * Side effects like post commits should be supported
 *
 * This is the usual thing:
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
        case whatever => rollback(); throw whatever
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

/**
 * Problems seen in a real codebase during refactoring to the final solution -
 * dev must remember to 'run' to get transaction - no compile time enforcement
 * - forgotten very often
 * - or having no idea whats going on multi-level transactions end up being constructed
 * post commits
 * - can't be bothered
 * - need global or thread-local objects to add success/fail
 * - harder to test
 * no nice error handling
 * - often just ignored
 * Wasteful
 * - typical pattern is to wrap every web request, or queue end point in a transaction when it may not be needed
 *
 * Step 1:
 * Solve the 'running in a transaction problem' with a Reader
 */


object ReaderToTheRescue {

  import scalaz.Reader

  type Key = String

  // The required transaction/connection/context for some database action
  trait Transaction

  // Work represents a unit of work to do against the Database
  // It is a type alias for a scalaz.Reader, wrapping a Transaction => A
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
        case whatever => rollback(); throw whatever
      }

    def startTransaction() = {}
    def commit() = {}
    def rollback() = {}

    // lift operations into Work - note both of these do nothing here
    def put[A](key: Key, a: A): Work[Unit] = Reader(Transaction => {})

    def find[A](key: Key): Work[Option[A]] = Reader(Transaction => None)
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

/**
 * Observations:
 * - everything now in for-comprehensions rather than imperative
 * - Reader is a function, nothing happens until Database.run
 * - Can no longer operate on the Database without a Transaction
 *
 * Step 2:
 * solve the post commits using a Writer, and the errors problem with
 * scalaz's either type, \/.
 */


object ReaderWriterForPostCommits {

  import scalaz.Scalaz._
  import scalaz._

  type Key = String

  trait Transaction

  // A class to hold the post commit function
  case class PostCommit(f: () => Unit)

  // Work represents some work to do on the Database
  // It is a Reader that takes a Transaction and returns a result
  // It is a Writer that will record post commit actions in a List
  // It is also a State which is ignored here
  // ReaderWriterState's type args are: the Reader type, Writer type, State type and A
  type Work[+A] = ReaderWriterState[Transaction, List[PostCommit], Unit, A]

  // helper to create Work for some Transaction => T
  def work[T](f: Transaction => T): Work[T]
  = ReaderWriterState { (trans, ignored) => (Nil, f(trans), ()) }

  // helper to create Work for a post commit, which is added to the written value
  def postCommit(f: () => Unit): Work[Unit]
  = ReaderWriterState { (trans, ignored) => (List(PostCommit(f)), (), ()) }

  object Database {

    object MyTransaction extends Transaction

    // a convenient method to drop the state part of the result
    // and also could be used in tests to check post commits
    def runWork[T](work: Work[T]): (List[PostCommit], T) = {
      val (postCommits, result, ignoredState) = work.run(MyTransaction, ())
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