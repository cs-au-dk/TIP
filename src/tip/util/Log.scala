package tip.util

import scala.reflect.ClassTag
import Log._

/**
  * Basic logging functionality.
  */
object Log {

  /**
    * Log levels.
    */
  object Level extends Enumeration {
    type Level = Value
    val None, Error, Warn, Info, Debug, Verbose = Value
  }

  var defaultLevel = Level.Info

  /**
    * Constructs a new logger.
    * @param forcedLevel log level
    * @param ct class the logger belongs to
    */
  def logger[A: ClassTag](forcedLevel: Level.Level = defaultLevel)(implicit ct: ClassTag[A]): Logger =
    Logger(ct.runtimeClass.getSimpleName, forcedLevel)
}

/**
  * A logger.
  */
final case class Logger(var tag: String, var level: Log.Level.Level) {

//  val TAG_MAX_LEN = 30
//
//  tag = s"[${tag.padTo(TAG_MAX_LEN, ' ')}]"

  private def log(message: String, t: Throwable, msgLev: Log.Level.Level): Unit = {
    if (msgLev.id <= level.id || msgLev.id < Log.defaultLevel.id) {
      var account = 0
      val color = msgLev match {
        case Level.Error =>
          account -= 9; Console.BOLD + Console.RED
        case Level.Warn =>
          account -= 9; Console.BOLD + Console.YELLOW
        case Level.Info =>
          account -= 9; Console.BOLD + Console.BLUE
        case Level.Verbose =>
          account -= 9; Console.BOLD + Console.BLUE
        case _ => account -= 1; Console.RESET
      }
//      val preamble = s"$color$tag ${msgLev.toString.toLowerCase}: "
//      val space = (1 to (preamble.length + account)).map(_ => " ").mkString("")
//      val nmessage = message.replace("\r", "").replace("\n", s"\r\n$space")
//      println(s"$preamble$nmessage" + Console.RESET)
      print(s"$color[${msgLev.toString.toLowerCase}] ")
      print(Console.RESET)
      println(message)
    }
    if (t != null) t.printStackTrace(System.out)

  }

  /**
    * Writes a message to the log at level "error" .
    */
  def error(message: String): Unit = log(message, null, Log.Level.Error)

  /**
    * Writes a message and a stack trace to the log at level "error".
    */
  def error(message: String, t: Throwable): Unit = log(message, t, Log.Level.Error)

  /**
    * Writes a message to the log at level "warn" .
    */
  def warn(message: String): Unit = log(message, null, Log.Level.Warn)

  /**
    * Writes a message and a stack trace to the log at level "warn".
    */
  def warn(message: String, t: Throwable): Unit = log(message, t, Log.Level.Warn)

  /**
    * Writes a message to the log at level "info" .
    */
  def info(message: String): Unit = log(message, null, Log.Level.Info)

  /**
    * Writes a message and a stack trace to the log at level "info".
    */
  def info(message: String, t: Throwable): Unit = log(message, t, Log.Level.Info)

  /**
    * Writes a message to the log at level "debug" .
    */
  def debug(message: String): Unit = log(message, null, Log.Level.Debug)

  /**
    * Writes a message and a stack trace to the log at level "debug".
    */
  def debug(message: String, t: Throwable): Unit = log(message, t, Log.Level.Debug)

  /**
    * Writes a message to the log at level "verbose" .
    */
  def verb(message: String): Unit = log(message, null, Log.Level.Verbose)

  /**
    * Writes a message and a stack trace to the log at level "verbose".
    */
  def verb(message: String, t: Throwable): Unit = log(message, t, Log.Level.Verbose)

}
