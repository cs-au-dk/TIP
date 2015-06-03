package tip.logging

import tip.logging.Log.Level

import scala.language.existentials
import scala.reflect.ClassTag

object Log {

  object Level extends Enumeration {
    type Level = Value
    val None, Error, Warn, Info, Debug, Verbose = Value
  }

  var defaultLevel = Level.None

  def classLogger(x: Class[_], forcedLevel: Level.Level = defaultLevel): Logger = {
    Logger(x.getSimpleName, forcedLevel)
  }

  def typeLogger[A: ClassTag](forcedLevel: Level.Level = defaultLevel)(implicit ct: ClassTag[A]): Logger = {
    Logger(ct.runtimeClass.getSimpleName, forcedLevel)
  }

  def tagLogger(tag: String, forcedLevel: Level.Level = defaultLevel): Logger = {
    Logger(tag, forcedLevel)
  }
}


final case class Logger(var tag: String, level: Log.Level.Level) {

  final val TAG_MAX_LEN = 40

  tag = s"[${tag.padTo(TAG_MAX_LEN, ' ')}]"

  private def log(message: String, t: Throwable, msgLev: Log.Level.Level): Unit = {
    if (msgLev.id <= level.id || msgLev.id < Log.defaultLevel.id) {
      val color = msgLev match {
        case Level.Error => Console.BOLD + Console.RED
        case Level.Warn => Console.BOLD + Console.YELLOW
        case Level.Info => Console.BOLD + Console.BLUE
        case _ => Console.RESET
      }
      val preamble = s"$color$tag ${msgLev.toString.toUpperCase}: "
      val space = preamble.map(c => ' ')
      val nmessage = message.replace("\n", s"\n$space")
      println(s"$preamble$nmessage" + Console.RESET)
    }
    if (t != null) t.printStackTrace()

  }

  def error(message: String): Unit = log(message, null, Log.Level.Error)

  def error(message: String, t: Throwable): Unit = log(message, t, Log.Level.Error)

  def warn(message: String): Unit = log(message, null, Log.Level.Warn)

  def warn(message: String, t: Throwable): Unit = log(message, t, Log.Level.Warn)

  def info(message: String): Unit = log(message, null, Log.Level.Info)

  def info(message: String, t: Throwable): Unit = log(message, t, Log.Level.Info)

  def debug(message: String): Unit = log(message, null, Log.Level.Debug)

  def debug(message: String, t: Throwable): Unit = log(message, t, Log.Level.Debug)

  def verb(message: String): Unit = log(message, null, Log.Level.Verbose)

  def verb(message: String, t: Throwable): Unit = log(message, t, Log.Level.Verbose)

}

