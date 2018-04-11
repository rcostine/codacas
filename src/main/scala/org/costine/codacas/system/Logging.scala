package org.costine.codacas.system

import java.io.{PrintWriter, StringWriter}
import org.log4s.Logger

/**
 * Created by rjc on 5/6/15.
 */
trait Logging {
  def logPrefix : String

  private val logger = org.log4s.getLogger

  def debug : Boolean

  def log(x:String) : Unit =
    if (logger.isInfoEnabled) logger.info(s"$logPrefix: $x")
    else println(s"INFO: $logPrefix: $x")


  def log(x:String, t: Throwable) : Unit =
    if (logger.isErrorEnabled) logger.info(s"$logPrefix: $x")
    else {
      val sw = new StringWriter()
      t.printStackTrace(new PrintWriter(sw))
      println(s"ERROR: $logPrefix: $x; $sw")
    }


  def debug(x:String) : Unit =
    if (debug) {
      if (logger.isDebugEnabled) logger.debug(s"$logPrefix$x")
      else println(s"DEBUG: $logPrefix$x")
    }

}
