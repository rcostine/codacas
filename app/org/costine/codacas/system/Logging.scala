package org.costine.codacas.system

import play.api._

/**
 * Created by rjc on 5/6/15.
 */
trait Logging {
  def logPrefix : String

  def debug : Boolean

  def log(x:String) : Unit = {
    if (Logger.isInfoEnabled) Logger.info(s"${logPrefix}: ${x}")
    else println(s"INFO: ${logPrefix}: ${x}")
  }

  def debug(x:String) : Unit = {
    if (debug) {
      if (Logger.isDebugEnabled) Logger.debug(s"${logPrefix}${x}")
      else println(s"DEBUG: ${logPrefix}${x}")
    }
  }
}
