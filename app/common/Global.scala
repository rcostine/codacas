package common

import org.costine.codacas.system.Codacas._
import play.api._

object Global extends GlobalSettings {
  private var codacasRuntime: Option[CodacasRuntime] = None

  override def onStart(app: Application) = {
    super.onStart(app)

    // start up the Codacas subsystem
    codacasRuntime = Some(runViaPlay(app))
    Logger.info("Codacas service application has started")
  }

  override def onStop(app: Application) = {

    // shutdown the Codacas subsystem if it is started
    codacasRuntime foreach { rt => shutdown(rt) }
    super.onStart(app)
    Logger.info("Codacas service application has stopped")

  }

}
