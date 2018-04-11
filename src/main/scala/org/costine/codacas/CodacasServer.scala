package org.costine.codacas

import cats.effect.IO
import fs2.StreamApp
import io.circe._
import org.costine.codacas.system.Codacas
import org.costine.codacas.system.Codacas.CodacasRuntime
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.server.blaze.BlazeBuilder

import scala.concurrent.ExecutionContext.Implicits.global

object CodacasServer extends StreamApp[IO] with Http4sDsl[IO] {

  var runtime: Option[CodacasRuntime] = None

  private def initRuntime(args: List[String]): Unit = {
    val result = Codacas.run(args)
    val gameActor = result.gameActor
    result.gameActor ! result
    runtime = Some(result)
  }


  val service = HttpService[IO] {
    case GET -> Root / "hello" / name =>
      Ok(Json.obj("message" -> Json.fromString(s"Hello, ${name}")))
  }

  def stream(args: List[String], requestShutdown: IO[Unit]) = {

    initRuntime(args)
    BlazeBuilder[IO]
      .bindHttp(2018, "0.0.0.0")
      .mountService(service, "/")
      .serve
  }
}
