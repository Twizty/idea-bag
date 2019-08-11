package frontend

import cats.effect.IO
import io.circe.{Encoder, ObjectEncoder}
import outwatch.dom._
import outwatch.dom.dsl._
import outwatch.router._
import outwatch.router.dsl.C
import outwatch.util.Store
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import monix.reactive.Observer
import outwatch.ProHandler
import outwatch.http.Http
import outwatch.http.Http.Request
import io.circe.syntax._
import io.circe.generic.semiauto.deriveEncoder

sealed trait Page
case object Home extends Page
case object Form extends Page
case object GetIdea extends Page
case object NotFound extends Page

object Frontend {
  case class SaveRequest(body: String)

  implicit val reqEncoder: Encoder[SaveRequest] = deriveEncoder

  def router: AppRouter[Page] = AppRouter.createParseSiteRoot[Page](NotFound) {
    case Root               => Home
    case Root / "idea_form" => Form
    case Root / "idea"      => GetIdea
    case _                  => NotFound
  }

  val ideasHandler = Handler.unsafe[String].mapObservable { x =>
    println(x)
    x
  }

  val saveIdeaHandler =
    Handler.unsafe[String].mapObservable(x => { println(("save", x)); x })

  val databaseSavingStream = Http.post(saveIdeaHandler.map { text =>
    Request("http://localhost:8081/api/idea",
            SaveRequest(text).asJson.toString,
            headers = Map("Content-Type" -> "application/json"))
  })

  val responseStream = databaseSavingStream.map { resp =>
    println(resp)
    resp.status.toString
  }

  val discardFieldHandler = saveIdeaHandler.mapObservable(_ => "")

  def main(args: Array[String]): Unit = {
    router.store
      .map { implicit store =>
        AppRouter.render[Page] {
          case Home     => homeTab
          case Form     => sendForm
          case GetIdea  => getIdeaTab
          case NotFound => notFoundTab
        } -> store
      }
      .flatMap {
        case (obs, store) =>
          OutWatch.renderInto("#app", layout(div(obs))(store))
      }
      .unsafeRunSync()
  }

  def layout(node: BasicVNode)(implicit store: RouterStore[Page]) =
    div(
      className := "navbar",
      ul(
        li(C.a[Page]("/idea_form")("Add Idea")),
        li(C.a[Page]("/idea")("Get Idea"))
      ),
      node
    )

  def homeTab = div(h1("Choose the action"))

  def getIdeaTab = div(h1("Get idea tab"))

  def notFoundTab = div(h1("Not found"))

  def sendForm = {
    form(
      className := "ideaForm",
      onSubmit.preventDefault(ideasHandler) --> saveIdeaHandler,
      textArea(
        onInput.value --> ideasHandler,
        value <-- discardFieldHandler,
        name := "body",
        placeholder := "input text of your idea"
      ),
      input(tpe := "submit", value := "Save"),
      span(responseStream)
    )
  }
}
