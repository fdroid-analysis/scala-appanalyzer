package de.halcony.appanalyzer.setup

import de.halcony.appanalyzer.appbinary
import de.halcony.appanalyzer.appbinary.MobileApp
import de.halcony.appanalyzer.platform.PlatformOS
import de.halcony.appanalyzer.platform.PlatformOS.Android
import spray.json.{JsObject, JsString, JsonParser}
import wvlet.log.LogSupport

import java.io.{File, FileWriter}
import scala.io.Source

object Manifest extends LogSupport {
  def readFile(path: String): Map[String, MobileApp] = {
    if (new File(path).exists) {
      info("detected app manifest")
      val source = Source.fromFile(path)
      try {
        JsonParser(source.getLines().mkString("\n")).asInstanceOf[JsObject].fields.map {
          case (path: String, app: JsObject) =>
            path -> appbinary.MobileApp(
              app.fields("id").asInstanceOf[JsString].value,
              app.fields("version").asInstanceOf[JsString].value,
              app.fields("os").asInstanceOf[JsString].value.toLowerCase match {
                case "android" => PlatformOS.Android
                case "ios" => PlatformOS.iOS
              },
              app.fields("path").asInstanceOf[JsString].value,
            )
          case _ =>
            error("manifest seems broken")
            "NO" -> appbinary.MobileApp("NO", "NO", Android, "NO")
        }.filter { case (path, _) => path != "NO" }
      } finally {
        source.close()
      }
    } else {
      Map()
    }
  }

  def writeFile(path: String, apps: Map[String, MobileApp]): Unit = {
    val file = new FileWriter(new File(path))
    try {
      file.write(JsObject(apps.map {
        case (path, app) =>
          path -> JsObject(
            "id" -> JsString(app.id),
            "version" -> JsString(app.version),
            "os" -> JsString(app.getOsString),
            "path" -> JsString(app.path)
          )
      }).prettyPrint)
    } finally {
      file.flush()
      file.close()
    }
  }

}
