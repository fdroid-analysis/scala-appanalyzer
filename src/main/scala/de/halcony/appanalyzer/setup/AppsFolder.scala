package de.halcony.appanalyzer.setup

import com.mchange.v3.concurrent.BoundedExecutorService
import de.halcony.appanalyzer.appbinary.MobileApp
import de.halcony.appanalyzer.appbinary.apk.APK
import de.halcony.appanalyzer.appbinary.ipa.IPA
import de.halcony.appanalyzer.platform.PlatformOS
import de.halcony.appanalyzer.platform.PlatformOS.{Android, PlatformOS}
import de.halcony.appanalyzer.platform.device.Device
import de.halcony.appanalyzer.setup.Args.getBatchSize
import de.halcony.appanalyzer.setup.Manifest.{readFile => readManifestFile, writeFile => writeManifestFile}
import de.halcony.appanalyzer.{Config, Experiment, appbinary}
import de.halcony.argparse.{OptionalValue, ParsingResult}
import wvlet.log.LogSupport

import java.io.File
import java.util.concurrent.Executors
import scala.collection.mutable.{Map => MMap}
import scala.concurrent.duration.Duration.Inf
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutorService, Future}
import scala.io.Source

object AppsFolder extends LogSupport {

  private val executorService = new BoundedExecutorService(
    Executors.newFixedThreadPool(10), // a pool of ten Threads
    100, // block new tasks when 100 are in process
    50 // restart accepting tasks when the number of in-process tasks falls below 50
  )
  private implicit val executionContext: ExecutionContextExecutorService = ExecutionContext.fromExecutorService(executorService)


  /** filters the apps contained in the folder by already analyzed apps and creates MobileApp objects
   *
   * @param appPaths the paths to all the relevant app packages
   * @param conf     the configuration
   * @param os       the operating system for which the apps are
   * @return a list of MobileApp objects
   */
  private def getAppsToAnalyze(folderPath: String, appPaths: List[String], conf: Config, os: PlatformOS, filtering: Boolean): List[MobileApp] = {
    val manifestFilePath = s"$folderPath/manifest.json"
    val manifest = MMap(readManifestFile(manifestFilePath).toList: _*)
    val inspector = os match {
      case Android => APK(conf)
      case PlatformOS.iOS => IPA(conf)
    }
    val alreadyAnalyzed: Set[String] = if (filtering) Experiment.getAnalyzedApps.map(_.id).toSet else Set()
    val appFuture = Future.sequence {
      appPaths.map {
        appPath =>
          Future {
            try {
              val app = manifest.synchronized(manifest.get(appPath)) match {
                case Some(app) => app
                case None =>
                  warn(s"app $appPath not contained in the manifest.json")
                  val app = appbinary.MobileApp(inspector.getAppId(appbinary.MobileApp("", "", os, appPath)), "NA", os, appPath)
                  manifest.synchronized(manifest.addOne(appPath -> app))
                  app
              }
              if (alreadyAnalyzed.contains(app.id)) {
                None
              } else {
                Some(app)
              }
            } catch {
              case x: Throwable =>
                error(x.getMessage)
                None
            }
          }
      }
    }
    val appsToAnalyze = Await.result(appFuture, Inf).filter(_.nonEmpty).map(_.get)
    writeManifestFile(manifestFilePath, manifest.toMap)
    appsToAnalyze
  }

  private def getOnlyApps(only: Option[String]): Option[Set[String]] = {
    only match {
      case Some(onlyElement) =>
        if (new File(onlyElement).exists()) {
          val source = Source.fromFile(onlyElement)
          try {
            Some(source.getLines().toSet)
          } finally {
            source.close()
          }
        } else {
          Some(onlyElement.split(",").toSet)
        }
      case None => None
    }
  }

  /** takes the provided path and returns the limited subset of all contained apps
   *
   * @param pargs  the parsed command line arguments
   * @param device the device to be used
   * @param conf   the parsed config
   * @return a list of mobile apps contained in the path
   */
  def getRelevantApps(pargs: ParsingResult, device: Device, conf: Config, filtering: Boolean = true): List[MobileApp] = {
    val path = pargs.getValue[String]("path")
    val apps = device.PLATFORM_OS match {
      case Android =>
        val (apks, folder) = if (new File(path).isDirectory) {
          (new File(path).listFiles().filter(_.isFile).filter(_.getPath.endsWith(".apk")).map(_.getPath).toList, path)
        } else {
          assert(path.endsWith(".apk"), s"path has to end with apk if not a directory in $path")
          (List(path), new File(path).getParentFile.getPath)
        }
        getAppsToAnalyze(folder, apks, conf, Android, filtering)

      case PlatformOS.iOS =>
        val (ipas, folder): (List[String], String) = if (new File(path).isDirectory) {
          (new File(path).listFiles().filter(_.isFile).filter(_.getPath.endsWith(".ipa")).map(_.getPath).toList, path)
        } else {
          assert(path.endsWith(".ipa"), s"path has to end with ipa if not a directory in $path")
          (List(path), new File(path).getParentFile.getPath)
        }
        getAppsToAnalyze(folder, ipas, conf, PlatformOS.iOS, filtering)
    }

    info(s"${apps.size} apps in the directory not analyzed so far")
    val filterListPath = pargs.get[OptionalValue[String]]("only").value
    val appsToAnalyze =  getOnlyApps(filterListPath) match {
      case Some(filterList) =>
        info(s"filtering apps with filter list: ${filterListPath.get}")
        apps.filter(app => filterList.contains(app.id))
      case None => apps
    }
    appsToAnalyze.slice(0, getBatchSize(pargs).getOrElse(apps.length))
  }

}
