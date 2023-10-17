package de.halcony.appanalyzer

import de.halcony.appanalyzer.analysis.Analysis
import de.halcony.appanalyzer.analysis.plugin.{ActorPlugin, PluginManager}
import de.halcony.appanalyzer.appbinary.MobileApp
import de.halcony.appanalyzer.database.Postgres
import de.halcony.appanalyzer.platform.PlatformOS
import de.halcony.appanalyzer.platform.appium.Appium
import de.halcony.appanalyzer.platform.exceptions.FatalError
import de.halcony.appanalyzer.setup.AppsFolder.getRelevantApps
import de.halcony.appanalyzer.setup.Args.{getDevice, initializeExperiment, readParameters}
import de.halcony.argparse.{OptionalValue, Parser, ParsingException, ParsingResult}
import scalikejdbc.scalikejdbcSQLInterpolationImplicitDef
import wvlet.log.LogSupport

import java.io.File
import scala.annotation.unused
import scala.io.Source
import scala.io.StdIn.readLine

object AppAnalyzer extends LogSupport {

  private val parser: Parser = Parser("AppAnalyzer",
    "run apps and analyze their consent dialogs")
    .addFlag("verbose","v","verbose","if set a stacktrace is provided with any fatal error")
    .addOptional("config",
      "c",
      "config",
      Some("./config.json"),
      " the configuration file")
    .addSubparser(PluginManager.parser)
    .addSubparser(Parser("removedAnalysis", "delete listed analysis ids")
      .addPositional("analysisIds", "csv list of ids or file containing list of ids")
      .addDefault[(ParsingResult,Config) => Unit]("func", deleteAnalysis))
    .addSubparser(Parser("run","run an action/analysis")
      .addPositional("platform","the platform to be analyzed [android_device,android_device_non_root,android_emulator_root,ios]")
      .addPositional("path", "path to the required data for the chosen action")
      .addSubparser(Parser("functionalityCheck", "run through all fundamental API actions to check if it works")
        .addDefault[(ParsingResult, Config) => Unit]("func", functionalityCheck))
      .addSubparser(Parser("plugin","run an analysis using a plugin")
        .addPositional("plugin","the name of the actor plugin providing the analysis capabilities")
        .addFlag("ephemeral", "e", "ephemeral", "if set the experiment will be deleted directly after execution")
        .addFlag("empty","w","without-app","if set then no app is installed and the analysis is run on the raw OS")
        .addOptional("only","o","only",None,"a file or a csv list listing app ids that shall be analyzed (any other app is ignored)")
        .addOptional("description","d","description",Some(""),"an optional experiment description")
        .addOptional("batchSize", "b", "batch", None, "limit the amount of apps that are analyzed in bulk")
        .addOptional("continue", "r", "resume", None, "provides the experiment to be continued")
        .addOptional("parameters","p","parameters",None,"a csv list of <key>=<value> pairs")
        .addDefault[(ParsingResult,Config) => Unit]("func",runPluginExperiment,"runs an experiment using the specified plugin")))

  @unused
  private object IgnoreMe extends Throwable

  /** main function parsing config and command line
    *
    * @param args the command line args provided by the jvm
    */
  def main(args: Array[String]): Unit = {
    try {
      val pargs: ParsingResult = parser.parse(args)
      try {
        val conf = Config.parse(pargs.getValue[String]("config"))
        pargs.getValue[(ParsingResult, Config) => Unit]("func")(pargs, conf)
      } catch {
        case x: Throwable =>
          error(x.getMessage + (if(pargs.getValue[Boolean]("verbose")) "\n" + x.getStackTrace.mkString("\n") else ""))
      }
    } catch {
      case _ : ParsingException =>
    }
  }

  /** the main to delete the provided analysis ids
    *
    * @param pargs the parsed command line arguments
    * @param conf the provided configuration
    */
  private def deleteAnalysis(pargs : ParsingResult, conf : Config) : Unit = {
    Postgres.initializeConnectionPool(conf)
    val delete = Source.fromFile(pargs.getValue[String]("path"))
    try {
      val json : List[Int] = if(new File(pargs.getValue[String]("analysisIds")).exists()) {
        val source = Source.fromFile(pargs.getValue[String]("analysisIds"))
        try {
          source.getLines().map(_.toInt).toList
        } finally {
          source.close()
        }
      } else {
        pargs.getValue[String]("analysisIds").split(",").map(_.toInt).toList
      }
      println(s"shall we really delete those [${json.length}] analysis: ${json.mkString(",")}")
      val userInput = readLine("[Yes/No]")
      if(userInput.trim.toLowerCase == "yes") {
        Postgres.withDatabaseSession {
          implicit session =>
            json.foreach {
              id =>
                info(s"deleting $id")
                sql"""DELETE FROM interfaceanalysis WHERE id = $id"""
                  .update.apply()
            }
        }
      }
    } finally {
      delete.close()
    }
  }


  /** wrapper to run arbitrary analysis for all apps contained in the batch with a possible resume flag
   *
   * @param getNextActor a function creating the actor for reach analysis
   * @param pargs the parsed command line arguments
   * @param conf the configuration
   */
  private def runExperiment(getNextActor : => ActorPlugin, pargs: ParsingResult, conf: Config, osOnlyAnalysis : Boolean) : Unit = {
    val device = getDevice(pargs, conf)
    Postgres.initializeConnectionPool(conf)
    initializeExperiment(pargs)

    if (conf.telegram.enable) {
      info("enabling telegram bot")
      // Experiment.initializeTelegram(conf.telegram.token, conf.telegram.chatId)
    }

    try {
      if(osOnlyAnalysis) {
        Analysis.runAnalysis(getNextActor, MobileApp("EMPTY","EMPTY",device.PLATFORM_OS,"EMPTY"), device, conf)
      } else { // app analysis
        val apps = getRelevantApps(pargs, device, conf)
        var appsRemaining = apps.length
        apps.foreach {
          app =>
            info(s"we have $appsRemaining app${if (appsRemaining > 1) "s" else ""} to analyze")
            Analysis.runAnalysis(getNextActor, app, device, conf)
            appsRemaining -= 1
        }
      }
    } catch {
      case x : FatalError =>
        error(x.getMessage)
      case x : Throwable =>
        error(s"${x.getMessage} \n ${x.getStackTrace.mkString("\n")}")
        Experiment.addEncounteredError(x)
    } finally {
      device.resetDevice()
      device.stopDevice() // this should do anything for physical devices but stops the emulator for a clean restart
      if (pargs.getValue[Boolean]("ephemeral")) {
        Experiment.deleteCurrentExperiment()
        info("ephemeral experiment is done")
      } else {
        info(s"experiment ${Experiment.getCurrentExperiment.id} is done")
      }
    }
  }

  private def runPluginExperiment(pargs : ParsingResult, conf : Config) : Unit = {
    val pluginName = pargs.getValue[String]("plugin")
    val empty = pargs.getValue[Boolean]("empty")
    val parameters : Map[String,String] = readParameters(pargs.get[OptionalValue[String]]("parameters"))

    val manager = PluginManager.getPluginManager(conf)
    val actor = manager.loadPlugin(pluginName,parameters)
    runExperiment(actor, pargs, conf, empty)
  }

  /** perform functionality check of the currently connect device
   *
   * @param pargs the parsed command line arguments
   * @param conf the parsed config file
   */
  private def functionalityCheck(pargs : ParsingResult, conf : Config) : Unit = {
    println("Welcome to the functionality check!")
    println()
    println("This is most likely the most important yet most frustrating feature of all.")
    println("We will go through each element of the Device and Appium API to check if it works with the currently")
    println("attached and configured device.")
    println("As the path parameter I expect a single app")
    println("If something fails I might give some useful hints or just crash ¯\\_(oo)_/¯.")
    println("Let's start...")

    val device = getDevice(pargs,conf)
    val path : String = pargs.getValue[String]("path")
    println(s"We are supposed to work on ${device.PLATFORM_OS}")
    println(s"As our Canary App we are using $path")
    val id = path.split('/').last.split('.').dropRight(1).mkString(".").split('_').dropRight(1).mkString("_")
    println(id)

    def tryApiCommand(description : String, failureHint : Option[String] = None, optional : Boolean = false)(func : => Option[String]): Unit ={
      try {
        println(s"action: $description")
        func match {
          case Some(value) => println(s"return: $value")
          case None =>
        }
      } catch {
        case x : Throwable =>
          println(s"Failed with: ${x.getMessage}")
          println(x.getStackTrace.mkString("\n"))
          failureHint match {
            case Some(value) => println(s"Hint: $value")
            case None =>
          }
          if(!optional) throw new RuntimeException(s"$description failed, no point in continuing")
      }
      println()
    }

    tryApiCommand("device.ensureDevice") {
      device.ensureDevice()
      None
    }

    tryApiCommand("device.startFrida") {
      // device.startFrida()
      None
    }

    tryApiCommand("device.restartPhone") {
      None
    }

    tryApiCommand("device.getAppPackageAnalysis") {
      device.getAppPackageAnalysis(conf)
      None
    }

    val app = appbinary.MobileApp("","",PlatformOS.iOS,path)

    tryApiCommand("appPackageAnalysis.getAppId") {
      Some(device.getAppPackageAnalysis(conf).getAppId(app))
      None
    }

    tryApiCommand("installApp") {
      device.installApp(app)
      None
    }

    val appId = device.getAppPackageAnalysis(conf).getAppId(app)

    tryApiCommand("grantPermissions") {
      device.setAppPermissions(appId)
      None
    }

    tryApiCommand("startApp") {
      device.startApp(appId)
      Some(s"app running: ${device.getForegroundAppId.get == appId}")
    }

    tryApiCommand("getPid") {
      Some(device.getPid(appId))
    }

    tryApiCommand("getPrefs") {
      device.getPrefs(appId)
    }

    tryApiCommand("getPlatformSpecificData") {
      device.getPlatformSpecificData(appId)
    }

    println("seems like everything is in working order")
    println("now we are going into some more involved features corresponding to our promised functionalities")

    // using appium to retrieve elements (which implies that we can also interact)
    tryApiCommand("appium.getElements") {
      Appium.withRunningAppium(appId, conf,device) {
        appium =>
          Some(appium.findElementsByXPath("//*").map(_.getText.trim).filter(_ != "").mkString("\n"))
      }
      None
    }

    // everything works as intended
    tryApiCommand("stopApp") {
      device.closeApp(appId)
      Some(s"app running: ${device.getForegroundAppId.getOrElse("nobody") == appId}")
    }


    tryApiCommand("device.stopFrida") {
      device.stopFrida()
      None
    }

    tryApiCommand("uninstallApp") {
      device.uninstallApp(appId)
      None
    }
  }
}
