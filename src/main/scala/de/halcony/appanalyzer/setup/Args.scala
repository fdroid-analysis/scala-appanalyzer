package de.halcony.appanalyzer.setup

import de.halcony.appanalyzer.platform.device
import de.halcony.appanalyzer.platform.device.{AndroidDeviceNonRoot, AndroidEmulatorRoot, Device}
import de.halcony.appanalyzer.{Config, Experiment}
import de.halcony.argparse.{OptionalValue, ParsingResult}
import wvlet.log.LogSupport

case class Args(
  verbose: Option[Boolean],
  config: Option[String],

)

object Args extends LogSupport {
  def getBatchSize(pargs: ParsingResult): Option[Int] = {
    try {
      Some(pargs.getValue[String]("batchSize").toInt)
    } catch {
      case _: Throwable => None
    }
  }

  def readParameters(parameters: OptionalValue[String]): Map[String, String] = {
    parameters.value match {
      case Some(keyValueCsv) =>
        keyValueCsv.split(',').map {
          keyValue =>
            keyValue.split('=').toList match {
              case key :: value :: Nil => key -> value
              case x => throw new RuntimeException(s"element $keyValue of $keyValueCsv has malformed split: $x")
            }
        }.toMap
      case None => Map[String, String]()
    }
  }

  def getDevice(pargs: ParsingResult, conf: Config): Device = {
    pargs.getValue[String]("platform") match {
      case "android_device" => device.AndroidDevice(conf)
      case "android_device_non_root" => new AndroidDeviceNonRoot(conf)
      case "android_emulator_root" => new AndroidEmulatorRoot(conf)
      case "ios" => device.iOSDevice(conf)
      case x =>
        throw new RuntimeException(s"device type $x is not yet supported")
    }
  }

  def initializeExperiment(pargs: ParsingResult): Unit = {
    val description = pargs.getValue[String]("description")
    info(s"running $description")
    pargs.get[OptionalValue[String]]("continue").value match {
      case Some(value) => Experiment.loadExperiment(value.toInt)
      case None => Experiment.createNewExperiment(description)
    }
  }
}
