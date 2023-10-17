package de.halcony.appanalyzer.platform.telegram

import de.halcony.appanalyzer.platform.telegram.AppAnalyzerBot.botsApi
import org.telegram.telegrambots.bots.TelegramLongPollingBot
import org.telegram.telegrambots.meta.TelegramBotsApi
import org.telegram.telegrambots.meta.api.methods.send.SendMessage
import org.telegram.telegrambots.meta.api.objects.{Message, Update}
import org.telegram.telegrambots.meta.exceptions.TelegramApiException
import org.telegram.telegrambots.updatesreceivers.DefaultBotSession

class AppAnalyzerBot(val botToken: String, val chatId: String) extends TelegramLongPollingBot(botToken) {

  override def getBotUsername: String = "AppAnalyzerBot"

  def sendMessage(content: String): Unit = {
    val sm: SendMessage = SendMessage.builder()
      .chatId(chatId)
      .text(content).build()
    try {
      this.execute[Message, SendMessage](sm)
    } catch {
      case e: TelegramApiException => print(e.getMessage)
    }
  }

  private def register(): Unit = {
    botsApi.registerBot(this)
  }

  register()

  override def onUpdateReceived(update: Update): Unit = {}
}
object AppAnalyzerBot {
  private val botsApi: TelegramBotsApi = new TelegramBotsApi(classOf[DefaultBotSession])
}
