package fintech.homework01

// Используя функции io.readLine и io.printLine напишите игру "Виселица"
// Пример ввода и тест можно найти в файле src/test/scala/fintech/homework01/HangmanTest.scala
// Тест можно запустить через в IDE или через sbt (написав в консоли sbt test)

// Правила игры "Виселица"
// 1) Загадывается слово
// 2) Игрок угадывает букву
// 3) Если такая буква есть в слове - они открывается
// 4) Если нет - рисуется следующий элемент висельника
// 5) Последней рисуется "веревка". Это означает что игрок проиграл
// 6) Если игрок все еще жив - перейти к пункту 2

// Пример игры:

// Word: _____
// Guess a letter:
// a
// Word: __a_a
// Guess a letter:
// b
// +----
// |
// |
// |
// |
// |

// и т.д.

class Hangman(io: IODevice) {

  def getUserInput(currentWord: String): String = {
    io.printLine("Word: " + currentWord)
    io.printLine("Guess a letter:")

    io.readLine()
  }

  def isCorrectInput(userInput: String): Boolean = {
    userInput.length == 1 && userInput.charAt(0) >= 'a' && userInput.charAt(0) <= 'z'
  }

  def getNewWordState(guessingLetter: Char, currentWord: String, word: String): String = {
    val newWord = StringBuilder.newBuilder
    for (i <- Range(0, word.length()) ) {
      newWord.append(
        if (guessingLetter == word.charAt(i))
          word.charAt(i)
        else
          currentWord.charAt(i)
      )
    }
    newWord.toString()
  }

  def printBadLetterMessage(mistakes: Int): Unit = {
    io.printLine(stages(mistakes - 1))
  }

  def printEndOfGameMessage(word: String, points: Int): Unit ={
    if (points == word.length()) {
      println("You won!")
      println("The word was " + word)
    }
    else
      println("You are dead")
  }

  def play(word: String): Unit = {
    var currentWord = "_" * word.length()
    var mistakes = 0
    var points = 0
    while (points < word.length() && mistakes < stages.size) {
      val userInput = getUserInput(currentWord)
      if (isCorrectInput(userInput)) {
        val newWord = getNewWordState(userInput.charAt(0), currentWord, word)
        if (currentWord == newWord) {
          mistakes += 1
          printBadLetterMessage(mistakes)
        }
        else {
          points += 1
          currentWord = newWord
        }
      }
      else {
        io.printLine("Incorrect input. Expected a letter")
      }
    }
    printEndOfGameMessage(word, points)
  }

  val stages = List(
    """+----
      ||
      ||
      ||
      ||
      ||
      |""".stripMargin,
    """+----
      ||
      ||   O
      ||
      ||
      ||
      |""".stripMargin,
    """+----
      ||
      ||   O
      ||   |
      ||
      ||
      |""".stripMargin,
    """+----
      ||
      ||   O
      ||   |
      ||  /
      ||
      |""".stripMargin,
    """+----
      ||
      ||   O
      ||   |
      ||  / \
      ||
      |""".stripMargin,
    """+----
      ||
      ||   O
      ||  /|
      ||  / \
      ||
      |""".stripMargin,
    """+----
      ||
      ||   O
      ||  /|\
      ||  / \
      ||
      |""".stripMargin,
    """+----
      ||   |
      ||   O
      ||  /|\
      ||  / \
      ||
      |""".stripMargin
  )
}

trait IODevice {
  def printLine(text: String): Unit
  def readLine(): String
}
