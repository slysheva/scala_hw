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

  def getUserInput(currentWord: String, mistakes: Int): String = {
    val add = ""
    if (mistakes > 0)
      io.printLine(stages(mistakes - 1))
    io.printLine("""Word: """ + currentWord)
    io.printLine("""Guess a letter:""")

    io.readLine()
  }

  def isCorrectInput(userInput: String): Boolean = {
    userInput.length == 1 && userInput.charAt(0) >= 'a' && userInput.charAt(0) <= 'z'
  }

  def getNewWordState(guessingLetter: Char, currentWord: String, word: String): String = {
    val newWord = for (i <- word.indices) yield
      if (guessingLetter == word.charAt(i))
        word.charAt(i)
      else
        currentWord.charAt(i)
    newWord.mkString
  }

  def printEndOfGameMessage(word: String, currentWord: String): Unit ={
    if (currentWord == word) {
      io.printLine("""You won!""")
      io.printLine("""The word was """ + word)
    }
    else {
      io.printLine(stages(stages.size - 1) + """You are dead""")
    }
  }

  def play(word: String): Unit = {
    var currentWord = "_" * word.length()
    var mistakes = 0
    var used :Set[Char] = Set()
    while (word != currentWord && mistakes < stages.size) {
      val userInput = getUserInput(currentWord, mistakes)
      if (isCorrectInput(userInput)) {
        val newWord = getNewWordState(userInput.charAt(0), currentWord, word)
        if (currentWord == newWord || used.contains(userInput.charAt(0)))
          mistakes += 1
        else {
          used += userInput.charAt(0)
          currentWord = newWord
        }
      }
      else
        io.printLine("Incorrect input. Expected a letter")
    }
    printEndOfGameMessage(word, currentWord)
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