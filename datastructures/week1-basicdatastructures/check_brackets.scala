import java.io.{BufferedReader, InputStreamReader}

/**
  * Created by davidtan on 7/22/16.
  */
case class Bracket(val `type`: Char = 0, val position: Int = 0) {
  def Match(c: Char): Boolean = {
    if (`type` == '[' && c == ']') return true
    if (`type` == '{' && c == '}') return true
    if (`type` == '(' && c == ')') return true
    false
  }
}

object check_brackets {
  def main(args: Array[String]) {
    val input_stream: InputStreamReader = new InputStreamReader(System.in)
    val reader: BufferedReader = new BufferedReader(input_stream)
    val text = reader.readLine

    val stack = text.zipWithIndex.foldLeft(List[Bracket]())({

      (acc, char_and_position) => (acc, char_and_position) match {
        ////// load /////
        case (ys1, x) if x._1 == '{' => Bracket(char_and_position._1, char_and_position._2) :: ys1
        case (ys1, y) if y._1 == '(' => Bracket(char_and_position._1, char_and_position._2) :: ys1
        case (ys1, y) if y._1 == '[' => Bracket(char_and_position._1, char_and_position._2) :: ys1
        ////// empty///////
        case (ys1, a) if ys1.isEmpty && a._1 == '}' => {println(char_and_position._2 + 1);return}
        case (ys1, b) if ys1.isEmpty && b._1 == ')' => {println(char_and_position._2 + 1);return}
        case (ys1, c) if ys1.isEmpty && c._1 == ']' => {println(char_and_position._2 + 1);return}

        //// unload /////
        case (ys1, a1) if ys1.nonEmpty && a1._1 == '}' => {if (ys1.head.Match(a1._1)) ys1.tail //shave
          else {
            println(char_and_position._2 + 1)
            return //#end
          }
        }
        case (ys1, b1) if ys1.nonEmpty && b1._1 == ')' => {if (ys1.head.Match(b1._1)) ys1.tail //shave
        else {
          println(char_and_position._2 + 1)
          return //#end
        }
        }
        case (ys1, c1) if ys1.nonEmpty && c1._1 == ']' => {if (ys1.head.Match(c1._1)) ys1.tail //shave
        else {
          println(char_and_position._2 + 1)
          return //#end
        }
        }

        case (ys1, _) => ys1 //skip

      }
    })

    if (stack.isEmpty) println("Success")
    else println(stack.head.position + 1)

  }
}

/*
Good job! (Max time used: 0.72/3.00, max memory used: 53112832/536870912.)

{[]}()

[]
{[]}
[
 */
