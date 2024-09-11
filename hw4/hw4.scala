import javax.print.DocFlavor.INPUT_STREAM
import scala.compiletime.ops.double
object hw4 extends hwtest.hw("CS478"):
  def userName = "Bryan Almeida Betancourt"

  // Fill in and sign the appropriate statement of assistance below
  // as described in DAAW Appendix B1


  ////////////////////////////////////////
  // PROBLEM 1

  def parse1(list: List[Char]): Boolean =
    // use a recursive helper function!
    // Expr = 'T'
    //      | 'F'
    //      | '|' Expr Expr
    //      | '&' Expr Expr
    //      | '!' Expr
    def helper(list: List[Char]): (Boolean,List[Char]) =
      list match 
      case 'T':: rest => (true,rest)
      case 'F':: rest => (false,rest)
      case '|':: rest =>
        val (left_value,remainig1)=helper(rest)
        val (right_value, remainig2)= helper(remainig1)
        (left_value || right_value, remainig2)
      case '&':: rest =>
        val (left_value,remainig1)=helper(rest)
        val (right_value, remainig2)= helper(remainig1)
        (left_value && right_value, remainig2)
      case '!' :: rest => 
        val (value,remaining) = helper(rest)
        (!(value),remaining)
      case Nil =>null

    val(result,_)= helper(list = list)
    result
  test("parse1", parse1, "list")

  println(parse1(List('|', 'T', 'F'))) // should return true
  println(parse1(List('&', 'T', 'F'))) // should return false
  println(parse1(List('!', 'T')))      // should return false
  println(parse1(List('|', '&', 'T', 'F', '!', 'F'))) // should return true

  ////////////////////////////////////////
  // PROBLEM 2

  def parse2(str: String): Option[Boolean] =
    // use a recursive helper function!

    def helper(index: Int): Option[(Boolean, Int)]=
      if index >= str.length() then
        return None
      str(index) match
        case'T'  => Some((true,index+1))
        case'F' => Some((false,index+1))
        case '|'=> 
          for
          (left_value,nextIndex) <- helper(index+1)
          (right_value, finalIndex) <- helper(nextIndex)
        yield (left_value || right_value, finalIndex+1)
        case '&'=>
          for
          (left_value,nextIndex) <- helper(index+1)
          (right_value, finalIndex) <- helper(nextIndex)
        yield(left_value && right_value, finalIndex+1)
        case '!'=> 
          for
          (value,nextIndex)<- helper(index+1)
          yield(!(value),nextIndex+1)
          // Invalid character
        case'('  =>
          for
          (value1, nextIndex) <- helper(index + 1) // Parse the left side
          operator = str(nextIndex)                    // Get the operator
          (value2, finalIndex) <- helper(nextIndex + 1) // Parse the right side
          if str(finalIndex) == ')'              // Ensure closing parenthesis
        yield (operator match {
          case '&' => value1 && value2 // Logical AND
          case '|' => value1 || value2 // Logical OR
        }, finalIndex + 1)              // Move past the closing parenthesis
        case _ => None
        

        
              
    
    helper(0).flatMap {
    case (result, finalIndex) =>
      if (finalIndex == str.length) Some(result) // Return result if parsing complete
      else None // Return None if extra characters are present
    }


  test("parse2", parse2, "str")
