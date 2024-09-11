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
      case Nil => null

    val result= helper(list = list)
    result._1
  test("parse1", parse1, "list")

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
            const1<- helper(index+1)
            const2<- helper(const1._2)
          yield (const1._1 || const2._1, const2._2)
        case '&'=>
          for
            const1<- helper(index+1)
            const2<- helper(const1._2)
          yield (const1._1 && const2._1, const2._2)
        case '!'=> 
          for
          const1 <- helper(index+1)
          yield(!(const1._1),const1._2)
          
        case _ => None // Invalid syntax
        

        

    helper(0) match
      case Some(value) => if value._2 == str.length() then Some(value._1) 
                          else None  
      case None => None
    
    


  test("parse2", parse2, "str")
