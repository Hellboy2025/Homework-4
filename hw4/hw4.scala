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

    ???

  ignoretest("parse2", parse2, "str")
