
object Main extends Interpreter {

  def main(args : Array[String]) {

    var gctx = Context()
    var continue = true

    do {
        readLine("> ") match {
          case ":reset" =>  gctx = Context()
          case ":exit"  =>  continue = false
          case p        =>  parseAll(programP(gctx), p /* "var x = 1; var y = 2; def foo() = 5; foo(1) + 5 - 10.0;" */) match {
                              case Success(expr, in)    => if (!expr.isEmpty) println(expr.get.value())
                              case NoSuccess(msg, in)   => println(msg)
                            }
        }
    } while(continue)

  }

}