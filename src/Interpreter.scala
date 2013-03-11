import collection.mutable
import scala.Some
import util.parsing.combinator._

import java.util.NoSuchElementException


class Interpreter extends JavaTokenParsers {

  // % SYNTACTIC SUGAR %

  class Branch[T](cond : => Boolean, t : => T) { def |(f : => T) : T = if (cond) t else f  }
  class TernaryConditionalOperator(cond : Boolean) { def ?[T](t : => T) : Branch[T] = new Branch(cond, t) }

  implicit def convertToTernary(b: Boolean) : TernaryConditionalOperator = new TernaryConditionalOperator(b)


  //
  // % CONTEXT %
  //

  object Context {
    def apply() = new Context(null)
  }

  sealed class Context(parent : Context) {

    private val vars  = new mutable.HashMap[String, Var]
    private val funcs = new mutable.HashMap[String, Func]


    def peekVar(name : String) : Var = {
      vars.get(name) match {
        case Some(v) => v
        case None => if (parent != null) parent.peekVar(name) else UndeclaredVar
      }
    }

    def declVar(name : String) = { vars.getOrElseUpdate(name, new Var(name)) }

    def bindVar(name : String, e : Expr) = { declVar(name).bind(e) }

    def peekFunc(name : String) : Func = {
      funcs.get(name) match {
        case Some(f) => f
        case None => if (parent != null) parent.peekFunc(name) else UndeclaredFunc
      }
    }

    def bindFunc(name : String, signature : List[String], body : Expr, lctx : Context) : Func = {
      val decl = new Func(name, signature, body, lctx); funcs.put(name, decl); decl
    }

  }

  sealed case class Var(name : String) {
    var expr : Expr = null
    def bind(e : Expr) = { expr = e; this }
  }

  case object UndeclaredVar extends Var("") {
    override def bind(e : Expr) = throw new NoSuchElementException("UndeclaredVar.bind")
  }


  sealed case class Func(name : String, signature : List[String], body : Expr, local : Context) {
    def eval() : BigDecimal = body.value()
    def context : Context = local
  }

  case object UndeclaredFunc extends Func("", List.empty, null, null) {
    override def eval() = throw new NoSuchElementException("UndeclaredFunc.eval")
  }


  /////////////////////////////////////////////////////////////////////////////////////////////////


  sealed trait Expr {
    def value() : BigDecimal
  }


  case class LiteralDecimalExpression(v : BigDecimal) extends Expr {
    override def value() = v
  }

  case class VariableRefExpression(v : Var) extends Expr {
    override def value() = v.expr.value()
  }

  case class FunctionInvokationExpression(f : Func) extends Expr {
    override def value() = f.eval()
  }

  case class BinaryExpression(op : String, lop : Expr, rop : Expr) extends Expr {

    override def value() = {
      op match {
        case "+" => lop.value() + rop.value()
        case "-" => lop.value() - rop.value()
        case "*" => lop.value() * rop.value()
        case "/" => lop.value() / rop.value()
      }
    }

  }


  // % HELPERS %

  def convertToBinaryOpExpr : PartialFunction[~[Expr, List[~[String, Expr]]], Expr] = {

    case lop ~ rem => rem.foldLeft(lop) {
        case (lop_, op ~ rop) => new BinaryExpression(op, lop_, rop)
      }

  }

  def pickLastFromSeq[T] : PartialFunction[List[T], T] = { case l => l.last }


  //
  // % PARSERS %
  //

  // % PROGRAM % parsers

  def programP(gctx : Context) : Parser[Option[Expr]] = {
    rep1sep(statP(gctx), (";" | "\n")) ^^ pickLastFromSeq
  }


  // % STATEMENT % parsers

  def statP(ctx : Context) : Parser[Option[Expr]] = funcDefP(ctx) ^^ { case _ => None    } |||
                                                    varDefP(ctx)  ^^ { case _ => None    } |||
                                                    exprP(ctx)    ^^ { case e => Some(e) }


  // % EXPRESSION % parsers

  def exprP(ctx : Context) : Parser[Expr] = exprSeqP(ctx)

  def exprSeqP(ctx : Context) : Parser[Expr] = rep1sep(exprSingletonP(ctx), ",") ^^ pickLastFromSeq

  def exprSingletonP(ctx : Context) : Parser[Expr] = assignExprP(ctx) ||| arithExprP(ctx)

  def assignExprP(ctx : Context) : Parser[Expr] = varRefP(ctx) ~ ("=" ~> exprP(ctx)) ^^ {
    case lval ~ expr => { ctx.bindVar(lval.v.name, expr); expr }
  }

  def arithExprP(ctx : Context) : Parser[Expr] = sumP(ctx)


  def sumP(ctx : Context) : Parser[Expr] = productP(ctx) ~ rep( ( "+" | "-" ) ~ productP(ctx) ) ^^ convertToBinaryOpExpr

  def productP(ctx : Context) : Parser[Expr] = multipleP(ctx) ~ rep( ( "*" | "/" ) ~ multipleP(ctx)) ^^ convertToBinaryOpExpr

  def multipleP(ctx : Context) : Parser[Expr] = "(" ~> sumP(ctx) <~ ")" | ( literalP ||| varRefP(ctx) ||| funInvP(ctx) )


  def funInvP(ctx : Context) : Parser[Expr] = {
    //val lctx = new Context(ctx)
    ident ~ ( "(" ~> repsep("(" ~> exprSeqP(ctx) <~ ")" ||| exprSingletonP(ctx), ",") <~ ")" ) into {
      case name ~ args => Parser {
        case in : Input => ctx.peekFunc(name) match {
          case UndeclaredFunc =>  Error("Function `" + name + "' is undeclared!", in)
          case f              =>  if (f.signature.size == args.size) {
                                    f.signature.zip(args).foreach { case (name, expr) => f.context.bindVar(name, expr) }
                                    Success(new FunctionInvokationExpression(f), in)
                                  } else {
                                    Error("Function `" + name + "' declaration contains " + f.signature.size + " arguments, supplied " + args.size + "!", in)
                                  }
        }
      }
    }
  }


  def varRefP(ctx : Context) : Parser[VariableRefExpression] = ident into {
    case name => Parser {
      case in : Input => ctx.peekVar(name) match {
        case UndeclaredVar  => Error("Variable `" + name + "' is undeclared", in.drop(2)) /* UGLYHACK */
        case v              => Success(new VariableRefExpression(v), in)
      }
    }
  }

  def literalP : Parser[Expr] = decimalNumber ^^ { case s => s.contains(".") ? new LiteralDecimalExpression(s.toDouble) | new LiteralDecimalExpression(s.toInt) }


  // % DEFINITION % parsers

  def funcDefP(ctx : Context)  : Parser[Func] = {

    val funCtx = new Context(ctx)

    "def" ~> ident ~ ( "(" ~> repsep(varDeclP(funCtx), ",") <~ (")" ~ "=" ) ) ~ exprP(funCtx) ^^ {
      case (name ~ signature) ~ body => ctx.bindFunc(name, signature.map { _.name }, body, funCtx)
    }

  }

  def varDeclP(ctx : Context)  : Parser[Var] = "var" ~> ident ^^ { case name => ctx.declVar(name) }

  def varDefP(ctx : Context)   : Parser[Var] = varDeclP(ctx) ~ ("=" ~> exprP(ctx)) ^^ { case decl ~ value => ctx.bindVar(decl.name, value) }

}
