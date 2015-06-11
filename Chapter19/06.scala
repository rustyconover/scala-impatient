import scala.util.parsing.combinator._
class Expr
case class Number(value : Int) extends Expr
case class Assignment(name : String, value: Expr) extends Expr
case class Definition(name : String, args: List[String], value: Expr) extends Expr
case class Variable(name : String) extends Expr
case class IfCondition(condition: Expr, trueBranch: Expr, falseBranch: Expr) extends Expr
case class WhileLoop(condition: Expr, body: Expr) extends Expr
case class Call(name : String, params: List[Expr]) extends Expr
case class Operator(op : String, left: Expr, right: Expr) extends Expr
case class Block(contents: List[Expr]) extends Expr

class ExprParser extends RegexParsers {
  val number = "\\-?[0-9]+".r
  val name = "[a-z]+".r

  def block: Parser[Expr] = repsep(work, ";") ~ opt(";") ^^ {
    case r ~ _ => Block(r)
  }

  def blockScope: Parser[Expr] = ("{" ~> block <~ "}") ^^ {
    case t => t
  }

  def work: Parser[Expr] = assignment | definition | condition | whileloop | call | expr 

  def definition: Parser[Expr] = name ~ (("(" ~> repsep(name, ",") <~ ")")) ~ blockScope ^^ {
    case funcName ~ args ~ blockCode => Definition(funcName, args, blockCode)
  }

  def condition: Parser[Expr] = "if" ~> work ~ blockScope ~ opt("else" ~> blockScope) ^^ {
    case cond ~ branchTrue ~ None => IfCondition(cond, branchTrue, Number(0))
    case cond ~ branchTrue ~ Some(branchFalse) => IfCondition(cond, branchTrue, branchFalse)
  }

  def whileloop: Parser[Expr] = "while" ~> work ~ blockScope ^^ {
    case cond ~ body => WhileLoop(cond, body)
  }

  def assignment: Parser[Expr] = name ~ "=" ~ work ^^ {
    case n ~ _ ~ e => Assignment(n, e)
  }

  def call: Parser[Expr] = name ~ "(" ~ repsep(work, ",") ~ ")" ^^ {
    case n ~ _ ~ e ~ _ => Call(n, e)
  }

  def expr: Parser[Expr] = term ~ rep(("+" | "-" | "<" | ">" | "==") ~ term) ^^ {
    case t ~ r => {
      r.foldLeft(t)((x, y) => Operator(y._1, x, y._2))
    }
  }

  def term: Parser[Expr] = (factor ~ opt(("*" | "/") ~ term)) ^^ {
    case a ~ None => a
    case a ~ Some(b ~ c) => Operator(b, a, c)
  }

  def factor: Parser[Expr] = number ^^ { n => Number(n.toInt) } |
  name ^^ { n => Variable(n) } |
  "(" ~> expr <~ ")"
}

class Engine {
  var vars = Map[String,Int]()
  var functions = Map[String,Tuple2[List[String], Expr]]()
  def eval(foo : Expr) : Int = {
    foo match {
      case Block(content) => {
        content.map(eval(_)).last

      }
      case IfCondition(cond, trueBranch, falseBranch) => {
        if(eval(cond) != 0) {
          eval(trueBranch)
        } else {
          eval(falseBranch)
        }
      }
      case WhileLoop(cond, body) => {
        while(eval(cond) != 0) {
          eval(body)
        }
        0
      }
      case Number(n) => n
      case Assignment(name, value) => {
        var l = eval(value)
        vars = vars + (name -> l)
        l
      }
      case Definition(name, params, value) => {
        functions = functions + (name -> (params, value))
        0
      }
      case Operator(op, left, right) => {
        op match {
          case "<" => if(eval(left) < eval(right)) { 1 } else { 0 }
          case ">" => if(eval(left) > eval(right)) { 1 } else { 0 }
          case "==" => if(eval(left) == eval(right)) { 1 } else { 0 }

          case "+" => eval(left) + eval(right)
          case "-" => eval(left) - eval(right)
          case "/" => eval(left) / eval(right)
          case "*" => eval(left) * eval(right)
        }
      }
      case Variable(name) => vars.getOrElse(name, 0)
      case Call("print", params) => {
        println(params.map(eval(_)).mkString(","))
        0
      }
      case Call(name, params) => {
        var f = functions(name)
        val oldVars = vars

        vars = vars ++ Map[String,Int]((f._1 zip params.map(eval(_))).toArray : _*)
        val result = eval(f._2)
        vars = oldVars
        result
      }
    }
  }
}


val parser = new ExprParser
//val result = parser.parseAll(parser.block,"print((3+2));print(1);rusty=23;print(rusty);print(conover);rusty={2};")
val result = parser.parseAll(parser.block,"""
increment(i){
i+1
};
decrement(i) {
i-1
};


print(increment(1));
print(decrement(1));
print(increment(decrement(0)));

tester(i){
print(i);
i = i+1;
if(i < 10) {
tester(i)
};
};


if 1 {
 print(444)
} else {
print(555)
};

tester(0);
print(i);
while(i < 10) {
i = i+1;
print(i);
};
i = -5000;
print(i);
""")
println(result)

val e = new Engine
e.eval(result.get)
