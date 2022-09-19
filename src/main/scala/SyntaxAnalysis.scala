/**
 * FunLang syntax analyser.
 *
 * Copyright 2022, Anthony Sloane, Matthew Roberts, Kym Haines, Macquarie University, All rights reserved.
 */

package funlang

import org.bitbucket.inkytonik.kiama.parsing.Parsers
import org.bitbucket.inkytonik.kiama.util.Positions

/**
 * Module containing parsers for FunLang.
 */
class SyntaxAnalysis (positions : Positions) extends Parsers (positions) {

    import FunLangTree._
    import scala.language.postfixOps

    lazy val parser : PackratParser[Program] =
        phrase (program)

    lazy val program : PackratParser[Program] =
        exp ^^ Program

    lazy val literal : PackratParser[Exp] =
        "false" ^^^ BoolExp (false) |
        "true" ^^^ BoolExp (true) |
        integer ^^ (s => IntExp (s.toInt))

    lazy val factor : PackratParser[Exp] =
        // FIXME
        "(" ~> exp <~ ")" |
        identifier ^^ IdnUse |
        literal |
        failure ("exp expected")

    lazy val matchterm : PackratParser[Exp] =
        // FIXME
        factor

    // matches an individual 
    lazy val caseline : PackratParser[(Pat,Exp)] =
        // FIXME
        "fixme" ^^^ ((AnyPat(), IntExp(1)))

    // FIXME   add parsers between factor and exp

    lazy val exp : PackratParser[Exp] =
        // FIXME
        "(" ~> repsep (factor, ",") <~ ")" ^^ {case e => TupleExp(e)} |
        "List(" ~> repsep (exp, ",") <~ ")" ^^ {case e => ListExp(e)} |
        ("(" ~> idndef <~ ")") ~ ("=>" ~> exp) ^^ {case arg ~ b => LamExp(arg, b)} | 
        exp ~ ("(" ~> exp <~ ")") ^^ {case fn ~ arg => AppExp(fn, arg)} | 
        exp ~ ("*" ~> exp) ^^ {case l ~ r => StarExp(l, r)} |
        exp ~ ("/" ~> exp) ^^ {case l ~ r => SlashExp(l, r)} |
        exp ~ ("+" ~> exp) ^^ {case l ~ r => PlusExp(l, r)} |
        exp ~ ("-" ~> exp) ^^ {case l ~ r => MinusExp(l, r)} |
        exp ~ ("::" ~> exp) ^^ {case l ~ r => ConsExp(l, r)} |
        exp ~ ("<" ~> exp) ^^ {case l ~ r => LessExp(l, r)} |
        exp ~ ("==" ~> exp) ^^ {case l ~ r => EqualExp(l, r)} |
        (keyword ~> "(" ~> exp <~ ")") ~ (exp <~ keyword) ~ exp ^^ {case con ~ thenExp ~ elseExp => IfExp(con, thenExp, elseExp)} | 
        factor

    lazy val definitions : PackratParser[Vector[Defn]] =
        // FIXME
        "fixme" ^^^ Vector()

    lazy val defn : PackratParser[Defn] =
        // FIXME
        "fixme" ^^^ Defn(IdnDef("fixme", IntType()), IntExp(3))

    lazy val pat : PackratParser[Pat] =
        // FIXME
        basicpat

    lazy val basicpat : PackratParser[Pat] =
        // FIXME
        literal ^^ LiteralPat

    lazy val tipe : PackratParser[Type] =
        // FIXME
        "(" ~> tipe <~ ")" |
        tipe ~ ("=>" ~> tipe) ^^ {case to ~ from => FunType(to, from)} |
        "(" ~> repsep (tipe, ",") <~ ")" ^^ {case v => TupleType(v)} |
        "List" ~> "[" ~> tipe <~ "]" ^^ {case t => ListType(t)} |
        basictipe


    lazy val basictipe : PackratParser[Type] =
        // FIXME
        "Int" ^^^ IntType () |
        "Bool" ^^^ BoolType ()

    // NOTE: You should not change anything below here...

    lazy val integer =
        regex ("[0-9]+".r)

    lazy val idndef =
        (identifier <~ ":") ~ tipe ^^ IdnDef

    val keywordStrings =
        List ("block", "else", "false", "if", "val", "true", "List", "match",
              "def", "case")

    lazy val keyword =
        keywords ("[^a-zA-Z0-9_]".r, keywordStrings)

    lazy val identifier =
        not (keyword) ~> identifierBase

    lazy val identifierBase =
        regex ("[a-zA-Z][a-zA-Z0-9_]*".r) |
        failure ("identifier expected")

    lazy val whitespaceParser : PackratParser[Any] =
        rep ( """\s""".r | comment)

    lazy val comment : PackratParser[Any] =
        "{-" ~ rep (not ("-}") ~ (comment | any)) ~ "-}" |
        "--.*(\n|\\z)".r

}
