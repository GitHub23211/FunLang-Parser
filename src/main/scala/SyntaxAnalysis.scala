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
        "(" ~> repsep (factor, ",") <~ ")" ^^ TupleExp |
        "(" ~> exp <~ ")" |
        identifier ^^ IdnUse |
        literal |
        failure ("exp expected")

    lazy val matchterm : PackratParser[Exp] =
        factor ~ ("match" ~> "{" ~> rep1 (caseline) <~ "}") ^^ MatchExp

    // matches an individual 
    lazy val caseline : PackratParser[(Pat,Exp)] =
        ("case" ~> pat) ~ ("=>" ~> exp) ^^ {case p ~ e => (p, e)}

    lazy val exp6 : PackratParser[Exp] =
        ("{" ~> definitions) ~ (exp <~ "}") ^^ BlockExp |
        "List" ~> "(" ~> repsep (exp, ",") <~ ")" ^^ ListExp |
        ("(" ~> idndef <~ ")") ~ ("=>" ~> exp) ^^ LamExp | 
        factor ~ ("(" ~> exp <~ ")") ^^ AppExp |
        factor 
    
    lazy val exp5 : PackratParser[Exp] =
        exp5 ~ ("*" ~> exp6) ^^ StarExp |
        exp5 ~ ("/" ~> exp6) ^^ SlashExp |
        exp6

    lazy val exp4 : PackratParser[Exp] =
        exp4 ~ ("+" ~> exp5) ^^ PlusExp |
        exp4 ~ ("-" ~> exp5) ^^ MinusExp |
        exp5

    lazy val exp3 : PackratParser[Exp] =
        exp4 ~ ("::" ~> exp3) ^^ ConsExp |
        exp4

    lazy val exp2 : PackratParser[Exp] =
        exp3 ~ ("<" ~> exp3) ^^ LessExp |
        exp3 ~ ("==" ~> exp3) ^^ EqualExp |
        exp3

    lazy val exp1 : PackratParser[Exp] =
        matchterm |
        exp2

    lazy val exp : PackratParser[Exp] =
        (keyword ~> "(" ~> exp <~ ")") ~ (exp <~ keyword) ~ exp ^^ IfExp | 
        exp1

    lazy val definitions : PackratParser[Vector[Defn]] =
        (rep1sep(defn, ";")) <~ ";"

    lazy val defn : PackratParser[Defn] =
        ("val" ~> idndef) ~ ("=" ~> exp) ^^ Defn |
        ("def" ~> identifier) ~ ("(" ~> identifier <~ ":") ~ (tipe <~ ")") ~ (":" ~> tipe) ~ ("=" ~> exp) ^^ {case i1 ~ i2 ~ t1 ~ t2 ~ e => Defn(IdnDef(i1, FunType(t1, t2)), 
        LamExp(IdnDef(i2, t1), e))}
        

    lazy val pat : PackratParser[Pat] =
        "List" ~> "(" ~> repsep (pat, ",") <~ ")" ^^ ListPat | 
        "(" ~> rep1sep (pat, ",") <~ ")" ^^ TuplePat | 
        pat ~ ("::" ~> pat) ^^ ConsPat |
        "(" ~> pat <~ ")" |
        basicpat

    lazy val basicpat : PackratParser[Pat] =
        literal ^^ LiteralPat | 
        identifier ^^ IdentPat |
        "_" ^^^ AnyPat()

    lazy val tipe : PackratParser[Type] =
        "(" ~> tipe <~ ")" |
        tipe ~ ("=>" ~> tipe) ^^ FunType |
        "(" ~> rep1sep (tipe, ",") <~ ")" ^^ TupleType |
        "List" ~> "[" ~> tipe <~ "]" ^^ ListType |
        basictipe


    lazy val basictipe : PackratParser[Type] =
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
