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
        factor ~ ("+" ~> factor) ^^ {case f1 ~ f2 => PlusExp(f1, f2)} |
        factor ~ ("-" ~> factor) ^^ {case f1 ~ f2 => MinusExp(f1, f2)} |
        factor ~ ("*" ~> factor) ^^ {case f1 ~ f2 => StarExp(f1, f2)} |
        factor ~ ("/" ~> factor) ^^ {case f1 ~ f2 => SlashExp(f1, f2)} |
        factor ~ ("==" ~> factor) ^^ {case f1 ~ f2 => EqualExp(f1, f2)} |
        factor ~ ("<" ~> factor) ^^ {case f1 ~ f2 => LessExp(f1, f2)} |
        factor ~ ("(" ~> factor <~ ")") ^^ {case f1 ~ f2 => AppExp(f1, f2)} |
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
        basictipe

    lazy val basictipe : PackratParser[Type] =
        // FIXME
        "Int" ^^^ IntType ()

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
