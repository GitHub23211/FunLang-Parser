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
        factor ~ ("match" ~> "{" ~> rep1 (caseline) <~ "}") ^^ {case e ~ c => MatchExp(e, c)} |
        exp

    // matches an individual 
    lazy val caseline : PackratParser[(Pat,Exp)] =
        // FIXME
        ("case" ~> pat) ~ ("=>" ~> exp) ^^ {case p ~ e => (p, e)}

    // FIXME   add parsers between factor and exp

    lazy val exp6 : PackratParser[Exp] =
        // FIXME
        ("{" ~> definitions) ~ (exp6 <~ "}") ^^ {case d ~ e => BlockExp(d, e)} |
        "(" ~> repsep (factor, ",") <~ ")" ^^ {case e => TupleExp(e)} |
        "List" ~> "(" ~> repsep (exp6, ",") <~ ")" ^^ {case e => ListExp(e)} |
        ("(" ~> idndef <~ ")") ~ ("=>" ~> exp6) ^^ {case arg ~ b => LamExp(arg, b)} | 
        exp6 ~ ("(" ~> exp6 <~ ")") ^^ {case fn ~ arg => AppExp(fn, arg)} |
        factor 
    
    lazy val exp5 : PackratParser[Exp] =
        exp5 ~ ("*" ~> exp5) ^^ {case l ~ r => StarExp(l, r)} |
        exp5 ~ ("/" ~> exp5) ^^ {case l ~ r => SlashExp(l, r)} |
        exp6

    lazy val exp4 : PackratParser[Exp] =
        exp4 ~ ("+" ~> exp4) ^^ {case l ~ r => PlusExp(l, r)} |
        exp4 ~ ("-" ~> exp4) ^^ {case l ~ r => MinusExp(l, r)} |
        exp5

    lazy val exp3 : PackratParser[Exp] =
        exp3 ~ ("::" ~> exp3) ^^ {case l ~ r => ConsExp(l, r)} |
        exp4

    lazy val exp2 : PackratParser[Exp] =
        exp2 ~ ("<" ~> exp2) ^^ {case l ~ r => LessExp(l, r)} |
        exp2 ~ ("==" ~> exp2) ^^ {case l ~ r => EqualExp(l, r)} |
        exp3

    lazy val exp1 : PackratParser[Exp] =
        matchterm |
        exp1

    lazy val exp : PackratParser[Exp] =
        (keyword ~> "(" ~> exp <~ ")") ~ (exp <~ keyword) ~ exp ^^ {case con ~ thenExp ~ elseExp => IfExp(con, thenExp, elseExp)} | 
        exp1

    lazy val definitions : PackratParser[Vector[Defn]] =
        // FIXME
        rep1sep(defn, ";") <~ ";"

    lazy val defn : PackratParser[Defn] =
        // FIXME
        ("val" ~> idndef) ~ ("=" ~> exp) ^^ {case i ~ e => Defn(i, e)} |
        ("def" ~> identifier) ~ ("(" ~> identifier <~ ":") ~ (tipe <~ ")") ~ (":" ~> tipe) ~ ("=" ~> exp) ^^ {case i1 ~ i2 ~ t1 ~ t2 ~ e => Defn(IdnDef(i1, FunType(t1, t2)), 
        LamExp(IdnDef(i2, t1), e))}
        

    lazy val pat : PackratParser[Pat] =
        // FIXME
        "List" ~> "(" ~> repsep (pat, ",") <~ ")" ^^ {case p => ListPat(p)} | 
        "(" ~> rep1sep (pat, ",") <~ ")" ^^ {case p => TuplePat(p)} | 
        pat ~ ("::" ~> pat) ^^ {case p1 ~ p2 => ConsPat(p1, p2)} |
        "(" ~> pat <~ ")" |
        basicpat

    lazy val basicpat : PackratParser[Pat] =
        // FIXME
        literal ^^ LiteralPat | 
        identifier ^^ IdentPat |
        "_" ^^^ AnyPat()

    lazy val tipe : PackratParser[Type] =
        // FIXME
        "(" ~> tipe <~ ")" |
        tipe ~ ("=>" ~> tipe) ^^ {case to ~ from => FunType(to, from)} |
        "(" ~> rep1sep (tipe, ",") <~ ")" ^^ {case v => TupleType(v)} |
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
