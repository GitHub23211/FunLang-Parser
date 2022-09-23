/**
 * FunLang syntax analysis tests.
 *
 * Copyright 2022, Anthony Sloane, Matthew Roberts, Kym Haines, Macquarie University, All rights reserved.
 */

package funlang

import org.bitbucket.inkytonik.kiama.util.ParseTests
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Tests that check that the syntax analyser works correctly.  I.e., it accepts
 * correct input and produces the appropriate trees, and it rejects illegal input.
 */
@RunWith(classOf[JUnitRunner])
class SyntaxAnalysisTests extends ParseTests {

    import FunLangTree._

    val parsers = new SyntaxAnalysis (positions)
    import parsers._

    // Tests of parsing basic expressions

    test ("SIMPLE: equal expression") {
        exp ("a == 1") should parseTo[FunLangNode] (EqualExp (IdnUse ("a"), IntExp (1)))
    }

    test ("less than expression") {
        exp ("a < 1") should parseTo[FunLangNode] (LessExp (IdnUse ("a"), IntExp (1)))
    }

    test ("addition expression") {
        exp ("a + 1") should parseTo[FunLangNode] (PlusExp (IdnUse ("a"), IntExp (1)))
    }

    test ("subtraction expression") {
        exp ("a - 1") should parseTo[FunLangNode] (MinusExp (IdnUse ("a"), IntExp (1)))
    }

    test ("multiplication expression") {
        exp ("a * 1") should parseTo[FunLangNode] (StarExp (IdnUse ("a"), IntExp (1)))
    }

    test ("division expression") {
        exp ("a / 1") should parseTo[FunLangNode] (SlashExp (IdnUse ("a"), IntExp (1)))
    }

    test ("integer expression") {
        exp ("823") should parseTo[FunLangNode] (IntExp (823))
    }

    test ("true expression") {
        exp ("true") should parseTo[FunLangNode] (BoolExp (true))
    }

    test ("false expression") {
        exp ("false") should parseTo[FunLangNode] (BoolExp (false))
    }

    test ("identifier expression") {
        exp ("v123") should parseTo[FunLangNode] (IdnUse ("v123"))
    }

    test ("BRACKETS: parenthesized expression") {
        exp ("(a + 5)") should parseTo[FunLangNode] (PlusExp (IdnUse ("a"), IntExp (5)))
    }

    test ("APPLICATION EXPRESSION: application expression 1") {
        exp ("a(b)") should parseTo[FunLangNode] (AppExp (IdnUse ("a"), IdnUse ("b")))
    }

    test ("expression containing an application expression") {
        exp ("1 + foo(2)") should parseTo[FunLangNode] (PlusExp(IntExp(1), AppExp (IdnUse ("foo"), IntExp (2))))
    }

    test ("IF: if expression") {
        exp ("if (true) 3 else 4") should parseTo[FunLangNode] (IfExp (BoolExp (true), IntExp (3), IntExp (4)))
    }

    test ("LAMBDA: lambda expression") {
        exp ("(a : Int) => a + 1") should parseTo[Exp] (LamExp(
                            IdnDef("a", IntType()),
                            PlusExp(IdnUse("a"), IntExp(1))))
    }

    test ("BASIC TYPES: basic type Bool") {
        tipe ("Bool") should parseTo[Type] (BoolType())
    }

    test ("COMPOSITE TYPES: parsing list type") {
        tipe ("List[Int]") should parseTo[Type] (ListType(IntType()))
    }

    test ("parsing tuple type") {
        tipe ("(Int,Bool,List[Bool])") should parseTo[Type] (TupleType(Vector(IntType(), BoolType(), ListType(BoolType()))))
    }

    test ("parsing function type") {
      tipe ("Int=>(Bool=>List[Int])") should parseTo[Type] (FunType(IntType(), FunType(BoolType(), ListType(IntType()))))
    }

    test ("LIST EXPRESSIONS: empty list") {
        exp ("List()") should parseTo[FunLangNode] (ListExp (Vector()))
    }

    test ("cons expression") {
        exp ("3 :: List()") should parseTo[FunLangNode] (ConsExp (IntExp (3), ListExp (Vector())))
    }

    test ("list expression") {
        exp ("List(3, 4, 5)") should parseTo[FunLangNode] (ListExp (Vector(IntExp(3), IntExp(4), IntExp(5))))
    }

    test ("TUPLE EXPRESSIONS: tuple expression") {
        exp ("(3, 4, 5)") should parseTo[FunLangNode] (TupleExp (Vector(IntExp(3), IntExp(4), IntExp(5))))
    }

    test ("BASIC PATTERNS: underscore pattern") {
        pat ("_") should parseTo[Pat] (AnyPat())
    }

    test ("literal pattern int") {
        pat ("3") should parseTo[Pat] (LiteralPat(IntExp(3)))
    }

    test ("COMPOSITE PATTERNS: list pattern") {
        pat ("List(3, _, 5)") should parseTo[Pat] (ListPat(Vector(LiteralPat(IntExp(3)), AnyPat(), LiteralPat(IntExp(5)))))
    }

    test ("cons pattern") {
        pat ("3 :: List()") should parseTo[Pat] (ConsPat(LiteralPat(IntExp(3)), ListPat(Vector())))
    }

    test ("tuple pattern") {
        pat ("(3, _, 5)") should parseTo[Pat] (TuplePat(Vector(LiteralPat(IntExp(3)), AnyPat(), LiteralPat(IntExp(5)))))
    }

    test ("DEFINITION: simple variable") {
        defn ("val x : Int = 100;") should parseTo[Defn] (Defn(IdnDef("x", IntType()), IntExp(100)))
    }

    test ("simple function") {
        defn ("def inc(x:Int):Int = x + 1;") should parseTo[Defn] (Defn (
                IdnDef ("inc", FunType (IntType (), IntType ())),
                LamExp (IdnDef ("x", IntType ()),
                        PlusExp (IdnUse ("x"), IntExp (1)))))
    }

    test ("MATCH: simple case") {
      caseline ("case 3 => 4") should parseTo[(Pat,Exp)] ((LiteralPat(IntExp(3)), IntExp(4)))
    }

    test ("simple case 2") {
      caseline ("case h::t => 1 + length(t)") should parseTo[(Pat,Exp)] ((ConsPat(IdentPat("h"), IdentPat("t")), PlusExp(IntExp(1), AppExp(IdnUse("length"), IdnUse("t")))))
    }

    test ("more complicated case line") {
      caseline ("case List(2, _, n) => n + 1") should parseTo[(Pat,Exp)] ((ListPat(Vector(LiteralPat(IntExp(2)), AnyPat(), IdentPat("n"))), PlusExp(IdnUse("n"), IntExp(1))))
    }

    test ("match with two lines") {
      matchterm ("""z match
               {
               case 0 => 1
               case n => n * fac (n - 1)
               }
            """) should parseTo[Exp] (MatchExp(IdnUse("z"),
                  Vector((LiteralPat(IntExp(0)), IntExp(1)),
                         (IdentPat("n"), StarExp(IdnUse("n"),
                                                 AppExp(IdnUse("fac"),
                                                        MinusExp(IdnUse("n"),
                                                               IntExp(1))))))))
    }

    test ("DEFINITIONS: one definition") {
      definitions ("""val x   : Int        = 100;
                """) should parseTo[Vector[Defn]] (Vector(Defn(
                            IdnDef("x", IntType()), IntExp(100))))
    }

    test ("one definition with lambda") {
      definitions ("""val inc : Int => Int = (a : Int) => a + 1;
                """) should parseTo[Vector[Defn]] (Vector(Defn(
                            IdnDef("inc", FunType(IntType(), IntType())),
                            LamExp(IdnDef("a", IntType()),
                                   PlusExp(IdnUse("a"), IntExp(1))))))
    }

    test ("two definitions") {
      definitions ("""val x   : Int        = 100;
                      val y   : Bool       = false;
                """) should parseTo[Vector[Defn]] (Vector(
                       Defn(IdnDef("x", IntType()), IntExp(100)),
                       Defn(IdnDef("y", BoolType()), BoolExp(false))))
    }

    test ("BLOCK: block with one definition") {
      program ("""{
                    val x   : Int        = 100;
                    inc(x)
                  }
                """) should parseTo[Program] (Program(BlockExp(
                    Vector(Defn(IdnDef("x", IntType()), IntExp(100))),
                    AppExp (IdnUse ("inc"), IdnUse ("x")))))
    }

    test ("block with two definitions") {
      program ("""{
                    val x   : Int        = 100;
                    val y   : Bool       = false;
                    inc(x)
                  }
                """) should parseTo[Program] (Program(BlockExp(
                    Vector(Defn( IdnDef("x", IntType()), IntExp(100)),
                           Defn(
                             IdnDef("y", BoolType()), BoolExp(false))),
                    AppExp (IdnUse ("inc"), IdnUse ("x")))))
    }

    test ("program with two definitions including lambda") {
      program ("""{
                    val x   : Int        = 100;
                    val inc : Int => Int = (a : Int) => a + 1;
                    inc(x)
                  }
                """) should parseTo[Program] (Program(BlockExp(
                    Vector(Defn(
                            IdnDef("x", IntType()), IntExp(100)),
                           Defn(
                            IdnDef("inc", FunType(IntType(), IntType())),
                            LamExp(IdnDef("a", IntType()),
                                   PlusExp(IdnUse("a"), IntExp(1))))),
                    AppExp (IdnUse ("inc"), IdnUse ("x")))))
    }

    test ("program with definitions including lambda and match fun") {
      program ("""{
                    val x   : Int        = 100;
                    val inc : Int => Int = (a : Int) => a + 1;
                    def length(s : List[Int]):Int = s match
                    {
                    case List() => 0
                    case h::t => 1 + length(t)
                    };
                    inc(x)
                  }
                """) should parseTo[Program] (Program(BlockExp(
                    Vector(Defn(
                            IdnDef("x", IntType()), IntExp(100)),
                           Defn(
                            IdnDef("inc", FunType(IntType(), IntType())),
                            LamExp(IdnDef("a", IntType()),
                                   PlusExp(IdnUse("a"), IntExp(1)))),
                           Defn(
                            IdnDef("length", FunType(ListType(IntType()),
                                                     IntType())),
                            LamExp(IdnDef("s",ListType(IntType())),
                                   MatchExp(IdnUse("s"),
                                     Vector((ListPat(Vector()), IntExp(0)),
                                            (ConsPat(IdentPat("h"),
                                                          IdentPat("t")),
                                             PlusExp(IntExp(1),
                                                   AppExp(IdnUse("length"),
                                                          IdnUse("t"))))))))),
                     AppExp (IdnUse ("inc"), IdnUse ("x")))))
    }

    // ================================================================
    //
    // FIXME: more tests here...

    test ("block statement with if statement") {
        program("""{
                    val x: Int = 5;
                    def sum(i:Int):Int = i + 3;
                    if (x + sum(4) < 10) 
                        true
                    else
                        false             
                }
            """) should parseTo[Program] (Program(BlockExp(
                    Vector(
                            Defn(IdnDef("x", IntType()), IntExp(5)), 
                            Defn(IdnDef("sum", FunType(IntType(), IntType())), 
                                LamExp(IdnDef("i", IntType()), PlusExp(IdnUse("i"), IntExp(3)))
                            )
                        ), 
                     IfExp(
                        LessExp(PlusExp(IdnUse("x"), AppExp(IdnUse("sum"), IntExp(4))), IntExp(10)), BoolExp(true), BoolExp(false)
                    )
                )))
    }

    test("associativity and precedence rules for basic operators (+ - / *)") {
        program("""{
                    val x: Int = 2;
                    val y: Int = 3;
                    val z: Int => Int = (i : Int) => i + 4 * 2;
                    x + y * z(4) + x + x / x - z(4) - y
                }
            """) should parseTo[Program] (Program(BlockExp(
                    Vector(
                        Defn(IdnDef("x", IntType()), IntExp(2)),
                        Defn(IdnDef("y", IntType()), IntExp(3)),
                        Defn(IdnDef("z", FunType(IntType(), IntType())), 
                            LamExp(
                                IdnDef("i", IntType()), 
                                PlusExp(
                                    IdnUse("i"), StarExp(IntExp(4), IntExp(2))
                                    )
                                )
                            )
                        ),
                        MinusExp(
                            MinusExp(
                                PlusExp(
                                    PlusExp(
                                        PlusExp(
                                            IdnUse("x"),
                                            StarExp(
                                                IdnUse("y"),
                                                AppExp(IdnUse("z"), IntExp(4))
                                            )
                                        ),
                                        IdnUse("x")
                                    ),
                                    SlashExp(IdnUse("x"), IdnUse("x"))
                                ),
                                AppExp(IdnUse("z"), IntExp(4))
                            ),
                            IdnUse("y")
                        )
                )))
    }

    test("associativity rules for non-associatives") {
        program("""{
                    val x: Int = 1;
                    val y: Int = 2;
                    val z: Int = 3;
                    x + y == z - x == z
                }
            """) should parseTo[Program] (Program(BlockExp(
                    Vector(
                        Defn(IdnDef("x", IntType()), IntExp(1)),
                        Defn(IdnDef("y", IntType()), IntExp(2)),
                        Defn(IdnDef("z", IntType()), IntExp(3)),
                        ),
                    EqualExp(PlusExp(IdnUse("x"), IdnUse("y")), MinusExp(IdnUse("z"), IdnUse("x")))
                )))
    }

    test("associativity rules for cons") {
    program("""{
                val arr : List[Int] = List(1, 2, 3);
                def pushOne(v:List[Int]):List[Int] = v :: 1;
                (1 :: arr) :: 2 :: pushOne(arr) :: 3
            }
        """) should parseTo[Program] (Program(BlockExp(
                Vector(
                    Defn(IdnDef("arr", ListType(IntType())), ListExp(Vector(IntExp(1), IntExp(2), IntExp(3)))),
                    Defn(IdnDef("pushOne", FunType(ListType(IntType()), ListType(IntType()))),
                            LamExp(IdnDef("v", ListType(IntType())), 
                                ConsExp(IdnUse("v"), IntExp(1))    
                            )
                        )
                    ),
                ConsExp(ConsExp(IntExp(1), IdnUse("arr")), ConsExp(IntExp(2), ConsExp(AppExp(IdnUse("pushOne"), IdnUse("arr")), IntExp(3))))
            )))
    }    

    test("Complex case and match") {
    program("""{
                val arr : List[Int] = List(1, 2, 3, 4);
                def matchCase(x:List[Int]):Int =
                (x, b) match {
                    case (List(y, _), 15) => x :: b
                    case (List(_), 15) => b
                    case _ => 0
                };
                matchCase(arr)
            }
        """) should parseTo[Program] (Program(BlockExp(
                Vector(
                    Defn(IdnDef("arr", ListType(IntType())), ListExp(Vector(IntExp(1), IntExp(2), IntExp(3), IntExp(4)))),
                    Defn(IdnDef("matchCase", FunType(ListType(IntType()), IntType())), 
                        LamExp(IdnDef("x", ListType(IntType())), 
                            MatchExp(TupleExp(Vector(IdnUse("x"), IdnUse("b"))),
                                Vector(
                                    (TuplePat(Vector(ListPat(Vector(IdentPat("y"), AnyPat())), LiteralPat(IntExp(15)))), ConsExp(IdnUse("x"), IdnUse("b"))),
                                    (TuplePat(Vector(ListPat(Vector(AnyPat())), LiteralPat(IntExp(15)))), IdnUse("b")),
                                    (AnyPat(), IntExp(0))
                                )
                            )
                        )
                    )
                ),
                AppExp(IdnUse("matchCase"), IdnUse("arr"))
            )))
    }

    test("Differentiate between tuples and brackets") {
    program("""{
                def func(x:Int):List[(Int, (Int, Int, Int))] = (x + 4 * (5 - 3)) :: List() :: (1, 2, (7 - 4 * (4 - 1)));
                func(10)
            }
        """) should parseTo[Program] (Program(BlockExp(
                Vector(
                    Defn(IdnDef("func", FunType(IntType(), ListType(TupleType(Vector(IntType(), TupleType(Vector(IntType(), IntType(), IntType()))))))), 
                        LamExp(IdnDef("x", IntType()),
                            ConsExp(
                                PlusExp(IdnUse("x"), StarExp(IntExp(4), MinusExp(IntExp(5), IntExp(3)))),
                                ConsExp(
                                    ListExp(Vector()),
                                    TupleExp(Vector(IntExp(1), IntExp(2), MinusExp(IntExp(7), StarExp(IntExp(4),MinusExp(IntExp(4), IntExp(1))))))
                                )
                            )   
                        )),
                    ),
                AppExp(IdnUse("func"), IntExp(10))
            )))
    } 

    test("Multiple block and bracketed statements") {
    program("""
            {
                val a : Int = 10;
                def fac(x:Int):Int = x match {
                    case 0 => 0
                    case a => a * fac(a - 1)
                    case _ => 0
                };
                func(a) * 2
            } 
            /
            (
                {
                    def square(x:Int):Int = x * x;
                    square(100)
                }
                +
                {
                    val a : Int = 1;
                    val b : Int = 23;
                    val c : Int = 764;
                    a * b + c
                }
            )
        """) should parseTo[Program] (Program(SlashExp(
            BlockExp(
                Vector(
                        Defn(IdnDef("a", IntType()), IntExp(10)),
                        Defn(IdnDef("fac", FunType(IntType(), IntType())), 
                            LamExp(IdnDef("x", IntType()),
                                MatchExp(IdnUse("x"), Vector(
                                        (LiteralPat(IntExp(0)), IntExp(0)),
                                        (IdentPat("a"), StarExp(IdnUse("a"), AppExp(IdnUse("fac"), MinusExp(IdnUse("a"), IntExp(1))))),
                                        (AnyPat(), IntExp(0))
                                    )
                                )
                            )
                        )
                    ),
                StarExp(AppExp(IdnUse("func"), IdnUse("a")), IntExp(2))
            ),
            PlusExp(
                BlockExp(
                    Vector(
                        Defn(IdnDef("square", FunType(IntType(), IntType())),
                            LamExp(IdnDef("x", IntType()), StarExp(IdnUse("x"), IdnUse("x")))
                        )
                    ),
                    AppExp(IdnUse("square"), IntExp(100))
                ),
                BlockExp(
                    Vector(
                        Defn(IdnDef("a", IntType()), IntExp(1)),
                        Defn(IdnDef("b", IntType()), IntExp(23)),
                        Defn(IdnDef("c", IntType()), IntExp(764)),
                    ),
                PlusExp(StarExp(IdnUse("a"), IdnUse("b")), IdnUse("c"))
                )
            )
            )))
    }

    test("Assign block statements to value") {
    program("""{
                val arr : List[Int] = List();
                val x: Int = {
                    val y : Int = 5;
                    def double(x:Int):Int = x * 2;
                    if (double(y) * double(y) < double(double(y)))
                        y
                    else
                        double(y)
                };
                def append(i:Int):List[Int] = i match {
                    case i => i :: arr
                    case _ => arr :: i
                };
                append(x)
            }
        """) should parseTo[Program] (Program(BlockExp(
                Vector(
                    Defn(IdnDef("arr", ListType(IntType())), ListExp(Vector())),
                    Defn(IdnDef("x", IntType()), BlockExp(
                        Vector(
                            Defn(IdnDef("y", IntType()), IntExp(5)),
                            Defn(IdnDef("double", FunType(IntType(), IntType())), LamExp(
                                IdnDef("x", IntType()), StarExp(IdnUse("x"), IntExp(2))
                            ))
                        ),
                        IfExp(LessExp(
                            StarExp(AppExp(IdnUse("double"), IdnUse("y")), AppExp(IdnUse("double"), IdnUse("y"))),
                            AppExp(IdnUse("double"), AppExp(IdnUse("double"), IdnUse("y")))
                        ),
                        IdnUse("y"),
                        AppExp(IdnUse("double"), IdnUse("y")))
                    )),
                    Defn(IdnDef("append", FunType(IntType(), ListType(IntType()))), LamExp(IdnDef("i", IntType()),
                        MatchExp(IdnUse("i"), Vector(
                            (IdentPat("i"), ConsExp(IdnUse("i"), IdnUse("arr"))),
                            (AnyPat(), ConsExp(IdnUse("arr"), IdnUse("i")))
                        )
                    )))
                ),
                AppExp(IdnUse("append"), IdnUse("x"))
            )))
    }         
}
