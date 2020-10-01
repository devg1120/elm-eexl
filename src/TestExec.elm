module TestExec exposing (..)


import Parser exposing (..)
import Char
import Set

{--
 run : Parser a -> String -> Result (List DeadEnd) a

--}


---------------------------------------------------------------------
type alias Name =
    String

type Statement
    = Var Name
    | If Statement (List Statement) (List Statement)
    | While Statement (List Statement) 
    | For Statement Statement (List Statement) 
    | Assign Statement Statement
    | Blank

script : Parser (List Statement)
script =
   statements
statements : Parser (List Statement)
statements =
    loop [] statementsHelp
     
spaces : Parser ()
spaces =
  chompWhile (\c -> c == ' ' || c == '\n' || c == '\r')


spsWorkAround : Parser ()
spsWorkAround =
   loop 0 <| ifProgress <|
   oneOf
   [ lineCommentWorkAround "//"
   , multiComment "/*" "*/" Nestable
   , spaces
   ]

lineCommentWorkAround : String -> Parser ()
lineCommentWorkAround start =
    succeed () |. symbol start |. chompWhile (\c -> c /= '\n')

ifProgress : Parser a -> Int -> Parser (Step Int ())
ifProgress p offset =
  succeed identity
    |. p
    |= getOffset
    |> map (\newOffset -> if offset == newOffset then Done () else Loop newOffset)

statementsHelp : List Statement -> Parser (Step (List Statement) (List Statement))
statementsHelp revStmts =
    oneOf
      [ succeed (\stmt -> Loop (stmt :: revStmts))
          |. spsWorkAround
          |= statement
          |. spsWorkAround
          |. spaces
--          |. symbol ";"
--          |. spaces
      , succeed ()
          |> map (\_ -> Done (List.reverse revStmts))
          --|> map (\_ -> Done ("--" ++ str ++ "--"))
      ]


statementsHelp2 : List Statement -> Parser (Step (List Statement) (List Statement))
statementsHelp2 revStmts =
    oneOf
      [ succeed (\stmt -> Loop (stmt :: revStmts))
          |= statement
          |. spaces
--          |. symbol ";"
--          |. spaces
      , succeed ()
          |> map (\_ -> Done (List.reverse revStmts))
          --|> map (\_ -> Done ("--" ++ str ++ "--"))
      ]

typeName : Parser Statement
typeName =
     succeed Var
        |= variable
           { start = Char.isLower
           , inner = \c -> Char.isAlphaNum c || c == '_'
           , reserved = Set.fromList [ "if", "then", "else","end", "while", "do","in", "for"  ]
           }



typeVar : Parser Statement
typeVar =
     succeed Var
        |= variable
          { start = Char.isLower
          , inner = \c -> Char.isAlphaNum c || c == '_'
          , reserved = Set.fromList [ "let", "in", "case", "of" ]
          }


statement : Parser Statement
statement =
   oneOf
      [ assignStatement
      , ifStatement
      , whileStatement
      , forStatement
      ]


assignStatement : Parser Statement 
assignStatement =
  succeed Assign
    |. spaces
    |= typeName
    |. spaces
    |. symbol "="
    |. spaces
    |= typeVar
    |. spaces

ifStatement : Parser Statement 
ifStatement =
  succeed  If
    |. spaces
    |. keyword "if"
    |. spaces
    |= lazy (\_ -> typeVar)
    |. spaces
    |. keyword "then"
    |. spaces
    |= lazy (\_ -> statements)
    |. spaces
    |. keyword "else"
    |. spaces
    |= lazy (\_ -> statements)
    |. spaces
    |. keyword "end"
    |. spaces

whileStatement : Parser Statement 
whileStatement =
  succeed  While
    |. spaces
    |. keyword "while"
    |. spaces
    |= lazy (\_ -> typeVar)
    |. spaces
    |. symbol "do"
    |. spaces
    |= lazy (\_ -> statements)
    |. spaces
    |. symbol "end"
    |. spaces


forStatement : Parser Statement 
forStatement =
  succeed  For
    |. spaces
    |. keyword "for"
    |. spaces
    |= lazy (\_ -> typeVar)
    |. spaces
    |. keyword "in"
    |. spaces
    |= lazy (\_ -> typeVar)
    |. spaces
    |. keyword "do"
    |. spaces
    |= lazy (\_ -> statements)
    |. spaces
    |. keyword "end"
    |. spaces

input = """
   Aaa,a12;
Baa,b12;
Baa,b12;
if ccc then ddd else eee;
"""
input2 = """
   Aaa,a12;
   Baa,b12;
   Baa,b12;

   if ccc then
      Ccc,ddd;
      Ccc,ddd;

   else 
      Ddd,eee;
      Ddd,eee;
   end;
   
   while test1 do
   
      Ddd,a;
      Ddd,a;
   end;

   for test in range do
   
      Str, zzzz;
      Ddd,a;
      Ddd,a;
   end;

   for test in range do
      Ddd,a;
      Ddd,a;
      if ccc then
         Ccc,ddd;
         Ccc,ddd;

      else 
         Ddd,eee;
         Ddd,eee;
      end;
   end;
"""

-- run : Result (List Parser.DeadEnd) (List Statement)
-- r005 = run script input2

input3 = """
  aaa = a11

/*
   baa = b12
   baa = b13


   if ccc then
      ccc = ddd
      ccc = ddd

   else 
      ddd = eee
      ddd = eee
   end
   
   while test1 do
   
      ddd = a
      ddd = a
   end

   for test in range do
   
      str =  zzzz
      ddd = a
      ddd = a
   end

   for test in range do
      ddd = a
      ddd = a
      if ccc then
         ccc = ddd
         ccc = ddd

      else 
         ddd = eee
         ddd = eee
      end
   end
*/

"""
exec =
   run script input3

-- r006 = run script input3
