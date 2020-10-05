module Test exposing (..)

--import Eexl.Context as Context exposing (Context)
import Eexl.Context as Context exposing (..)
import Eexl.Eexl exposing (evaluateBool, evaluateInt)
import Eexl.Parse exposing (parse)
import Array


r11 = parse Context.empty " 1 + 2"

r12 = parse Context.empty "true"          --True
r13 = parse Context.empty "100 * (3 + 2)" --500
r14 = parse Context.empty "100 *  3 + 2 " --302
--r15 = parse Context.empty "(1 + 2) < 4" --True   NG
r16 = parse Context.empty " 1 + 2  < 4"   --True




-- multi line

formula1 = """ 1
+ 
2
"""

r21 = parse Context.empty formula1      --3

r31 = parse (Context.empty |> Context.addConstant "x" 9) "1 + x"      --10

{--
add : String -> Int
add a  =
   let
     a_ = String.toInt a  |> Maybe.withDefault 0 
   in
   a_ 


r41 = parse (Context.empty |> Context.addFunction "add" add ) "add(\"9\")"      --9
--}


{--
add :(Array.Array String) -> Int
add ar  =
   let
     a_ = String.toInt (Array.get 0 ar  |> Maybe.withDefault "0") |> Maybe.withDefault 0 
     b_ = String.toInt (Array.get 1 ar  |> Maybe.withDefault "0") |> Maybe.withDefault 0
   in
    a_ + b_


r41 = parse (Context.empty |> Context.addFunction "add" add ) "add(\"9\",\"2\")"      --11


formula2 = """ 
    add("222","111")

"""
r42 = parse (Context.empty |> Context.addFunction "add" add ) formula2     --9
--}

add :(Context ArrayString) -> Int
add ar  =
   let
     a_ = String.toInt (Array.get 0 ar  |> 
     b_ = String.toInt (Array.get 1 ar  |> 
   in
    a_ + b_


r41 = parse (Context.empty |> Context.addFu


formula2 = """ 
    add("222","111")

"""
r42 = parse (Context.empty |> Context.addFu
