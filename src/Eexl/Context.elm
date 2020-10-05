--module Eexl.Context exposing
--    ( Context
--    , Input
--    , empty
--    , addConstant, addFunction
--    , getConstant, getFunction
--    )

module Eexl.Context exposing (..)

{-| This module manages the context that an expression runs with. Specifically it is used to assign
values to constants, and set functions that can be called from the expression.

    context : Context
    context =
        Context.empty
            |> Context.addConstant "x" 5
            |> Context.addFunction "stringToInt" stringToInt


    Eexl.evaluateInt context """x + 5 + stringToInt("7")""" -- = Ok 17


# Definition

@docs Context


# Creation

@docs empty


# Adding constants and functions

@docs addConstant, addFunction


# Reading constants and function

@docs getConstant, getFunction

-}

import Dict exposing (Dict)
import Array

type Input                                                                  
    = ArrayString (Array.Array String)  

{-| This is the type of `Context` that is passed into the functions that evaluate expressions.
-}
type Context
    = Context
        { constants : Dict String Int
        --, functions : Dict String (String -> Int)
        --, functions : Dict String ((Array.Array String) -> Int)
        , functions : Dict String (Input -> Int)
        }


{-| An empty context containing no functions or constants.
-}
empty : Context
empty =
    Context
        { constants = Dict.empty
        , functions = Dict.empty
        }


{-| Add a constant to the context.
-}
addConstant : String -> Int -> Context -> Context
addConstant name value (Context context) =
    Context
        { context
            | constants = context.constants |> Dict.insert name value
        }


{-| Add a function to the context.
-}
--addFunction : String -> (String -> Int) -> Context -> Context
--addFunction : String -> ((Array.Array String) -> Int) -> Context -> Context
addFunction : String -> (Input -> Int) -> Context -> Context
addFunction name f (Context context) =
    Context
        { context
            | functions = context.functions |> Dict.insert name f
        }


{-| Retrieve a constant from the context (not usually used).
-}
getConstant : String -> Context -> Maybe Int
getConstant name (Context { constants }) =
    Dict.get name constants


{-| Retrieve a constant from the context (not usually used).
-}
--getFunction : String -> Context -> Maybe (String -> Int)
--getFunction : String -> Context -> Maybe ((Array.Array String) -> Int)
getFunction : String -> Context -> Maybe (Input -> Int)
getFunction name (Context { functions }) =
    Dict.get name functions
