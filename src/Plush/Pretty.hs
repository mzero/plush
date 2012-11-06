{-
Copyright 2012 Google Inc. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Plush.Pretty ( pp ) where

import Plush.Types

import Data.List
import Text.PrettyPrint


pp :: CommandList -> String
pp = render . ppCommandList
  where
    ppCommandList = vcat . map ppCommandListElement
    ppCommandListElement (ao, e) = ppAndOr ao <+> ppExecution e

    ppExecution Sequential = semi
    ppExecution Background = char '&'

    ppAndOr = sep . map ppAndOrElem . zip (True:repeat False)
    ppAndOrElem (f, (c, (s, p))) =
        ppConnector f c <+> ppSense s <+> ppPipeline p

    ppConnector True  AndThen = empty
    ppConnector False AndThen = text "&&"
    ppConnector _     OrThen = text "||"

    ppSense Normal = empty
    ppSense Inverted = char '!'

    ppPipeline :: Pipeline -> Doc
    ppPipeline = sep . intersperse (char '|') . map ppCommand

    ppCommand (Simple cmd) = ppSimpleCommand cmd
    ppCommand (Compound cmd redirects) = hsep $
        ppCompoundCommand cmd : map ppRedirect redirects
    ppCommand (Function {}) = text "TODO(elaforge): function not implemented"

    ppCompoundCommand cmd = case cmd of
        BraceGroup cmds -> hsep [char '{', ppCommandList cmds, char '}']
        Subshell cmds -> hsep [char '(', ppCommandList cmds, char ')']
        ForClause name words cmds -> hsep $
            text "for" : ppName name : text "in"
            : map ppWord words
            ++ [text "do", ppCommandList cmds, text "done"]
        IfClause condition consequent alts -> hsep $
            text "if" : ppCommandList condition : text "then"
            : ppCommandList consequent : text "else"
            : map ppCommandList alts
        WhileClause condition cmds -> hsep
            [ text "while", ppCommandList condition
            , text "do", ppCommandList cmds, text "done"
            ]
        UntilClause condition cmds -> hsep
            [text "until", ppCommandList cmds
            , text "do", ppCommandList condition, text "done"
            ]

    ppSimpleCommand (SimpleCommand ws as rs) = hsep $
        map ppAssignment as ++ map ppWord ws ++ map ppRedirect rs

    ppName (Name loc str) =
        leadin <> text str <> leadmid <> ppLoc loc <> leadout
    ppWord w = leadin <> text (wordText w)
        <> leadmid <> ppLoc (location w) <> leadout
    leadin = text "\ESC[4;30;47m"  -- underline, black on white
    leadmid = text "\ESC[34m"      -- blue
    leadout = text "\ESC[0m"       -- reset

    ppLoc (Span a b) = text . ('<' :) . shows a . (',' :) . shows b $ ">"

    ppAssignment (Assignment name value) = brackets $
        text name <> char '=' <> ppWord value

    ppRedirect (Redirect mn t w) = brackets $
        maybe empty (text . show) mn <> text (redirOperator t) <> ppWord w

redirOperator :: RedirectType -> String
redirOperator t = case t of
    RedirInput              -> "<"
    RedirOutput             -> ">"
    RedirOutputClobber      -> ">|"
    RedirAppend             -> ">>"
    RedirHere               -> "<<"
    RedirHereStrip          -> "<<-"
    RedirDuplicateInput     -> "<&"
    RedirDuplicateOutput    -> ">&"
    RedirInputOutput        -> "<>"
