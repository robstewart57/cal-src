
module Main where

import qualified PrintCAL as C
import qualified AbsCAL as C

main :: IO ()
main = do
    let actorCode = C.printTree actor
    writeFile "my_actor.cal" actorCode

actor :: C.Actor
actor = actorAST
    where
      actorAST      = C.Actr (C.PathN [C.PNameCons (C.Ident "cal")]) imports (C.Ident "my_actor") actorPar ioSig varDecl [action] priorityBlock
      priorityBlock = []
      actorPar      = []
      imports       = []
      varDecl       = [ C.VDecl (intCalType 8)  (C.Ident "i") [] ]
      ioSig         = C.IOSg [C.PortDcl (intCalType 8) (C.Ident "In")] [C.PortDcl (intCalType 8) (C.Ident "Out")]
      inputPattern  = [ C.InPattTagIds (C.Ident "In") [(C.Ident "x")] ]
      outputPattern = [ C.OutPattTagIds (C.Ident "Out") [(C.Ident "i")] ]
      actionHead    = C.ActnHead inputPattern outputPattern
      action        = C.AnActn (C.ActnTagsStmts (C.ActnTagDecl [C.Ident "the_action"]) actionHead stmts)
      stmts         = [ C.SemiColonSeparatedStmt (C.AssignStt (C.AssStmt (C.Ident "i") (C.BEAdd (C.EIdent (C.Ident "i")) (C.EIdent (C.Ident "x"))))) ]
      intCalType i  = C.TypParam C.TUint [C.TypeAttrSizeDf (C.LitExpCons (C.IntLitExpr (C.IntegerLit i)))]
