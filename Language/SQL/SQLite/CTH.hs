{-# LANGUAGE ForeignFunctionInterface, TemplateHaskell #-}
module Language.SQL.SQLite.CTH where

import Data.Char
import Data.List
import Language.Haskell.TH.Syntax


exportShowTokensForType :: Name -> Q [Dec]
exportShowTokensForType typeName = do
  return $ [ForeignD
            $ ExportF CCall
                      ("hsql_show_tokens_" ++ (haskellStyleToCStyle $ nameBase typeName))
                      (mkName $ "showTokens" ++ nameBase typeName)
                      (AppT (AppT ArrowT
                                  (AppT (ConT $ mkName "Ptr")
                                        (ConT $ mkName "()")))
                            (AppT (ConT $ mkName "IO")
                                  (AppT (ConT $ mkName "Ptr")
                                        (ConT $ mkName "()")))),
            FunD (mkName $ "showTokens" ++ nameBase typeName)
                 [Clause [VarP $ mkName "objectPointer"]
                         (NormalB
                          $ DoE [LetS [ValD (VarP $ mkName "objectStablePointer")
                                            (NormalB
                                             $ AppE (VarE $ mkName "castPtrToStablePtr")
                                                    (VarE $ mkName "objectPointer"))
                                            []],
                                 BindS (VarP $ mkName "object")
                                       (SigE (AppE (VarE $ mkName "deRefStablePtr")
                                                   (VarE $ mkName "objectStablePointer"))
                                             (AppT (ConT $ mkName "IO")
                                                   (ConT $ mkName $ nameBase typeName))),
                                 LetS [ValD (VarP $ mkName "tokens")
                                            (NormalB
                                             $ AppE (VarE $ mkName "showTokens")
                                                    (VarE $ mkName "object"))
                                            []],
                                 BindS (VarP $ mkName "tokensStablePointer")
                                       (AppE (VarE $ mkName "newStablePtr")
                                             (VarE $ mkName "tokens")),
                                 LetS [ValD (VarP $ mkName "tokensPointer")
                                            (NormalB
                                             $ AppE (VarE $ mkName "castStablePtrToPtr")
                                                    (VarE $ mkName "tokensStablePointer"))
                                            []],
                                 NoBindS (AppE (VarE 'return)
                                               (VarE $ mkName "tokensPointer"))])
                         []]]
{-
foreign export ccall "hsql_show_tokens_..." showTokens... :: Ptr () -> IO (Ptr ())
showTokens... objectPointer = do
  let objectStablePointer = castPtrToStablePtr objectPointer
  object <- deRefStablePtr objectStablePointer :: IO ...
  let tokens = showTokens object
  tokensStablePointer <- newStablePtr tokens
  let tokensPointer = castStablePtrToPtr tokensStablePointer
  return tokensPointer
-}


haskellStyleToCStyle :: String -> String
haskellStyleToCStyle haskellStyle =
    let maybeNextWordStart :: String -> Maybe Int
        maybeNextWordStart string = findIndex isUpper string
        downcaseWord :: String -> String
        downcaseWord word = map toLower word
        downcaseFirstCharacter :: String -> String
        downcaseFirstCharacter string = (toLower $ head string) : tail string
        splitIntoWords :: String -> [String]
        splitIntoWords string = case maybeNextWordStart string of
                                  Nothing -> [string]
                                  Just index -> let (word, rest) = splitAt index string
                                                in (word : (splitIntoWords
                                                              $ downcaseFirstCharacter
                                                                  rest))
        words = map downcaseWord $ splitIntoWords $ downcaseFirstCharacter haskellStyle
        result = intercalate "_" words
    in result