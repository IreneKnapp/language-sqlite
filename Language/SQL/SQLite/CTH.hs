{-# LANGUAGE ForeignFunctionInterface, TemplateHaskell, Rank2Types #-}
module Language.SQL.SQLite.CTH (
                                exportType,
                                exportShowTokensForType,
                                showTokensGeneric,
                                getGeneric
                               )
    where

import Data.Char
import Data.List
import Foreign
import Language.Haskell.Semiquotation
import Language.Haskell.TH.Syntax
import qualified Language.SQL.SQLite as SQL


haskellStyleToCStyle :: String -> String
haskellStyleToCStyle haskellStyle =
    let maybeNextWordStart :: String -> Maybe Int
        maybeNextWordStart string = findIndex isUpper string
        downcaseWord :: String -> String
        downcaseWord word = map toLower word
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


cStyleToHaskellStyle :: String -> String
cStyleToHaskellStyle cStyle = cStyle


downcaseFirstCharacter :: String -> String
downcaseFirstCharacter string = (toLower $ head string) : tail string


upcaseFirstCharacter :: String -> String
upcaseFirstCharacter string = (toUpper $ head string) : tail string


declarationForForeignExportWithAllPointers :: String -> Name -> Int -> Dec
declarationForForeignExportWithAllPointers cName haskellName arity
    = let typeForArity 0 = AppT (ConT $ mkName "IO")
                                (AppT (ConT $ mkName "Ptr")
                                      (ConT $ mkName "()"))
          typeForArity n = AppT (AppT ArrowT
                                      (AppT (ConT $ mkName "Ptr")
                                            (ConT $ mkName "()")))
                                $ typeForArity $ n - 1
      in ForeignD $ ExportF CCall cName haskellName $ typeForArity arity
{-
foreign export ccall "cName" haskellName :: Ptr () -> ... -> IO (Ptr ())
Where Ptr () is repeated arity times.
-}


simpleLetStatement :: String -> Exp -> Stmt
simpleLetStatement boundVariable body
    = LetS [ValD (VarP $ mkName boundVariable)
                 (NormalB body)
                 []]
{-
let boundVariable = body
In do-notation.
-}


simpleBindStatement :: String -> Exp -> Stmt
simpleBindStatement boundVariable body
    = BindS (VarP $ mkName boundVariable)
            body
{-
boundVariable <- body
In do-notation.
-}


constructionExpression :: Name -> [Exp] -> Exp
constructionExpression name parameters =
    let applyToParameters [] = ConE name
        applyToParameters (parameter:rest) = AppE (applyToParameters rest)
                                                  parameter
    in applyToParameters $ reverse parameters
{-
Name a1 ...
-}


exportConstructor :: Name -> String -> Name -> [(Name, Type)] -> Q [Dec]
exportConstructor typeName constructorCName constructorHaskellName parameters = do
  let arity = length parameters
      parameterNames = map fst parameters
      haskellName = (mkName $ "new" ++ (upcaseFirstCharacter
                                        $ nameBase constructorHaskellName))
  return $ [declarationForForeignExportWithAllPointers
              ("hsql_new_" ++ constructorCName)
              haskellName
              arity,
            FunD haskellName
                 [Clause
                  (map VarP parameterNames)
                  (NormalB
                   $ DoE
                   $ concat
                     [(map (\(parameterName, parameterType) ->
                              simpleBindStatement
                                (nameBase parameterName)
                                (AppE (AppE (VarE $ mkName "getGeneric")
                                            (SigE (VarE $ mkName "undefined")
                                                  parameterType))
                                      (VarE parameterName)))
                           parameters),
                      [NoBindS
                       $ InfixE (Just
                                 (AppE (VarE $ mkName "newStablePtr")
                                       (constructionExpression
                                          constructorHaskellName
                                          $ map VarE parameterNames)))
                                (VarE $ mkName ">>=")
                                (Just
                                 (InfixE (Just (VarE $ mkName "return"))
                                         (VarE $ mkName ".")
                                         (Just
                                          (VarE $ mkName "castStablePtrToPtr"))))]])
                 []]]
{-
foreign export ccall "hsql_new_type"
  newType :: Ptr () -> Ptr () -> IO (Ptr ())
newType :: Ptr () -> Ptr () -> IO (Ptr ())
newType a1 ... = do
  a1 <- getGeneric (unused :: UnqualifiedIdentifier) a1
  ... -- Repetitions of the above line for each parameter of the data constructor
  newStablePointer (Type a' stuff') >>= return . castStablePtrToPtr
-}


exportAccessor :: String -> Name -> String -> Type -> Int -> Int -> Q [Dec]
exportAccessor constructorCName constructorHaskellName
               parameterCName parameterType parameterIndex
               arity
    = return []


exportType :: Name -> [(String, Name, [(String, Q Type)])] -> Q [Dec]
exportType typeName definitions = do
  mapM (\(constructorCName, constructorHaskellName, parameterDefinitions) -> do
          let parameterCNames = map fst parameterDefinitions
              parameterHaskellNames = map (mkName . cStyleToHaskellStyle) parameterCNames
              arity = length parameterDefinitions
          parameterTypes <- mapM id $ map snd parameterDefinitions
          constructorFunctionDefinitions
              <- exportConstructor typeName constructorCName constructorHaskellName
                                   $ zip parameterHaskellNames parameterTypes
          accessorFunctionDefinitions
              <- mapM (\(parameterIndex, parameterCName, parameterType) -> do
                         exportAccessor constructorCName
                                        constructorHaskellName
                                        parameterCName
                                        parameterType
                                        parameterIndex
                                        arity)
                      (zip3 [0..] parameterCNames parameterTypes) >>= return . concat
          return $ concat [constructorFunctionDefinitions,
                           accessorFunctionDefinitions])
       definitions >>= return . concat
{-
As exportConstructor and exportAccessor, for every given constructor and accessor.
-}


exportShowTokensForType :: Name -> Q [Dec]
exportShowTokensForType typeName = do
  return $ [declarationForForeignExportWithAllPointers
              ("hsql_show_tokens_" ++ (haskellStyleToCStyle $ nameBase typeName))
              (mkName $ "showTokens" ++ nameBase typeName)
              1,
            FunD (mkName $ "showTokens" ++ nameBase typeName)
                 [Clause [VarP $ mkName "objectPointer"]
                         (NormalB
                          $ (AppE (AppE (VarE $ mkName "showTokensGeneric")
                                        (SigE (VarE $ mkName "undefined")
                                              (ConT $ typeName)))
                                  (VarE $ mkName "objectPointer")))
                         []]]
{-
foreign export ccall "hsql_show_tokens_..." showTokens... :: Ptr () -> IO (Ptr ())
showTokens... objectPointer = showTokensGeneric (undefined :: ...) objectPointer
-}


showTokensGeneric :: (SQL.ShowTokens a) => a -> Ptr () -> IO (Ptr ())
showTokensGeneric unused objectPointer = do
  object <- asTypeOf (return unused) $ deRefStablePtr $ castPtrToStablePtr objectPointer
  tokensStablePointer <- newStablePtr $ SQL.showTokens object
  return $ castStablePtrToPtr tokensStablePointer


getGeneric :: forall a . a -> Ptr () -> IO a
getGeneric unused objectPointer = do
  asTypeOf (return unused) $ deRefStablePtr $ castPtrToStablePtr objectPointer
