{-# LANGUAGE GADTs #-}
module Language.SQL.SQLite.Tools (
                                  changeTableSchema
                                 )
    where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

import Language.SQL.SQLite.Syntax
import Language.SQL.SQLite.Types

changeTableSchema
    :: (Statement L0 NT NS)
    -> (Statement L0 NT NS)
    -> (Map UnqualifiedIdentifier Expression)
    -> StatementList
changeTableSchema oldDefinition newDefinition columnDefaultMap
    = let tableColumnNames :: (Statement L0 NT NS) -> Set UnqualifiedIdentifier
          tableColumnNames (CreateTable _ _ _ (ColumnsAndConstraints columns _))
              = Set.fromList $ map columnDefinitionName $ fromOneOrMore columns
          columnDefinitionName (ColumnDefinition name _ _) = name
          oldColumns = tableColumnNames oldDefinition
          newColumns = tableColumnNames newDefinition
          removedColumns = Set.difference oldColumns newColumns
          addedColumns = Set.difference newColumns oldColumns
          preservedColumns = Set.intersection oldColumns newColumns
          tableName :: (Statement L0 NT NS) -> SinglyQualifiedIdentifier
          tableName (CreateTable _ _ name _) = name
          oldName = tableName oldDefinition
          newName = tableName newDefinition
          temporaryName = SinglyQualifiedIdentifier Nothing
                          $ "temporary_" ++ (identifierProperName oldName)
                            ++ "_redefined_as_" ++ (identifierProperName newName)
          columnDefault column = maybe ExpressionLiteralNull
                                       id
                                       $ Map.lookup column columnDefaultMap
          beginTransactionStatement = Begin NoTransactionType True
          createTemporaryStatement
              = CreateTable
                  Temporary
                  NoIfNotExists
                  temporaryName
                  (AsSelect
                   $ Select (SelectCore NoDistinctness
                                        (fromJust $ mkOneOrMore [Star])
                                        (Just
                                         $ From
                                         $ JoinSource (TableSource oldName
                                                                   NoAs
                                                                   NoIndexedBy)
                                                      [])
                                        Nothing
                                        Nothing)
                            [] Nothing Nothing)
          dropStatement = DropTable NoIfExists oldName
          recreateStatement = newDefinition
          reinsertStatement
              = Insert InsertNoAlternative
                       newName
                       $ InsertSelect
                             (concat [Set.toList preservedColumns,
                                      Set.toList addedColumns])
                             (Select (SelectCore NoDistinctness
                                                 (fromJust
                                                  $ mkOneOrMore
                                                  $ map (\result -> Result result NoAs)
                                                  $ concat
                                                    [map (ExpressionIdentifier
                                                          . toDoublyQualifiedIdentifier)
                                                     $ Set.toList preservedColumns,
                                                     map columnDefault
                                                         $ Set.toList addedColumns])
                                                 (Just
                                                  $ From
                                                  $ JoinSource
                                                    (TableSource temporaryName
                                                                 NoAs
                                                                 NoIndexedBy)
                                                    [])
                                                 Nothing
                                                 Nothing)
                                     [] Nothing Nothing)
          dropTemporaryStatement = DropTable NoIfExists temporaryName
          commitTransactionStatement = Commit False True
          statementList = StatementList [Statement beginTransactionStatement,
                                         Statement createTemporaryStatement,
                                         Statement dropStatement,
                                         Statement recreateStatement,
                                         Statement reinsertStatement,
                                         Statement dropTemporaryStatement,
                                         Statement commitTransactionStatement]
      in statementList
