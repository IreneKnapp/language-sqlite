{-# LANGUAGE GADTs, EmptyDataDecls, FlexibleInstances, ExistentialQuantification,
             StandaloneDeriving, TypeSynonymInstances #-}
module Language.SQL.SQLite.Types (
                                  ShowTokens(..),
                                  OneOrMore,
                                  mkOneOrMore,
                                  fromOneOrMore,
                                  NonnegativeDouble,
                                  mkNonnegativeDouble,
                                  fromNonnegativeDouble,
                                  Type(..),
                                  MaybeType(..),
                                  MaybeTypeSize(..),
                                  TypeSizeField(..),
                                  LikeType(..),
                                  Escape(..),
                                  MaybeSwitchExpression(..),
                                  CasePair(..),
                                  Else(..),
                                  Expression(..),
                                  MaybeUnique(..),
                                  MaybeIfNotExists(..),
                                  MaybeIfExists(..),
                                  MaybeForEachRow(..),
                                  Permanence(..),
                                  MaybeCollation(..),
                                  MaybeAscDesc(..),
                                  MaybeAutoincrement(..),
                                  MaybeSign(..),
                                  MaybeColumn(..),
                                  AlterTableBody(..),
                                  ColumnDefinition(..),
                                  DefaultValue(..),
                                  IndexedColumn(..),
                                  ColumnConstraint(..),
                                  TableConstraint(..),
                                  TriggerTime(..),
                                  TriggerCondition(..),
                                  ModuleArgument(..),
                                  QualifiedTableName(..),
                                  OrderingTerm(..),
                                  PragmaBody(..),
                                  PragmaValue(..),
                                  EitherColumnsAndConstraintsSelect(..),
                                  InsertHead(..),
                                  InsertBody(..),
                                  UpdateHead(..),
                                  Distinctness(..),
                                  MaybeHaving(..),
                                  MaybeAs(..),
                                  CompoundOperator(..),
                                  SelectCore(..),
                                  ResultColumn(..),
                                  JoinSource(..),
                                  SingleSource(..),
                                  JoinOperation(..),
                                  JoinConstraint(..),
                                  MaybeIndexedBy(..),
                                  FromClause(..),
                                  WhereClause(..),
                                  GroupClause(..),
                                  OrderClause(..),
                                  LimitClause(..),
                                  WhenClause(..),
                                  ConflictClause(..),
                                  ForeignKeyClause(..),
                                  ForeignKeyClauseActionOrMatchPart(..),
                                  ForeignKeyClauseActionPart(..),
                                  MaybeForeignKeyClauseDeferrablePart(..),
                                  MaybeInitialDeferralStatus(..),
                                  MaybeTransaction(..),
                                  MaybeTransactionType(..),
                                  MaybeDatabase(..),
                                  MaybeSavepoint(..),
                                  MaybeReleaseSavepoint(..),
                                  StatementList(..),
                                  AnyStatement(..),
                                  fromAnyStatement,
                                  ExplainableStatement(..),
                                  fromExplainableStatement,
                                  TriggerStatement(..),
                                  fromTriggerStatement,
                                  Statement(..),
                                  Explain,
                                  ExplainQueryPlan,
                                  AlterTable,
                                  Analyze,
                                  Attach,
                                  Begin,
                                  Commit,
                                  CreateIndex,
                                  CreateTable,
                                  CreateTrigger,
                                  CreateView,
                                  CreateVirtualTable,
                                  Delete,
                                  DeleteLimited,
                                  Detach,
                                  DropIndex,
                                  DropTable,
                                  DropTrigger,
                                  DropView,
                                  Insert,
                                  Pragma,
                                  Reindex,
                                  Release,
                                  Rollback,
                                  Savepoint,
                                  Select,
                                  Update,
                                  UpdateLimited,
                                  Vacuum,
                                  Identifier(..),
                                  toDoublyQualifiedIdentifier,
                                  UnqualifiedIdentifier(..),
                                  SinglyQualifiedIdentifier(..),
                                  DoublyQualifiedIdentifier(..),
                                  Token(..)
                                 )
    where

import qualified Data.ByteString as BS
import Data.Char
import Data.Int
import Data.List
import Data.Word
import Numeric
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable


class ShowTokens a where
    showTokens :: a -> [Token]


data OneOrMore a = MkOneOrMore [a]
                   deriving (Eq)

instance (Show a) => Show (OneOrMore a) where
    show (MkOneOrMore list) = "1" ++ show list

mkOneOrMore :: [a] -> Maybe (OneOrMore a)
mkOneOrMore [] = Nothing
mkOneOrMore list = Just $ MkOneOrMore list

mapOneOrMore :: (a -> b) -> (OneOrMore a) -> [b]
mapOneOrMore function (MkOneOrMore list) = map function list

fromOneOrMore :: (OneOrMore a) -> [a]
fromOneOrMore (MkOneOrMore list) = list

data NonnegativeDouble = MkNonnegativeDouble Double
                         deriving (Eq)

instance Show NonnegativeDouble where
    show (MkNonnegativeDouble double) = "+" ++ show double

mkNonnegativeDouble :: Double -> Maybe NonnegativeDouble
mkNonnegativeDouble double =
    if double < 0.0
       then Nothing
       else Just $ MkNonnegativeDouble double

fromNonnegativeDouble :: NonnegativeDouble -> Double
fromNonnegativeDouble (MkNonnegativeDouble double) = double

data Type = Type UnqualifiedIdentifier
                 MaybeTypeSize
            deriving (Eq, Show)
instance ShowTokens Type where
    showTokens (Type name maybeTypeSize)
        = showTokens name
          ++ showTokens maybeTypeSize


data MaybeType = NoType | JustType Type
                 deriving (Eq, Show)
instance ShowTokens MaybeType where
    showTokens NoType = []
    showTokens (JustType type') = showTokens type'


data MaybeTypeSize = NoTypeSize
                   | TypeMaximumSize TypeSizeField
                   | TypeSize TypeSizeField TypeSizeField
                     deriving (Eq, Show)
instance ShowTokens MaybeTypeSize where
    showTokens NoTypeSize = []
    showTokens (TypeMaximumSize maximumSize)
        = [PunctuationLeftParenthesis]
          ++ showTokens maximumSize
          ++ [PunctuationRightParenthesis]
    showTokens (TypeSize minimumSize maximumSize)
        = [PunctuationLeftParenthesis]
          ++ showTokens minimumSize
          ++ [PunctuationComma]
          ++ showTokens maximumSize
          ++ [PunctuationRightParenthesis]


data TypeSizeField = DoubleSize MaybeSign NonnegativeDouble
                   | IntegerSize MaybeSign Word64
                     deriving (Eq, Show)
instance ShowTokens TypeSizeField where
    showTokens (DoubleSize maybeSign nonnegativeDouble)
        = showTokens maybeSign
          ++ [LiteralFloat nonnegativeDouble]
    showTokens (IntegerSize maybeSign word)
        = showTokens maybeSign
          ++ [LiteralInteger word]


data LikeType = Like
              | NotLike
              | Glob
              | NotGlob
              | Regexp
              | NotRegexp
              | Match
              | NotMatch
                deriving (Eq, Show)
instance ShowTokens LikeType where
    showTokens Like = [KeywordLike]
    showTokens NotLike = [KeywordNot, KeywordLike]
    showTokens Glob = [KeywordGlob]
    showTokens NotGlob = [KeywordNot, KeywordGlob]
    showTokens Regexp = [KeywordRegexp]
    showTokens NotRegexp = [KeywordNot, KeywordRegexp]
    showTokens Match = [KeywordMatch]
    showTokens NotMatch = [KeywordNot, KeywordMatch]

data Escape = NoEscape | Escape Expression
              deriving (Eq, Show)
instance ShowTokens Escape where
    showTokens NoEscape = []
    showTokens (Escape expression)
        = [KeywordEscape]
          ++ showTokens expression

data MaybeSwitchExpression = NoSwitch | Switch Expression
                             deriving (Eq, Show)
instance ShowTokens MaybeSwitchExpression where
    showTokens NoSwitch = []
    showTokens (Switch expression) = showTokens expression

data CasePair = WhenThen Expression Expression
                deriving (Eq, Show)
instance ShowTokens CasePair where
    showTokens (WhenThen condition result)
        = [KeywordWhen]
          ++ showTokens condition
          ++ [KeywordThen]
          ++ showTokens result

data Else = NoElse
          | Else Expression
            deriving (Eq, Show)
instance ShowTokens Else where
    showTokens NoElse = []
    showTokens (Else expression) = [KeywordElse] ++ showTokens expression

data Expression = ExpressionLiteralInteger Word64
                | ExpressionLiteralFloat NonnegativeDouble
                | ExpressionLiteralString String
                | ExpressionLiteralBlob BS.ByteString
                | ExpressionLiteralNull
                | ExpressionLiteralCurrentTime
                | ExpressionLiteralCurrentDate
                | ExpressionLiteralCurrentTimestamp
                | ExpressionVariable
                | ExpressionVariableN Word64
                | ExpressionVariableNamed String
                | ExpressionIdentifier DoublyQualifiedIdentifier
                | ExpressionUnaryNegative Expression
                | ExpressionUnaryPositive Expression
                | ExpressionUnaryBitwiseNot Expression
                | ExpressionUnaryLogicalNot Expression
                | ExpressionBinaryConcatenate Expression Expression
                | ExpressionBinaryMultiply Expression Expression
                | ExpressionBinaryDivide Expression Expression
                | ExpressionBinaryModulus Expression Expression
                | ExpressionBinaryAdd Expression Expression
                | ExpressionBinarySubtract Expression Expression
                | ExpressionBinaryLeftShift Expression Expression
                | ExpressionBinaryRightShift Expression Expression
                | ExpressionBinaryBitwiseAnd Expression Expression
                | ExpressionBinaryBitwiseOr Expression Expression
                | ExpressionBinaryLess Expression Expression
                | ExpressionBinaryLessEquals Expression Expression
                | ExpressionBinaryGreater Expression Expression
                | ExpressionBinaryGreaterEquals Expression Expression
                | ExpressionBinaryEquals Expression Expression
                | ExpressionBinaryEqualsEquals Expression Expression
                | ExpressionBinaryNotEquals Expression Expression
                | ExpressionBinaryLessGreater Expression Expression
                | ExpressionBinaryLogicalAnd Expression Expression
                | ExpressionBinaryLogicalOr Expression Expression
                | ExpressionFunctionCall UnqualifiedIdentifier [Expression]
                | ExpressionFunctionCallDistinct UnqualifiedIdentifier
                                                 (OneOrMore Expression)
                | ExpressionFunctionCallStar UnqualifiedIdentifier
                | ExpressionCast Expression Type
                | ExpressionCollate Expression UnqualifiedIdentifier
                | ExpressionLike Expression LikeType Expression Escape
                | ExpressionIsnull Expression
                | ExpressionNotnull Expression
                | ExpressionNotNull Expression
                | ExpressionIs Expression Expression
                | ExpressionIsNot Expression Expression
                | ExpressionBetween Expression Expression Expression
                | ExpressionNotBetween Expression Expression Expression
                | ExpressionInSelect Expression (Select)
                | ExpressionNotInSelect Expression (Select)
                | ExpressionInList Expression [Expression]
                | ExpressionNotInList Expression [Expression]
                | ExpressionInTable Expression SinglyQualifiedIdentifier
                | ExpressionNotInTable Expression SinglyQualifiedIdentifier
                | ExpressionSubquery (Select)
                | ExpressionExistsSubquery (Select)
                | ExpressionNotExistsSubquery (Select)
                | ExpressionCase MaybeSwitchExpression
                                 (OneOrMore CasePair)
                                 Else
                | ExpressionRaiseIgnore
                | ExpressionRaiseRollback String
                | ExpressionRaiseAbort String
                | ExpressionRaiseFail String
                | ExpressionParenthesized Expression
                  deriving (Eq, Show)

instance ShowTokens Expression where
    showTokens (ExpressionLiteralInteger word)
        = [LiteralInteger word]
    showTokens (ExpressionLiteralFloat nonnegativeDouble)
        = [LiteralFloat nonnegativeDouble]
    showTokens (ExpressionLiteralString string)
        = [LiteralString string]
    showTokens (ExpressionLiteralBlob bytestring)
        = [LiteralBlob bytestring]
    showTokens (ExpressionLiteralNull)
        = [KeywordNull]
    showTokens (ExpressionLiteralCurrentTime)
        = [KeywordCurrentTime]
    showTokens (ExpressionLiteralCurrentDate)
        = [KeywordCurrentDate]
    showTokens (ExpressionLiteralCurrentTimestamp)
        = [KeywordCurrentTimestamp]
    showTokens (ExpressionVariable)
        = [Variable]
    showTokens (ExpressionVariableN integer)
        = [VariableN integer]
    showTokens (ExpressionVariableNamed string)
        = [VariableNamed string]
    showTokens (ExpressionIdentifier doublyQualifiedIdentifier)
        = showTokens doublyQualifiedIdentifier
    showTokens (ExpressionUnaryNegative expression)
        = [PunctuationMinus] ++ showTokens expression
    showTokens (ExpressionUnaryPositive expression)
        = [PunctuationPlus] ++ showTokens expression
    showTokens (ExpressionUnaryBitwiseNot expression)
        = [PunctuationTilde] ++ showTokens expression
    showTokens (ExpressionUnaryLogicalNot expression)
        = [KeywordNot] ++ showTokens expression
    showTokens (ExpressionBinaryConcatenate a b)
        = showTokens a ++ [PunctuationBarBar] ++ showTokens b
    showTokens (ExpressionBinaryMultiply a b)
        = showTokens a ++ [PunctuationStar] ++ showTokens b
    showTokens (ExpressionBinaryDivide a b)
        = showTokens a ++ [PunctuationSlash] ++ showTokens b
    showTokens (ExpressionBinaryModulus a b)
        = showTokens a ++ [PunctuationPercent] ++ showTokens b
    showTokens (ExpressionBinaryAdd a b)
        = showTokens a ++ [PunctuationPlus] ++ showTokens b
    showTokens (ExpressionBinarySubtract a b)
        = showTokens a ++ [PunctuationMinus] ++ showTokens b
    showTokens (ExpressionBinaryLeftShift a b)
        = showTokens a ++ [PunctuationLessLess] ++ showTokens b
    showTokens (ExpressionBinaryRightShift a b)
        = showTokens a ++ [PunctuationGreaterGreater] ++ showTokens b
    showTokens (ExpressionBinaryBitwiseAnd a b)
        = showTokens a ++ [PunctuationAmpersand] ++ showTokens b
    showTokens (ExpressionBinaryBitwiseOr a b)
        = showTokens a ++ [PunctuationBar] ++ showTokens b
    showTokens (ExpressionBinaryLess a b)
        = showTokens a ++ [PunctuationLess] ++ showTokens b
    showTokens (ExpressionBinaryLessEquals a b)
        = showTokens a ++ [PunctuationLessEquals] ++ showTokens b
    showTokens (ExpressionBinaryGreater a b)
        = showTokens a ++ [PunctuationGreater] ++ showTokens b
    showTokens (ExpressionBinaryGreaterEquals a b)
        = showTokens a ++ [PunctuationGreaterEquals] ++ showTokens b
    showTokens (ExpressionBinaryEquals a b)
        = showTokens a ++ [PunctuationEquals] ++ showTokens b
    showTokens (ExpressionBinaryEqualsEquals a b)
        = showTokens a ++ [PunctuationEqualsEquals] ++ showTokens b
    showTokens (ExpressionBinaryNotEquals a b)
        = showTokens a ++ [PunctuationBangEquals] ++ showTokens b
    showTokens (ExpressionBinaryLessGreater a b)
        = showTokens a ++ [PunctuationLessGreater] ++ showTokens b
    showTokens (ExpressionBinaryLogicalAnd a b)
        = showTokens a ++ [KeywordAnd] ++ showTokens b
    showTokens (ExpressionBinaryLogicalOr a b)
        = showTokens a ++ [KeywordOr] ++ showTokens b
    showTokens (ExpressionFunctionCall name parameters)
        = showTokens name
          ++ [PunctuationLeftParenthesis]
          ++ intercalate [PunctuationComma] (map showTokens parameters)
          ++ [PunctuationRightParenthesis]
    showTokens (ExpressionFunctionCallDistinct name parameters)
        = showTokens name
          ++ [PunctuationLeftParenthesis,
              KeywordDistinct]
          ++ intercalate [PunctuationComma] (mapOneOrMore showTokens parameters)
          ++ [PunctuationRightParenthesis]
    showTokens (ExpressionFunctionCallStar name)
        = showTokens name
          ++ [PunctuationLeftParenthesis,
              PunctuationStar,
              PunctuationRightParenthesis]
    showTokens (ExpressionCast expression typeDescriptor)
        = [KeywordCast,
           PunctuationLeftParenthesis]
          ++ showTokens expression
          ++ [KeywordAs]
          ++ showTokens typeDescriptor
          ++ [PunctuationRightParenthesis]
    showTokens (ExpressionCollate expression collation)
        = showTokens expression
          ++ [KeywordCollate]
          ++ showTokens collation
    showTokens (ExpressionLike a likeType b escape)
        = showTokens a
          ++ showTokens likeType
          ++ showTokens b
          ++ showTokens escape
    showTokens (ExpressionIsnull expression)
        = showTokens expression
          ++ [KeywordIsnull]
    showTokens (ExpressionNotnull expression)
        = showTokens expression
          ++ [KeywordNotnull]
    showTokens (ExpressionNotNull expression)
        = showTokens expression
          ++ [KeywordNot, KeywordNull]
    showTokens (ExpressionIs a b)
        = showTokens a
          ++ [KeywordIs]
          ++ showTokens b
    showTokens (ExpressionIsNot a b)
        = showTokens a
          ++ [KeywordIs, KeywordNot]
          ++ showTokens b
    showTokens (ExpressionBetween a b c)
        = showTokens a
          ++ [KeywordBetween]
          ++ showTokens b
          ++ [KeywordAnd]
          ++ showTokens c
    showTokens (ExpressionNotBetween a b c)
        = showTokens a
          ++ [KeywordNot, KeywordBetween]
          ++ showTokens b
          ++ [KeywordAnd]
          ++ showTokens c
    showTokens (ExpressionInSelect expression statement)
        = showTokens expression
          ++ [KeywordIn, PunctuationLeftParenthesis]
          ++ showTokens statement
          ++ [PunctuationRightParenthesis]
    showTokens (ExpressionNotInSelect expression statement)
        = showTokens expression
          ++ [KeywordNot, KeywordIn, PunctuationLeftParenthesis]
          ++ showTokens statement
          ++ [PunctuationRightParenthesis]
    showTokens (ExpressionInList expression list)
        = showTokens expression
          ++ [KeywordIn, PunctuationLeftParenthesis]
          ++ intercalate [PunctuationComma] (map showTokens list)
          ++ [PunctuationRightParenthesis]
    showTokens (ExpressionNotInList expression list)
        = showTokens expression
          ++ [KeywordNot, KeywordIn, PunctuationLeftParenthesis]
          ++ intercalate [PunctuationComma] (map showTokens list)
          ++ [PunctuationRightParenthesis]
    showTokens (ExpressionInTable expression table)
        = showTokens expression
          ++ [KeywordIn]
          ++ showTokens table
    showTokens (ExpressionNotInTable expression table)
        = showTokens expression
          ++ [KeywordNot, KeywordIn]
          ++ showTokens table
    showTokens (ExpressionSubquery statement)
        = [PunctuationLeftParenthesis]
          ++ showTokens statement
          ++ [PunctuationRightParenthesis]
    showTokens (ExpressionExistsSubquery statement)
        = [KeywordExists, PunctuationLeftParenthesis]
          ++ showTokens statement
          ++ [PunctuationRightParenthesis]
    showTokens (ExpressionNotExistsSubquery statement)
        = [KeywordNot, KeywordExists, PunctuationLeftParenthesis]
          ++ showTokens statement
          ++ [PunctuationRightParenthesis]
    showTokens (ExpressionCase maybeSwitchExpression cases else')
        = [KeywordCase]
          ++ showTokens maybeSwitchExpression
          ++ (concat $ mapOneOrMore showTokens cases)
          ++ showTokens else'
          ++ [KeywordEnd]
    showTokens (ExpressionRaiseIgnore)
        = [KeywordRaise,
           PunctuationLeftParenthesis,
           KeywordIgnore,
           PunctuationRightParenthesis]
    showTokens (ExpressionRaiseRollback message)
        = [KeywordRaise,
           PunctuationLeftParenthesis,
           KeywordRollback,
           PunctuationComma,
           LiteralString message,
           PunctuationRightParenthesis]
    showTokens (ExpressionRaiseAbort message)
        = [KeywordRaise,
           PunctuationLeftParenthesis,
           KeywordAbort,
           PunctuationComma,
           LiteralString message,
           PunctuationRightParenthesis]
    showTokens (ExpressionRaiseFail message)
        = [KeywordRaise,
           PunctuationLeftParenthesis,
           KeywordFail,
           PunctuationComma,
           LiteralString message,
           PunctuationRightParenthesis]
    showTokens (ExpressionParenthesized subexpression)
        = [PunctuationLeftParenthesis]
          ++ showTokens subexpression
          ++ [PunctuationRightParenthesis]


data MaybeUnique = NoUnique | Unique
                   deriving (Eq, Show)
instance ShowTokens MaybeUnique where
    showTokens NoUnique = []
    showTokens Unique = [KeywordUnique]

data MaybeIfNotExists = NoIfNotExists | IfNotExists
                        deriving (Eq, Show)
instance ShowTokens MaybeIfNotExists where
    showTokens NoIfNotExists = []
    showTokens IfNotExists = [KeywordIf, KeywordNot, KeywordExists]

data MaybeIfExists = NoIfExists | IfExists
                     deriving (Eq, Show)
instance ShowTokens MaybeIfExists where
    showTokens NoIfExists = []
    showTokens IfExists = [KeywordIf, KeywordExists]

data MaybeForEachRow = NoForEachRow | ForEachRow
                       deriving (Eq, Show)
instance ShowTokens MaybeForEachRow where
    showTokens NoForEachRow = []
    showTokens ForEachRow = [KeywordFor, KeywordEach, KeywordRow]

data Permanence = Permanent | Temp | Temporary
                  deriving (Eq, Show)
instance ShowTokens Permanence where
    showTokens Permanent = []
    showTokens Temp = [KeywordTemp]
    showTokens Temporary = [KeywordTemporary]

data MaybeCollation = NoCollation | Collation UnqualifiedIdentifier
                      deriving (Eq, Show)
instance ShowTokens MaybeCollation where
    showTokens NoCollation = []
    showTokens (Collation name) = [KeywordCollate] ++ showTokens name

data MaybeAscDesc = NoAscDesc | Asc | Desc
                    deriving (Eq, Show)
instance ShowTokens MaybeAscDesc where
    showTokens NoAscDesc = []
    showTokens Asc = [KeywordAsc]
    showTokens Desc = [KeywordDesc]

data MaybeAutoincrement = NoAutoincrement | Autoincrement
                          deriving (Eq, Show)
instance ShowTokens MaybeAutoincrement where
    showTokens NoAutoincrement = []
    showTokens Autoincrement = [KeywordAutoincrement]

data MaybeSign = NoSign | PositiveSign | NegativeSign
                 deriving (Eq, Show)
instance ShowTokens MaybeSign where
    showTokens NoSign = []
    showTokens PositiveSign = [PunctuationPlus]
    showTokens NegativeSign = [PunctuationMinus]

data MaybeColumn = ElidedColumn | Column
                   deriving (Eq, Show)
instance ShowTokens MaybeColumn where
    showTokens ElidedColumn = []
    showTokens Column = [KeywordColumn]

data AlterTableBody
    = RenameTo UnqualifiedIdentifier
    | AddColumn MaybeColumn ColumnDefinition
      deriving (Eq, Show)
instance ShowTokens AlterTableBody where
    showTokens (RenameTo newTableName)
        = [KeywordRename, KeywordTo]
          ++ showTokens newTableName
    showTokens (AddColumn maybeColumn columnDefinition)
        = [KeywordAdd]
          ++ showTokens maybeColumn
          ++ showTokens columnDefinition

data ColumnDefinition
    = ColumnDefinition UnqualifiedIdentifier MaybeType [ColumnConstraint]
      deriving (Eq, Show)
instance ShowTokens ColumnDefinition where
    showTokens (ColumnDefinition name maybeType constraints)
        = showTokens name
          ++ showTokens maybeType
          ++ (concat $ map showTokens constraints)

data DefaultValue
    = DefaultValueSignedInteger MaybeSign Word64
    | DefaultValueSignedFloat MaybeSign NonnegativeDouble
    | DefaultValueLiteralString String
    | DefaultValueLiteralBlob BS.ByteString
    | DefaultValueLiteralNull
    | DefaultValueLiteralCurrentTime
    | DefaultValueLiteralCurrentDate
    | DefaultValueLiteralCurrentTimestamp
    | DefaultValueExpression Expression
      deriving (Eq, Show)
instance ShowTokens DefaultValue where
    showTokens (DefaultValueSignedInteger maybeSign word)
        = showTokens maybeSign
          ++ [LiteralInteger word]
    showTokens (DefaultValueSignedFloat maybeSign double)
        = showTokens maybeSign
          ++ [LiteralFloat double]
    showTokens (DefaultValueLiteralString string)
        = [LiteralString string]
    showTokens (DefaultValueLiteralBlob bytestring)
        = [LiteralBlob bytestring]
    showTokens DefaultValueLiteralNull
        = [KeywordNull]
    showTokens DefaultValueLiteralCurrentTime
        = [KeywordCurrentTime]
    showTokens DefaultValueLiteralCurrentDate
        = [KeywordCurrentDate]
    showTokens DefaultValueLiteralCurrentTimestamp
        = [KeywordCurrentTimestamp]
    showTokens (DefaultValueExpression expression)
        = [PunctuationLeftParenthesis]
          ++ showTokens expression
          ++ [PunctuationRightParenthesis]

data IndexedColumn
    = IndexedColumn UnqualifiedIdentifier MaybeCollation MaybeAscDesc
      deriving (Eq, Show)
instance ShowTokens IndexedColumn where
    showTokens (IndexedColumn name maybeCollation maybeAscDesc)
        = showTokens name
          ++ showTokens maybeCollation
          ++ showTokens maybeAscDesc

data ColumnConstraint
    = ColumnPrimaryKey (Maybe UnqualifiedIdentifier)
                       MaybeAscDesc
                       (Maybe ConflictClause)
                       MaybeAutoincrement
    | ColumnNotNull (Maybe UnqualifiedIdentifier) (Maybe ConflictClause)
    | ColumnUnique (Maybe UnqualifiedIdentifier) (Maybe ConflictClause)
    | ColumnCheck (Maybe UnqualifiedIdentifier) Expression
    | ColumnDefault (Maybe UnqualifiedIdentifier) DefaultValue
    | ColumnCollate (Maybe UnqualifiedIdentifier) UnqualifiedIdentifier
    | ColumnForeignKey (Maybe UnqualifiedIdentifier) ForeignKeyClause
      deriving (Eq, Show)
instance ShowTokens ColumnConstraint where
    showTokens (ColumnPrimaryKey maybeConstraintName
                                 maybeAscDesc
                                 maybeConflictClause
                                 maybeAutoincrement)
        = (case maybeConstraintName of
             Nothing -> []
             Just constraintName -> [KeywordConstraint]
                                    ++ showTokens constraintName)
          ++ [KeywordPrimary, KeywordKey]
          ++ showTokens maybeAscDesc
          ++ (case maybeConflictClause of
                Nothing -> []
                Just conflictClause -> showTokens conflictClause)
          ++ showTokens maybeAutoincrement
    showTokens (ColumnNotNull maybeConstraintName maybeConflictClause)
        = (case maybeConstraintName of
             Nothing -> []
             Just constraintName -> [KeywordConstraint]
                                    ++ showTokens constraintName)
          ++ [KeywordNot, KeywordNull]
          ++ (case maybeConflictClause of
                Nothing -> []
                Just conflictClause -> showTokens conflictClause)
    showTokens (ColumnUnique maybeConstraintName maybeConflictClause)
        = (case maybeConstraintName of
             Nothing -> []
             Just constraintName -> [KeywordConstraint]
                                    ++ showTokens constraintName)
          ++ [KeywordUnique]
          ++ (case maybeConflictClause of
                Nothing -> []
                Just conflictClause -> showTokens conflictClause)
    showTokens (ColumnCheck maybeConstraintName expression)
        = (case maybeConstraintName of
             Nothing -> []
             Just constraintName -> [KeywordConstraint]
                                    ++ showTokens constraintName)
          ++ [KeywordCheck, PunctuationLeftParenthesis]
          ++ showTokens expression
          ++ [PunctuationRightParenthesis]
    showTokens (ColumnDefault maybeConstraintName defaultValue)
        = (case maybeConstraintName of
             Nothing -> []
             Just constraintName -> [KeywordConstraint]
                                    ++ showTokens constraintName)
          ++ [KeywordDefault]
          ++ showTokens defaultValue
    showTokens (ColumnCollate maybeConstraintName collationName)
        = (case maybeConstraintName of
             Nothing -> []
             Just constraintName -> [KeywordConstraint]
                                    ++ showTokens constraintName)
          ++ [KeywordCollate]
          ++ showTokens collationName
    showTokens (ColumnForeignKey maybeConstraintName foreignKeyClause)
        = (case maybeConstraintName of
             Nothing -> []
             Just constraintName -> [KeywordConstraint]
                                    ++ showTokens constraintName)
          ++ showTokens foreignKeyClause

data TableConstraint
    = TablePrimaryKey (Maybe UnqualifiedIdentifier)
                      (OneOrMore IndexedColumn)
                      (Maybe ConflictClause)
    | TableUnique (Maybe UnqualifiedIdentifier)
                  (OneOrMore IndexedColumn)
                  (Maybe ConflictClause)
    | TableCheck (Maybe UnqualifiedIdentifier)
                 Expression
    | TableForeignKey (Maybe UnqualifiedIdentifier)
                      (OneOrMore UnqualifiedIdentifier)
                      ForeignKeyClause
      deriving (Eq, Show)
instance ShowTokens TableConstraint where
    showTokens (TablePrimaryKey maybeConstraintName indexedColumns maybeConflictClause)
        = (case maybeConstraintName of
             Nothing -> []
             Just constraintName -> [KeywordConstraint]
                                    ++ showTokens constraintName)
          ++ [KeywordPrimary, KeywordKey, PunctuationLeftParenthesis]
          ++ (intercalate [PunctuationComma] $ mapOneOrMore showTokens indexedColumns)
          ++ [PunctuationRightParenthesis]
          ++ (case maybeConflictClause of
                Nothing -> []
                Just conflictClause -> showTokens conflictClause)
    showTokens (TableUnique maybeConstraintName indexedColumns maybeConflictClause)
        = (case maybeConstraintName of
             Nothing -> []
             Just constraintName -> [KeywordConstraint]
                                    ++ showTokens constraintName)
          ++ [KeywordUnique, PunctuationLeftParenthesis]
          ++ (intercalate [PunctuationComma] $ mapOneOrMore showTokens indexedColumns)
          ++ [PunctuationRightParenthesis]
          ++ (case maybeConflictClause of
                Nothing -> []
                Just conflictClause -> showTokens conflictClause)
    showTokens (TableCheck maybeConstraintName expression)
        = (case maybeConstraintName of
             Nothing -> []
             Just constraintName -> [KeywordConstraint]
                                    ++ showTokens constraintName)
          ++ [KeywordCheck, PunctuationLeftParenthesis]
          ++ showTokens expression
          ++ [PunctuationRightParenthesis]
    showTokens (TableForeignKey maybeConstraintName columns foreignKeyClause)
        = (case maybeConstraintName of
             Nothing -> []
             Just constraintName -> [KeywordConstraint]
                                    ++ showTokens constraintName)
          ++ [KeywordForeign, KeywordKey, PunctuationLeftParenthesis]
          ++ (intercalate [PunctuationComma] $ mapOneOrMore showTokens columns)
          ++ [PunctuationRightParenthesis]
          ++ showTokens foreignKeyClause

data TriggerTime = Before | After | InsteadOf
                   deriving (Eq, Show)
instance ShowTokens TriggerTime where
    showTokens Before = [KeywordBefore]
    showTokens After = [KeywordAfter]
    showTokens InsteadOf = [KeywordInstead, KeywordOf]
    
data TriggerCondition = DeleteOn | InsertOn | UpdateOn [UnqualifiedIdentifier]
                        deriving (Eq, Show)
instance ShowTokens TriggerCondition where
    showTokens DeleteOn = [KeywordDelete, KeywordOn]
    showTokens InsertOn = [KeywordInsert, KeywordOn]
    showTokens (UpdateOn []) = [KeywordUpdate, KeywordOn]
    showTokens (UpdateOn columnNames) = [KeywordUpdate, KeywordOf]
                                        ++ intercalate [PunctuationComma]
                                                       (map showTokens columnNames)
                                        ++ [KeywordOn]

data ModuleArgument = ModuleArgument String
                      deriving (Eq, Show)
instance ShowTokens ModuleArgument where
    showTokens (ModuleArgument string) = [ModuleArgumentToken string]

data QualifiedTableName
    = TableNoIndexedBy SinglyQualifiedIdentifier
    | TableIndexedBy SinglyQualifiedIdentifier UnqualifiedIdentifier
    | TableNotIndexed SinglyQualifiedIdentifier
      deriving (Eq, Show)
instance ShowTokens QualifiedTableName where
    showTokens (TableNoIndexedBy tableName) =
        showTokens tableName
    showTokens (TableIndexedBy tableName indexName) =
        showTokens tableName
        ++ [KeywordIndexed, KeywordBy]
        ++ showTokens indexName
    showTokens (TableNotIndexed tableName) =
        showTokens tableName
        ++ [KeywordNot, KeywordIndexed]

data OrderingTerm = OrderingTerm Expression MaybeCollation MaybeAscDesc
                    deriving (Eq, Show)
instance ShowTokens OrderingTerm where
    showTokens (OrderingTerm expression maybeCollation maybeAscDesc) =
        showTokens expression
        ++ showTokens maybeCollation
        ++ showTokens maybeAscDesc

data PragmaBody = EmptyPragmaBody
                | EqualsPragmaBody PragmaValue
                | CallPragmaBody PragmaValue
                  deriving (Eq, Show)
instance ShowTokens PragmaBody where
    showTokens EmptyPragmaBody = []
    showTokens (EqualsPragmaBody pragmaValue)
        = [PunctuationEquals]
          ++ showTokens pragmaValue
    showTokens (CallPragmaBody pragmaValue)
        = [PunctuationLeftParenthesis]
          ++ showTokens pragmaValue
          ++ [PunctuationRightParenthesis]

data PragmaValue = SignedIntegerPragmaValue MaybeSign Word64
                 | SignedFloatPragmaValue MaybeSign NonnegativeDouble
                 | NamePragmaValue UnqualifiedIdentifier
                 | StringPragmaValue String
                   deriving (Eq, Show)
instance ShowTokens PragmaValue where
    showTokens (SignedIntegerPragmaValue maybeSign word)
        = showTokens maybeSign
          ++ [LiteralInteger word]
    showTokens (SignedFloatPragmaValue maybeSign double)
        = showTokens maybeSign
          ++ [LiteralFloat double]
    showTokens (NamePragmaValue name)
        = showTokens name
    showTokens (StringPragmaValue string)
        = [LiteralString string]

data EitherColumnsAndConstraintsSelect
    = ColumnsAndConstraints (OneOrMore ColumnDefinition) [TableConstraint]
    | AsSelect (Select)
      deriving (Eq, Show)
instance ShowTokens EitherColumnsAndConstraintsSelect where
    showTokens (ColumnsAndConstraints columns constraints)
        = [PunctuationLeftParenthesis]
          ++ (intercalate [PunctuationComma]
                              $ concat [mapOneOrMore showTokens columns,
                                        map showTokens constraints])
          ++ [PunctuationRightParenthesis]
    showTokens (AsSelect select)
        = [KeywordAs]
          ++ showTokens select

data InsertHead = InsertNoAlternative
                | InsertOrRollback
                | InsertOrAbort
                | InsertOrReplace
                | InsertOrFail
                | InsertOrIgnore
                | Replace
                  deriving (Eq, Show)
instance ShowTokens InsertHead where
    showTokens InsertNoAlternative = [KeywordInsert]
    showTokens InsertOrRollback = [KeywordInsert, KeywordOr, KeywordRollback]
    showTokens InsertOrAbort = [KeywordInsert, KeywordOr, KeywordAbort]
    showTokens InsertOrReplace = [KeywordInsert, KeywordOr, KeywordReplace]
    showTokens InsertOrFail = [KeywordInsert, KeywordOr, KeywordFail]
    showTokens InsertOrIgnore = [KeywordInsert, KeywordOr, KeywordIgnore]
    showTokens Replace = [KeywordReplace]

data InsertBody = InsertValues [UnqualifiedIdentifier] (OneOrMore Expression)
                | InsertSelect [UnqualifiedIdentifier] (Select)
                | InsertDefaultValues
                  deriving (Eq, Show)
instance ShowTokens InsertBody where
    showTokens (InsertValues columns expressions)
        = (case columns of
             [] -> []
             _ -> [PunctuationLeftParenthesis]
                  ++ (intercalate [PunctuationComma] $ map showTokens columns)
                  ++ [PunctuationRightParenthesis])
          ++ [KeywordValues, PunctuationLeftParenthesis]
          ++ (intercalate [PunctuationComma] $ mapOneOrMore showTokens expressions)
          ++ [PunctuationRightParenthesis]
    showTokens (InsertSelect columns select)
        = (case columns of
             [] -> []
             _ -> [PunctuationLeftParenthesis]
                  ++ (intercalate [PunctuationComma] $ map showTokens columns)
                  ++ [PunctuationRightParenthesis])
          ++ showTokens select
    showTokens InsertDefaultValues
        = [KeywordDefault, KeywordValues]

data UpdateHead = UpdateNoAlternative
                | UpdateOrRollback
                | UpdateOrAbort
                | UpdateOrReplace
                | UpdateOrFail
                | UpdateOrIgnore
                  deriving (Eq, Show)
instance ShowTokens UpdateHead where
    showTokens UpdateNoAlternative = [KeywordUpdate]
    showTokens UpdateOrRollback = [KeywordUpdate, KeywordOr, KeywordRollback]
    showTokens UpdateOrAbort = [KeywordUpdate, KeywordOr, KeywordAbort]
    showTokens UpdateOrReplace = [KeywordUpdate, KeywordOr, KeywordReplace]
    showTokens UpdateOrFail = [KeywordUpdate, KeywordOr, KeywordFail]
    showTokens UpdateOrIgnore = [KeywordUpdate, KeywordOr, KeywordIgnore]

data Distinctness = NoDistinctness | Distinct | All
                    deriving (Eq, Show)
instance ShowTokens Distinctness where
    showTokens NoDistinctness = []
    showTokens Distinct = [KeywordDistinct]
    showTokens All = [KeywordAll]

data MaybeHaving = NoHaving | Having Expression
                   deriving (Eq, Show)
instance ShowTokens MaybeHaving where
    showTokens NoHaving = []
    showTokens (Having expression) = [KeywordHaving] ++ showTokens expression

data MaybeAs = NoAs | As UnqualifiedIdentifier | ElidedAs UnqualifiedIdentifier
               deriving (Eq, Show)
instance ShowTokens MaybeAs where
    showTokens NoAs = []
    showTokens (As thingAlias) = [KeywordAs] ++ showTokens thingAlias
    showTokens (ElidedAs thingAlias) = showTokens thingAlias

data CompoundOperator = Union | UnionAll | Intersect | Except
                        deriving (Eq, Show)
instance ShowTokens CompoundOperator where
    showTokens Union = [KeywordUnion]
    showTokens UnionAll = [KeywordUnion, KeywordAll]
    showTokens Intersect = [KeywordIntersect]
    showTokens Except = [KeywordExcept]

data SelectCore = SelectCore Distinctness
                             (OneOrMore ResultColumn)
                             (Maybe FromClause)
                             (Maybe WhereClause)
                             (Maybe GroupClause)
                  deriving (Eq, Show)
instance ShowTokens SelectCore where
    showTokens (SelectCore distinctness
                           resultColumns
                           maybeFromClause
                           maybeWhereClause
                           maybeGroupClause)
        = [KeywordSelect]
          ++ showTokens distinctness
          ++ (intercalate [PunctuationComma] $ mapOneOrMore showTokens resultColumns)
          ++ (case maybeFromClause of
                Nothing -> []
                Just fromClause -> showTokens fromClause)
          ++ (case maybeWhereClause of
                Nothing -> []
                Just whereClause -> showTokens whereClause)
          ++ (case maybeGroupClause of
                Nothing -> []
                Just groupClause -> showTokens groupClause)

data ResultColumn = Star
                  | TableStar UnqualifiedIdentifier
                  | Result Expression MaybeAs
                    deriving (Eq, Show)
instance ShowTokens ResultColumn where
    showTokens Star
        = [PunctuationStar]
    showTokens (TableStar tableName)
        = showTokens tableName
          ++ [PunctuationDot, PunctuationStar]
    showTokens (Result expression maybeAs)
        = showTokens expression
          ++ showTokens maybeAs

data JoinSource = JoinSource SingleSource
                             [(JoinOperation, SingleSource, JoinConstraint)]
                  deriving (Eq, Show)
instance ShowTokens JoinSource where
    showTokens (JoinSource firstSource additionalSources)
        = showTokens firstSource
          ++ (concat $ map (\(joinOperation, additionalSource, joinConstraint) ->
                             showTokens joinOperation
                             ++ showTokens additionalSource
                             ++ showTokens joinConstraint)
                           additionalSources)

data SingleSource = TableSource SinglyQualifiedIdentifier
                                MaybeAs
                                MaybeIndexedBy
                  | SelectSource (Select)
                                 MaybeAs
                  | SubjoinSource JoinSource
                    deriving (Eq, Show)
instance ShowTokens SingleSource where
    showTokens (TableSource tableName maybeAs maybeIndexedBy)
        = showTokens tableName
          ++ showTokens maybeAs
          ++ showTokens maybeIndexedBy
    showTokens (SelectSource select maybeAs)
        = [PunctuationLeftParenthesis]
          ++ showTokens select
          ++ [PunctuationRightParenthesis]
          ++ showTokens maybeAs
    showTokens (SubjoinSource joinSource)
        = [PunctuationLeftParenthesis]
          ++ showTokens joinSource
          ++ [PunctuationRightParenthesis]

data JoinOperation = Comma
                   | Join
                   | OuterJoin
                   | LeftJoin
                   | LeftOuterJoin
                   | InnerJoin
                   | CrossJoin
                   | NaturalJoin
                   | NaturalOuterJoin
                   | NaturalLeftJoin
                   | NaturalLeftOuterJoin
                   | NaturalInnerJoin
                   | NaturalCrossJoin
                     deriving (Eq, Show)
instance ShowTokens JoinOperation where
    showTokens Comma = [PunctuationComma]
    showTokens Join = [KeywordJoin]
    showTokens OuterJoin = [KeywordOuter, KeywordJoin]
    showTokens LeftJoin = [KeywordLeft, KeywordJoin]
    showTokens LeftOuterJoin = [KeywordLeft, KeywordOuter, KeywordJoin]
    showTokens InnerJoin = [KeywordInner, KeywordJoin]
    showTokens CrossJoin = [KeywordCross, KeywordJoin]
    showTokens NaturalJoin = [KeywordNatural, KeywordJoin]
    showTokens NaturalOuterJoin = [KeywordNatural, KeywordOuter, KeywordJoin]
    showTokens NaturalLeftJoin = [KeywordNatural, KeywordLeft, KeywordJoin]
    showTokens NaturalLeftOuterJoin
        = [KeywordNatural, KeywordLeft, KeywordOuter, KeywordJoin]
    showTokens NaturalInnerJoin = [KeywordNatural, KeywordInner, KeywordJoin]
    showTokens NaturalCrossJoin = [KeywordNatural, KeywordCross, KeywordJoin]

data JoinConstraint = NoConstraint
                    | On Expression
                    | Using (OneOrMore UnqualifiedIdentifier)
                      deriving (Eq, Show)
instance ShowTokens JoinConstraint where
    showTokens NoConstraint = []
    showTokens (On expression) = [KeywordOn] ++ showTokens expression
    showTokens (Using columns)
        = [KeywordUsing, PunctuationLeftParenthesis]
          ++ (intercalate [PunctuationComma] $ mapOneOrMore showTokens columns)

data MaybeIndexedBy = NoIndexedBy
                    | IndexedBy UnqualifiedIdentifier
                    | NotIndexed
                      deriving (Eq, Show)
instance ShowTokens MaybeIndexedBy where
    showTokens NoIndexedBy = []
    showTokens (IndexedBy indexName)
        = [KeywordIndexed, KeywordBy] ++ showTokens indexName
    showTokens NotIndexed = [KeywordNot, KeywordIndexed]

data FromClause = From JoinSource
                  deriving (Eq, Show)
instance ShowTokens FromClause where
    showTokens (From joinSource) = [KeywordFrom] ++ showTokens joinSource

data WhereClause = Where Expression
                   deriving (Eq, Show)
instance ShowTokens WhereClause where
    showTokens (Where expression) = [KeywordWhere] ++ showTokens expression

data GroupClause = GroupBy (OneOrMore OrderingTerm) MaybeHaving
                   deriving (Eq, Show)
instance ShowTokens GroupClause where
    showTokens (GroupBy orderingTerms maybeHaving)
        = [KeywordGroup, KeywordBy]
          ++ (intercalate [PunctuationComma] $ mapOneOrMore showTokens orderingTerms)
          ++ showTokens maybeHaving

data OrderClause = OrderBy (OneOrMore OrderingTerm)
                   deriving (Eq, Show)
instance ShowTokens OrderClause where
    showTokens (OrderBy orderingTerms)
        = [KeywordOrder, KeywordBy]
          ++ (intercalate [PunctuationComma] $ mapOneOrMore showTokens orderingTerms)

data LimitClause = Limit Word64
                 | LimitOffset Word64 Word64
                 | LimitComma Word64 Word64
                   deriving (Eq, Show)
instance ShowTokens LimitClause where
    showTokens (Limit count)
        = [KeywordLimit, LiteralInteger count]
    showTokens (LimitOffset count offset)
        = [KeywordLimit, LiteralInteger count, KeywordOffset, LiteralInteger offset]
    showTokens (LimitComma offset count)
        = [KeywordLimit, LiteralInteger offset, KeywordOffset, LiteralInteger count]

data WhenClause = When Expression
                  deriving (Eq, Show)
instance ShowTokens WhenClause where
    showTokens (When expression) = [KeywordWhen] ++ showTokens expression

data ConflictClause
    = OnConflictRollback
    | OnConflictAbort
    | OnConflictFail
    | OnConflictIgnore
    | OnConflictReplace
      deriving (Eq, Show)
instance ShowTokens ConflictClause where
    showTokens OnConflictRollback = [KeywordOn, KeywordConflict, KeywordRollback]
    showTokens OnConflictAbort = [KeywordOn, KeywordConflict, KeywordAbort]
    showTokens OnConflictFail = [KeywordOn, KeywordConflict, KeywordFail]
    showTokens OnConflictIgnore = [KeywordOn, KeywordConflict, KeywordIgnore]
    showTokens OnConflictReplace = [KeywordOn, KeywordConflict, KeywordReplace]

data ForeignKeyClause
    = References UnqualifiedIdentifier
                 [UnqualifiedIdentifier]
                 [ForeignKeyClauseActionOrMatchPart]
                 MaybeForeignKeyClauseDeferrablePart
      deriving (Eq, Show)
instance ShowTokens ForeignKeyClause where
    showTokens (References tableName
                           columnNames
                           actionOrMatchParts
                           maybeDeferrablePart)
        = [KeywordReferences]
          ++ (case columnNames of
                [] -> []
                _ -> [PunctuationLeftParenthesis]
                     ++ (intercalate [PunctuationComma] $ map showTokens columnNames)
                     ++ [PunctuationRightParenthesis])
          ++ (concat $ map showTokens actionOrMatchParts)
          ++ showTokens maybeDeferrablePart

data ForeignKeyClauseActionOrMatchPart
    = OnDelete ForeignKeyClauseActionPart
    | OnUpdate ForeignKeyClauseActionPart
    | ReferencesMatch UnqualifiedIdentifier
      deriving (Eq, Show)
instance ShowTokens ForeignKeyClauseActionOrMatchPart where
    showTokens (OnDelete actionPart)
        = [KeywordOn, KeywordDelete]
          ++ showTokens actionPart
    showTokens (OnUpdate actionPart)
        = [KeywordOn, KeywordUpdate]
          ++ showTokens actionPart
    showTokens (ReferencesMatch name)
        = [KeywordMatch]
          ++ showTokens name

data ForeignKeyClauseActionPart
    = SetNull
    | SetDefault
    | Cascade
    | Restrict
    | NoAction
      deriving (Eq, Show)
instance ShowTokens ForeignKeyClauseActionPart where
    showTokens SetNull = [KeywordSet, KeywordNull]
    showTokens SetDefault = [KeywordSet, KeywordDefault]
    showTokens Cascade = [KeywordCascade]
    showTokens Restrict = [KeywordRestrict]
    showTokens NoAction = [KeywordNo, KeywordAction]

data MaybeForeignKeyClauseDeferrablePart
    = NoDeferrablePart
    | Deferrable MaybeInitialDeferralStatus
    | NotDeferrable MaybeInitialDeferralStatus
      deriving (Eq, Show)
instance ShowTokens MaybeForeignKeyClauseDeferrablePart where
    showTokens NoDeferrablePart = []
    showTokens (Deferrable maybeInitialDeferralStatus)
        = [KeywordDeferrable]
          ++ showTokens maybeInitialDeferralStatus
    showTokens (NotDeferrable maybeInitialDeferralStatus)
        = [KeywordNot, KeywordDeferrable]
          ++ showTokens maybeInitialDeferralStatus

data MaybeInitialDeferralStatus
    = NoInitialDeferralStatus
    | InitiallyDeferred
    | InitiallyImmediate
      deriving (Eq, Show)
instance ShowTokens MaybeInitialDeferralStatus where
    showTokens NoInitialDeferralStatus = []
    showTokens InitiallyDeferred = [KeywordInitially, KeywordDeferred]
    showTokens InitiallyImmediate = [KeywordInitially, KeywordImmediate]

data MaybeTransaction = ElidedTransaction | Transaction
                        deriving (Eq, Show)
instance ShowTokens MaybeTransaction where
    showTokens ElidedTransaction = []
    showTokens Transaction = [KeywordTransaction]

data MaybeTransactionType
    = NoTransactionType
    | Deferred
    | Immediate
    | Exclusive
      deriving (Eq, Show)
instance ShowTokens MaybeTransactionType where
    showTokens NoTransactionType = []
    showTokens Deferred = [KeywordDeferred]
    showTokens Immediate = [KeywordImmediate]
    showTokens Exclusive = [KeywordExclusive]

data MaybeDatabase = ElidedDatabase | Database
                     deriving (Eq, Show)
instance ShowTokens MaybeDatabase where
    showTokens ElidedDatabase = []
    showTokens Database = [KeywordDatabase]

data MaybeSavepoint = NoSavepoint
                    | To UnqualifiedIdentifier
                    | ToSavepoint UnqualifiedIdentifier
                      deriving (Eq, Show)
instance ShowTokens MaybeSavepoint where
    showTokens NoSavepoint = []
    showTokens (To savepointName)
        = [KeywordTo]
          ++ showTokens savepointName
    showTokens (ToSavepoint savepointName)
        = [KeywordTo, KeywordSavepoint]
          ++ showTokens savepointName

data MaybeReleaseSavepoint = ElidedReleaseSavepoint UnqualifiedIdentifier
                           | ReleaseSavepoint UnqualifiedIdentifier
                             deriving (Eq, Show)
instance ShowTokens MaybeReleaseSavepoint where
    showTokens (ElidedReleaseSavepoint savepointName)
               = showTokens savepointName
    showTokens (ReleaseSavepoint savepointName)
               = [KeywordSavepoint]
                 ++ showTokens savepointName

data StatementList = StatementList [AnyStatement]
                     deriving (Eq, Show)
instance ShowTokens StatementList where
    showTokens (StatementList list) =
        intercalate [PunctuationSemicolon] $ map showTokens list

data AnyStatement = forall l t v w . Statement (Statement l t v w)
instance Eq AnyStatement where
    a@(Statement (Explain _))
         == b@(Statement (Explain _))
        = a == b
    a@(Statement (ExplainQueryPlan _))
         == b@(Statement (ExplainQueryPlan _))
        = a == b
    a@(Statement (AlterTable _ _))
         == b@(Statement (AlterTable _ _))
        = a == b
    a@(Statement (Analyze _))
         == b@(Statement (Analyze _))
        = a == b
    a@(Statement (Attach _ _ _))
         == b@(Statement (Attach _ _ _))
        = a == b
    a@(Statement (Begin _ _))
         == b@(Statement (Begin _ _))
        = a == b
    a@(Statement (Commit _ _))
         == b@(Statement (Commit _ _))
        = a == b
    a@(Statement (CreateIndex _ _ _ _ _))
         == b@(Statement (CreateIndex _ _ _ _ _))
        = a == b
    a@(Statement (CreateTable _ _ _ _))
         == b@(Statement (CreateTable _ _ _ _))
        = a == b
    a@(Statement (CreateTrigger _ _ _ _ _ _ _ _ _))
         == b@(Statement (CreateTrigger _ _ _ _ _ _ _ _ _))
        = a == b
    a@(Statement (CreateView _ _ _ _))
         == b@(Statement (CreateView _ _ _ _))
        = a == b
    a@(Statement (CreateVirtualTable _ _ _))
         == b@(Statement (CreateVirtualTable _ _ _))
        = a == b
    a@(Statement (Delete _ _))
         == b@(Statement (Delete _ _))
        = a == b
    a@(Statement (DeleteLimited _ _ _ _))
         == b@(Statement (DeleteLimited _ _ _ _))
        = a == b
    a@(Statement (Detach _ _))
         == b@(Statement (Detach _ _))
        = a == b
    a@(Statement (DropIndex _ _))
         == b@(Statement (DropIndex _ _))
        = a == b
    a@(Statement (DropTable _ _))
         == b@(Statement (DropTable _ _))
        = a == b
    a@(Statement (DropTrigger _ _))
         == b@(Statement (DropTrigger _ _))
        = a == b
    a@(Statement (DropView _ _))
         == b@(Statement (DropView _ _))
        = a == b
    a@(Statement (Insert _ _ _))
         == b@(Statement (Insert _ _ _))
        = a == b
    a@(Statement (Pragma _ _))
         == b@(Statement (Pragma _ _))
        = a == b
    a@(Statement (Reindex _))
         == b@(Statement (Reindex _))
        = a == b
    a@(Statement (Release _ _))
         == b@(Statement (Release _ _))
        = a == b
    a@(Statement (Rollback _ _))
         == b@(Statement (Rollback _ _))
        = a == b
    a@(Statement (Savepoint _))
         == b@(Statement (Savepoint _))
        = a == b
    a@(Statement (Select _ _ _ _))
         == b@(Statement (Select _ _ _ _))
        = a == b
    a@(Statement (Update _ _ _ _))
         == b@(Statement (Update _ _ _ _))
        = a == b
    a@(Statement (UpdateLimited _ _ _ _ _ _))
         == b@(Statement (UpdateLimited _ _ _ _ _ _))
        = a == b
    a@(Statement (Vacuum))
         == b@(Statement (Vacuum))
        = a == b
    _ == _ = False
deriving instance Show AnyStatement
instance ShowTokens AnyStatement where
    showTokens (Statement statement) = showTokens statement

class StatementClass a where
    fromAnyStatement :: AnyStatement -> a
    fromExplainableStatement :: ExplainableStatement -> a
    fromTriggerStatement :: TriggerStatement -> a
    
instance StatementClass Explain where
    fromAnyStatement (Statement result@(Explain _)) = result
    fromExplainableStatement _ = undefined
    fromTriggerStatement _ = undefined

instance StatementClass ExplainQueryPlan where
    fromAnyStatement (Statement result@(ExplainQueryPlan _)) = result
    fromExplainableStatement _ = undefined
    fromTriggerStatement _ = undefined
    
instance StatementClass AlterTable where
    fromAnyStatement (Statement result@(AlterTable _ _)) = result
    fromExplainableStatement (ExplainableStatement result@(AlterTable _ _)) = result
    fromTriggerStatement _ = undefined

instance StatementClass Analyze where
    fromAnyStatement (Statement result@(Analyze _)) = result
    fromExplainableStatement (ExplainableStatement result@(Analyze _)) = result
    fromTriggerStatement _ = undefined

instance StatementClass Attach where
    fromAnyStatement (Statement result@(Attach _ _ _)) = result
    fromExplainableStatement (ExplainableStatement result@(Attach _ _ _)) = result
    fromTriggerStatement _ = undefined

instance StatementClass Begin where
    fromAnyStatement (Statement result@(Begin _ _)) = result
    fromExplainableStatement (ExplainableStatement result@(Begin _ _)) = result
    fromTriggerStatement _ = undefined

instance StatementClass Commit where
    fromAnyStatement (Statement result@(Commit _ _)) = result
    fromExplainableStatement (ExplainableStatement result@(Commit _ _)) = result
    fromTriggerStatement _ = undefined

instance StatementClass CreateIndex where
    fromAnyStatement (Statement result@(CreateIndex _ _ _ _ _)) = result
    fromExplainableStatement
      (ExplainableStatement result@(CreateIndex _ _ _ _ _)) = result
    fromTriggerStatement _ = undefined

instance StatementClass CreateTable where
    fromAnyStatement (Statement result@(CreateTable _ _ _ _)) = result
    fromExplainableStatement (ExplainableStatement result@(CreateTable _ _ _ _)) = result
    fromTriggerStatement _ = undefined

instance StatementClass CreateTrigger where
    fromAnyStatement (Statement result@(CreateTrigger _ _ _ _ _ _ _ _ _)) = result
    fromExplainableStatement
      (ExplainableStatement result@(CreateTrigger _ _ _ _ _ _ _ _ _)) = result
    fromTriggerStatement _ = undefined

instance StatementClass CreateView where
    fromAnyStatement (Statement result@(CreateView _ _ _ _)) = result
    fromExplainableStatement (ExplainableStatement result@(CreateView _ _ _ _)) = result
    fromTriggerStatement _ = undefined

instance StatementClass CreateVirtualTable where
    fromAnyStatement (Statement result@(CreateVirtualTable _ _ _)) = result
    fromExplainableStatement
      (ExplainableStatement result@(CreateVirtualTable _ _ _)) = result
    fromTriggerStatement _ = undefined

instance StatementClass DeleteLimited where
    fromAnyStatement (Statement result@(DeleteLimited _ _ _ _)) = result
    fromExplainableStatement
      (ExplainableStatement result@(DeleteLimited _ _ _ _)) = result
    fromTriggerStatement _ = undefined

instance StatementClass Detach where
    fromAnyStatement (Statement result@(Detach _ _)) = result
    fromExplainableStatement (ExplainableStatement result@(Detach _ _)) = result
    fromTriggerStatement _ = undefined

instance StatementClass DropIndex where
    fromAnyStatement (Statement result@(DropIndex _ _)) = result
    fromExplainableStatement (ExplainableStatement result@(DropIndex _ _)) = result
    fromTriggerStatement _ = undefined

instance StatementClass DropTable where
    fromAnyStatement (Statement result@(DropTable _ _)) = result
    fromExplainableStatement (ExplainableStatement result@(DropTable _ _)) = result
    fromTriggerStatement _ = undefined

instance StatementClass DropTrigger where
    fromAnyStatement (Statement result@(DropTrigger _ _)) = result
    fromExplainableStatement (ExplainableStatement result@(DropTrigger _ _)) = result
    fromTriggerStatement _ = undefined

instance StatementClass DropView where
    fromAnyStatement (Statement result@(DropView _ _)) = result
    fromExplainableStatement (ExplainableStatement result@(DropView _ _)) = result
    fromTriggerStatement _ = undefined

instance StatementClass Pragma where
    fromAnyStatement (Statement result@(Pragma _ _)) = result
    fromExplainableStatement (ExplainableStatement result@(Pragma _ _)) = result
    fromTriggerStatement _ = undefined

instance StatementClass Reindex where
    fromAnyStatement (Statement result@(Reindex _)) = result
    fromExplainableStatement (ExplainableStatement result@(Reindex _)) = result
    fromTriggerStatement _ = undefined

instance StatementClass Rollback where
    fromAnyStatement (Statement result@(Rollback _ _)) = result
    fromExplainableStatement (ExplainableStatement result@(Rollback _ _)) = result
    fromTriggerStatement _ = undefined

instance StatementClass Savepoint where
    fromAnyStatement (Statement result@(Savepoint _)) = result
    fromExplainableStatement (ExplainableStatement result@(Savepoint _)) = result
    fromTriggerStatement _ = undefined

instance StatementClass UpdateLimited where
    fromAnyStatement (Statement result@(UpdateLimited _ _ _ _ _ _)) = result
    fromExplainableStatement
      (ExplainableStatement result@(UpdateLimited _ _ _ _ _ _)) = result
    fromTriggerStatement _ = undefined

instance StatementClass Vacuum where
    fromAnyStatement (Statement result@(Vacuum)) = result
    fromExplainableStatement (ExplainableStatement result@(Vacuum)) = result
    fromTriggerStatement _ = undefined
    
instance StatementClass Delete where
    fromAnyStatement (Statement result@(Delete _ _)) = result
    fromExplainableStatement (ExplainableStatement result@(Delete _ _)) = result
    fromTriggerStatement (TriggerStatement result@(Delete _ _)) = result

instance StatementClass Insert where
    fromAnyStatement (Statement result@(Insert _ _ _)) = result
    fromExplainableStatement (ExplainableStatement result@(Insert _ _ _)) = result
    fromTriggerStatement (TriggerStatement result@(Insert _ _ _)) = result

instance StatementClass Update where
    fromAnyStatement (Statement result@(Update _ _ _ _)) = result
    fromExplainableStatement (ExplainableStatement result@(Update _ _ _ _)) = result
    fromTriggerStatement (TriggerStatement result@(Update _ _ _ _)) = result
    
instance StatementClass Select where
    fromAnyStatement (Statement result@(Select _ _ _ _)) = result
    fromExplainableStatement (ExplainableStatement result@(Select _ _ _ _)) = result
    fromTriggerStatement (TriggerStatement result@(Select _ _ _ _)) = result

data ExplainableStatement = forall t v w . ExplainableStatement (Statement L0 t v w)
instance Eq ExplainableStatement where
    ExplainableStatement a == ExplainableStatement b = Statement a == Statement b
deriving instance Show ExplainableStatement
instance ShowTokens ExplainableStatement where
    showTokens (ExplainableStatement statement) = showTokens statement

data TriggerStatement = forall l v w . TriggerStatement (Statement l T v w)
instance Eq TriggerStatement where
    TriggerStatement a == TriggerStatement b = Statement a == Statement b
deriving instance Show TriggerStatement
instance ShowTokens TriggerStatement where
    showTokens (TriggerStatement statement) = showTokens statement

-- | Used as a GADT parameter to Statement to indicate a type which can be EXPLAINed.
data L0
-- | Used as a GADT parameter to Statement to indicate a type which can not be EXPLAINed.
data L1

-- | Used as a GADT parameter to Statement to indicate a type which does not represent
--   a DELETE, INSERT, UPDATE, or SELECT statement.
data NT
-- | Used as a GADT parameter to Statement to indicate a type which represents
--   a DELETE, INSERT, UPDATE, or SELECT statement.
data T

-- | Used as a GADT parameter to Statement to indicate a type which does not represent
--   a SELECT statement.
data NS
-- | Used as a GADT parameter to Statement to indicate a type which represents a
--   SELECT statement.
data S

data Explain'
type Explain = Statement L1 NT NS Explain'
data ExplainQueryPlan'
type ExplainQueryPlan = Statement L1 NT NS ExplainQueryPlan'
data AlterTable'
type AlterTable = Statement L0 NT NS AlterTable'
data Analyze'
type Analyze = Statement L0 NT NS Analyze'
data Attach'
type Attach = Statement L0 NT NS Attach'
data Begin'
type Begin = Statement L0 NT NS Begin'
data Commit'
type Commit = Statement L0 NT NS Commit'
data CreateIndex'
type CreateIndex = Statement L0 NT NS CreateIndex'
data CreateTable'
type CreateTable = Statement L0 NT NS CreateTable'
data CreateTrigger'
type CreateTrigger = Statement L0 NT NS CreateTrigger'
data CreateView'
type CreateView = Statement L0 NT NS CreateView'
data CreateVirtualTable'
type CreateVirtualTable = Statement L0 NT NS CreateVirtualTable'
data Delete'
type Delete = Statement L0 T NS Delete'
data DeleteLimited'
type DeleteLimited = Statement L0 NT NS DeleteLimited'
data Detach'
type Detach = Statement L0 NT NS Detach'
data DropIndex'
type DropIndex = Statement L0 NT NS DropIndex'
data DropTable'
type DropTable = Statement L0 NT NS DropTable'
data DropTrigger'
type DropTrigger = Statement L0 NT NS DropTrigger'
data DropView'
type DropView = Statement L0 NT NS DropView'
data Insert'
type Insert = Statement L0 T NS Insert'
data Pragma'
type Pragma = Statement L0 NT NS Pragma'
data Reindex'
type Reindex = Statement L0 NT NS Reindex'
data Release'
type Release = Statement L0 NT NS Release'
data Rollback'
type Rollback = Statement L0 NT NS Rollback'
data Savepoint'
type Savepoint = Statement L0 NT NS Savepoint'
data Select'
type Select = Statement L0 T S Select'
data Update'
type Update = Statement L0 T NS Update'
data UpdateLimited'
type UpdateLimited = Statement L0 NT NS UpdateLimited'
data Vacuum'
type Vacuum = Statement L0 NT NS Vacuum'

data Statement level triggerable valueReturning which where
    Explain
        :: ExplainableStatement
        -> Statement L1 NT NS Explain'
    ExplainQueryPlan
        :: ExplainableStatement
        -> Statement L1 NT NS ExplainQueryPlan'
    AlterTable
        :: SinglyQualifiedIdentifier
        -> AlterTableBody
        -> Statement L0 NT NS AlterTable'
    Analyze
        :: SinglyQualifiedIdentifier
        -> Statement L0 NT NS Analyze'
    Attach
        :: MaybeDatabase
        -> String
        -> UnqualifiedIdentifier
        -> Statement L0 NT NS Attach'
    Begin
        :: MaybeTransactionType
        -> MaybeTransaction
        -> Statement L0 NT NS Begin'
    Commit
        :: Bool
        -> MaybeTransaction
        -> Statement L0 NT NS Commit'
    CreateIndex
        :: MaybeUnique
        -> MaybeIfNotExists
        -> SinglyQualifiedIdentifier
        -> UnqualifiedIdentifier
        -> (OneOrMore IndexedColumn)
        -> Statement L0 NT NS CreateIndex'
    CreateTable
        :: Permanence
        -> MaybeIfNotExists
        -> SinglyQualifiedIdentifier
        -> EitherColumnsAndConstraintsSelect
        -> Statement L0 NT NS CreateTable'
    CreateTrigger
        :: Permanence
        -> MaybeIfNotExists
        -> SinglyQualifiedIdentifier
        -> TriggerTime
        -> TriggerCondition
        -> UnqualifiedIdentifier
        -> MaybeForEachRow
        -> (Maybe WhenClause)
        -> (OneOrMore TriggerStatement)
        -> Statement L0 NT NS CreateTrigger'
    CreateView
        :: Permanence
        -> MaybeIfNotExists
        -> SinglyQualifiedIdentifier
        -> (Statement L0 T S Select')
        -> Statement L0 NT NS CreateView'
    CreateVirtualTable
        :: SinglyQualifiedIdentifier
        -> UnqualifiedIdentifier
        -> [ModuleArgument]
        -> Statement L0 NT NS CreateVirtualTable'
    Delete
        :: QualifiedTableName
        -> (Maybe WhereClause)
        -> Statement L0 T NS Delete'
    DeleteLimited
        :: QualifiedTableName
        -> (Maybe WhereClause)
        -> (Maybe OrderClause)
        -> LimitClause
        -> Statement L0 NT NS DeleteLimited'
    Detach
        :: Bool
        -> UnqualifiedIdentifier
        -> Statement L0 NT NS Detach'
    DropIndex
        :: MaybeIfExists
        -> SinglyQualifiedIdentifier
        -> Statement L0 NT NS DropIndex'
    DropTable
        :: MaybeIfExists
        -> SinglyQualifiedIdentifier
        -> Statement L0 NT NS DropTable'
    DropTrigger
        :: MaybeIfExists
        -> SinglyQualifiedIdentifier
        -> Statement L0 NT NS DropTrigger'
    DropView
        :: MaybeIfExists
        -> SinglyQualifiedIdentifier
        -> Statement L0 NT NS DropView'
    Insert
        :: InsertHead
        -> SinglyQualifiedIdentifier
        -> InsertBody
        -> Statement L0 T NS Insert'
    Pragma
        :: SinglyQualifiedIdentifier
        -> PragmaBody
        -> Statement L0 NT NS Pragma'
    Reindex
        :: SinglyQualifiedIdentifier
        -> Statement L0 NT NS Reindex'
    Release
        :: MaybeReleaseSavepoint
        -> UnqualifiedIdentifier
        -> Statement L0 NT NS Release'
    Rollback
        :: MaybeTransaction
        -> MaybeSavepoint
        -> Statement L0 NT NS Rollback'
    Savepoint
        :: UnqualifiedIdentifier
        -> Statement L0 NT NS Savepoint'
    Select
        :: SelectCore
        -> [(CompoundOperator, SelectCore)]
        -> (Maybe OrderClause)
        -> (Maybe LimitClause)
        -> Statement L0 T S Select'
    Update
        :: UpdateHead
        -> QualifiedTableName
        -> (OneOrMore (UnqualifiedIdentifier, Expression))
        -> (Maybe WhereClause)
        -> Statement L0 T NS Update'
    UpdateLimited
        :: UpdateHead
        -> QualifiedTableName
        -> (OneOrMore (UnqualifiedIdentifier, Expression))
        -> (Maybe WhereClause)
        -> (Maybe OrderClause)
        -> LimitClause
        -> Statement L0 NT NS UpdateLimited'
    Vacuum
        :: Statement L0 NT NS Vacuum'
deriving instance Eq (Statement l t v w)
deriving instance Show (Statement l t v w)


instance ShowTokens (Statement l t v w) where
    showTokens (Explain statement)
        = [KeywordExplain]
          ++ showTokens statement
    showTokens (ExplainQueryPlan statement)
        = [KeywordExplain, KeywordQuery, KeywordPlan]
          ++ showTokens statement
    showTokens (AlterTable tableName alterTableBody)
        = [KeywordAlter, KeywordTable]
          ++ showTokens tableName
          ++ showTokens alterTableBody
    showTokens (Analyze analyzableThingName)
        = [KeywordAnalyze]
          ++ showTokens analyzableThingName
    showTokens (Attach maybeDatabase filename databaseName)
        = [KeywordAttach]
          ++ showTokens maybeDatabase
          ++ [LiteralString filename, KeywordAs]
          ++ showTokens databaseName
    showTokens (Begin maybeTransactionType maybeTransaction)
        = [KeywordBegin]
          ++ showTokens maybeTransactionType
          ++ showTokens maybeTransaction
    showTokens (Commit endKeywordInsteadOfCommitKeyword maybeTransaction)
        = (if endKeywordInsteadOfCommitKeyword
             then [KeywordEnd]
             else [KeywordCommit])
          ++ showTokens maybeTransaction
    showTokens (CreateIndex maybeUnique maybeIfNotExists indexName tableName
                            indexedColumns)
        = [KeywordCreate]
          ++ showTokens maybeUnique
          ++ [KeywordIndex]
          ++ showTokens maybeIfNotExists
          ++ showTokens indexName
          ++ showTokens tableName
          ++ [PunctuationLeftParenthesis]
          ++ (intercalate [PunctuationComma] $ mapOneOrMore showTokens indexedColumns)
          ++ [PunctuationRightParenthesis]
    showTokens (CreateTable permanence maybeIfNotExists name
                            eitherColumnsAndConstraintsSelect)
        = [KeywordCreate]
          ++ showTokens permanence
          ++ [KeywordTable]
          ++ showTokens maybeIfNotExists
          ++ showTokens name
          ++ showTokens eitherColumnsAndConstraintsSelect
    showTokens (CreateTrigger permanence maybeIfNotExists name time condition
                              tableName maybeForEachRow maybeWhenClause
                              statements)
        = [KeywordCreate]
          ++ showTokens permanence
          ++ [KeywordTrigger]
          ++ showTokens maybeIfNotExists
          ++ showTokens name
          ++ showTokens time
          ++ showTokens condition
          ++ showTokens tableName
          ++ showTokens maybeForEachRow
          ++ (case maybeWhenClause of
                Nothing -> []
                Just whenClause -> showTokens whenClause)
          ++ (intercalate [PunctuationSemicolon] $ mapOneOrMore showTokens statements)
          ++ [PunctuationSemicolon, KeywordEnd]
    showTokens (CreateView permanence maybeIfNotExists viewName selectStatement)
        = [KeywordCreate]
          ++ showTokens permanence
          ++ [KeywordView]
          ++ showTokens maybeIfNotExists
          ++ showTokens viewName
          ++ [KeywordAs]
          ++ showTokens selectStatement
    showTokens (CreateVirtualTable tableName moduleName moduleArguments)
        = [KeywordCreate, KeywordVirtual, KeywordTable]
          ++ showTokens tableName
          ++ [KeywordUsing]
          ++ showTokens moduleName
          ++ (case moduleArguments of
                [] -> []
                _ -> [PunctuationLeftParenthesis]
                     ++ (intercalate [PunctuationComma] $ map showTokens moduleArguments)
                     ++ [PunctuationRightParenthesis])
    showTokens (Delete qualifiedTableName maybeWhereClause)
        = [KeywordDelete, KeywordFrom]
          ++ showTokens qualifiedTableName
          ++ (case maybeWhereClause of
                Nothing -> []
                Just whereClause -> showTokens whereClause)
    showTokens (DeleteLimited qualifiedTableName maybeWhereClause maybeOrderClause
                              limitClause)
        = [KeywordDelete, KeywordFrom]
          ++ showTokens qualifiedTableName
          ++ (case maybeWhereClause of
                Nothing -> []
                Just whereClause -> showTokens whereClause)
          ++ (case maybeOrderClause of
                Nothing -> []
                Just orderClause -> showTokens orderClause)
          ++ showTokens limitClause
    showTokens (Detach databaseKeywordPresent databaseName)
        = [KeywordDetach]
          ++ (if databaseKeywordPresent
                then [KeywordDatabase]
                else [])
          ++ showTokens databaseName
    showTokens (DropIndex maybeIfExists indexName)
        = [KeywordDrop, KeywordIndex]
          ++ showTokens maybeIfExists
          ++ showTokens indexName
    showTokens (DropTable maybeIfExists tableName)
        = [KeywordDrop, KeywordTable]
          ++ showTokens maybeIfExists
          ++ showTokens tableName
    showTokens (DropTrigger maybeIfExists triggerName)
        = [KeywordDrop, KeywordTrigger]
          ++ showTokens maybeIfExists
          ++ showTokens triggerName
    showTokens (DropView maybeIfExists viewName)
        = [KeywordDrop, KeywordView]
          ++ showTokens maybeIfExists
          ++ showTokens viewName
    showTokens (Insert insertHead tableName insertBody)
        = showTokens insertHead
          ++ [KeywordInto]
          ++ showTokens tableName
          ++ showTokens insertBody
    showTokens (Pragma pragmaName pragmaBody)
        = [KeywordPragma]
          ++ showTokens pragmaName
          ++ showTokens pragmaBody
    showTokens (Reindex thingName)
        = [KeywordReindex]
          ++ showTokens thingName
    showTokens (Release maybeReleaseSavepoint savepointName)
        = [KeywordRelease]
          ++ showTokens maybeReleaseSavepoint
    showTokens (Rollback maybeTransaction maybeSavepoint)
        = [KeywordRollback]
          ++ showTokens maybeTransaction
          ++ showTokens maybeSavepoint
    showTokens (Savepoint savepointName)
        = [KeywordSavepoint]
          ++ showTokens savepointName
    showTokens (Select firstCore compoundCores maybeOrderClause maybeLimitClause)
        = showTokens firstCore
          ++ (concat $ map (\(compoundOperator, additionalCore) -> 
                                concat [showTokens compoundOperator,
                                        showTokens additionalCore])
                           compoundCores)
          ++ (case maybeOrderClause of
                Nothing -> []
                Just orderClause -> showTokens orderClause)
          ++ (case maybeLimitClause of
                Nothing -> []
                Just limitClause -> showTokens limitClause)
    showTokens (Update updateHead qualifiedTableName setOperations maybeWhereClause)
        = showTokens updateHead
          ++ showTokens qualifiedTableName
          ++ [KeywordSet]
          ++ (intercalate [PunctuationComma]
                          $ mapOneOrMore (\(columnName, expression) ->
                                           showTokens columnName
                                           ++ [PunctuationEquals]
                                           ++ showTokens expression)
                                         setOperations)
          ++ (case maybeWhereClause of
                Nothing -> []
                Just whereClause -> showTokens whereClause)
    showTokens (UpdateLimited updateHead qualifiedTableName setOperations
                    maybeWhereClause maybeOrderClause limitClause)
        = showTokens updateHead
          ++ showTokens qualifiedTableName
          ++ [KeywordSet]
          ++ (intercalate [PunctuationComma]
                          $ mapOneOrMore (\(columnName, expression) ->
                                           showTokens columnName
                                           ++ [PunctuationEquals]
                                           ++ showTokens expression)
                                         setOperations)
          ++ (case maybeWhereClause of
                Nothing -> []
                Just whereClause -> showTokens whereClause)
          ++ (case maybeOrderClause of
                Nothing -> []
                Just orderClause -> showTokens orderClause)
          ++ showTokens limitClause
    showTokens Vacuum
        = [KeywordVacuum]


class Identifier a where
    identifierProperName :: a -> String
    identifierParentName :: a -> Maybe String
    identifierGrandparentName :: a -> Maybe String


toDoublyQualifiedIdentifier :: (Identifier a) => a -> DoublyQualifiedIdentifier
toDoublyQualifiedIdentifier identifier =
    case (identifierGrandparentName identifier,
          identifierParentName identifier, 
          identifierProperName identifier) of
      (Nothing, Nothing, properName)
          -> DoublyQualifiedIdentifier Nothing properName
      (Nothing, Just parentName, properName)
          -> DoublyQualifiedIdentifier (Just (parentName, Nothing)) properName
      (Just grandparentName, Just parentName, properName)
          -> DoublyQualifiedIdentifier (Just (parentName, Just grandparentName))
                                       properName


data UnqualifiedIdentifier = UnqualifiedIdentifier String
                             deriving (Eq, Show)
instance Ord UnqualifiedIdentifier where
    compare (UnqualifiedIdentifier aProper)
            (UnqualifiedIdentifier bProper)
        = compare aProper bProper
instance ShowTokens UnqualifiedIdentifier where
    showTokens (UnqualifiedIdentifier properName)
        = [Identifier properName]
instance Identifier UnqualifiedIdentifier where
    identifierProperName (UnqualifiedIdentifier properName) = properName
    identifierParentName _ = Nothing
    identifierGrandparentName _ = Nothing


data SinglyQualifiedIdentifier
    = SinglyQualifiedIdentifier (Maybe String) String
      deriving (Eq, Show)
instance Ord SinglyQualifiedIdentifier where
    compare a b = let tupleForm (SinglyQualifiedIdentifier Nothing proper)
                          = (Nothing, Just proper)
                      tupleForm (SinglyQualifiedIdentifier (Just parent) proper)
                          = (Just parent, Just proper)
                  in compare (tupleForm a) (tupleForm b)
instance ShowTokens SinglyQualifiedIdentifier where
    showTokens (SinglyQualifiedIdentifier Nothing properName)
        = [Identifier properName]
    showTokens (SinglyQualifiedIdentifier (Just databaseName) properName)
        = [Identifier databaseName, PunctuationDot, Identifier properName]
instance Identifier SinglyQualifiedIdentifier where
    identifierProperName (SinglyQualifiedIdentifier _ properName) = properName
    identifierParentName (SinglyQualifiedIdentifier maybeParentName _) = maybeParentName
    identifierGrandparentName _ = Nothing


data DoublyQualifiedIdentifier
    = DoublyQualifiedIdentifier (Maybe (String, (Maybe String))) String
      deriving (Eq, Show)
instance Ord DoublyQualifiedIdentifier where
    compare a b = let tupleForm (DoublyQualifiedIdentifier Nothing proper)
                          = (Nothing, Nothing, Just proper)
                      tupleForm (DoublyQualifiedIdentifier
                                 (Just (parent, Nothing)) proper)
                          = (Nothing, Just parent, Just proper)
                      tupleForm (DoublyQualifiedIdentifier
                                 (Just (parent, Just grandparent)) proper)
                          = (Just grandparent, Just parent, Just proper)
                  in compare (tupleForm a) (tupleForm b)
instance ShowTokens DoublyQualifiedIdentifier where
    showTokens (DoublyQualifiedIdentifier Nothing properName)
        = [Identifier properName]
    showTokens (DoublyQualifiedIdentifier (Just (tableName, Nothing)) properName)
        = [Identifier tableName,
           PunctuationDot,
           Identifier properName]
    showTokens (DoublyQualifiedIdentifier
                (Just (tableName, Just databaseName)) properName)
        = [Identifier databaseName,
           PunctuationDot,
           Identifier tableName,
           PunctuationDot,
           Identifier properName]
instance Identifier DoublyQualifiedIdentifier where
    identifierProperName (DoublyQualifiedIdentifier _ properName) = properName
    identifierParentName
      (DoublyQualifiedIdentifier (Just (parentName, _)) _) = Just parentName
    identifierParentName _ = Nothing
    identifierGrandparentName
      (DoublyQualifiedIdentifier (Just (_, maybeGrandparentName)) _)
          = maybeGrandparentName
    identifierGrandparentName _ = Nothing


data Token = EndOfInputToken
           | Identifier String
           | LiteralInteger Word64
           | LiteralFloat NonnegativeDouble
           | LiteralString String
           | LiteralBlob BS.ByteString
           | Variable
           | VariableN Word64
           | VariableNamed String
           | ModuleArgumentToken String
           | PunctuationBarBar
           | PunctuationStar
           | PunctuationSlash
           | PunctuationPercent
           | PunctuationPlus
           | PunctuationMinus
           | PunctuationLessLess
           | PunctuationGreaterGreater
           | PunctuationAmpersand
           | PunctuationBar
           | PunctuationLess
           | PunctuationLessEquals
           | PunctuationGreater
           | PunctuationGreaterEquals
           | PunctuationEquals
           | PunctuationEqualsEquals
           | PunctuationBangEquals
           | PunctuationLessGreater
           | PunctuationTilde
           | PunctuationLeftParenthesis
           | PunctuationRightParenthesis
           | PunctuationComma
           | PunctuationDot
           | PunctuationSemicolon
           | KeywordAbort
           | KeywordAction
           | KeywordAdd
           | KeywordAfter
           | KeywordAll
           | KeywordAlter
           | KeywordAnalyze
           | KeywordAnd
           | KeywordAs
           | KeywordAsc
           | KeywordAttach
           | KeywordAutoincrement
           | KeywordBefore
           | KeywordBegin
           | KeywordBetween
           | KeywordBy
           | KeywordCascade
           | KeywordCase
           | KeywordCast
           | KeywordCheck
           | KeywordCollate
           | KeywordColumn
           | KeywordCommit
           | KeywordConflict
           | KeywordConstraint
           | KeywordCreate
           | KeywordCross
           | KeywordCurrentDate
           | KeywordCurrentTime
           | KeywordCurrentTimestamp
           | KeywordDatabase
           | KeywordDefault
           | KeywordDeferrable
           | KeywordDeferred
           | KeywordDelete
           | KeywordDesc
           | KeywordDetach
           | KeywordDistinct
           | KeywordDrop
           | KeywordEach
           | KeywordElse
           | KeywordEnd
           | KeywordEscape
           | KeywordExcept
           | KeywordExclusive
           | KeywordExists
           | KeywordExplain
           | KeywordFail
           | KeywordFor
           | KeywordForeign
           | KeywordFrom
           | KeywordFull
           | KeywordGlob
           | KeywordGroup
           | KeywordHaving
           | KeywordIf
           | KeywordIgnore
           | KeywordImmediate
           | KeywordIn
           | KeywordIndex
           | KeywordIndexed
           | KeywordInitially
           | KeywordInner
           | KeywordInsert
           | KeywordInstead
           | KeywordIntersect
           | KeywordInto
           | KeywordIs
           | KeywordIsnull
           | KeywordJoin
           | KeywordKey
           | KeywordLeft
           | KeywordLike
           | KeywordLimit
           | KeywordMatch
           | KeywordNatural
           | KeywordNo
           | KeywordNot
           | KeywordNotnull
           | KeywordNull
           | KeywordOf
           | KeywordOffset
           | KeywordOn
           | KeywordOr
           | KeywordOrder
           | KeywordOuter
           | KeywordPlan
           | KeywordPragma
           | KeywordPrimary
           | KeywordQuery
           | KeywordRaise
           | KeywordReferences
           | KeywordRegexp
           | KeywordReindex
           | KeywordRelease
           | KeywordRename
           | KeywordReplace
           | KeywordRestrict
           | KeywordRight
           | KeywordRollback
           | KeywordRow
           | KeywordSavepoint
           | KeywordSelect
           | KeywordSet
           | KeywordTable
           | KeywordTemp
           | KeywordTemporary
           | KeywordThen
           | KeywordTo
           | KeywordTransaction
           | KeywordTrigger
           | KeywordUnion
           | KeywordUnique
           | KeywordUpdate
           | KeywordUsing
           | KeywordVacuum
           | KeywordValues
           | KeywordView
           | KeywordVirtual
           | KeywordWhen
           | KeywordWhere


instance Show Token where
    show EndOfInputToken = "<eof>"
    show (Identifier identifier) =
        let validCharacter c = if isAscii c
                                 then (isAlphaNum c) || (elem c "_$")
                                 else True
            escapeCharacter '"' = "\"\""
            escapeCharacter c = [c]
        in if (all validCharacter identifier) && (not $ elem identifier keywordList)
             then identifier
             else "\"" ++ (concat $ map escapeCharacter identifier) ++ "\""
    show (LiteralInteger integer) = show integer
    show (LiteralFloat nonnegativeDouble) = show $ fromNonnegativeDouble nonnegativeDouble
    show (LiteralString string) =
        let showChar char = case char of
                              '\'' -> "''"
                              _ -> [char]
            showString string = concat $ map showChar string
        in "'" ++ showString string ++ "'"
    show (LiteralBlob bytestring) =
        let showWord word = case showHex word "" of
                              [a] -> ['0', a]
                              a -> a
            showBytestring bytestring = concat $ map showWord $ BS.unpack bytestring
        in "x'" ++ showBytestring bytestring ++ "'"
    show Variable = "?"
    show (VariableN n) = "?" ++ (show n)
    show (VariableNamed name) = ":" ++ name
    show (ModuleArgumentToken string) = string
    show PunctuationBarBar = "||"
    show PunctuationStar = "*"
    show PunctuationSlash = "/"
    show PunctuationPercent = "%"
    show PunctuationPlus = "+" 
    show PunctuationMinus = "-"
    show PunctuationLessLess = "<<"
    show PunctuationGreaterGreater = ">>"
    show PunctuationAmpersand = "&"
    show PunctuationBar = "|"
    show PunctuationLess = "<"
    show PunctuationLessEquals = "<="
    show PunctuationGreater = ">"
    show PunctuationGreaterEquals = ">="
    show PunctuationEquals = "="
    show PunctuationEqualsEquals = "=="
    show PunctuationBangEquals = "!="
    show PunctuationLessGreater = "<>"
    show PunctuationTilde = "~"
    show PunctuationLeftParenthesis = "("
    show PunctuationRightParenthesis = ")"
    show PunctuationComma = ","
    show PunctuationDot = "."
    show PunctuationSemicolon = ";"
    show KeywordAbort = "ABORT"
    show KeywordAction = "ACTION"
    show KeywordAdd = "ADD"
    show KeywordAfter = "AFTER"
    show KeywordAll = "ALL"
    show KeywordAlter = "ALTER"
    show KeywordAnalyze = "ANALYZE"
    show KeywordAnd = "AND"
    show KeywordAs = "AS"
    show KeywordAsc = "ASC"
    show KeywordAttach = "ATTACH"
    show KeywordAutoincrement = "AUTOINCREMENT"
    show KeywordBefore = "BEFORE"
    show KeywordBegin = "BEGIN"
    show KeywordBetween = "BETWEEN"
    show KeywordBy = "BY"
    show KeywordCascade = "CASCADE"
    show KeywordCase = "CASE"
    show KeywordCast = "CAST"
    show KeywordCheck = "CHECK"
    show KeywordCollate = "COLLATE"
    show KeywordColumn = "COLUMN"
    show KeywordCommit = "COMMIT"
    show KeywordConflict = "CONFLICT"
    show KeywordConstraint = "CONSTRAINT"
    show KeywordCreate = "CREATE"
    show KeywordCross = "CROSS"
    show KeywordCurrentDate = "CURRENT_DATE"
    show KeywordCurrentTime = "CURRENT_TIME"
    show KeywordCurrentTimestamp = "CURRENT_TIMESTAMP"
    show KeywordDatabase = "DATABASE"
    show KeywordDefault = "DEFAULT"
    show KeywordDeferrable = "DEFERRABLE"
    show KeywordDeferred = "DEFERRED"
    show KeywordDelete = "DELETE"
    show KeywordDesc = "DESC"
    show KeywordDetach = "DETACH"
    show KeywordDistinct = "DISTINCT"
    show KeywordDrop = "DROP"
    show KeywordEach = "EACH"
    show KeywordElse = "ELSE"
    show KeywordEnd = "END"
    show KeywordEscape = "ESCAPE"
    show KeywordExcept = "EXCEPT"
    show KeywordExclusive = "EXCLUSIVE"
    show KeywordExists = "EXISTS"
    show KeywordExplain = "EXPLAIN"
    show KeywordFail = "FAIL"
    show KeywordFor = "FOR"
    show KeywordForeign = "FOREIGN"
    show KeywordFrom = "FROM"
    show KeywordFull = "FULL"
    show KeywordGlob = "GLOB"
    show KeywordGroup = "GROUP"
    show KeywordHaving = "HAVING"
    show KeywordIf = "IF"
    show KeywordIgnore = "IGNORE"
    show KeywordImmediate = "IMMEDIATE"
    show KeywordIn = "IN"
    show KeywordIndex = "INDEX"
    show KeywordIndexed = "INDEXED"
    show KeywordInitially = "INITIALLY"
    show KeywordInner = "INNER"
    show KeywordInsert = "INSERT"
    show KeywordInstead = "INSTEAD"
    show KeywordIntersect = "INTERSECT"
    show KeywordInto = "INTO"
    show KeywordIs = "IS"
    show KeywordIsnull = "ISNULL"
    show KeywordJoin = "JOIN"
    show KeywordKey = "KEY"
    show KeywordLeft = "LEFT"
    show KeywordLike = "LIKE"
    show KeywordLimit = "LIMIT"
    show KeywordMatch = "MATCH"
    show KeywordNatural = "NATURAL"
    show KeywordNo = "NO"
    show KeywordNot = "NOT"
    show KeywordNotnull = "NOTNULL"
    show KeywordNull = "NULL"
    show KeywordOf = "OF"
    show KeywordOffset = "OFFSET"
    show KeywordOn = "ON"
    show KeywordOr = "OR"
    show KeywordOrder = "ORDER"
    show KeywordOuter = "OUTER"
    show KeywordPlan = "PLAN"
    show KeywordPragma = "PRAGMA"
    show KeywordPrimary = "PRIMARY"
    show KeywordQuery = "QUERY"
    show KeywordRaise = "RAISE"
    show KeywordReferences = "REFERENCES"
    show KeywordRegexp = "REGEXP"
    show KeywordReindex = "REINDEX"
    show KeywordRelease = "RELEASE"
    show KeywordRename = "RENAME"
    show KeywordReplace = "REPLACE"
    show KeywordRestrict = "RESTRICT"
    show KeywordRight = "RIGHT"
    show KeywordRollback = "ROLLBACK"
    show KeywordRow = "ROW"
    show KeywordSavepoint = "SAVEPOINT"
    show KeywordSelect = "SELECT"
    show KeywordSet = "SET"
    show KeywordTable = "TABLE"
    show KeywordTemp = "TEMP"
    show KeywordTemporary = "TEMPORARY"
    show KeywordThen = "THEN"
    show KeywordTo = "TO"
    show KeywordTransaction = "TRANSACTION"
    show KeywordTrigger = "TRIGGER"
    show KeywordUnion = "UNION"
    show KeywordUnique = "UNIQUE"
    show KeywordUpdate = "UPDATE"
    show KeywordUsing = "USING"
    show KeywordVacuum = "VACUUM"
    show KeywordValues = "VALUES"
    show KeywordView = "VIEW"
    show KeywordVirtual = "VIRTUAL"
    show KeywordWhen = "WHEN"
    show KeywordWhere = "WHERE"
    showList [] string = "" ++ string
    showList (onlyToken:[]) string
        = show onlyToken ++ string
    showList (firstToken:rest@(PunctuationComma:_)) string
        = show firstToken ++ show rest
    showList (firstToken:rest@(PunctuationSemicolon:_)) string
        = show firstToken ++ show rest
    showList (firstToken:rest@(PunctuationDot:_)) string
        = show firstToken ++ show rest
    showList (firstToken:rest@(PunctuationRightParenthesis:_)) string
        = show firstToken ++ show rest
    showList (PunctuationSemicolon:rest@(_:_)) string
        = show PunctuationSemicolon ++ "\n" ++ show rest
    showList (PunctuationDot:rest@(_:_)) string
        = show PunctuationDot ++ show rest
    showList (PunctuationLeftParenthesis:rest@(_:_)) string
        = show PunctuationLeftParenthesis ++ show rest
    showList (firstToken:rest) string
        = show firstToken ++ " " ++ show rest ++ string


keywordList :: [String]
keywordList
    = ["ABORT", "ACTION", "ADD", "AFTER", "ALL", "ALTER", "ANALYZE", "AND", "AS",
       "ASC", "ATTACH", "AUTOINCREMENT", "BEFORE", "BEGIN", "BETWEEN", "BY", "CASCADE",
       "CASE", "CAST", "CHECK", "COLLATE", "COLUMN", "COMMIT", "CONFLICT", "CONSTRAINT",
       "CREATE", "CROSS", "CURRENT_DATE", "CURRENT_TIME", "CURRENT_TIMESTAMP",
       "DATABASE", "DEFAULT", "DEFERRABLE", "DEFERRED", "DELETE", "DESC", "DETACH",
       "DISTINCT", "DROP", "EACH", "ELSE", "END", "ESCAPE", "EXCEPT", "EXCLUSIVE",
       "EXISTS", "EXPLAIN", "FAIL", "FOR", "FOREIGN", "FROM", "FULL", "GLOB", "GROUP",
       "HAVING", "IF", "IGNORE", "IMMEDIATE", "IN", "INDEX", "INDEXED", "INITIALLY",
       "INNER", "INSERT", "INSTEAD", "INTERSECT", "INTO", "IS", "ISNULL", "JOIN",
       "KEY", "LEFT", "LIKE", "LIMIT", "MATCH", "NATURAL", "NO", "NOT", "NOTNULL",
       "NULL", "OF", "OFFSET", "ON", "OR", "ORDER", "OUTER", "PLAN", "PRAGMA",
       "PRIMARY", "QUERY", "RAISE", "REFERENCES", "REGEXP", "REINDEX", "RELEASE",
       "RENAME", "REPLACE", "RESTRICT", "RIGHT", "ROLLBACK", "ROW", "SAVEPOINT",
       "SELECT", "SET", "TABLE", "TEMP", "TEMPORARY", "THEN", "TO", "TRANSACTION",
       "TRIGGER", "UNION", "UNIQUE", "UPDATE", "USING", "VACUUM", "VALUES", "VIEW",
       "VIRTUAL", "WHEN", "WHERE"]
