{-# LANGUAGE GADTs, EmptyDataDecls, FlexibleInstances, ExistentialQuantification,
             StandaloneDeriving, TypeSynonymInstances #-}
module Language.SQL.SQLite.Types (
                                  -- * Building blocks
                                  ShowTokens(..),
                                  OneOrMore,
                                  mkOneOrMore,
                                  fromOneOrMore,
                                  NonnegativeDouble,
                                  mkNonnegativeDouble,
                                  fromNonnegativeDouble,
                                  computeTypeNameAffinity,
                                  computeAffinityTypeName,
                                  Identifier(..),
                                  toDoublyQualifiedIdentifier,
                                  UnqualifiedIdentifier(..),
                                  SinglyQualifiedIdentifier(..),
                                  DoublyQualifiedIdentifier(..),
                                  Token(..),
                                  -- * Abstract syntax tree nodes
                                  -- | There are a great many types of nodes in the
                                  --   abstract syntax tree.  They are loosely divided
                                  --   into
                                  --   statements (commands to possibly be executed),
                                  --   expressions (algebraic expressions to possibly
                                  --   be evaluated),
                                  --   clauses (major portions of a statement or
                                  --   expression which have very complicated
                                  --   grammatical structure),
                                  --   subclauses (portions of clauses which still have
                                  --   some grammatical structure),
                                  --   qualifiers (things which have minimal grammatical
                                  --   structure of their own, but can be present and
                                  --   cause some change in semantics if they are),
                                  --   keywords (things which have minimal grammatical
                                  --   structure of their own, and no semantic meaning
                                  --   either, but can be present),
                                  --   heads (within a statement, groups of multiple
                                  --   clauses which include the verb of the
                                  --   statement),
                                  --   and
                                  --   bodies (within a statement, groups of multiple
                                  --   clauses which do not include the verb of the
                                  --   statement).
                                  --   
                                  --   The guiding principle behind the selection of
                                  --   which things to give their own node-types to is
                                  --   that it should be possible to parse SQL and
                                  --   print it back out identically except for
                                  --   whitespace.  This means for example that @!=@
                                  --   and @<>@ are distinct in the AST, as are
                                  --   @NOT NULL@ and @NOTNULL@, and is the rationale
                                  --   behind the inclusion of the "keywords" category
                                  --   which has no semantic meaning.  A likely use of
                                  --   this library is to implement a system which allows
                                  --   the same queries to be edited both as plaintext
                                  --   SQL and as some graphical form, and if a user
                                  --   edits as SQL, he expects these things to be
                                  --   preserved, as they can be important to
                                  --   readability.
                                  --   
                                  --   When a qualifier is omitted, it's prefixed with
                                  --   @No@ as in 'NoIfNotExists'.  When a keyword is
                                  --   omitted, it's prefixed with @Elided@ as in
                                  --   'ElidedTransaction'.  This is to remind you that
                                  --   an omitted qualifier has some sensible default
                                  --   semantic, whereas an omitted keyword has the
                                  --   same semantics as if it were present.
                                  --   
                                  --   There is a great deal of sharing of structure,
                                  --   so I have made no attempt in this documentation to
                                  --   organize the exports by category, except to give
                                  --   expressions and statements their own sections;
                                  --   instead, please enjoy this alphabetical index!
                                  AlterTableBody(..),
                                  CasePair(..),
                                  ColumnConstraint(..),
                                  ColumnDefinition(..),
                                  CommitHead(..),
                                  CompoundOperator(..),
                                  ConflictClause(..),
                                  DefaultValue(..),
                                  Distinctness(..),
                                  CreateTableBody(..),
                                  Else(..),
                                  Escape(..),
                                  ForeignKeyClause(..),
                                  ForeignKeyClauseActionOrMatchPart(..),
                                  ForeignKeyClauseActionPart(..),
                                  FromClause(..),
                                  GroupClause(..),
                                  IndexedColumn(..),
                                  InsertBody(..),
                                  InsertHead(..),
                                  JoinConstraint(..),
                                  JoinOperation(..),
                                  JoinSource(..),
                                  LikeType(..),
                                  LimitClause(..),
                                  MaybeAs(..),
                                  MaybeAscDesc(..),
                                  MaybeAutoincrement(..),
                                  MaybeCollation(..),
                                  MaybeColumn(..),
                                  MaybeConstraintName(..),
                                  MaybeDatabase(..),
                                  MaybeForEachRow(..),
                                  MaybeForeignKeyClauseDeferrablePart(..),
                                  MaybeHaving(..),
                                  MaybeIfExists(..),
                                  MaybeIfNotExists(..),
                                  MaybeIndexedBy(..),
                                  MaybeInitialDeferralStatus(..),
                                  MaybeReleaseSavepoint(..),
                                  MaybeSavepoint(..),
                                  MaybeSign(..),
                                  MaybeSwitchExpression(..),
                                  MaybeTemporary(..),
                                  MaybeTransaction(..),
                                  MaybeTransactionType(..),
                                  MaybeType(..),
                                  MaybeTypeName(..),
                                  MaybeTypeSize(..),
                                  MaybeUnique(..),
                                  ModuleArgument(..),
                                  OrderClause(..),
                                  OrderingTerm(..),
                                  PragmaBody(..),
                                  PragmaValue(..),
                                  QualifiedTableName(..),
                                  ResultColumn(..),
                                  SelectCore(..),
                                  SingleSource(..),
                                  StatementList(..),
                                  TableConstraint(..),
                                  TriggerCondition(..),
                                  TriggerTime(..),
                                  Type(..),
                                  TypeAffinity(..),
                                  TypeSizeField(..),
                                  UpdateHead(..),
                                  WhenClause(..),
                                  WhereClause(..),
                                  
                                  -- * Abstract syntax tree nodes - Expressions
                                  Expression(..),
                                  
                                  -- * Abstract syntax tree nodes - Statements
                                  AnyStatement(..),
                                  fromAnyStatement,
                                  ExplainableStatement(..),
                                  fromExplainableStatement,
                                  TriggerStatement(..),
                                  fromTriggerStatement,
                                  Statement(..),
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
                                  Explain,
                                  ExplainQueryPlan,
                                  Insert,
                                  Pragma,
                                  Reindex,
                                  Release,
                                  Rollback,
                                  Savepoint,
                                  Select,
                                  Update,
                                  UpdateLimited,
                                  Vacuum
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


-- | A class implemented by every node of the AST; converts the node and its
--   children into a list of tokens which correspond to the SQL representation
--   of the node.
class ShowTokens a where
    showTokens :: a -> [Token]

-- | A class with hidden implementation so as to enforce the constraint that
--   it is a nonempty homogeneous list of items.
data OneOrMore a = MkOneOrMore [a]
                   deriving (Eq)

instance (Show a) => Show (OneOrMore a) where
    show (MkOneOrMore list) = "1" ++ show list

-- | The constructor for 'OneOrMore' @a@.  Returns 'Nothing' if the list it's
--   given is empty, or 'Just' 'OneOrMore' @a@ if it is not.
mkOneOrMore :: [a] -> Maybe (OneOrMore a)
mkOneOrMore [] = Nothing
mkOneOrMore list = Just $ MkOneOrMore list

mapOneOrMore :: (a -> b) -> (OneOrMore a) -> [b]
mapOneOrMore function (MkOneOrMore list) = map function list

-- | The accessor for 'OneOrMore' @a@.  Returns @[a]@.
fromOneOrMore :: (OneOrMore a) -> [a]
fromOneOrMore (MkOneOrMore list) = list

-- | A class with hidden implementation so as to enforce the constraint that
--   it is a nonnegative double.
data NonnegativeDouble = MkNonnegativeDouble Double
                         deriving (Eq)

instance Show NonnegativeDouble where
    show (MkNonnegativeDouble double) = "+" ++ show double

-- | The constructor for 'NonnegativeDouble'.  Returns 'Nothing' if the double it's
--   given is negative, or 'Just' 'NonnegativeDouble' if it is not.
mkNonnegativeDouble :: Double -> Maybe NonnegativeDouble
mkNonnegativeDouble double =
    if double < 0.0
       then Nothing
       else Just $ MkNonnegativeDouble double

-- | The accessor for 'NonnegativeDouble'.  Returns a double.
fromNonnegativeDouble :: NonnegativeDouble -> Double
fromNonnegativeDouble (MkNonnegativeDouble double) = double

-- | Computes a 'TypeAffinity' from a 'MaybeTypeName', as used in
--   'Type'.
computeTypeNameAffinity :: MaybeTypeName -> TypeAffinity
computeTypeNameAffinity NoTypeName = TypeAffinityNone
computeTypeNameAffinity (TypeName oneOrMoreIdentifier) =
  let identifiers = fromOneOrMore oneOrMoreIdentifier
      containsString string =
        foldl (\result (UnqualifiedIdentifier identifier) ->
                 if result
                   then result
                   else isInfixOf string $ map toUpper identifier)
              False
              identifiers
  in if containsString "INT"
       then TypeAffinityInteger
       else if containsString "CHAR"
               || containsString "CLOB"
               || containsString "TEXT"
              then TypeAffinityText
              else if containsString "BLOB"
                     then TypeAffinityNone
                     else if containsString "REAL"
                             || containsString "FLOA"
                             || containsString "DOUB"
                            then TypeAffinityReal
                            else TypeAffinityNumeric


-- | Computes a 'MaybeTypeName' from a 'TypeAffinity', as used in
--   'Type'.
computeAffinityTypeName :: TypeAffinity -> MaybeTypeName
computeAffinityTypeName TypeAffinityText
  = TypeName $ MkOneOrMore [UnqualifiedIdentifier "TEXT"]
computeAffinityTypeName TypeAffinityNumeric
  = TypeName $ MkOneOrMore [UnqualifiedIdentifier "NUMERIC"]
computeAffinityTypeName TypeAffinityInteger
  = TypeName $ MkOneOrMore [UnqualifiedIdentifier "INTEGER"]
computeAffinityTypeName TypeAffinityReal
  = TypeName $ MkOneOrMore [UnqualifiedIdentifier "REAL"]
computeAffinityTypeName TypeAffinityNone
  = NoTypeName


-- | The AST node corresponding to a column or value type.  Used by
--   'MaybeType' which is used by 'ColumnDefinition', and by 'ExpressionCast'.
data Type = Type TypeAffinity
                 MaybeTypeName
                 MaybeTypeSize
            deriving (Eq, Show)
instance ShowTokens Type where
    showTokens (Type affinity name maybeTypeSize)
        = (case name of
            NoTypeName -> showTokens affinity
            _ -> showTokens name)
          ++ showTokens maybeTypeSize


-- | The AST node corresponding to the affinity of a column or value type.
--   Used by 'Type'.
data TypeAffinity = TypeAffinityText
                  | TypeAffinityNumeric
                  | TypeAffinityInteger
                  | TypeAffinityReal
                  | TypeAffinityNone
                    deriving (Eq, Show)
instance ShowTokens TypeAffinity where
    showTokens affinity = showTokens $ computeAffinityTypeName affinity


data MaybeTypeName = NoTypeName
                   | TypeName (OneOrMore UnqualifiedIdentifier)
                     deriving (Eq, Show)
instance ShowTokens MaybeTypeName where
    showTokens NoTypeName = []
    showTokens (TypeName identifiers) =
      concat $ mapOneOrMore showTokens identifiers


-- | The AST node corresponding to an optional column type.  Used by 'ColumnDefinition'.
data MaybeType = NoType
               | JustType Type
                 deriving (Eq, Show)
instance ShowTokens MaybeType where
    showTokens NoType = []
    showTokens (JustType type') = showTokens type'

-- | The AST node corresponding to an optional size annotation on a column or value
--   type.  Used by 'Type'.
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

-- | The AST node corresponding to one of zero to two fields annotating a column or
--   value type with size limits.  Used by 'MaybeTypeSize'.
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

-- | The AST node corresponding to a textual comparison operator in an expression.
--   Used by 'ExpressionLike'.
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

-- | The AST node corresponding to the @ESCAPE@ subclause of a textual comparison
--   expression.  Used by 'ExpressionLike'.
data Escape = NoEscape | Escape Expression
              deriving (Eq, Show)
instance ShowTokens Escape where
    showTokens NoEscape = []
    showTokens (Escape expression)
        = [KeywordEscape]
          ++ showTokens expression

-- | The AST node corresponding to the optional first subexpression in a @CASE@
--   expression.  Used by 'ExpressionCase'.
data MaybeSwitchExpression = NoSwitch | Switch Expression
                             deriving (Eq, Show)
instance ShowTokens MaybeSwitchExpression where
    showTokens NoSwitch = []
    showTokens (Switch expression) = showTokens expression

-- | The AST node corresponding to each @WHEN@-@THEN@ pair of subexpressions in a
--   @CASE@ expression.  Used by 'ExpressionCase'.
data CasePair = WhenThen Expression Expression
                deriving (Eq, Show)
instance ShowTokens CasePair where
    showTokens (WhenThen condition result)
        = [KeywordWhen]
          ++ showTokens condition
          ++ [KeywordThen]
          ++ showTokens result

-- | The AST node corresponding to the optional @ELSE@ subclause in a @CASE@ expression.
--   Used by 'ExpressionCase'.
data Else = NoElse
          | Else Expression
            deriving (Eq, Show)
instance ShowTokens Else where
    showTokens NoElse = []
    showTokens (Else expression) = [KeywordElse] ++ showTokens expression

-- | The AST node corresponding to an expression.  Used by 'DefaultValue',
--   'ColumnConstraint', 'TableConstraint', 'OrderingTerm', 'InsertBody',
--   'MaybeHaving', 'ResultColumn', 'JoinConstraint', 'WhereClause', 'WhenClause',
--   'Update', and 'UpdateLimited'.  Also useful at top level.
data Expression = ExpressionLiteralInteger Word64
                -- ^ Represents a literal integer expression.
                | ExpressionLiteralFloat NonnegativeDouble
                -- ^ Represents a literal floating-point expression.
                | ExpressionLiteralString String
                -- ^ Represents a literal string expression.
                | ExpressionLiteralBlob BS.ByteString
                -- ^ Represents a literal blob (binary large object) expression.
                | ExpressionLiteralNull
                -- ^ Represents a literal @NULL@ expression.
                | ExpressionLiteralCurrentTime
                -- ^ Represents a literal @current_time@ expression.
                | ExpressionLiteralCurrentDate
                -- ^ Represents a literal @current_date@ expression.
                | ExpressionLiteralCurrentTimestamp
                -- ^ Represents a literal @current_timestamp@ expression.
                | ExpressionVariable
                -- ^ Represents a positional-variable expression, written in SQL as @?@.
                | ExpressionVariableN Word64
                -- ^ Represents a numbered positional variable expression, written in
                --   SQL as @?nnn@.
                | ExpressionVariableNamed String
                -- ^ Represents a named positional variable expression, written in
                --   SQL as @:aaaa@.
                | ExpressionIdentifier DoublyQualifiedIdentifier
                -- ^ Represents a column-name expression, optionally qualified by a
                --   table name and further by a database name.
                | ExpressionUnaryNegative Expression
                -- ^ Represents a unary negation expression.
                | ExpressionUnaryPositive Expression
                -- ^ Represents a unary positive-sign expression.  Yes, this is an nop.
                | ExpressionUnaryBitwiseNot Expression
                -- ^ Represents a unary bitwise negation expression.
                | ExpressionUnaryLogicalNot Expression
                -- ^ Represents a unary logical negation expression.
                | ExpressionBinaryConcatenate Expression Expression
                -- ^ Represents a binary string-concatenation expression.
                | ExpressionBinaryMultiply Expression Expression
                -- ^ Represents a binary multiplication expression.
                | ExpressionBinaryDivide Expression Expression
                -- ^ Represents a binary division expression.
                | ExpressionBinaryModulus Expression Expression
                -- ^ Represents a binary modulus expression.
                | ExpressionBinaryAdd Expression Expression
                -- ^ Represents a binary addition expression.
                | ExpressionBinarySubtract Expression Expression
                -- ^ Represents a binary subtraction expression.
                | ExpressionBinaryLeftShift Expression Expression
                -- ^ Represents a binary left-shift expression.
                | ExpressionBinaryRightShift Expression Expression
                -- ^ Represents a binary right-shift expression.
                | ExpressionBinaryBitwiseAnd Expression Expression
                -- ^ Represents a binary bitwise-and expression.
                | ExpressionBinaryBitwiseOr Expression Expression
                -- ^ Represents a binary bitwise-or expression.
                | ExpressionBinaryLess Expression Expression
                -- ^ Represents a binary less-than comparison expression.
                | ExpressionBinaryLessEquals Expression Expression
                -- ^ Represents a binary less-than-or-equal-to comparison expression.
                | ExpressionBinaryGreater Expression Expression
                -- ^ Represents a binary greater-than comparison expression.
                | ExpressionBinaryGreaterEquals Expression Expression
                -- ^ Represents a binary greater-than-or-equal-to comparison expression.
                | ExpressionBinaryEquals Expression Expression
                -- ^ Represents a binary equal-to comparison expression, written in SQL
                --   as @=@.
                | ExpressionBinaryEqualsEquals Expression Expression
                -- ^ Represents a binary equal-to comparison expression, written in SQL
                --   as @==@.
                | ExpressionBinaryNotEquals Expression Expression
                -- ^ Represents a binary not-equal-to comparison expression, written in
                --   SQL as @!=@.
                | ExpressionBinaryLessGreater Expression Expression
                -- ^ Represents a binary not-equal-to comparison expression, written in
                --   SQL as @<>@.
                | ExpressionBinaryLogicalAnd Expression Expression
                -- ^ Represents a binary logical-and expression.
                | ExpressionBinaryLogicalOr Expression Expression
                -- ^ Represents a binary logical-or expression.
                | ExpressionFunctionCall UnqualifiedIdentifier [Expression]
                -- ^ Represents a call to a built-in function.
                | ExpressionFunctionCallDistinct UnqualifiedIdentifier
                                                 (OneOrMore Expression)
                -- ^ Represents a call to a built-in function, with the @DISTINCT@
                --   qualifier.
                | ExpressionFunctionCallStar UnqualifiedIdentifier
                -- ^ Represents a call to a built-in function, with @*@ as 
                --   parameter.
                | ExpressionCast Expression Type
                -- ^ Represents a type-cast expression.
                | ExpressionCollate Expression UnqualifiedIdentifier
                -- ^ Represents a @COLLATE@ expression.
                | ExpressionLike Expression LikeType Expression Escape
                -- ^ Represents a textual comparison expression.
                | ExpressionIsnull Expression
                -- ^ Represents an @ISNULL@ expression.  Not to be confused with an
                --   @IS@ expression with a literal @NULL@ as its right side; the
                --   meaning is the same but the parsing is different.
                | ExpressionNotnull Expression
                -- ^ Represents a @NOTNULL@ expression.  Not to be confused with a
                --   @NOT NULL@ expression; the meaning is the same but the parsing is
                --   different.
                | ExpressionNotNull Expression
                -- ^ Represents a @NOT NULL@ expression.  Not to be confused with a
                --   @NOTNULL@ expression; the meaning is the same but the parsing is
                --   different.
                | ExpressionIs Expression Expression
                -- ^ Represents an @IS@ expression.
                | ExpressionIsNot Expression Expression
                -- ^ Represents an @IS NOT@ expression.
                | ExpressionBetween Expression Expression Expression
                -- ^ Represents a @BETWEEN@ expression.
                | ExpressionNotBetween Expression Expression Expression
                -- ^ Represents a @NOT BETWEEN@ expression.
                | ExpressionInSelect Expression (Select)
                -- ^ Represents an @IN@ expression with the right-hand side being a
                --   @SELECT@ statement.
                | ExpressionNotInSelect Expression (Select)
                -- ^ Represents a @NOT IN@ expression with the right-hand side being a
                --   @SELECT@ statement.
                | ExpressionInList Expression [Expression]
                -- ^ Represents an @IN@ expression with the right-hand side being a
                --   list of subexpressions.
                | ExpressionNotInList Expression [Expression]
                -- ^ Represents a @NOT IN@ expression with the right-hand side being a
                --   list of subexpressions.
                | ExpressionInTable Expression SinglyQualifiedIdentifier
                -- ^ Represents an @IN@ expression with the right-hand side being a
                --   table name, optionally qualified by a database name.
                | ExpressionNotInTable Expression SinglyQualifiedIdentifier
                -- ^ Represents a @NOT IN@ expression with the right-hand side being a
                --   table name, optionally qualified by a database name.
                | ExpressionSubquery (Select)
                -- ^ Represents a subquery @SELECT@ expression.
                | ExpressionExistsSubquery (Select)
                -- ^ Represents a subquery @SELECT@ expression with the @EXISTS@
                --   qualifier.
                | ExpressionNotExistsSubquery (Select)
                -- ^ Represents a subquery @SELECT@ expression with the @NOT EXISTS@
                --   qualifier.
                | ExpressionCase MaybeSwitchExpression
                                 (OneOrMore CasePair)
                                 Else
                -- ^ Represents a @CASE@ expression.
                | ExpressionRaiseIgnore
                -- ^ Represents a @RAISE(IGNORE)@ expression.
                | ExpressionRaiseRollback String
                -- ^ Represents a @RAISE(ROLLBACK, string)@ expression.
                | ExpressionRaiseAbort String
                -- ^ Represents a @RAISE(ABORT, string)@ expression.
                | ExpressionRaiseFail String
                -- ^ Represents a @RAISE(FAIL, string)@ expression.
                | ExpressionParenthesized Expression
                -- ^ Represents a parenthesized subexpression.
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

-- | The AST node corresponding to an optional @UNIQUE@ qualifier.  Used by
--   'CreateIndex'.
data MaybeUnique = NoUnique | Unique
                   deriving (Eq, Show)
instance ShowTokens MaybeUnique where
    showTokens NoUnique = []
    showTokens Unique = [KeywordUnique]

-- | The AST node corresponding to an optional @IF NOT EXISTS@ qualifier.  Used by
--   'CreateIndex', 'CreateTable', 'CreateTrigger', and 'CreateView'.
data MaybeIfNotExists = NoIfNotExists | IfNotExists
                        deriving (Eq, Show)
instance ShowTokens MaybeIfNotExists where
    showTokens NoIfNotExists = []
    showTokens IfNotExists = [KeywordIf, KeywordNot, KeywordExists]

-- | The AST node corresponding to an optional @IF EXISTS@ qualifier.  Used by
--   'DropIndex', 'DropTable', 'DropTrigger', and 'DropView'.
data MaybeIfExists = NoIfExists | IfExists
                     deriving (Eq, Show)
instance ShowTokens MaybeIfExists where
    showTokens NoIfExists = []
    showTokens IfExists = [KeywordIf, KeywordExists]

-- | The AST node corresponding to an optional @FOR EACH ROW@ qualifier.  Used by
--   'CreateTrigger'.
data MaybeForEachRow = NoForEachRow | ForEachRow
                       deriving (Eq, Show)
instance ShowTokens MaybeForEachRow where
    showTokens NoForEachRow = []
    showTokens ForEachRow = [KeywordFor, KeywordEach, KeywordRow]

-- | The AST node corresponding to an optional @TEMP@ or @TEMPORARY@ qualifier.  Used
--   by 'CreateTable', 'CreateTrigger', and 'CreateView'.
data MaybeTemporary = NoTemporary | Temp | Temporary
                  deriving (Eq, Show)
instance ShowTokens MaybeTemporary where
    showTokens NoTemporary = []
    showTokens Temp = [KeywordTemp]
    showTokens Temporary = [KeywordTemporary]

-- | The AST node corresponding to an optional @COLLATE@ subclause.  Used by
--   'IndexedColumn' and 'OrderingTerm'.
data MaybeCollation = NoCollation | Collation UnqualifiedIdentifier
                      deriving (Eq, Show)
instance ShowTokens MaybeCollation where
    showTokens NoCollation = []
    showTokens (Collation name) = [KeywordCollate] ++ showTokens name

-- | The AST node corresponding to an optional @ASC@ or @DESC@ qualifier.  Used by
--   'IndexedColumn', 'ColumnConstraint', and 'OrderingTerm'.
data MaybeAscDesc = NoAscDesc | Asc | Desc
                    deriving (Eq, Show)
instance ShowTokens MaybeAscDesc where
    showTokens NoAscDesc = []
    showTokens Asc = [KeywordAsc]
    showTokens Desc = [KeywordDesc]

-- | The AST node corresponding to an optional @AUTOINCREMENT@ qualifier.  Used by
--   'ColumnConstraint'.
data MaybeAutoincrement = NoAutoincrement | Autoincrement
                          deriving (Eq, Show)
instance ShowTokens MaybeAutoincrement where
    showTokens NoAutoincrement = []
    showTokens Autoincrement = [KeywordAutoincrement]

-- | The AST node corresponding to an optional @+@ or @-@ sign.  Used by
--   'TypeSizeField', 'DefaultValue', and 'PragmaValue'.
data MaybeSign = NoSign | PositiveSign | NegativeSign
                 deriving (Eq, Show)
instance ShowTokens MaybeSign where
    showTokens NoSign = []
    showTokens PositiveSign = [PunctuationPlus]
    showTokens NegativeSign = [PunctuationMinus]

-- | The AST node corresponding to an optional @COLUMN@ keyword.
--   Used by 'AlterTableBody'.
data MaybeColumn = ElidedColumn | Column
                   deriving (Eq, Show)
instance ShowTokens MaybeColumn where
    showTokens ElidedColumn = []
    showTokens Column = [KeywordColumn]

-- | The AST node corresponding to the body of an 'AlterTable' statement.
--   Used by 'AlterTable'.
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

-- | The AST node corresponding to a column-definition subclause.  Used by
--   'AlterTableBody' and 'CreateTableBody'.
data ColumnDefinition
    = ColumnDefinition UnqualifiedIdentifier MaybeType [ColumnConstraint]
      deriving (Eq, Show)
instance ShowTokens ColumnDefinition where
    showTokens (ColumnDefinition name maybeType constraints)
        = showTokens name
          ++ showTokens maybeType
          ++ (concat $ map showTokens constraints)

-- | The AST node corresponding to a default-value subclause.  Used by
--   'ColumnConstraint'.
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

-- | The AST node corresponding to an indexed-column subclause.  Used by
--   'TableConstraint' and 'CreateIndex'.
data IndexedColumn
    = IndexedColumn UnqualifiedIdentifier MaybeCollation MaybeAscDesc
      deriving (Eq, Show)
instance ShowTokens IndexedColumn where
    showTokens (IndexedColumn name maybeCollation maybeAscDesc)
        = showTokens name
          ++ showTokens maybeCollation
          ++ showTokens maybeAscDesc

-- | The AST node corresponding to a column constraint subclause.  Used by
--   'ColumnDefinition'.
data ColumnConstraint
    = ColumnPrimaryKey MaybeConstraintName
                       MaybeAscDesc
                       (Maybe ConflictClause)
                       MaybeAutoincrement
    | ColumnNotNull MaybeConstraintName (Maybe ConflictClause)
    | ColumnUnique MaybeConstraintName (Maybe ConflictClause)
    | ColumnCheck MaybeConstraintName Expression
    | ColumnDefault MaybeConstraintName DefaultValue
    | ColumnCollate MaybeConstraintName UnqualifiedIdentifier
    | ColumnForeignKey MaybeConstraintName ForeignKeyClause
      deriving (Eq, Show)
instance ShowTokens ColumnConstraint where
    showTokens (ColumnPrimaryKey maybeConstraintName
                                 maybeAscDesc
                                 maybeConflictClause
                                 maybeAutoincrement)
        = showTokens maybeConstraintName
          ++ [KeywordPrimary, KeywordKey]
          ++ showTokens maybeAscDesc
          ++ (case maybeConflictClause of
                Nothing -> []
                Just conflictClause -> showTokens conflictClause)
          ++ showTokens maybeAutoincrement
    showTokens (ColumnNotNull maybeConstraintName maybeConflictClause)
        = showTokens maybeConstraintName
          ++ [KeywordNot, KeywordNull]
          ++ (case maybeConflictClause of
                Nothing -> []
                Just conflictClause -> showTokens conflictClause)
    showTokens (ColumnUnique maybeConstraintName maybeConflictClause)
        = showTokens maybeConstraintName
          ++ [KeywordUnique]
          ++ (case maybeConflictClause of
                Nothing -> []
                Just conflictClause -> showTokens conflictClause)
    showTokens (ColumnCheck maybeConstraintName expression)
        = showTokens maybeConstraintName
          ++ [KeywordCheck, PunctuationLeftParenthesis]
          ++ showTokens expression
          ++ [PunctuationRightParenthesis]
    showTokens (ColumnDefault maybeConstraintName defaultValue)
        = showTokens maybeConstraintName
          ++ [KeywordDefault]
          ++ showTokens defaultValue
    showTokens (ColumnCollate maybeConstraintName collationName)
        = showTokens maybeConstraintName
          ++ [KeywordCollate]
          ++ showTokens collationName
    showTokens (ColumnForeignKey maybeConstraintName foreignKeyClause)
        = showTokens maybeConstraintName
          ++ showTokens foreignKeyClause

-- | The AST node corresponding to a table-constraint subclause.  Used by
--   'CreateTableBody'.
data TableConstraint
    = TablePrimaryKey MaybeConstraintName
                      (OneOrMore IndexedColumn)
                      (Maybe ConflictClause)
    | TableUnique MaybeConstraintName
                  (OneOrMore IndexedColumn)
                  (Maybe ConflictClause)
    | TableCheck MaybeConstraintName
                 Expression
    | TableForeignKey MaybeConstraintName
                      (OneOrMore UnqualifiedIdentifier)
                      ForeignKeyClause
      deriving (Eq, Show)
instance ShowTokens TableConstraint where
    showTokens (TablePrimaryKey maybeConstraintName indexedColumns maybeConflictClause)
        = showTokens maybeConstraintName
          ++ [KeywordPrimary, KeywordKey, PunctuationLeftParenthesis]
          ++ (intercalate [PunctuationComma] $ mapOneOrMore showTokens indexedColumns)
          ++ [PunctuationRightParenthesis]
          ++ (case maybeConflictClause of
                Nothing -> []
                Just conflictClause -> showTokens conflictClause)
    showTokens (TableUnique maybeConstraintName indexedColumns maybeConflictClause)
        = showTokens maybeConstraintName
          ++ [KeywordUnique, PunctuationLeftParenthesis]
          ++ (intercalate [PunctuationComma] $ mapOneOrMore showTokens indexedColumns)
          ++ [PunctuationRightParenthesis]
          ++ (case maybeConflictClause of
                Nothing -> []
                Just conflictClause -> showTokens conflictClause)
    showTokens (TableCheck maybeConstraintName expression)
        = showTokens maybeConstraintName
          ++ [KeywordCheck, PunctuationLeftParenthesis]
          ++ showTokens expression
          ++ [PunctuationRightParenthesis]
    showTokens (TableForeignKey maybeConstraintName columns foreignKeyClause)
        = showTokens maybeConstraintName
          ++ [KeywordForeign, KeywordKey, PunctuationLeftParenthesis]
          ++ (intercalate [PunctuationComma] $ mapOneOrMore showTokens columns)
          ++ [PunctuationRightParenthesis]
          ++ showTokens foreignKeyClause

-- | The AST node corresponding to an optional constraint name subclause.  Used by
--   'ColumnConstraint' and 'Table Constraint'.
data MaybeConstraintName = NoConstraintName
                         | ConstraintName UnqualifiedIdentifier
                           deriving (Eq, Show)
instance ShowTokens MaybeConstraintName where
    showTokens NoConstraintName = []
    showTokens (ConstraintName constraintName)
        = [KeywordConstraint]
          ++ showTokens constraintName

-- | The AST node corresponding to a trigger-time qualifier.  Used by 'CreateTrigger'.
data TriggerTime = Before | After | InsteadOf
                   deriving (Eq, Show)
instance ShowTokens TriggerTime where
    showTokens Before = [KeywordBefore]
    showTokens After = [KeywordAfter]
    showTokens InsteadOf = [KeywordInstead, KeywordOf]

-- | The AST node corresponding to a trigger-condition subclause.  Used by
--   'CreateTrigger'.
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

-- | The AST node corresponding to a module argument.  Used by 'CreateVirtualTable'.
data ModuleArgument = ModuleArgument String
                      deriving (Eq, Show)
instance ShowTokens ModuleArgument where
    showTokens (ModuleArgument string) = [ModuleArgumentToken string]

-- | The AST node corresponding to a qualified table name subclause.  Used by
--   'Delete', 'DeleteLimited', 'Update', and 'UpdateLimited'.
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

-- | The AST node corresponding to an ordering term subclause.  Used by
--   'GroupClause' and 'OrderClause'.
data OrderingTerm = OrderingTerm Expression MaybeCollation MaybeAscDesc
                    deriving (Eq, Show)
instance ShowTokens OrderingTerm where
    showTokens (OrderingTerm expression maybeCollation maybeAscDesc) =
        showTokens expression
        ++ showTokens maybeCollation
        ++ showTokens maybeAscDesc

-- | The AST node corresponding to a pragma body.  Used by 'Pragma'.
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

-- | The AST node corresponding to a pragma value subclause.  Used by 'PragmaBody'.
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

-- | The AST node corresponding to a create-table body.  Used by 'CreateTable'.
data CreateTableBody
    = ColumnsAndConstraints (OneOrMore ColumnDefinition) [TableConstraint]
    | AsSelect (Select)
      deriving (Eq, Show)
instance ShowTokens CreateTableBody where
    showTokens (ColumnsAndConstraints columns constraints)
        = [PunctuationLeftParenthesis]
          ++ (intercalate [PunctuationComma]
                              $ concat [mapOneOrMore showTokens columns,
                                        map showTokens constraints])
          ++ [PunctuationRightParenthesis]
    showTokens (AsSelect select)
        = [KeywordAs]
          ++ showTokens select

-- | The AST node corresponding to an insert head.  Used by 'Insert'.
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

-- | The AST node corresponding to an insert body.  Used by 'Insert'.
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

-- | The AST node corresponding to an update head.  Used by 'Update' and
--   'UpdateLimited'.
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

-- | The AST node corresponding to an optional @DISTINCT@ or @ALL@ qualifier.
--   Used by 'SelectCore'.
data Distinctness = NoDistinctness | Distinct | All
                    deriving (Eq, Show)
instance ShowTokens Distinctness where
    showTokens NoDistinctness = []
    showTokens Distinct = [KeywordDistinct]
    showTokens All = [KeywordAll]

-- | The AST node corresponding to an optional @HAVING@ subclause.  Used by
--   'GroupClause'.
data MaybeHaving = NoHaving | Having Expression
                   deriving (Eq, Show)
instance ShowTokens MaybeHaving where
    showTokens NoHaving = []
    showTokens (Having expression) = [KeywordHaving] ++ showTokens expression

-- | The AST node corresponding to an optional @AS@ subclause, possibly with the
--   actual keyword elided.  Used by 'ResultColumn' and 'SingleSource'.
data MaybeAs = NoAs | As UnqualifiedIdentifier | ElidedAs UnqualifiedIdentifier
               deriving (Eq, Show)
instance ShowTokens MaybeAs where
    showTokens NoAs = []
    showTokens (As thingAlias) = [KeywordAs] ++ showTokens thingAlias
    showTokens (ElidedAs thingAlias) = showTokens thingAlias

-- | The AST node corresponding to a compound operator in a @SELECT@ statement.
--   Used by 'Select'.
data CompoundOperator = Union | UnionAll | Intersect | Except
                        deriving (Eq, Show)
instance ShowTokens CompoundOperator where
    showTokens Union = [KeywordUnion]
    showTokens UnionAll = [KeywordUnion, KeywordAll]
    showTokens Intersect = [KeywordIntersect]
    showTokens Except = [KeywordExcept]

-- | The AST node corresponding to the core part of a @SELECT@ statement, which may
--   be the head of the overall statement, or, in the case of a compound @SELECT@,
--   only part of it.  Used by 'Select'.
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

-- | The AST node corresponding to a result column in a @SELECT@ statement.  Used by
--   'SelectCore'.
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

-- | The AST node corresponding to a source from which to join columns in a @SELECT@
--   statement, which may be the head of the statement's @FROM@ clause, or, in the
--   case of a subjoin, only part of it.  Used by 'FromClause' and 'SingleSource'.
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

-- | The AST node corresponding to a primitive source from which to join columns in
--   a @SELECT@ statement, which is a body of the statement's @FROM@ clause.  Used by
--   'JoinSource'.
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

-- | The AST node corresponding to a join operation, a conjunction in the @FROM@
--   clause of a @SELECT@ statement.  Used by 'JoinSource'.
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

-- | The AST node corresponding to a join constraint, a qualifier in the @FROM@
--   clause of a @SELECT@ statement.  Used by 'JoinSource'.
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

-- | The AST node corresponding to an optional @INDEXED BY@ or @NOT INDEXED@ qualifier.
--   Used by 'SingleSource'.
data MaybeIndexedBy = NoIndexedBy
                    | IndexedBy UnqualifiedIdentifier
                    | NotIndexed
                      deriving (Eq, Show)
instance ShowTokens MaybeIndexedBy where
    showTokens NoIndexedBy = []
    showTokens (IndexedBy indexName)
        = [KeywordIndexed, KeywordBy] ++ showTokens indexName
    showTokens NotIndexed = [KeywordNot, KeywordIndexed]

-- | The AST node corresponding to a @FROM@ clause.  Used by 'SelectCore'.
data FromClause = From JoinSource
                  deriving (Eq, Show)
instance ShowTokens FromClause where
    showTokens (From joinSource) = [KeywordFrom] ++ showTokens joinSource

-- | The AST node corresponding to a @WHERE@ clause.  Used by 'SelectCore',
--   'Delete', 'DeleteLimited', 'Update', and 'UpdateLimited'.
data WhereClause = Where Expression
                   deriving (Eq, Show)
instance ShowTokens WhereClause where
    showTokens (Where expression) = [KeywordWhere] ++ showTokens expression

-- | The AST node corresponding to a @GROUP BY@ clause.  Used by 'SelectCore'.
data GroupClause = GroupBy (OneOrMore OrderingTerm) MaybeHaving
                   deriving (Eq, Show)
instance ShowTokens GroupClause where
    showTokens (GroupBy orderingTerms maybeHaving)
        = [KeywordGroup, KeywordBy]
          ++ (intercalate [PunctuationComma] $ mapOneOrMore showTokens orderingTerms)
          ++ showTokens maybeHaving

-- | The AST node corresponding to an @ORDER BY@ clause.  Used by 'Select',
--   'DeleteLimited', and 'UpdateLimited'.
data OrderClause = OrderBy (OneOrMore OrderingTerm)
                   deriving (Eq, Show)
instance ShowTokens OrderClause where
    showTokens (OrderBy orderingTerms)
        = [KeywordOrder, KeywordBy]
          ++ (intercalate [PunctuationComma] $ mapOneOrMore showTokens orderingTerms)

-- | The AST node corresponding to a @LIMIT@ clause.  Used by 'Select',
--   'DeleteLimited', and 'UpdateLimited'.
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

-- | The AST node corresponding to a @WHEN@ clause.  Used by 'CreateTrigger'.
data WhenClause = When Expression
                  deriving (Eq, Show)
instance ShowTokens WhenClause where
    showTokens (When expression) = [KeywordWhen] ++ showTokens expression

-- | The AST node corresponding to an @ON CONFLICT@ clause.  Used by
--   'ColumnConstraint' and 'TableConstraint'.
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

-- | The AST node corresponding to a @FOREIGN KEY@ clause.  Used by
--   'ColumnConstraint' and 'TableConstraint'.
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

-- | The AST node corresponding to the first partial body of a @FOREIGN KEY@ clause.
--   Used by 'ForeignKeyClause'.
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

-- | The AST node corresponding to an action subclause in the first partial body of
--   a @FOREIGN KEY@ clause.  Used by 'ForeignKeyClauseActionOrMatchPart'.
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

-- | The AST node corresponding to the second partial body of a @FOREIGN KEY@ clause.
--   Used by 'ForeignKeyClause'.
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

-- | The AST node corresponding to an optional @INITIALLY DEFERRED@ or
--   @INITIALLY IMMEDIATE@ qualifier in a @FOREIGN KEY@ clause.  Used by
--   'MaybeForeignKeyClauseDeferrablePart'.
data MaybeInitialDeferralStatus
    = NoInitialDeferralStatus
    | InitiallyDeferred
    | InitiallyImmediate
      deriving (Eq, Show)
instance ShowTokens MaybeInitialDeferralStatus where
    showTokens NoInitialDeferralStatus = []
    showTokens InitiallyDeferred = [KeywordInitially, KeywordDeferred]
    showTokens InitiallyImmediate = [KeywordInitially, KeywordImmediate]

-- | The AST node corresponding to the head of a @COMMIT@ statement.  Used by
--   'Commit'.
data CommitHead
    = CommitCommit
    | CommitEnd
      deriving (Eq, Show)
instance ShowTokens CommitHead where
    showTokens CommitCommit = [KeywordCommit]
    showTokens CommitEnd = [KeywordEnd]

-- | The AST node corresponding to an optional @TRASACTION@ keyword.  Used by
--   'Begin', 'Commit', and 'Rollback'.
data MaybeTransaction = ElidedTransaction | Transaction
                        deriving (Eq, Show)
instance ShowTokens MaybeTransaction where
    showTokens ElidedTransaction = []
    showTokens Transaction = [KeywordTransaction]

-- | The AST node corresponding to an optional transaction-type qualifier.  Used
--   by 'Begin'.
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

-- | The AST node corresponding to an optional @DATABASE@ keyword.  Used by
--   'Attach' and 'Detach'.
data MaybeDatabase = ElidedDatabase | Database
                     deriving (Eq, Show)
instance ShowTokens MaybeDatabase where
    showTokens ElidedDatabase = []
    showTokens Database = [KeywordDatabase]

-- | The AST node corresponding to an optional @TO SAVEPOINT@ qualifier.  Used by
--   'Rollback'.
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

-- | The AST node corresponding to an optional @RELEASE SAVEPOINT@ qualifier.
--   Used by 'Release'.
data MaybeReleaseSavepoint = ElidedReleaseSavepoint UnqualifiedIdentifier
                           | ReleaseSavepoint UnqualifiedIdentifier
                             deriving (Eq, Show)
instance ShowTokens MaybeReleaseSavepoint where
    showTokens (ElidedReleaseSavepoint savepointName)
               = showTokens savepointName
    showTokens (ReleaseSavepoint savepointName)
               = [KeywordSavepoint]
                 ++ showTokens savepointName

-- | The AST node corresponding to a semicolon-separated list of statements.
--   Used at the top level of an SQL file.
data StatementList = StatementList [AnyStatement]
                     deriving (Eq, Show)
instance ShowTokens StatementList where
    showTokens (StatementList list) =
        intercalate [PunctuationSemicolon] $ map showTokens list

-- | The AST node corresponding to any statement.  Used by 'StatementList'.
--   Also useful at top level.
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
-- | A type synonym which matches only the AST node corresponding to
--   an @EXPLAIN@ statement.
--   Useful at top level.
type Explain = Statement L1 NT NS Explain'
data ExplainQueryPlan'
-- | A type synonym which matches only the AST node corresponding to
--   an @EXPLAIN QUERY PLAN@ statement.
--   Useful at top level.
type ExplainQueryPlan = Statement L1 NT NS ExplainQueryPlan'
data AlterTable'
-- | A type synonym which matches only the AST node corresponding to
--   an @ALTER TABLE@ statement.
--   Useful at top level.
type AlterTable = Statement L0 NT NS AlterTable'
data Analyze'
-- | A type synonym which matches only the AST node corresponding to
--   an @ANALYZE@ statement.
--   Useful at top level.
type Analyze = Statement L0 NT NS Analyze'
data Attach'
-- | A type synonym which matches only the AST node corresponding to
--   an @ATTACH@ statement.
--   Useful at top level.
type Attach = Statement L0 NT NS Attach'
data Begin'
-- | A type synonym which matches only the AST node corresponding to
--   a @BEGIN@ statement.
--   Useful at top level.
type Begin = Statement L0 NT NS Begin'
data Commit'
-- | A type synonym which matches only the AST node corresponding to
--   a @COMMIT@ statement.
--   Useful at top level.
type Commit = Statement L0 NT NS Commit'
data CreateIndex'
-- | A type synonym which matches only the AST node corresponding to
--   a @CREATE INDEX@ statement.
--   Useful at top level.
type CreateIndex = Statement L0 NT NS CreateIndex'
data CreateTable'
-- | A type synonym which matches only the AST node corresponding to
--   a @CREATE TABLE@ statement.
--   Useful at top level.
type CreateTable = Statement L0 NT NS CreateTable'
data CreateTrigger'
-- | A type synonym which matches only the AST node corresponding to
--   a @CREATE TRIGGER@ statement.
--   Useful at top level.
type CreateTrigger = Statement L0 NT NS CreateTrigger'
data CreateView'
-- | A type synonym which matches only the AST node corresponding to
--   a @CREATE VIEW@ statement.
--   Useful at top level.
type CreateView = Statement L0 NT NS CreateView'
data CreateVirtualTable'
-- | A type synonym which matches only the AST node corresponding to
--   a @CREATE VIRTUAL TABLE@ statement.
--   Useful at top level.
type CreateVirtualTable = Statement L0 NT NS CreateVirtualTable'
data Delete'
-- | A type synonym which matches only the AST node corresponding to
--   a @DELETE@ statement without a @LIMIT@ clause.
--   Useful at top level.
type Delete = Statement L0 T NS Delete'
data DeleteLimited'
-- | A type synonym which matches only the AST node corresponding to
--   a @DELETE@ statement with a @LIMIT@ clause.
--   Useful at top level.
type DeleteLimited = Statement L0 NT NS DeleteLimited'
data Detach'
-- | A type synonym which matches only the AST node corresponding to
--   a @DETACH@ statement.
--   Useful at top level.
type Detach = Statement L0 NT NS Detach'
data DropIndex'
-- | A type synonym which matches only the AST node corresponding to
--   a @DROP INDEX@ statement.
--   Useful at top level.
type DropIndex = Statement L0 NT NS DropIndex'
data DropTable'
-- | A type synonym which matches only the AST node corresponding to
--   a @DROP TABLE@ statement.
--   Useful at top level.
type DropTable = Statement L0 NT NS DropTable'
data DropTrigger'
-- | A type synonym which matches only the AST node corresponding to
--   a @DROP TRIGGER@ statement.
--   Useful at top level.
type DropTrigger = Statement L0 NT NS DropTrigger'
data DropView'
-- | A type synonym which matches only the AST node corresponding to
--   a @DROP VIEW@ statement.
--   Useful at top level.
type DropView = Statement L0 NT NS DropView'
data Insert'
-- | A type synonym which matches only the AST node corresponding to
--   an @INSERT@ statement.
--   Useful at top level.
type Insert = Statement L0 T NS Insert'
data Pragma'
-- | A type synonym which matches only the AST node corresponding to
--   a @PRAGMA@ statement.
--   Useful at top level.
type Pragma = Statement L0 NT NS Pragma'
data Reindex'
-- | A type synonym which matches only the AST node corresponding to
--   a @REINDEX@ statement.
--   Useful at top level.
type Reindex = Statement L0 NT NS Reindex'
data Release'
-- | A type synonym which matches only the AST node corresponding to
--   a @RELEASE@ statement.
--   Useful at top level.
type Release = Statement L0 NT NS Release'
data Rollback'
-- | A type synonym which matches only the AST node corresponding to
--   a @ROLLBACK@ statement.
--   Useful at top level.
type Rollback = Statement L0 NT NS Rollback'
data Savepoint'
-- | A type synonym which matches only the AST node corresponding to
--   a @SAVEPOINT@ statement.
--   Useful at top level.
type Savepoint = Statement L0 NT NS Savepoint'
data Select'
-- | A type synonym which matches only the AST node corresponding to
--   a @SELECT@ statement.
--   Useful at top level.
type Select = Statement L0 T S Select'
data Update'
-- | A type synonym which matches only the AST node corresponding to
--   an @UPDATE@ statement without a @LIMIT@ clause.
--   Useful at top level.
type Update = Statement L0 T NS Update'
data UpdateLimited'
-- | A type synonym which matches only the AST node corresponding to
--   an @UPDATE@ statement with a @LIMIT@ clause.
--   Useful at top level.
type UpdateLimited = Statement L0 NT NS UpdateLimited'
data Vacuum'
-- | A type synonym which matches only the AST node corresponding to
--   a @VACUUM@ statement.
--   Useful at top level.
type Vacuum = Statement L0 NT NS Vacuum'

-- | The AST node which corresponds to a statement.  Not directly useful at
--   top level because it is a generalized algebraic datatype the type parameters
--   to which are not exported; instead, see the existentially qualified
--   types 'AnyStatement', 'ExplainableStatement', and 'TriggerStatement', and the
--   type synonyms such as 'Select' which correspond to individual statement types.
--   
--   I apologize for the lack of documentation on these individual entries, but
--   Haddock won't let me do it!  At any rate, each of them is an AST node corresponding
--   to an individual statement type.
--   
--   Note the distinctions between
--   'Delete' and 'DeleteLimited' and 'Update' and 'UpdateLimited':  The @Limited@ ones
--   have @LIMIT@ clauses and the others do not.  Because SQL imposes stricter
--   restrictions on where the ones with @LIMIT@ clauses can occur, these are are
--   separate types.
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
        :: CommitHead
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
        :: MaybeTemporary
        -> MaybeIfNotExists
        -> SinglyQualifiedIdentifier
        -> CreateTableBody
        -> Statement L0 NT NS CreateTable'
    CreateTrigger
        :: MaybeTemporary
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
        :: MaybeTemporary
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
        :: MaybeDatabase
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
    showTokens (Commit commitHead maybeTransaction)
        = showTokens commitHead
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
    showTokens (Detach maybeDatabase databaseName)
        = [KeywordDetach]
          ++ showTokens maybeDatabase
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


-- | A class implemented by all identifiers regardless of how many levels of
--   qualification they allow.
class Identifier a where
    identifierProperName :: a -> String
    -- ^ Returns the final, "proper name" component of an identifier.  In an identifier
    --   which names a column, this is the column name.  In an identifier which names
    --   a table, this is the table name.  All identifiers
    --   have this component, so it is a 'String' and not a 'Maybe'.
    identifierParentName :: a -> Maybe String
    -- ^ Returns the "parent name" component of an identifier, if it exists.  In an
    --   identifier which names a column, this is the table name.  In an identifier
    --   which names a table or other database-level object, this is the database name.
    identifierGrandparentName :: a -> Maybe String
    -- ^ Returns the "grandparent name" component of an identifier, if it exists.  In
    --   an identifier which names a column, this is the database name.


-- | Converts an identifier to be doubly-qualified.  This does not actually synthesize
--   any missing components, merely provides 'Nothing' for them.
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


-- | An identifier which does not allow any levels of qualification.  This is typically
--   a database name.
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


-- | An identifier which allows a single level of qualification.  This is typically
--   the name of a table or other database-level object.
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


-- | An identifier which allows two levels of qualification.  This is typically a
--   column name.
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


-- | Not an AST node but a token which corresponds to a primitive of SQL syntax.
--   Has an instance of 'Show' which prints a list of them as syntactically-valid
--   SQL with no line wrapping.
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
