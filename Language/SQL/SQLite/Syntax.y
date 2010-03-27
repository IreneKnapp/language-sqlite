{
{-# LANGUAGE ExistentialQuantification, Rank2Types #-}
module Language.SQL.SQLite.Syntax (
                                   ParseError,
                                   readType,
                                   readMaybeTypeSize,
                                   readTypeSizeField,
                                   readLikeType,
                                   readExpression,
                                   readMaybeUnique,
                                   readMaybeIfNotExists,
                                   readMaybeIfExists,
                                   readMaybeForEachRow,
                                   readPermanence,
                                   readMaybeCollation,
                                   readMaybeAscDesc,
                                   readMaybeAutoincrement,
                                   readMaybeSign,
                                   readMaybeColumn,
                                   readAlterTableBody,
                                   readColumnDefinition,
                                   readDefaultValue,
                                   readIndexedColumn,
                                   readColumnConstraint,
                                   readTableConstraint,
                                   readTriggerTime,
                                   readTriggerCondition,
                                   -- TODO remember to uncomment this
                                   -- readModuleArgument,
                                   readTriggerStatement,
                                   readQualifiedTableName,
                                   readOrderingTerm,
                                   readPragmaBody,
                                   readPragmaValue,
                                   readEitherColumnsAndConstraintsSelect,
                                   readInsertHead,
                                   readInsertBody,
                                   readUpdateHead,
                                   readDistinctness,
                                   readMaybeHaving,
                                   readMaybeAs,
                                   readCompoundOperator,
                                   readSelectCore,
                                   readResultColumn,
                                   readJoinSource,
                                   readSingleSource,
                                   readJoinOperation,
                                   readJoinConstraint,
                                   readMaybeIndexedBy,
                                   readFromClause,
                                   readWhereClause,
                                   readGroupClause,
                                   readOrderClause,
                                   readLimitClause,
                                   readWhenClause,
                                   readConflictClause,
                                   readForeignKeyClause,
                                   readForeignKeyClauseActionOrMatchPart,
                                   readForeignKeyClauseActionPart,
                                   readForeignKeyClauseDeferrablePart,
                                   readMaybeInitialDeferralStatus,
                                   readMaybeTransaction,
                                   readMaybeTransactionType,
                                   readMaybeDatabase,
                                   readMaybeSavepoint,
                                   readMaybeReleaseSavepoint,
                                   readStatementList,
                                   readAnyStatement,
                                   readExplainableStatement,
                                   readExplain,
                                   readExplainQueryPlan,
                                   readAlterTable,
                                   readAnalyze,
                                   readAttach,
                                   readBegin,
                                   readCommit,
                                   readCreateIndex,
                                   readCreateTable,
                                   readCreateTrigger,
                                   readCreateView,
                                   -- TODO remember to uncomment this
                                   -- readCreateVirtualTable,
                                   readDelete,
                                   readDeleteLimited,
                                   readDeleteOrDeleteLimited,
                                   readDetach,
                                   readDropIndex,
                                   readDropTable,
                                   readDropTrigger,
                                   readDropView,
                                   readInsert,
                                   readPragma,
                                   readReindex,
                                   readRelease,
                                   readRollback,
                                   readSavepoint,
                                   readSelect,
                                   readUpdate,
                                   readUpdateLimited,
                                   readUpdateOrUpdateLimited,
                                   readVacuum,
                                   readUnqualifiedIdentifier,
                                   readSinglyQualifiedIdentifier,
                                   readDoublyQualifiedIdentifier
        			  )
    where

import Control.Monad.Error
import Control.Monad.State
import Data.Char
import Data.Maybe
import Data.Word
import Numeric

import Language.SQL.SQLite.Types

}

%name parseType Type
%name parseMaybeTypeSize MaybeTypeSize
%name parseTypeSizeField TypeSizeField
%name parseLikeType LikeType
%name parseExpression Expression
%name parseMaybeUnique MaybeUnique
%name parseMaybeIfNotExists MaybeIfNotExists
%name parseMaybeIfExists MaybeIfExists
%name parseMaybeForEachRow MaybeForEachRow
%name parsePermanence Permanence
%name parseMaybeCollation MaybeCollation
%name parseMaybeAscDesc MaybeAscDesc
%name parseMaybeAutoincrement MaybeAutoincrement
%name parseMaybeSign MaybeSign
%name parseMaybeColumn MaybeColumn
%name parseAlterTableBody AlterTableBody
%name parseColumnDefinition ColumnDefinition
%name parseDefaultValue DefaultValue
%name parseIndexedColumn IndexedColumn
%name parseColumnConstraint ColumnConstraint
%name parseTableConstraint TableConstraint
%name parseTriggerTime TriggerTime
%name parseTriggerCondition TriggerCondition
-- %name parseModuleArgument ModuleArgument
-- TODO remember to uncomment this
%name parseTriggerStatement TriggerStatement
%name parseQualifiedTableName QualifiedTableName
%name parseOrderingTerm OrderingTerm
%name parsePragmaBody PragmaBody
%name parsePragmaValue PragmaValue
%name parseEitherColumnsAndConstraintsSelect EitherColumnsAndConstraintsSelect
%name parseInsertHead InsertHead
%name parseInsertBody InsertBody
%name parseUpdateHead UpdateHead
%name parseDistinctness Distinctness
%name parseMaybeHaving MaybeHaving
%name parseMaybeAs MaybeAs
%name parseCompoundOperator CompoundOperator
%name parseSelectCore SelectCore
%name parseResultColumn ResultColumn
%name parseJoinSource JoinSource
%name parseSingleSource SingleSource
%name parseJoinOperation JoinOperation
%name parseJoinConstraint JoinConstraint
%name parseMaybeIndexedBy MaybeIndexedBy
%name parseFromClause FromClause
%name parseWhereClause WhereClause
%name parseGroupClause GroupClause
%name parseOrderClause OrderClause
%name parseLimitClause LimitClause
%name parseWhenClause WhenClause
%name parseConflictClause ConflictClause
%name parseForeignKeyClause ForeignKeyClause
%name parseForeignKeyClauseActionOrMatchPart ForeignKeyClauseActionOrMatchPart
%name parseForeignKeyClauseActionPart ForeignKeyClauseActionPart
%name parseForeignKeyClauseDeferrablePart ForeignKeyClauseDeferrablePart
%name parseMaybeInitialDeferralStatus MaybeInitialDeferralStatus
%name parseMaybeTransaction MaybeTransaction
%name parseMaybeTransactionType MaybeTransactionType
%name parseMaybeDatabase MaybeDatabase
%name parseMaybeSavepoint MaybeSavepoint
%name parseMaybeReleaseSavepoint MaybeReleaseSavepoint
%name parseStatementList StatementList
%name parseAnyStatement Statement
%name parseExplainableStatement ExplainableStatement
%name parseExplain Explain
%name parseExplainQueryPlan ExplainQueryPlan
%name parseAlterTable AlterTable
%name parseAnalyze Analyze
%name parseAttach Attach
%name parseBegin Begin
%name parseCommit Commit
%name parseCreateIndex CreateIndex
%name parseCreateTable CreateTable
%name parseCreateTrigger CreateTrigger
%name parseCreateView CreateView
-- %name parseCreateVirtualTable CreateVirtualTable
-- TODO remember to uncomment this
%name parseDelete Delete
%name parseDeleteLimited DeleteLimited
%name parseDeleteOrDeleteLimited DeleteOrDeleteLimited
%name parseDetach Detach
%name parseDropIndex DropIndex
%name parseDropTable DropTable
%name parseDropTrigger DropTrigger
%name parseDropView DropView
%name parseInsert Insert
%name parsePragma Pragma
%name parseReindex Reindex
%name parseRelease Release
%name parseRollback Rollback
%name parseSavepoint Savepoint
%name parseSelect Select
%name parseUpdate Update
%name parseUpdateLimited UpdateLimited
%name parseUpdateOrUpdateLimited UpdateOrUpdateLimited
%name parseVacuum Vacuum
%name parseUnqualifiedIdentifier UnqualifiedIdentifier
%name parseSinglyQualifiedIdentifier SinglyQualifiedIdentifier
%name parseDoublyQualifiedIdentifier DoublyQualifiedIdentifier

%tokentype { Token }
%monad { Parse } { >>= } { return }
%lexer { lexerTakingContinuation } { EndOfInputToken }
%error { parseError }

%left or
%left and
%left LOOSER_THAN_NOT
%right not
%left is match like glob regexp between in isnull notnull '!=' '<>' '=' '=='
%left '>' '>=' '<' '<='
%right escape
%left '&' '|' '<<' '>>'
%left LOOSER_THAN_ADDITIVE
%left '+' '-'
%left '*' '/' '%'
%left '|'
%left LOOSER_THAN_COLLATE
%left collate
%right '~'

%left LOOSER_THAN_DOT
%left '.'
%left '('

%token
        identifier            { Identifier $$ }
        integer               { LiteralInteger $$ }
        float                 { LiteralFloat $$ }
        string                { LiteralString $$ }
        blob                  { LiteralBlob $$ }
        variable              { Variable }
        variableN             { VariableN $$ }
        variableNamed         { VariableNamed $$ }
        moduleArgumentToken   { ModuleArgumentToken $$ }
        '||'                  { PunctuationBarBar }
        '*'                   { PunctuationStar }
        '/'                   { PunctuationSlash }
        '%'                   { PunctuationPercent }
        '+'                   { PunctuationPlus }
        '-'                   { PunctuationMinus }
        '<<'                  { PunctuationLessLess }
        '>>'                  { PunctuationGreaterGreater }
        '&'                   { PunctuationAmpersand }
        '|'                   { PunctuationBar }
        '<'                   { PunctuationLess }
        '<='                  { PunctuationLessEquals }
        '>'                   { PunctuationGreater }
        '>='                  { PunctuationGreaterEquals }
        '='                   { PunctuationEquals }
        '=='                  { PunctuationEqualsEquals }
        '!='                  { PunctuationBangEquals }
        '<>'                  { PunctuationLessGreater }
        '~'                   { PunctuationTilde }
        '('                   { PunctuationLeftParenthesis }
        ')'                   { PunctuationRightParenthesis }
        ','                   { PunctuationComma }
        '.'                   { PunctuationDot }
        ';'                   { PunctuationSemicolon }
        abort                 { KeywordAbort }
        action                { KeywordAction }
        add                   { KeywordAdd }
        after                 { KeywordAfter }
        all                   { KeywordAll }
        alter                 { KeywordAlter }
        analyze               { KeywordAnalyze }
        and                   { KeywordAnd }
        as                    { KeywordAs }
        asc                   { KeywordAsc }
        attach                { KeywordAttach }
        autoincrement         { KeywordAutoincrement }
        before                { KeywordBefore }
        begin                 { KeywordBegin }
        between               { KeywordBetween }
        by                    { KeywordBy }
        cascade               { KeywordCascade }
        case                  { KeywordCase }
        cast                  { KeywordCast }
        check                 { KeywordCheck }
        collate               { KeywordCollate }
        column                { KeywordColumn }
        commit                { KeywordCommit }
        conflict              { KeywordConflict }
        constraint            { KeywordConstraint }
        create                { KeywordCreate }
        cross                 { KeywordCross }
        currentDate           { KeywordCurrentDate }
        currentTime           { KeywordCurrentTime }
        currentTimestamp      { KeywordCurrentTimestamp }
        database              { KeywordDatabase }
        default               { KeywordDefault }
        deferrable            { KeywordDeferrable }
        deferred              { KeywordDeferred }
        delete                { KeywordDelete }
        desc                  { KeywordDesc }
        detach                { KeywordDetach }
        distinct              { KeywordDistinct }
        drop                  { KeywordDrop }
        each                  { KeywordEach }
        else                  { KeywordElse }
        end                   { KeywordEnd }
        escape                { KeywordEscape }
        except                { KeywordExcept }
        exclusive             { KeywordExclusive }
        exists                { KeywordExists }
        explain               { KeywordExplain }
        fail                  { KeywordFail }
        for                   { KeywordFor }
        foreign               { KeywordForeign }
        from                  { KeywordFrom }
        full                  { KeywordFull }
        glob                  { KeywordGlob }
        group                 { KeywordGroup }
        having                { KeywordHaving }
        if                    { KeywordIf }
        ignore                { KeywordIgnore }
        immediate             { KeywordImmediate }
        in                    { KeywordIn }
        index                 { KeywordIndex }
        indexed               { KeywordIndexed }
        initially             { KeywordInitially }
        inner                 { KeywordInner }
        insert                { KeywordInsert }
        instead               { KeywordInstead }
        intersect             { KeywordIntersect }
        into                  { KeywordInto }
        is                    { KeywordIs }
        isnull                { KeywordIsnull }
        join                  { KeywordJoin }
        key                   { KeywordKey }
        left                  { KeywordLeft }
        like                  { KeywordLike }
        limit                 { KeywordLimit }
        match                 { KeywordMatch }
        natural               { KeywordNatural }
        no                    { KeywordNo }
        not                   { KeywordNot }
        notnull               { KeywordNotnull }
        null                  { KeywordNull }
        of                    { KeywordOf }
        offset                { KeywordOffset }
        on                    { KeywordOn }
        or                    { KeywordOr }
        order                 { KeywordOrder }
        outer                 { KeywordOuter }
        plan                  { KeywordPlan }
        pragma                { KeywordPragma }
        primary               { KeywordPrimary }
        query                 { KeywordQuery }
        raise                 { KeywordRaise }
        references            { KeywordReferences }
        regexp                { KeywordRegexp }
        reindex               { KeywordReindex }
        release               { KeywordRelease }
        rename                { KeywordRename }
        replace               { KeywordReplace }
        restrict              { KeywordRestrict }
        right                 { KeywordRight }
        rollback              { KeywordRollback }
        row                   { KeywordRow }
        savepoint             { KeywordSavepoint }
        select                { KeywordSelect }
        set                   { KeywordSet }
        table                 { KeywordTable }
        temp                  { KeywordTemp }
        temporary             { KeywordTemporary }
        then                  { KeywordThen }
        to                    { KeywordTo }
        transaction           { KeywordTransaction }
        trigger               { KeywordTrigger }
        union                 { KeywordUnion }
        unique                { KeywordUnique }
        update                { KeywordUpdate }
        using                 { KeywordUsing }
        vacuum                { KeywordVacuum }
        values                { KeywordValues }
        view                  { KeywordView }
        virtual               { KeywordVirtual }
        when                  { KeywordWhen }
        where                 { KeywordWhere }

%%

Type :: { Type }
    : UnqualifiedIdentifier MaybeTypeSize
    { Type $1 $2 }

MaybeTypeSize :: { MaybeTypeSize }
    :
    { NoTypeSize }
    | '(' TypeSizeField ')'
    { TypeMaximumSize $2 }
    | '(' TypeSizeField ',' TypeSizeField ')'
    { TypeSize $2 $4 }

TypeSizeField :: { TypeSizeField }
    : MaybeSign float
    { DoubleSize $1 $2 }
    | MaybeSign integer
    { IntegerSize $1 $2 }

LikeType :: { LikeType }
    : like
    { Like }
    | not like
    { NotLike }
    | glob
    { Glob }
    | not glob
    { NotGlob }
    | regexp
    { Regexp }
    | not regexp
    { NotRegexp }
    | match
    { Match }
    | not match
    { NotMatch }

Expression0 :: { Expression }
    : integer
    { ExpressionLiteralInteger $1 }
    | float
    { ExpressionLiteralFloat $1 }
    | string
    { ExpressionLiteralString $1 }
    | blob
    { ExpressionLiteralBlob $1 }
    | null
    { ExpressionLiteralNull }
    | currentTime
    { ExpressionLiteralCurrentTime }
    | currentDate
    { ExpressionLiteralCurrentDate }
    | currentTimestamp
    { ExpressionLiteralCurrentTimestamp }
    | variable
    { ExpressionVariable }
    | variableN
    { ExpressionVariableN $1 }
    | variableNamed
    { ExpressionVariableNamed $1 }
    | DoublyQualifiedIdentifier
    { ExpressionIdentifier $1 }
    | UnqualifiedIdentifier '(' ExpressionList ')'
    { ExpressionFunctionCall $1 $3 }
    | UnqualifiedIdentifier '(' distinct OneOrMoreExpression ')'
    { ExpressionFunctionCallDistinct $1 (fromJust $ mkOneOrMore $4) }
    | UnqualifiedIdentifier '(' '*' ')'
    { ExpressionFunctionCallStar $1 }
    | raise '(' ignore ')'
    { ExpressionRaiseIgnore }
    | raise '(' rollback ',' string ')'
    { ExpressionRaiseRollback $5 }
    | raise '(' abort ',' string ')'
    { ExpressionRaiseAbort $5 }
    | raise '(' fail ',' string ')'
    { ExpressionRaiseFail $5 }
    | '(' Expression ')'
    { ExpressionParenthesized $2 }

Expression1 :: { Expression }
    : Expression0
    { $1 }
    | cast '(' Expression as Type ')'
    { ExpressionCast $3 $5 }

Expression2 :: { Expression }
    : Expression1
    { $1 }
    | Expression2 collate UnqualifiedIdentifier
    { ExpressionCollate $1 $3 }

Expression3 :: { Expression }
    : Expression2 %prec LOOSER_THAN_COLLATE
    { $1 }
    | case CaseList end
    { ExpressionCase Nothing (fromJust $ mkOneOrMore $2) Nothing }
    | case CaseList else Expression end
    { ExpressionCase Nothing (fromJust $ mkOneOrMore $2) (Just $4) }
    | case Expression CaseList end
    { ExpressionCase (Just $2) (fromJust $ mkOneOrMore $3) Nothing }
    | case Expression CaseList else Expression end
    { ExpressionCase (Just $2) (fromJust $ mkOneOrMore $3) (Just $5) }

Expression4 :: { Expression }
    : Expression3
    { $1 }
    | exists '(' Select ')'
    { ExpressionExistsSubquery $3 }

Expression5 :: { Expression }
    : Expression4
    { $1 }
    | Expression5 in '(' Select ')'
    { ExpressionInSelect $1 $4 }
    | Expression5 not in '(' Select ')'
    { ExpressionNotInSelect $1 $5 }
    | Expression5 in SinglyQualifiedIdentifier
    { ExpressionInTable $1 $3 }
    | Expression5 not in SinglyQualifiedIdentifier
    { ExpressionNotInTable $1 $4 }

Expression6 :: { Expression }
    : Expression5 %prec LOOSER_THAN_NOT
    { $1 }
    | '(' Select ')'
    { ExpressionSubquery $2 }

Expression7 :: { Expression }
    : Expression6
    { $1 }
    | Expression7 between Expression17 and Expression6
    { ExpressionBetween $1 $3 $5 }
    | Expression7 not between Expression17 and Expression6
    { ExpressionNotBetween $1 $4 $6 }

Expression8 :: { Expression }
    : Expression7 %prec LOOSER_THAN_NOT
    { $1 }
    | '-' Expression8
    { ExpressionUnaryNegative $2 }
    | '+' Expression8
    { ExpressionUnaryPositive $2 }
    | '~' Expression8
    { ExpressionUnaryBitwiseNot $2 }
    | not Expression8
    { case $2 of
        ExpressionExistsSubquery subquery -> ExpressionNotExistsSubquery subquery
        subexpression -> ExpressionUnaryLogicalNot subexpression }

Expression9 :: { Expression }
    : Expression8
    { $1 }
    | Expression9 is Expression8
    { case $3 of
        ExpressionUnaryLogicalNot subexpression -> ExpressionIsNot $1 subexpression
        _ -> ExpressionIs $1 $3 }

Expression10 :: { Expression }
    : Expression9
    { $1 }
    | Expression10 isnull
    { ExpressionIsnull $1 }
    | Expression10 notnull
    { ExpressionNotnull $1 }
    | Expression10 not null
    { ExpressionNotNull $1 }

Expression11 :: { Expression }
    : Expression10 %prec LOOSER_THAN_NOT
    { $1 }
    | Expression11 LikeType Expression10 %prec LOOSER_THAN_NOT
    { ExpressionLike $1 $2 $3 Nothing }
    | Expression11 LikeType Expression10 escape Expression10 %prec LOOSER_THAN_NOT
    { ExpressionLike $1 $2 $3 (Just $5) }

Expression12 :: { Expression }
    : Expression11 %prec LOOSER_THAN_NOT
    { $1 }
    | Expression12 '||' Expression11 %prec LOOSER_THAN_NOT
    { ExpressionBinaryConcatenate $1 $3 }

Expression13 :: { Expression }
    : Expression12
    { $1 }
    | Expression13 '*' Expression12
    { ExpressionBinaryMultiply $1 $3 }
    | Expression13 '/' Expression12
    { ExpressionBinaryDivide $1 $3 }
    | Expression13 '%' Expression12
    { ExpressionBinaryModulus $1 $3 }

Expression14 :: { Expression }
    : Expression13
    { $1 }
    | Expression14 '+' Expression13
    { ExpressionBinaryAdd $1 $3 }
    | Expression14 '-' Expression13
    { ExpressionBinarySubtract $1 $3 }

Expression15 :: { Expression }
    : Expression14 %prec LOOSER_THAN_ADDITIVE
    { $1 }
    | Expression15 '<<' Expression14
    { ExpressionBinaryLeftShift $1 $3 }
    | Expression15 '>>' Expression14
    { ExpressionBinaryRightShift $1 $3 }
    | Expression15 '&' Expression14
    { ExpressionBinaryBitwiseAnd $1 $3 }
    | Expression15 '|' Expression14
    { ExpressionBinaryBitwiseOr $1 $3 }

Expression16 :: { Expression }
    : Expression15
    { $1 }
    | Expression16 '<' Expression15
    { ExpressionBinaryLess $1 $3 }
    | Expression16 '<=' Expression15
    { ExpressionBinaryLessEquals $1 $3 }
    | Expression16 '>' Expression15
    { ExpressionBinaryGreater $1 $3 }
    | Expression16 '>=' Expression15
    { ExpressionBinaryGreaterEquals $1 $3 }

Expression17 :: { Expression }
    : Expression16
    { $1 }
    | Expression17 '=' Expression16
    { ExpressionBinaryEquals $1 $3 }
    | Expression17 '==' Expression16
    { ExpressionBinaryEqualsEquals $1 $3 }
    | Expression17 '!=' Expression16
    { ExpressionBinaryNotEquals $1 $3 }
    | Expression17 '<>' Expression16
    { ExpressionBinaryLessGreater $1 $3 }
    | Expression17 in '(' ExpressionList ')'
    { ExpressionInList $1 $4 }
    | Expression17 not in '(' ExpressionList ')'
    { ExpressionNotInList $1 $5 }

Expression18 :: { Expression }
    : Expression17 %prec LOOSER_THAN_NOT
    { $1 }
    | Expression18 and Expression17
    { ExpressionBinaryLogicalAnd $1 $3 }

Expression19 :: { Expression }
    : Expression18
    { $1 }
    | Expression19 or Expression18
    { ExpressionBinaryLogicalOr $1 $3 }

Expression :: { Expression }
    : Expression19
    { $1 }

ExpressionList :: { [Expression] }
    :
    { [] }
    | ExpressionList Expression
    { $1 ++ [$2] }

OneOrMoreExpression :: { [Expression] }
    : Expression
    { [$1] }
    | OneOrMoreExpression ',' Expression
    { $1 ++ [$3] }

OneOrMoreSetPair :: { [(UnqualifiedIdentifier, Expression)] }
    : UnqualifiedIdentifier '=' Expression
    { [($1, $3)] }
    | OneOrMoreSetPair ',' UnqualifiedIdentifier '=' Expression
    { $1 ++ [($3, $5)] }

CaseList :: { [(Expression, Expression)] }
    : when Expression then Expression
    { [($2, $4)] }
    | CaseList when Expression then Expression
    { $1 ++ [($3, $5)] }

MaybeUnique :: { MaybeUnique }
    :
    { NoUnique }
    | unique
    { Unique }

MaybeIfNotExists :: { MaybeIfNotExists }
    :
    { NoIfNotExists }
    | if not exists
    { IfNotExists }

MaybeIfExists :: { MaybeIfExists }
    :
    { NoIfExists }
    | if exists
    { IfExists }

MaybeForEachRow :: { MaybeForEachRow }
    :
    { NoForEachRow }
    | for each row
    { ForEachRow }

Permanence :: { Permanence }
    :
    { Permanent }
    | temp
    { Temp }
    | temporary
    { Temporary }

MaybeCollation :: { MaybeCollation }
    :
    { NoCollation }
    | collate UnqualifiedIdentifier
    { Collation $2 }

MaybeAscDesc :: { MaybeAscDesc }
    :
    { NoAscDesc }
    | asc
    { Asc }
    | desc
    { Desc }

MaybeAutoincrement :: { MaybeAutoincrement }
    :
    { NoAutoincrement }
    | autoincrement
    { Autoincrement }

MaybeSign :: { MaybeSign }
    :
    { NoSign }
    | '+'
    { PositiveSign }
    | '-'
    { NegativeSign }

MaybeColumn :: { MaybeColumn }
    :
    { ElidedColumn }
    | column
    { Column }

AlterTableBody :: { AlterTableBody }
    : rename to UnqualifiedIdentifier
    { RenameTo $3 }
    | add MaybeColumn ColumnDefinition
    { AddColumn $2 $3 }

ColumnDefinition :: { ColumnDefinition }
    : UnqualifiedIdentifier ColumnConstraintList
    { ColumnDefinition $1 Nothing $2 }
    | UnqualifiedIdentifier Type ColumnConstraintList
    { ColumnDefinition $1 (Just $2) $3 }

OneOrMoreColumnDefinition :: { [ColumnDefinition] }
    : ColumnDefinition
    { [$1] }
    | OneOrMoreColumnDefinition ',' ColumnDefinition
    { $1 ++ [$3] }

DefaultValue :: { DefaultValue }
    : MaybeSign integer
    { DefaultValueSignedInteger $1 $2 }
    | MaybeSign float
    { DefaultValueSignedFloat $1 $2 }
    | string
    { DefaultValueLiteralString $1 }
    | blob
    { DefaultValueLiteralBlob $1 }
    | null
    { DefaultValueLiteralNull }
    | currentTime
    { DefaultValueLiteralCurrentTime }
    | currentDate
    { DefaultValueLiteralCurrentDate }
    | currentTimestamp
    { DefaultValueLiteralCurrentTimestamp }
    | '(' Expression ')'
    { DefaultValueExpression $2 }

IndexedColumn :: { IndexedColumn }
    : UnqualifiedIdentifier MaybeCollation MaybeAscDesc
    { IndexedColumn $1 $2 $3 }

OneOrMoreIndexedColumn :: { [IndexedColumn] }
    : IndexedColumn
    { [$1] }
    | OneOrMoreIndexedColumn ',' IndexedColumn
    { $1 ++ [$3] }

MaybeConstraintName :: { Maybe UnqualifiedIdentifier }
    :
    { Nothing }
    | constraint UnqualifiedIdentifier
    { Just $2 }

ColumnConstraint :: { ColumnConstraint }
    : MaybeConstraintName primary key MaybeAscDesc MaybeConflictClause
      MaybeAutoincrement
    { ColumnPrimaryKey $1 $4 $5 $6 }
    | MaybeConstraintName not null MaybeConflictClause
    { ColumnNotNull $1 $4 }
    | MaybeConstraintName unique MaybeConflictClause
    { ColumnUnique $1 $3 }
    | MaybeConstraintName check '(' Expression ')'
    { ColumnCheck $1 $4 }
    | MaybeConstraintName default DefaultValue
    { ColumnDefault $1 $3 }
    | MaybeConstraintName collate UnqualifiedIdentifier
    { ColumnCollate $1 $3 }
    | MaybeConstraintName ForeignKeyClause
    { ColumnForeignKey $1 $2 }

ColumnConstraintList :: { [ColumnConstraint] }
    :
    { [] }
    | ColumnConstraintList ColumnConstraint
    { $1 ++ [$2] }

TableConstraint :: { TableConstraint }
    : MaybeConstraintName primary key '(' OneOrMoreIndexedColumn ')' MaybeConflictClause
    { TablePrimaryKey $1 (fromJust $ mkOneOrMore $5) $7 }
    | MaybeConstraintName unique '(' OneOrMoreIndexedColumn ')' MaybeConflictClause
    { TableUnique $1 (fromJust $ mkOneOrMore $4) $6 }
    | MaybeConstraintName check '(' Expression ')'
    { TableCheck $1 $4 }
    | MaybeConstraintName foreign key '(' OneOrMoreUnqualifiedIdentifier ')'
      ForeignKeyClause
    { TableForeignKey $1 (fromJust $ mkOneOrMore $5) $7 }

OneOrMoreTableConstraint :: { [TableConstraint] }
    : TableConstraint
    { [$1] }
    | OneOrMoreTableConstraint ',' TableConstraint
    { $1 ++ [$3] }

EitherColumnsAndConstraintsSelect :: { EitherColumnsAndConstraintsSelect }
    : '(' OneOrMoreColumnDefinition ')'
    { ColumnsAndConstraints (fromJust $ mkOneOrMore $2) [] }
    | '(' OneOrMoreColumnDefinition ',' OneOrMoreTableConstraint ')'
    { ColumnsAndConstraints (fromJust $ mkOneOrMore $2) $4 }
    | as Select
    { AsSelect $2 }

TriggerTime :: { TriggerTime }
    : before
    { Before }
    | after
    { After }
    | instead of
    { InsteadOf }

TriggerCondition :: { TriggerCondition }
    : delete on
    { DeleteOn }
    | insert on
    { InsertOn }
    | update on
    { UpdateOn [] }
    | update of OneOrMoreUnqualifiedIdentifier on
    { UpdateOn $3 }

-- ModuleArgument :: { ModuleArgument }
--     :
--     { }
-- TODO definition (requires monadic parser)

TriggerStatement :: { TriggerStatement }
    : Update
    { TriggerStatement $1 }
    | Insert
    { TriggerStatement $1 }
    | Delete
    { TriggerStatement $1 }
    | Select
    { TriggerStatement $1 }

OneOrMoreTriggerStatement :: { [TriggerStatement] }
    : TriggerStatement
    { [$1] }
    | OneOrMoreTriggerStatement ';' TriggerStatement
    { $1 ++ [$3] }

QualifiedTableName :: { QualifiedTableName }
    : SinglyQualifiedIdentifier
    { TableNoIndexedBy $1 }
    | SinglyQualifiedIdentifier indexed by UnqualifiedIdentifier
    { TableIndexedBy $1 $4 }
    | SinglyQualifiedIdentifier not indexed
    { TableNotIndexed $1 }

OrderingTerm :: { OrderingTerm }
    : Expression MaybeCollation MaybeAscDesc
    { OrderingTerm $1 $2 $3 }

OneOrMoreOrderingTerm :: { [OrderingTerm] }
    : OrderingTerm
    { [$1] }
    | OneOrMoreOrderingTerm ',' OrderingTerm
    { $1 ++ [$3] }

PragmaBody :: { PragmaBody }
    :
    { EmptyPragmaBody }
    | '=' PragmaValue
    { EqualsPragmaBody $2 }
    | '(' PragmaValue ')'
    { CallPragmaBody $2 }

PragmaValue :: { PragmaValue }
    : MaybeSign integer
    { SignedIntegerPragmaValue $1 $2 }
    | MaybeSign float
    { SignedFloatPragmaValue $1 $2 }
    | UnqualifiedIdentifier
    { NamePragmaValue $1 }
    | string
    { StringPragmaValue $1 }

InsertHead :: { InsertHead }
    : insert
    { InsertNoAlternative }
    | insert or rollback
    { InsertOrRollback }
    | insert or abort
    { InsertOrAbort }
    | insert or replace
    { InsertOrReplace }
    | insert or fail
    { InsertOrFail }
    | insert or ignore
    { InsertOrIgnore }
    | replace
    { Replace }

InsertBody :: { InsertBody }
    : values '(' OneOrMoreExpression ')'
    { InsertValues [] (fromJust $ mkOneOrMore $3) }
    | '(' OneOrMoreUnqualifiedIdentifier ')' values '(' OneOrMoreExpression ')'
    { InsertValues $2 (fromJust $ mkOneOrMore $6) }
    | Select
    { InsertSelect [] $1 }
    | '(' OneOrMoreUnqualifiedIdentifier ')' Select
    { InsertSelect $2 $4 }
    | default values
    { InsertDefaultValues }

UpdateHead :: { UpdateHead }
    : update
    { UpdateNoAlternative }
    | update or rollback
    { UpdateOrRollback }
    | update or abort
    { UpdateOrAbort }
    | update or replace
    { UpdateOrReplace }
    | update or fail
    { UpdateOrFail }
    | update or ignore
    { UpdateOrIgnore }

Distinctness :: { Distinctness }
    :
    { NoDistinctness }
    | distinct
    { Distinct }
    | all
    { All }

MaybeHaving :: { MaybeHaving }
    :
    { NoHaving }
    | having Expression
    { Having $2 }

MaybeAs :: { MaybeAs }
    :
    { NoAs }
    | as UnqualifiedIdentifier
    { As $2 }
    | UnqualifiedIdentifier
    { ElidedAs $1 }

CompoundOperator :: { CompoundOperator }
    : union
    { Union }
    | union all
    { UnionAll }
    | intersect
    { Intersect }
    | except
    { Except }

SelectCore :: { SelectCore }
    : select Distinctness OneOrMoreResultColumn MaybeFromClause MaybeWhereClause
      MaybeGroupClause
    { SelectCore $2 (fromJust $ mkOneOrMore $3) $4 $5 $6 }

SelectCoreList :: { [(CompoundOperator, SelectCore)] }
    :
    { [] }
    | SelectCoreList CompoundOperator SelectCore
    { $1 ++ [($2, $3)] }

ResultColumn :: { ResultColumn }
    : '*'
    { Star }
    | UnqualifiedIdentifier '.' '*'
    { TableStar $1 }
    | Expression MaybeAs
    { Result $1 $2 }

OneOrMoreResultColumn :: { [ResultColumn] }
    : ResultColumn
    { [$1] }
    | OneOrMoreResultColumn ',' ResultColumn
    { $1 ++ [$3] }

JoinSource :: { JoinSource }
    : SingleSource ListJoins
    { JoinSource $1 $2 }

ListJoins :: { [(JoinOperation, SingleSource, JoinConstraint)] }
    :
    { [] }
    | ListJoins JoinOperation SingleSource JoinConstraint
    { $1 ++ [($2, $3, $4)] }

SingleSource :: { SingleSource }
    : SinglyQualifiedIdentifier MaybeAs MaybeIndexedBy
    { TableSource $1 $2 $3 }
    | '(' Select ')' MaybeAs
    { SelectSource $2 $4 }
    | '(' JoinSource ')'
    { SubjoinSource $2 }

JoinOperation :: { JoinOperation }
    : ','
    { Comma }
    | join
    { Join }
    | outer join
    { OuterJoin }
    | left join
    { LeftJoin }
    | left outer join
    { LeftOuterJoin }
    | inner join
    { InnerJoin }
    | cross join
    { CrossJoin }
    | natural join
    { NaturalJoin }
    | natural outer join
    { NaturalOuterJoin }
    | natural left join
    { NaturalLeftJoin }
    | natural left outer join
    { NaturalLeftOuterJoin }
    | natural inner join
    { NaturalInnerJoin }
    | natural cross join
    { NaturalCrossJoin }

JoinConstraint :: { JoinConstraint }
    :
    { NoConstraint }
    | on Expression
    { On $2 }
    | using '(' OneOrMoreUnqualifiedIdentifier ')'
    { Using (fromJust $ mkOneOrMore $3) }

MaybeIndexedBy :: { MaybeIndexedBy }
    :
    { NoIndexedBy }
    | indexed by UnqualifiedIdentifier
    { IndexedBy $3 }
    | not indexed
    { NotIndexed }

FromClause :: { FromClause }
    : from JoinSource
    { From $2 }

MaybeFromClause :: { Maybe FromClause }
    :
    { Nothing }
    | FromClause
    { Just $1 }

WhereClause :: { WhereClause }
    : where Expression
    { Where $2 }

MaybeWhereClause :: { Maybe WhereClause }
    :
    { Nothing }
    | WhereClause
    { Just $1 }

GroupClause :: { GroupClause }
    : group by OneOrMoreOrderingTerm MaybeHaving
    { GroupBy (fromJust $ mkOneOrMore $3) $4 }

MaybeGroupClause :: { Maybe GroupClause }
    :
    { Nothing }
    | GroupClause
    { Just $1 }

OrderClause :: { OrderClause }
    : order by OneOrMoreOrderingTerm
    { OrderBy (fromJust $ mkOneOrMore $3) }

MaybeOrderClause :: { Maybe OrderClause }
    :
    { Nothing }
    | OrderClause
    { Just $1 }

LimitClause :: { LimitClause }
    : limit integer
    { Limit $2 }
    | limit integer offset integer
    { LimitOffset $2 $4 }
    | limit integer ',' integer
    { LimitComma $2 $4 }

MaybeLimitClause :: { Maybe LimitClause }
    :
    { Nothing }
    | LimitClause
    { Just $1 }

WhenClause :: { WhenClause }
    : when Expression
    { When $2 }

MaybeWhenClause :: { Maybe WhenClause }
    :
    { Nothing }
    | WhenClause
    { Just $1 }

ConflictClause :: { ConflictClause }
    : on conflict rollback
    { OnConflictRollback }
    | on conflict abort
    { OnConflictAbort }
    | on conflict fail
    { OnConflictFail }
    | on conflict ignore
    { OnConflictIgnore }
    | on conflict replace
    { OnConflictReplace }

MaybeConflictClause :: { Maybe ConflictClause }
    :
    { Nothing }
    | ConflictClause
    { Just $1 }

ForeignKeyClause :: { ForeignKeyClause }
    : references UnqualifiedIdentifier ForeignKeyClauseActionOrMatchPartList
      MaybeForeignKeyClauseDeferrablePart
    { References $2 [] $3 $4 }
    | references UnqualifiedIdentifier '(' OneOrMoreUnqualifiedIdentifier ')'
      ForeignKeyClauseActionOrMatchPartList MaybeForeignKeyClauseDeferrablePart
    { References $2 $4 $6 $7 }

ForeignKeyClauseActionOrMatchPart :: { ForeignKeyClauseActionOrMatchPart }
    : on delete ForeignKeyClauseActionPart
    { OnDelete $3 }
    | on update ForeignKeyClauseActionPart
    { OnUpdate $3 }
    | match UnqualifiedIdentifier
    { ReferencesMatch $2 }

ForeignKeyClauseActionOrMatchPartList :: { [ForeignKeyClauseActionOrMatchPart] }
    :
    { [] }
    | ForeignKeyClauseActionOrMatchPartList ForeignKeyClauseActionOrMatchPart
    { $1 ++ [$2] }

ForeignKeyClauseActionPart :: { ForeignKeyClauseActionPart }
    : set null
    { SetNull }
    | set default
    { SetDefault }
    | cascade
    { Cascade }
    | restrict
    { Restrict }
    | no action
    { NoAction }

ForeignKeyClauseDeferrablePart :: { ForeignKeyClauseDeferrablePart }
    : deferrable MaybeInitialDeferralStatus
    { Deferrable $2 }
    | not deferrable MaybeInitialDeferralStatus
    { NotDeferrable $3 }

MaybeForeignKeyClauseDeferrablePart :: { Maybe ForeignKeyClauseDeferrablePart }
    : %prec LOOSER_THAN_NOT
    { Nothing }
    | ForeignKeyClauseDeferrablePart
    { Just $1 }

MaybeInitialDeferralStatus :: { MaybeInitialDeferralStatus }
    :
    { NoInitialDeferralStatus }
    | initially deferred
    { InitiallyDeferred }
    | initially immediate
    { InitiallyImmediate }

MaybeTransaction :: { MaybeTransaction }
    :
    { ElidedTransaction }
    | transaction
    { Transaction }

MaybeTransactionType :: { MaybeTransactionType }
    :
    { NoTransactionType }
    | deferred
    { Deferred }
    | immediate
    { Immediate }
    | exclusive
    { Exclusive }

MaybeDatabase :: { MaybeDatabase }
    :
    { ElidedDatabase }
    | database
    { Database }

MaybeSavepoint :: { MaybeSavepoint }
    :
    { NoSavepoint }
    | to UnqualifiedIdentifier
    { To $2 }
    | to savepoint UnqualifiedIdentifier
    { ToSavepoint $3 }

MaybeReleaseSavepoint :: { MaybeReleaseSavepoint }
    : UnqualifiedIdentifier
    { ElidedReleaseSavepoint $1 }
    | savepoint UnqualifiedIdentifier
    { ReleaseSavepoint $2 }

StatementList :: { StatementList }
    :
    { StatementList [] }
    | OneOrMoreStatement
    { StatementList $1 }

OneOrMoreStatement :: { [AnyStatement] }
    : Statement
    { [$1] }
    | OneOrMoreStatement ';' Statement
    { $1 ++ [$3] }

Statement :: { AnyStatement }
    : Explain
    { Statement $1 }
    | ExplainQueryPlan
    { Statement $1 }
    | AlterTable
    { Statement $1 }
    | Analyze
    { Statement $1 }
    | Attach
    { Statement $1 }
    | Begin
    { Statement $1 }
    | Commit
    { Statement $1 }
    | CreateIndex
    { Statement $1 }
    | CreateTable
    { Statement $1 }
    | CreateTrigger
    { Statement $1 }
    | CreateView
    { Statement $1 }
--     | CreateVirtualTable
--     { Statement $1 }
-- TODO don't forget to uncomment this
    | Delete
    { Statement $1 }
    | DeleteLimited
    { Statement $1 }
    | Detach
    { Statement $1 }
    | DropIndex
    { Statement $1 }
    | DropTable
    { Statement $1 }
    | DropTrigger
    { Statement $1 }
    | DropView
    { Statement $1 }
    | Insert
    { Statement $1 }
    | Pragma
    { Statement $1 }
    | Reindex
    { Statement $1 }
    | Release
    { Statement $1 }
    | Rollback
    { Statement $1 }
    | Savepoint
    { Statement $1 }
    | Select
    { Statement $1 }
    | Update
    { Statement $1 }
    | UpdateLimited
    { Statement $1 }
    | Vacuum
    { Statement $1 }

ExplainableStatement :: { ExplainableStatement }
    : AlterTable
    { ExplainableStatement $1 }
    | Analyze
    { ExplainableStatement $1 }
    | Attach
    { ExplainableStatement $1 }
    | Begin
    { ExplainableStatement $1 }
    | Commit
    { ExplainableStatement $1 }
    | CreateIndex
    { ExplainableStatement $1 }
    | CreateTable
    { ExplainableStatement $1 }
    | CreateTrigger
    { ExplainableStatement $1 }
--     | CreateVirtualTable
--     { ExplainableStatement $1 }
-- TODO don't forget to uncomment this
    | Delete
    { ExplainableStatement $1 }
    | DeleteLimited
    { ExplainableStatement $1 }
    | Detach
    { ExplainableStatement $1 }
    | DropIndex
    { ExplainableStatement $1 }
    | DropTable
    { ExplainableStatement $1 }
    | DropTrigger
    { ExplainableStatement $1 }
    | DropView
    { ExplainableStatement $1 }
    | Insert
    { ExplainableStatement $1 }
    | Pragma
    { ExplainableStatement $1 }
    | Reindex
    { ExplainableStatement $1 }
    | Release
    { ExplainableStatement $1 }
    | Rollback
    { ExplainableStatement $1 }
    | Savepoint
    { ExplainableStatement $1 }
    | Select
    { ExplainableStatement $1 }
    | Update
    { ExplainableStatement $1 }
    | UpdateLimited
    { ExplainableStatement $1 }
    | Vacuum
    { ExplainableStatement $1 }

Explain :: { Explain }
    : explain ExplainableStatement
    { Explain $2 }

ExplainQueryPlan :: { ExplainQueryPlan }
    : explain query plan ExplainableStatement
    { ExplainQueryPlan $4 }

AlterTable :: { AlterTable }
    : alter table SinglyQualifiedIdentifier AlterTableBody
    { AlterTable $3 $4 }

Analyze :: { Analyze }
    : analyze SinglyQualifiedIdentifier
    { Analyze $2 }

Attach :: { Attach }
    : attach MaybeDatabase string as UnqualifiedIdentifier
    { Attach $2 $3 $5 }

Begin :: { Begin }
    : begin MaybeTransactionType MaybeTransaction
    { Begin $2 $3 }

Commit :: { Commit }
    : commit MaybeTransaction
    { Commit False $2 }
    | end MaybeTransaction
    { Commit True $2 }

CreateIndex :: { CreateIndex }
    : create MaybeUnique index MaybeIfNotExists SinglyQualifiedIdentifier on
      UnqualifiedIdentifier '(' OneOrMoreIndexedColumn ')'
    { CreateIndex $2 $4 $5 $7 (fromJust $ mkOneOrMore $9) }

CreateTable :: { CreateTable }
    : create Permanence table MaybeIfNotExists SinglyQualifiedIdentifier
      EitherColumnsAndConstraintsSelect
    { CreateTable $2 $4 $5 $6 }

CreateTrigger :: { CreateTrigger }
    : create Permanence trigger MaybeIfNotExists SinglyQualifiedIdentifier
      TriggerTime TriggerCondition UnqualifiedIdentifier MaybeForEachRow
      MaybeWhenClause begin OneOrMoreTriggerStatement ';' end
    { CreateTrigger $2 $4 $5 $6 $7 $8 $9 $10 (fromJust $ mkOneOrMore $12) }

CreateView :: { CreateView }
    : create Permanence view MaybeIfNotExists SinglyQualifiedIdentifier as Select
    { CreateView $2 $4 $5 $7 }

-- CreateVirtualTable :: { CreateVirtualTable }
--     :
--     { }
-- TODO definition (requires monadic parser)

Delete :: { Delete }
    : delete from QualifiedTableName MaybeWhereClause
    { Delete $3 $4 }

DeleteLimited :: { DeleteLimited }
    : delete from QualifiedTableName MaybeWhereClause MaybeOrderClause LimitClause
    { DeleteLimited $3 $4 $5 $6 }

DeleteOrDeleteLimited :: { AnyStatement }
    : Delete
    { Statement $1 }
    | DeleteLimited
    { Statement $1 }

Detach :: { Detach }
    : detach UnqualifiedIdentifier
    { Detach False $2 }
    | detach database UnqualifiedIdentifier
    { Detach True $3 }

DropIndex :: { DropIndex }
    : drop index MaybeIfExists SinglyQualifiedIdentifier
    { DropIndex $3 $4 }

DropTable :: { DropTable }
    : drop table MaybeIfExists SinglyQualifiedIdentifier
    { DropTable $3 $4 }

DropTrigger :: { DropTrigger }
    : drop trigger MaybeIfExists SinglyQualifiedIdentifier
    { DropTrigger $3 $4 }

DropView :: { DropView }
    : drop view MaybeIfExists SinglyQualifiedIdentifier
    { DropView $3 $4 }

Insert :: { Insert }
    : InsertHead into SinglyQualifiedIdentifier InsertBody
    { Insert $1 $3 $4 }

Pragma :: { Pragma }
    : pragma SinglyQualifiedIdentifier PragmaBody
    { Pragma $2 $3 }

Reindex :: { Reindex }
    : reindex SinglyQualifiedIdentifier
    { Reindex $2 }

Release :: { Release }
    : release MaybeReleaseSavepoint UnqualifiedIdentifier
    { Release $2 $3 }

Rollback :: { Rollback }
    : rollback MaybeTransaction MaybeSavepoint
    { Rollback $2 $3 }

Savepoint :: { Savepoint }
    : savepoint UnqualifiedIdentifier
    { Savepoint $2 }

Select :: { Select }
    : SelectCore SelectCoreList MaybeOrderClause MaybeLimitClause
    { Select $1 $2 $3 $4 }

Update :: { Update }
    : UpdateHead QualifiedTableName set OneOrMoreSetPair MaybeWhereClause
    { Update $1 $2 (fromJust $ mkOneOrMore $4) $5 }

UpdateLimited :: { UpdateLimited }
    : UpdateHead QualifiedTableName set OneOrMoreSetPair MaybeWhereClause
      MaybeOrderClause LimitClause
    { UpdateLimited $1 $2 (fromJust $ mkOneOrMore $4) $5 $6 $7 }

UpdateOrUpdateLimited :: { AnyStatement }
    : Update
    { Statement $1 }
    | UpdateLimited
    { Statement $1 }

Vacuum :: { Vacuum }
    : vacuum
    { Vacuum }

UnqualifiedIdentifier :: { UnqualifiedIdentifier }
    : identifier
    { UnqualifiedIdentifier $1 }

OneOrMoreUnqualifiedIdentifier :: { [UnqualifiedIdentifier] }
    : UnqualifiedIdentifier
    { [$1] }
    | OneOrMoreUnqualifiedIdentifier ',' UnqualifiedIdentifier
    { $1 ++ [$3] }

SinglyQualifiedIdentifier :: { SinglyQualifiedIdentifier }
    : UnqualifiedIdentifier %prec LOOSER_THAN_DOT
    { let UnqualifiedIdentifier properName = $1
      in SinglyQualifiedIdentifier Nothing properName }
    | UnqualifiedIdentifier '.' UnqualifiedIdentifier
    { let { UnqualifiedIdentifier parentName = $1;
            UnqualifiedIdentifier properName = $3 }
      in SinglyQualifiedIdentifier (Just parentName) properName }

DoublyQualifiedIdentifier :: { DoublyQualifiedIdentifier }
    : SinglyQualifiedIdentifier
    { case $1 of
        SinglyQualifiedIdentifier Nothing properName
          -> DoublyQualifiedIdentifier Nothing properName
        SinglyQualifiedIdentifier (Just parentName) properName
          -> DoublyQualifiedIdentifier (Just (parentName, Nothing)) properName }
    | SinglyQualifiedIdentifier '.' identifier
    { let properName = $3
      in case $1 of
           SinglyQualifiedIdentifier Nothing parentName
             -> DoublyQualifiedIdentifier (Just (parentName, Nothing))
                                          properName
           SinglyQualifiedIdentifier (Just grandparentName) parentName
             -> DoublyQualifiedIdentifier (Just (parentName, Just grandparentName))
                                          properName }

{

data ParseError = ParseError {
      parseErrorMessage :: String,
      parseErrorLineNumber :: Word64
    }
instance Error ParseError where
    strMsg message = ParseError {
                            parseErrorMessage = message,
                            parseErrorLineNumber = 0
                          }
instance Show ParseError where
    show parseError = "Line " ++ (show $ parseErrorLineNumber parseError)
                      ++ " of SQL: " ++ (parseErrorMessage parseError)
data ParseState = ParseState {
      parseStateInput :: String,
      parseStateLineNumber :: Word64
    }
type Parse = ErrorT ParseError (State ParseState)

throwParseError :: String -> Parse a
throwParseError message = do
  state <- getParseState
  let lineNumber = parseStateLineNumber state
      error = ParseError {
                parseErrorMessage = message,
                parseErrorLineNumber = lineNumber
              }
  throwError error

getParseState :: Parse ParseState
getParseState = lift get

putParseState :: ParseState -> Parse ()
putParseState state = lift $ put state


readType :: String -> Either ParseError Type
readType input = runParse parseType input


readMaybeTypeSize :: String -> Either ParseError MaybeTypeSize
readMaybeTypeSize input = runParse parseMaybeTypeSize input


readTypeSizeField :: String -> Either ParseError TypeSizeField
readTypeSizeField input = runParse parseTypeSizeField input


readLikeType :: String -> Either ParseError LikeType
readLikeType input = runParse parseLikeType input


readExpression :: String -> Either ParseError Expression
readExpression input = runParse parseExpression input


readMaybeUnique :: String -> Either ParseError MaybeUnique
readMaybeUnique input = runParse parseMaybeUnique input


readMaybeIfNotExists :: String -> Either ParseError MaybeIfNotExists
readMaybeIfNotExists input = runParse parseMaybeIfNotExists input


readMaybeIfExists :: String -> Either ParseError MaybeIfExists
readMaybeIfExists input = runParse parseMaybeIfExists input


readMaybeForEachRow :: String -> Either ParseError MaybeForEachRow
readMaybeForEachRow input = runParse parseMaybeForEachRow input


readPermanence :: String -> Either ParseError Permanence
readPermanence input = runParse parsePermanence input


readMaybeCollation :: String -> Either ParseError MaybeCollation
readMaybeCollation input = runParse parseMaybeCollation input


readMaybeAscDesc :: String -> Either ParseError MaybeAscDesc
readMaybeAscDesc input = runParse parseMaybeAscDesc input


readMaybeAutoincrement :: String -> Either ParseError MaybeAutoincrement
readMaybeAutoincrement input = runParse parseMaybeAutoincrement input


readMaybeSign :: String -> Either ParseError MaybeSign
readMaybeSign input = runParse parseMaybeSign input


readMaybeColumn :: String -> Either ParseError MaybeColumn
readMaybeColumn input = runParse parseMaybeColumn input


readAlterTableBody :: String -> Either ParseError AlterTableBody
readAlterTableBody input = runParse parseAlterTableBody input


readColumnDefinition :: String -> Either ParseError ColumnDefinition
readColumnDefinition input = runParse parseColumnDefinition input


readDefaultValue :: String -> Either ParseError DefaultValue
readDefaultValue input = runParse parseDefaultValue input


readIndexedColumn :: String -> Either ParseError IndexedColumn
readIndexedColumn input = runParse parseIndexedColumn input


readColumnConstraint :: String -> Either ParseError ColumnConstraint
readColumnConstraint input = runParse parseColumnConstraint input


readTableConstraint :: String -> Either ParseError TableConstraint
readTableConstraint input = runParse parseTableConstraint input


readTriggerTime :: String -> Either ParseError TriggerTime
readTriggerTime input = runParse parseTriggerTime input


readTriggerCondition :: String -> Either ParseError TriggerCondition
readTriggerCondition input = runParse parseTriggerCondition input


{- TODO remember to uncomment this
readModuleArgument :: String -> Either ParseError ModuleArgument
readModuleArgument input = runParse parseModuleArgument input
-}


readTriggerStatement :: String -> Either ParseError TriggerStatement
readTriggerStatement input = runParse parseTriggerStatement input


readQualifiedTableName :: String -> Either ParseError QualifiedTableName
readQualifiedTableName input = runParse parseQualifiedTableName input


readOrderingTerm :: String -> Either ParseError OrderingTerm
readOrderingTerm input = runParse parseOrderingTerm input


readPragmaBody :: String -> Either ParseError PragmaBody
readPragmaBody input = runParse parsePragmaBody input


readPragmaValue :: String -> Either ParseError PragmaValue
readPragmaValue input = runParse parsePragmaValue input


readEitherColumnsAndConstraintsSelect
    :: String -> Either ParseError EitherColumnsAndConstraintsSelect
readEitherColumnsAndConstraintsSelect input
    = runParse parseEitherColumnsAndConstraintsSelect input


readInsertHead :: String -> Either ParseError InsertHead
readInsertHead input = runParse parseInsertHead input


readInsertBody :: String -> Either ParseError InsertBody
readInsertBody input = runParse parseInsertBody input


readUpdateHead :: String -> Either ParseError UpdateHead
readUpdateHead input = runParse parseUpdateHead input


readDistinctness :: String -> Either ParseError Distinctness
readDistinctness input = runParse parseDistinctness input


readMaybeHaving :: String -> Either ParseError MaybeHaving
readMaybeHaving input = runParse parseMaybeHaving input


readMaybeAs :: String -> Either ParseError MaybeAs
readMaybeAs input = runParse parseMaybeAs input


readCompoundOperator :: String -> Either ParseError CompoundOperator
readCompoundOperator input = runParse parseCompoundOperator input


readSelectCore :: String -> Either ParseError SelectCore
readSelectCore input = runParse parseSelectCore input


readResultColumn :: String -> Either ParseError ResultColumn
readResultColumn input = runParse parseResultColumn input


readJoinSource :: String -> Either ParseError JoinSource
readJoinSource input = runParse parseJoinSource input


readSingleSource :: String -> Either ParseError SingleSource
readSingleSource input = runParse parseSingleSource input


readJoinOperation :: String -> Either ParseError JoinOperation
readJoinOperation input = runParse parseJoinOperation input


readJoinConstraint :: String -> Either ParseError JoinConstraint
readJoinConstraint input = runParse parseJoinConstraint input


readMaybeIndexedBy :: String -> Either ParseError MaybeIndexedBy
readMaybeIndexedBy input = runParse parseMaybeIndexedBy input


readFromClause :: String -> Either ParseError FromClause
readFromClause input = runParse parseFromClause input


readWhereClause :: String -> Either ParseError WhereClause
readWhereClause input = runParse parseWhereClause input


readGroupClause :: String -> Either ParseError GroupClause
readGroupClause input = runParse parseGroupClause input


readOrderClause :: String -> Either ParseError OrderClause
readOrderClause input = runParse parseOrderClause input


readLimitClause :: String -> Either ParseError LimitClause
readLimitClause input = runParse parseLimitClause input


readWhenClause :: String -> Either ParseError WhenClause
readWhenClause input = runParse parseWhenClause input


readConflictClause :: String -> Either ParseError ConflictClause
readConflictClause input = runParse parseConflictClause input


readForeignKeyClause :: String -> Either ParseError ForeignKeyClause
readForeignKeyClause input = runParse parseForeignKeyClause input


readForeignKeyClauseActionOrMatchPart
    :: String -> Either ParseError ForeignKeyClauseActionOrMatchPart
readForeignKeyClauseActionOrMatchPart input
    = runParse parseForeignKeyClauseActionOrMatchPart input


readForeignKeyClauseActionPart
    :: String -> Either ParseError ForeignKeyClauseActionPart
readForeignKeyClauseActionPart input
    = runParse parseForeignKeyClauseActionPart input


readForeignKeyClauseDeferrablePart
    :: String -> Either ParseError ForeignKeyClauseDeferrablePart
readForeignKeyClauseDeferrablePart input
    = runParse parseForeignKeyClauseDeferrablePart input


readMaybeInitialDeferralStatus
    :: String -> Either ParseError MaybeInitialDeferralStatus
readMaybeInitialDeferralStatus input
    = runParse parseMaybeInitialDeferralStatus input


readMaybeTransaction :: String -> Either ParseError MaybeTransaction
readMaybeTransaction input = runParse parseMaybeTransaction input


readMaybeTransactionType :: String -> Either ParseError MaybeTransactionType
readMaybeTransactionType input = runParse parseMaybeTransactionType input


readMaybeDatabase :: String -> Either ParseError MaybeDatabase
readMaybeDatabase input = runParse parseMaybeDatabase input


readMaybeSavepoint :: String -> Either ParseError MaybeSavepoint
readMaybeSavepoint input = runParse parseMaybeSavepoint input


readMaybeReleaseSavepoint :: String -> Either ParseError MaybeReleaseSavepoint
readMaybeReleaseSavepoint input = runParse parseMaybeReleaseSavepoint input


readStatementList :: String -> Either ParseError StatementList
readStatementList input = runParse parseStatementList input


readAnyStatement :: String -> Either ParseError AnyStatement
readAnyStatement input = runParse parseAnyStatement input


readExplainableStatement :: String -> Either ParseError ExplainableStatement
readExplainableStatement input = runParse parseExplainableStatement input


readExplain :: String -> Either ParseError Explain
readExplain input = runParse parseExplain input


readExplainQueryPlan :: String -> Either ParseError ExplainQueryPlan
readExplainQueryPlan input = runParse parseExplainQueryPlan input


readAlterTable :: String -> Either ParseError AlterTable
readAlterTable input = runParse parseAlterTable input


readAnalyze :: String -> Either ParseError Analyze
readAnalyze input = runParse parseAnalyze input


readAttach :: String -> Either ParseError Attach
readAttach input = runParse parseAttach input


readBegin :: String -> Either ParseError Begin
readBegin input = runParse parseBegin input


readCommit :: String -> Either ParseError Commit
readCommit input = runParse parseCommit input


readCreateIndex :: String -> Either ParseError CreateIndex
readCreateIndex input = runParse parseCreateIndex input


readCreateTable :: String -> Either ParseError CreateTable
readCreateTable input = runParse parseCreateTable input


readCreateTrigger :: String -> Either ParseError CreateTrigger
readCreateTrigger input = runParse parseCreateTrigger input


readCreateView :: String -> Either ParseError CreateView
readCreateView input = runParse parseCreateView input


{- TODO remember to uncomment this
readCreateVirtualTable :: String -> Either ParseError CreateVirtualTable
readCreateVirtualTable input = runParse parseCreateVirtualTable input
-}


readDelete :: String -> Either ParseError Delete
readDelete input = runParse parseDelete input


readDeleteLimited :: String -> Either ParseError DeleteLimited
readDeleteLimited input = runParse parseDeleteLimited input


readDeleteOrDeleteLimited :: String -> Either ParseError AnyStatement
readDeleteOrDeleteLimited input = runParse parseDeleteOrDeleteLimited input


readDetach :: String -> Either ParseError Detach
readDetach input = runParse parseDetach input


readDropIndex :: String -> Either ParseError DropIndex
readDropIndex input = runParse parseDropIndex input


readDropTable :: String -> Either ParseError DropTable
readDropTable input = runParse parseDropTable input


readDropTrigger :: String -> Either ParseError DropTrigger
readDropTrigger input = runParse parseDropTrigger input


readDropView :: String -> Either ParseError DropView
readDropView input = runParse parseDropView input


readInsert :: String -> Either ParseError Insert
readInsert input = runParse parseInsert input


readPragma :: String -> Either ParseError Pragma
readPragma input = runParse parsePragma input


readReindex :: String -> Either ParseError Reindex
readReindex input = runParse parseReindex input


readRelease :: String -> Either ParseError Release
readRelease input = runParse parseRelease input


readRollback :: String -> Either ParseError Rollback
readRollback input = runParse parseRollback input


readSavepoint :: String -> Either ParseError Savepoint
readSavepoint input = runParse parseSavepoint input


readSelect :: String -> Either ParseError Select
readSelect input = runParse parseSelect input


readUpdate :: String -> Either ParseError Update
readUpdate input = runParse parseUpdate input


readUpdateLimited :: String -> Either ParseError UpdateLimited
readUpdateLimited input = runParse parseUpdateLimited input


readUpdateOrUpdateLimited :: String -> Either ParseError AnyStatement
readUpdateOrUpdateLimited input = runParse parseUpdateOrUpdateLimited input


readVacuum :: String -> Either ParseError Vacuum
readVacuum input = runParse parseVacuum input


readUnqualifiedIdentifier :: String -> Either ParseError UnqualifiedIdentifier
readUnqualifiedIdentifier input = runParse parseUnqualifiedIdentifier input


readSinglyQualifiedIdentifier
    :: String -> Either ParseError SinglyQualifiedIdentifier
readSinglyQualifiedIdentifier input
    = runParse parseSinglyQualifiedIdentifier input


readDoublyQualifiedIdentifier :: String -> Either ParseError DoublyQualifiedIdentifier
readDoublyQualifiedIdentifier input
    = runParse parseDoublyQualifiedIdentifier input


parseError :: Token -> Parse a
parseError token = throwParseError
                     $ "Parsing error near " ++ (show $ show token) ++ "."


runParse :: (Parse a) -> String -> Either ParseError a
runParse parser input =
    let initialState = ParseState {
                         parseStateInput = input,
                         parseStateLineNumber = 1
                       }
    in evalState (runErrorT parser) initialState


lexerTakingContinuation :: (Token -> Parse a) -> Parse a
lexerTakingContinuation continuation = do
  state <- getParseState
  (token, rest) <- lexer $ parseStateInput state
  state <- getParseState
  putParseState $ state { parseStateInput = rest }
  continuation token


lexer :: String -> Parse (Token, String)
lexer "" = return (EndOfInputToken, "")
lexer all@('.':c:_) | isDigit c = readNumericLiteral all
lexer ('!':'=':rest) = return (PunctuationBangEquals, rest)
lexer ('%':rest) = return (PunctuationPercent, rest)
lexer ('&':rest) = return (PunctuationAmpersand, rest)
lexer ('(':rest) = return (PunctuationLeftParenthesis, rest)
lexer (')':rest) = return (PunctuationRightParenthesis, rest)
lexer ('*':rest) = return (PunctuationStar, rest)
lexer ('+':rest) = return (PunctuationPlus, rest)
lexer (',':rest) = return (PunctuationComma, rest)
lexer ('-':rest) = return (PunctuationMinus, rest)
lexer ('.':rest) = return (PunctuationDot, rest)
lexer ('/':rest) = return (PunctuationSlash, rest)
lexer (';':rest) = return (PunctuationSemicolon, rest)
lexer ('<':'<':rest) = return (PunctuationLessLess, rest)
lexer ('<':'=':rest) = return (PunctuationLessEquals, rest)
lexer ('<':'>':rest) = return (PunctuationLessGreater, rest)
lexer ('<':rest) = return (PunctuationLess, rest)
lexer ('=':'=':rest) = return (PunctuationEqualsEquals, rest)
lexer ('=':rest) = return (PunctuationEquals, rest)
lexer ('>':'=':rest) = return (PunctuationGreaterEquals, rest)
lexer ('>':'>':rest) = return (PunctuationGreaterGreater, rest)
lexer ('>':rest) = return (PunctuationGreater, rest)
lexer ('|':'|':rest) = return (PunctuationBarBar, rest)
lexer ('|':rest) = return (PunctuationBar, rest)
lexer ('~':rest) = return (PunctuationTilde, rest)
lexer ('\n':rest) = do
  state <- getParseState
  let lineNumber = parseStateLineNumber state
  putParseState $ state { parseStateLineNumber = lineNumber + 1 }
  lexer rest
lexer all@('x':'\'':_) = readBlobLiteral all
lexer all@('X':'\'':_) = readBlobLiteral all
lexer all@('\'':_) = readStringLiteral all
lexer all@('"':_) = readQuotedIdentifier all
lexer all@(c:_)
  | isDigit c = readNumericLiteral all
  | isAlpha c = let (identifierOrKeyword, rest) = readIdentifierOrKeyword all
                    keyword = map toLower identifierOrKeyword
                    identifier = identifierOrKeyword
                in case identifierOrKeyword of
                  _ | keyword == "abort" -> return (KeywordAbort, rest)
                    | keyword == "action" -> return (KeywordAction, rest)
                    | keyword == "add" -> return (KeywordAdd, rest)
                    | keyword == "after" -> return (KeywordAfter, rest)
                    | keyword == "all" -> return (KeywordAll, rest)
                    | keyword == "alter" -> return (KeywordAlter, rest)
                    | keyword == "analyze" -> return (KeywordAnalyze, rest)
                    | keyword == "and" -> return (KeywordAnd, rest)
                    | keyword == "as" -> return (KeywordAs, rest)
                    | keyword == "asc" -> return (KeywordAsc, rest)
                    | keyword == "attach" -> return (KeywordAttach, rest)
                    | keyword == "autoincrement"
                        -> return (KeywordAutoincrement, rest)
                    | keyword == "before" -> return (KeywordBefore, rest)
                    | keyword == "begin" -> return (KeywordBegin, rest)
                    | keyword == "between" -> return (KeywordBetween, rest)
                    | keyword == "by" -> return (KeywordBy, rest)
                    | keyword == "cascade" -> return (KeywordCascade, rest)
                    | keyword == "case" -> return (KeywordCase, rest)
                    | keyword == "cast" -> return (KeywordCast, rest)
                    | keyword == "check" -> return (KeywordCheck, rest)
                    | keyword == "collate" -> return (KeywordCollate, rest)
                    | keyword == "column" -> return (KeywordColumn, rest)
                    | keyword == "commit" -> return (KeywordCommit, rest)
                    | keyword == "conflict" -> return (KeywordConflict, rest)
                    | keyword == "constraint" -> return (KeywordConstraint, rest)
                    | keyword == "create" -> return (KeywordCreate, rest)
                    | keyword == "cross" -> return (KeywordCross, rest)
                    | keyword == "current_date" -> return (KeywordCurrentDate, rest)
                    | keyword == "current_time" -> return (KeywordCurrentTime, rest)
                    | keyword == "current_timestamp"
                        -> return (KeywordCurrentTimestamp, rest)
                    | keyword == "database" -> return (KeywordDatabase, rest)
                    | keyword == "default" -> return (KeywordDefault, rest)
                    | keyword == "deferrable" -> return (KeywordDeferrable, rest)
                    | keyword == "deferred" -> return (KeywordDeferred, rest)
                    | keyword == "delete" -> return (KeywordDelete, rest)
                    | keyword == "desc" -> return (KeywordDesc, rest)
                    | keyword == "detach" -> return (KeywordDetach, rest)
                    | keyword == "distinct" -> return (KeywordDistinct, rest)
                    | keyword == "drop" -> return (KeywordDrop, rest)
                    | keyword == "each" -> return (KeywordEach, rest)
                    | keyword == "else" -> return (KeywordElse, rest)
                    | keyword == "end" -> return (KeywordEnd, rest)
                    | keyword == "escape" -> return (KeywordEscape, rest)
                    | keyword == "except" -> return (KeywordExcept, rest)
                    | keyword == "exclusive" -> return (KeywordExclusive, rest)
                    | keyword == "exists" -> return (KeywordExists, rest)
                    | keyword == "explain" -> return (KeywordExplain, rest)
                    | keyword == "fail" -> return (KeywordFail, rest)
                    | keyword == "for" -> return (KeywordFor, rest)
                    | keyword == "foreign" -> return (KeywordForeign, rest)
                    | keyword == "from" -> return (KeywordFrom, rest)
                    | keyword == "full" -> return (KeywordFull, rest)
                    | keyword == "glob" -> return (KeywordGlob, rest)
                    | keyword == "group" -> return (KeywordGroup, rest)
                    | keyword == "having" -> return (KeywordHaving, rest)
                    | keyword == "if" -> return (KeywordIf, rest)
                    | keyword == "ignore" -> return (KeywordIgnore, rest)
                    | keyword == "immediate" -> return (KeywordImmediate, rest)
                    | keyword == "in" -> return (KeywordIn, rest)
                    | keyword == "index" -> return (KeywordIndex, rest)
                    | keyword == "indexed" -> return (KeywordIndexed, rest)
                    | keyword == "initially" -> return (KeywordInitially, rest)
                    | keyword == "inner" -> return (KeywordInner, rest)
                    | keyword == "insert" -> return (KeywordInsert, rest)
                    | keyword == "instead" -> return (KeywordInstead, rest)
                    | keyword == "intersect" -> return (KeywordIntersect, rest)
                    | keyword == "into" -> return (KeywordInto, rest)
                    | keyword == "is" -> return (KeywordIs, rest)
                    | keyword == "isnull" -> return (KeywordIsnull, rest)
                    | keyword == "join" -> return (KeywordJoin, rest)
                    | keyword == "key" -> return (KeywordKey, rest)
                    | keyword == "left" -> return (KeywordLeft, rest)
                    | keyword == "like" -> return (KeywordLike, rest)
                    | keyword == "limit" -> return (KeywordLimit, rest)
                    | keyword == "match" -> return (KeywordMatch, rest)
                    | keyword == "natural" -> return (KeywordNatural, rest)
                    | keyword == "no" -> return (KeywordNo, rest)
                    | keyword == "not" -> return (KeywordNot, rest)
                    | keyword == "notnull" -> return (KeywordNotnull, rest)
                    | keyword == "null" -> return (KeywordNull, rest)
                    | keyword == "of" -> return (KeywordOf, rest)
                    | keyword == "offset" -> return (KeywordOffset, rest)
                    | keyword == "on" -> return (KeywordOn, rest)
                    | keyword == "or" -> return (KeywordOr, rest)
                    | keyword == "order" -> return (KeywordOrder, rest)
                    | keyword == "outer" -> return (KeywordOuter, rest)
                    | keyword == "plan" -> return (KeywordPlan, rest)
                    | keyword == "pragma" -> return (KeywordPragma, rest)
                    | keyword == "primary" -> return (KeywordPrimary, rest)
                    | keyword == "query" -> return (KeywordQuery, rest)
                    | keyword == "raise" -> return (KeywordRaise, rest)
                    | keyword == "references" -> return (KeywordReferences, rest)
                    | keyword == "regexp" -> return (KeywordRegexp, rest)
                    | keyword == "reindex" -> return (KeywordReindex, rest)
                    | keyword == "release" -> return (KeywordRelease, rest)
                    | keyword == "rename" -> return (KeywordRename, rest)
                    | keyword == "replace" -> return (KeywordReplace, rest)
                    | keyword == "restrict" -> return (KeywordRestrict, rest)
                    | keyword == "right" -> return (KeywordRight, rest)
                    | keyword == "rollback" -> return (KeywordRollback, rest)
                    | keyword == "row" -> return (KeywordRow, rest)
                    | keyword == "savepoint" -> return (KeywordSavepoint, rest)
                    | keyword == "select" -> return (KeywordSelect, rest)
                    | keyword == "set" -> return (KeywordSet, rest)
                    | keyword == "table" -> return (KeywordTable, rest)
                    | keyword == "temp" -> return (KeywordTemp, rest)
                    | keyword == "temporary" -> return (KeywordTemporary, rest)
                    | keyword == "then" -> return (KeywordThen, rest)
                    | keyword == "to" -> return (KeywordTo, rest)
                    | keyword == "transaction" -> return (KeywordTransaction, rest)
                    | keyword == "trigger" -> return (KeywordTrigger, rest)
                    | keyword == "union" -> return (KeywordUnion, rest)
                    | keyword == "unique" -> return (KeywordUnique, rest)
                    | keyword == "update" -> return (KeywordUpdate, rest)
                    | keyword == "using" -> return (KeywordUsing, rest)
                    | keyword == "vacuum" -> return (KeywordVacuum, rest)
                    | keyword == "values" -> return (KeywordValues, rest)
                    | keyword == "view" -> return (KeywordView, rest)
                    | keyword == "virtual" -> return (KeywordVirtual, rest)
                    | keyword == "when" -> return (KeywordWhen, rest)
                    | keyword == "where" -> return (KeywordWhere, rest)
                    | otherwise -> return (Identifier identifier, rest)
  | isSpace c = lexer $ drop 1 all
  | otherwise = throwParseError $ "Unexpected character '" ++ [c] ++ "'."


readStringLiteral :: String -> Parse (Token, String)
readStringLiteral input = do
    let readString' ('\'':('\'':rest)) = do
          (a, b) <- readString' rest
          return ("'" ++ a, b)
        readString' ('\'':rest) = return ("", rest)
        readString' (c:rest) = do
          (a, b) <- readString' rest
          return ([c] ++ a, b)
        readString' "" = throwParseError
                           $ "Missing close-quote in string or blob literal."
    (string, unparsed) <- readString' $ drop 1 input
    return (LiteralString string, unparsed)


readBlobLiteral :: String -> Parse (Token, String)
readBlobLiteral input = do
    (LiteralString blobAsText, unparsed) <- readStringLiteral $ drop 1 input
    if (all isHexDigit blobAsText) && ((length blobAsText `mod` 2) == 0)
      then return (LiteralBlob $ read ("\"" ++ blobAsText ++ "\""), unparsed)
      else throwParseError $ "Invalid blob literal."


readQuotedIdentifier :: String -> Parse (Token, String)
readQuotedIdentifier input = do
    let readString' ('"':('"':rest)) = do
          (a, b) <- readString' rest
          return ("\"" ++ a, b)
        readString' ('"':rest) = return ("", rest)
        readString' (c:rest) = do
          (a, b) <- readString' rest
          return ([c] ++ a, b)
        readString' "" = throwParseError
                           $ "Missing close-quote in quoted identifier."
    (string, unparsed) <- readString' $ drop 1 input
    return (Identifier string, unparsed)


readNumericLiteral :: String -> Parse (Token, String)
readNumericLiteral input =
    let (initialDigitSpan, restInitialDigitSpan) = span isDigit input
        (dotSpan, secondDigitSpan, restSecondDigitSpan)
            = if (length restInitialDigitSpan > 0) && (head restInitialDigitSpan == '.')
                then let (secondDigitSpan, restSecondDigitSpan)
                             = span isDigit $ tail restInitialDigitSpan
                     in (take 1 restInitialDigitSpan,
                         secondDigitSpan,
                         restSecondDigitSpan)
                else ("", "", restInitialDigitSpan)
        (exponentESpan, exponentSignSpan, exponentDigitSpan, restExponent)
            = if (length restSecondDigitSpan > 0)
                 && (toLower (head restSecondDigitSpan) == 'e')
                then let (exponentESpan, restE) = (take 1 restSecondDigitSpan,
                                                   drop 1 restSecondDigitSpan)
                         hasExponentSign
                             = (length restE > 0) && (elem (head restE) "+-")
                         (exponentSignSpan, restSign)
                             = if hasExponentSign
                                 then (take 1 $ drop 1 restE, drop 1 restE)
                                 else ("", restE)
                         (exponentDigitSpan, restExponent) = span isDigit restSign
                     in (exponentESpan,
                         exponentSignSpan,
                         exponentDigitSpan,
                         restExponent)
                else ("", "", "", restSecondDigitSpan)
        floatSpan = initialDigitSpan ++ dotSpan ++ secondDigitSpan
                    ++ exponentESpan ++ exponentSignSpan ++ exponentDigitSpan
        isFollowedByIdentifierCharacter
            = case restExponent of
                "" -> False
                (c:_) | (isAlphaNum c) || (elem c "_$") -> True
                      | otherwise -> False
        (trailingIdentifierSpan, _)
            = span (\c -> (isAlphaNum c) || (elem c "_$")) restExponent
        errorSpan = floatSpan ++ trailingIdentifierSpan
        integerResult = let [(initialDigits, _)] = reads initialDigitSpan
                        in return (LiteralInteger initialDigits, restExponent)
        floatResult = let tweakedFloatSpan = if not $ isDigit $ head floatSpan
                                               then "0" ++ floatSpan
                                               else floatSpan
                          [(double, _)] = reads tweakedFloatSpan
                      in return (LiteralFloat $ fromJust $ mkNonnegativeDouble double,
                                 restExponent)
        errorResult = throwParseError $ "Invalid number token " ++ (show errorSpan)
    in case (initialDigitSpan,
             dotSpan,
             secondDigitSpan,
             exponentESpan,
             exponentSignSpan,
             exponentDigitSpan,
             isFollowedByIdentifierCharacter) of
         (_, _, _, _, _, _, True) -> errorResult
         ((_:_), "", "", "", "", "", _) -> integerResult
         (_, ".", _, _, _, _, _) -> floatResult
         (_, _, _, (_:_), _, (_:_), _) -> floatResult
         _ -> errorResult
        

readIdentifierOrKeyword :: String -> (String, String)
readIdentifierOrKeyword input
    = span (\c -> (isAlphaNum c) || (elem c "_$")) input

}
