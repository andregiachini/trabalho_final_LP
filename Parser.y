{
module Parser where 

import Lexer 
}

%name parser 
%tokentype { Token }
%error { parseError }

%left "||"
%left "&&"
%left '+' '-'
%left '*'
%left '#' 

%token 
    num             { TokenNum $$ }
    true            { TokenTrue }
    false           { TokenFalse }
    '+'             { TokenPlus }
    '*'             { TokenTimes }
    "&&"            { TokenAnd }
    "||"            { TokenOr }
    '('             { TokenLParen }
    ')'             { TokenRParen }
    if              { TokenIf }
    then            { TokenThen }
    else            { TokenElse }
    lam             { TokenLam }
    "->"            { TokenArrow }
    ':'             { TokenColon }
    ','             { TokenComma }
    '#'             { TokenHash }
    var             { TokenVar $$ }
    Num             { TokenTNum }
    Bool            { TokenTBool }
    Tuple           { TokenTTuple }

%% 

Exp     : num                       { Num $1 }
        | true                      { BTrue }
        | false                     { BFalse }
        | Exp '+' Exp               { Add  $1 $3 }
        | Exp '*' Exp               { Times $1 $3 }
        | Exp "&&" Exp              { And  $1 $3 }
        | Exp "||" Exp              { Or   $1 $3 }
        | '(' Exp ')'               { Paren $2 }
        | if Exp then Exp else Exp  { If   $2 $4 $6 }
        | var                       { Var $1 }
        | lam var ':' Type "->" Exp { Lam $2 $4 $6 }
        | Exp Exp                   { App $1 $2 }
        | '(' TupleContent ')'      { Tuple $2 }
        | '#' num Exp               { Proj $2 $3 }

TupleContent : Exp ',' Exp          { [$1, $3] }
             | Exp ',' TupleContent { $1 : $3 }

Type    : Num                       { TNum }
        | Bool                      { TBool }
        | Tuple TypeList            { TTuple $2 }
        | Type "->" Type            { TFun $1 $3 }
        | '(' Type ')'              { $2 }

TypeList : Type                     { [$1] }
         | Type ',' TypeList        { $1 : $3 }

{ 
parseError :: [Token] -> a 
parseError _ = error "Syntax error!"
}