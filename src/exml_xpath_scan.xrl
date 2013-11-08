%% This is the grammar taken from:
%% http://www.w3.org/TR/1999/REC-xpath-19991116/#exprlex
%%
%% NodeType, FunctionName, AxisName, MultiplyOperator and OperatorName
%% are recognized later due to special lexical rules
%%
Definitions.
Digit = [0-9]
Whitespace = [\000-\s]+
Literal = ('[^\']+'|"[^\"]+")

Name = [a-zA-Z-]+
QualifiedName = ({Name}:{Name}|{Name})
Reference = \${QualifiedName}

Rules.
%% Expression tokens
\(   : {token, {'(', TokenLine}}.
\)   : {token, {')', TokenLine}}.
\[   : {token, {'[', TokenLine}}.
\]   : {token, {']', TokenLine}}.
\.\. : {token, {'..', TokenLine}}.
\.   : {token, {'.', TokenLine}}.
@    : {token, {'@', TokenLine}}.
,    : {token, {',', TokenLine}}.
::   : {token, {'::', TokenLine}}.
%% Operator
\* : {token, {'*', TokenLine}}.
// : {token, {'//', TokenLine}}.
/  : {token, {'/', TokenLine}}.
\| : {token, {'|', TokenLine}}.
\+ : {token, {'+', TokenLine}}.
-  : {token, {'-', TokenLine}}.
=  : {token, {'=', TokenLine}}.
!= : {token, {'!=', TokenLine}}.
<  : {token, {'<', TokenLine}}.
<= : {token, {'<=', TokenLine}}.
>  : {token, {'>', TokenLine}}.
>= : {token, {'>=', TokenLine}}.
%% Literal
{Literal}    : {token, {literal, TokenLine, literal(TokenChars, TokenLen)}}.
%% Number
{Digit}+           : {token, {number, TokenLine, list_to_integer(TokenChars)}}.
{Digit}+\.{Digit}+ : {token, {number, TokenLine, list_to_float(TokenChars)}}.
%% Reference
{Reference} : {token, {reference, TokenLine, reference(TokenChars, TokenLen)}}.
%% Name test
\*              : {token, {'*', TokenLine}}.
{Name}:\*       : {token, {prefix, TokenLine, nc_name(TokenChars)}}.
{Name}:{Name}   : {token, {name, TokenLine, q_name(TokenChars)}}.
{Name}          : {token, {name, TokenLine, TokenChars}}.
%% Whitespace
{Whitespace} : skip_token.

Erlang code.
literal(TokenChars, TokenLen) ->
    lists:sublist(TokenChars, 2, TokenLen-2).

nc_name(TokenChars) ->
    [Namespace, "*"] = string:tokens(TokenChars, ":"),
    {Namespace, wildcard}.

q_name(TokenChars) ->
    [Namespace, Name] = string:tokens(TokenChars, ":"),
    {Namespace, Name}.

reference(TokenChars, TokenLen) ->
    lists:sublist(TokenChars, 2, TokenLen-1).
