%% This is the grammar taken from:
%% http://www.w3.org/TR/1999/REC-xpath-19991116/#exprlex
%% However, due to the special lexical rules during tokenizing
%% '*' is recognized in an ambiguous way
%% and/or/mod/div are recognized as names not operators
%%
Definitions.
Expression = (\(|\)|\[|\]|\.|\.\.|@|,)
Operator = (\*|/|//|\||\+|-|=|!=|<|<=|>|>=)
Digit = [0-9]
Whitespace = [\000-\s]+
Literal = ('[^\']+'|"[^\"]+")
Axis = (ancestor|ancestor-or-self|attribute|child|descendant|descendant-or-self|following|following-sibling|namespace|parent|preceding|preceding-sibling|self)::
%% all chars but ':' should be here:
Name = [a-zA-Z]+
QualifiedName = ({Name}:{Name}|{Name})
Function = {QualifiedName}*\(
NodeType = (comment|text|processing-instruction|node)*\(
Reference = \${QualifiedName}

Rules.
%% Node type
{NodeType}   : {token, {node_type, TokenLine, node_type(TokenChars, TokenLen)}}.
%% Function name
{Function} : {token, {function, TokenLine, function(TokenChars, TokenLen)}}. 
%% Name test
%% the following rule has to be specially treated
\*              : {token, {'*', TokenLine, '*'}}.
{Name}:\*       : {token, {name, TokenLine, nc_name(TokenChars)}}.
{Name}:{Name}   : {token, {name, TokenLine, q_name(TokenChars)}}.
{Name}          : {token, {name, TokenLine, TokenChars}}.
%% Expression tokens
{Expression} : {token, {expression, TokenLine, list_to_atom(TokenChars)}}.
%% Axis
{Axis}       : {token, {axis, TokenLine, axis(TokenChars, TokenLen)}}.
%% Operator
{Operator}   : {token, {operator, TokenLine, list_to_atom(TokenChars)}}. 
%% Literal
{Literal}    : {token, {literal, TokenLine, literal(TokenChars, TokenLen)}}.
%% Number
{Digit}+           : {token, {number, TokenLine, list_to_integer(TokenChars)}}.
{Digit}+\.{Digit}+ : {token, {number, TokenLine, list_to_float(TokenChars)}}.
%% Reference
{Reference} : {token, {reference, TokenLine, reference(TokenChars, TokenLen)}}.
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

function(TokenChars, TokenLen) ->
    lists:sublist(TokenChars, 1, TokenLen-1).

node_type(TokenChars, TokenLen) ->
    list_to_atom(lists:sublist(TokenChars, 1, TokenLen-1)).

axis(TokenChars, TokenLen) ->
    list_to_atom(lists:sublist(TokenChars, 1, TokenLen-2)).

reference(TokenChars, TokenLen) ->
    lists:sublist(TokenChars, 2, TokenLen-1).
