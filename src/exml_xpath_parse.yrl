Nonterminals
'LocationPath'
'RelativeLocationPath'
'AbsoluteLocationPath'
'Step'
'<PredicateList>'
'<PredicateMember>'
'NodeTest'
'Predicate'
'PredicateExpr'
'AbbreviatedAbsoluteLocationPath'
'AbbreviatedRelativeLocationPath'
'AbbreviatedStep'
'Expr'
'PrimaryExpr'
'FunctionCall'
'<ArgumentList>'
'<ArgumentMember>'
'Argument'
'UnionExpr'
'PathExpr'
'FilterExpr'
'OrExpr'
'AndExpr'
'EqualityExpr'
'RelationalExpr'
'AdditiveExpr'
'MultiplicativeExpr'
'UnaryExpr'
'NameTest'.

Terminals
name
prefix
'('
')'
'['
']'
'..'
'.'
'@'
','
'::'
'*'
'//'
'/'
'|'
'+'
'-'
'='
'!='
'<'
'<='
'>'
'>='
literal
number
reference
wildcard
'and'
'or'
'div'
'mod'
node
'processing-instruction'
function
axis.

Rootsymbol
'Expr'.

%% [1]
'LocationPath' -> 'RelativeLocationPath' : {path, '$1'}.
'LocationPath' -> 'AbsoluteLocationPath' : {abs_path, '$1'}.

%% [2]
'AbsoluteLocationPath' -> '/' 'RelativeLocationPath' : '$2'.
'AbsoluteLocationPath' -> 'AbbreviatedAbsoluteLocationPath' : '$1'.

%% [3]
'RelativeLocationPath' -> 'Step' : '$1'.
'RelativeLocationPath' -> 'RelativeLocationPath' '/' 'Step' : ['$1', '$3'].
'RelativeLocationPath' -> 'AbbreviatedRelativeLocationPath' : '$1'.

%% [4]
'Step' -> 'axis' '::' 'NodeTest' '<PredicateList>' :
    {value('$1'), '$3', '$4'}.
'Step' -> 'axis' '::' 'NodeTest' :
    {value('$1'), '$3'}.
'Step' -> '@' 'name' '<PredicateList>' :
    {'attr', value('$2'), '$3'}.
'Step' -> '@' 'name' :
    {'attr', value('$2')}.
'Step' -> 'NodeTest' '<PredicateList>' :
    {'element', '$1', '$2'}.
'Step' -> 'NodeTest' :
    {'element', '$1'}.
'Step' -> 'AbbreviatedStep' :
    '$1'.

'<PredicateList>' -> '<PredicateMember>' : lists:reverse('$1').

'<PredicateMember>' -> '<PredicateMember>' 'Predicate' : ['$2'|'$1'].
'<PredicateMember>' -> 'Predicate' : ['$1'].

%% [7]
'NodeTest' -> 'NameTest' : '$1'.
'NodeTest' -> 'node' '(' ')' : {node, value('$1')}.
'NodeTest' -> 'processing-instruction' '(' 'literal' ')' : {processing_instruction, '$3'}.

%% [8]
'Predicate' -> '[' 'PredicateExpr' ']' : {predicate, '$2'}.

%% [9]
'PredicateExpr' -> 'Expr' : '$1'.

%% [10]
'AbbreviatedAbsoluteLocationPath' -> '//' 'RelativeLocationPath' : {'//', '$2'}.

%% [11]
'AbbreviatedRelativeLocationPath' -> 'RelativeLocationPath' '//' 'Step' : {'$1', '//', '$3'}.

%% [12]
'AbbreviatedStep' -> '.' : '$1'.
'AbbreviatedStep' -> '..' : '$1'.

%% [14]
'Expr' -> 'OrExpr' : '$1'.

%% [15]
'PrimaryExpr' -> 'reference' : {ref, value('$1')}.
'PrimaryExpr' -> '(' 'Expr' ')' : '$2'.
'PrimaryExpr' -> 'literal' : {literal, value('$1')}.
'PrimaryExpr' -> 'number' : {number, value('$1')}.
'PrimaryExpr' -> 'FunctionCall' : '$1'.

%% [16]
'FunctionCall' -> 'function' '(' ')' : {function, value('$1'), []}.
'FunctionCall' -> 'function' '(' '<ArgumentList>' ')' : {function, value('$1'), '$3'}.

'<ArgumentList>' -> '<ArgumentMember>' : lists:reverse('$1').
'<ArgumentMember>' -> '<ArgumentMember>' ',' 'Argument' : ['$3'|'$1'].
'<ArgumentMember>' -> 'Argument' : ['$1'].

%% [17]
'Argument' -> 'Expr'.

%% [18]
'UnionExpr' -> 'PathExpr' : '$1'.
'UnionExpr' -> 'UnionExpr' '|' 'PathExpr' : {path, union, {'$1', '$3'}}.

%% [19]
'PathExpr' -> 'LocationPath' : '$1'.
'PathExpr' -> 'FilterExpr' : '$1'.
'PathExpr' -> 'FilterExpr' '/' 'RelativeLocationPath' : {'$1', '/', '$3'}.
'PathExpr' -> 'FilterExpr' '//' 'RelativeLocationPath' : {'$1', '//', '$3'}.

%% [20]
'FilterExpr' -> 'PrimaryExpr' : '$1'.
'FilterExpr' -> 'FilterExpr' 'Predicate': {path, filter, {'$1', '$2'}}.

%% [21]
'OrExpr' -> 'AndExpr' : '$1'.
'OrExpr' -> 'OrExpr' 'or' 'AndExpr' : {bool, 'or', '$1', '$3'}.

%% [22]
'AndExpr' -> 'EqualityExpr' : '$1'.
'AndExpr' -> 'AndExpr' 'and' 'EqualityExpr' : {bool, 'and', '$1', '$3'}.

%% [23]
'EqualityExpr' -> 'RelationalExpr' : '$1'.
'EqualityExpr' -> 'EqualityExpr' '=' 'RelationalExpr' : {comp, '=', '$1', '$3'}.
'EqualityExpr' -> 'EqualityExpr' '!=' 'RelationalExpr' : {comp, '!=', '$1', '$3'}.

%% [24]
'RelationalExpr' -> 'AdditiveExpr' : '$1'.
'RelationalExpr' -> 'RelationalExpr' '<' 'AdditiveExpr' : {comp, '<', '$1', '$3'}.
'RelationalExpr' -> 'RelationalExpr' '>' 'AdditiveExpr' : {comp, '>', '$1', '$3'}.
'RelationalExpr' -> 'RelationalExpr' '<=' 'AdditiveExpr' : {comp, '<=', '$1', '$3'}.
'RelationalExpr' -> 'RelationalExpr' '>=' 'AdditiveExpr' : {comp, '>=', '$1', '$3'}.

%% [25]
'AdditiveExpr' -> 'MultiplicativeExpr' : '$1'.
'AdditiveExpr' -> 'AdditiveExpr' '+' 'MultiplicativeExpr' : {arith, '+', '$1', '$3'}.
'AdditiveExpr' -> 'AdditiveExpr' '-' 'MultiplicativeExpr' : {arith, '-', '$1', '$3'}.

%% [26]
'MultiplicativeExpr' -> 'UnaryExpr' : '$1'.
'MultiplicativeExpr' -> 'MultiplicativeExpr' '*' 'UnaryExpr' : {arith, '*', '$1', '$3'}.
'MultiplicativeExpr' -> 'MultiplicativeExpr' 'div' 'UnaryExpr' : {arith, 'div', '$1', '$3'}.
'MultiplicativeExpr' -> 'MultiplicativeExpr' 'mod' 'UnaryExpr' : {arith, 'mod', '$1', '$3'}.

%% [27]
'UnaryExpr' -> 'UnionExpr' : '$1'.
'UnaryExpr' -> '-' 'UnaryExpr' : {negative, '$2'}.

%% [37]
'NameTest' -> 'wildcard' : 'wildcard'.
'NameTest' -> 'prefix' : {'prefix', value('$1')}.
'NameTest' -> 'name' : value('$1').

Erlang code.
value({Token, _Line}) ->
    Token;
value({_Token, _Line, Value}) when is_list(Value) ->
    list_to_binary(Value);
value({_Token, _Line, Value}) ->
    Value.
