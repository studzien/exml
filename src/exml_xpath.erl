-module(exml_xpath).

-export([parse/1]).

-define(LEXER, exml_xpath_scan).
-define(PARSER, exml_xpath_parse).

-define(IS_OPERATOR(Name), (Name =:= "and" orelse
                            Name =:= "or"  orelse
                            Name =:= "div" orelse
                            Name =:= "mod")).
-define(IS_NODETYPE(Name), (Name =:= "comment" orelse
                            Name =:= "text"    orelse
                            Name =:= "node")).
-define(IS_AXIS(Name), (Name =:= "ancestor" orelse
                        Name =:= "ancestor-or-self" orelse
                        Name =:= "attribute" orelse
                        Name =:= "child" orelse
                        Name =:= "descendant" orelse
                        Name =:= "descendant-or-self" orelse
                        Name =:= "following" orelse
                        Name =:= "following-sibling" orelse
                        Name =:= "namespace" orelse
                        Name =:= "parent" orelse
                        Name =:= "preceding" orelse
                        Name =:= "preceding-sibling" orelse
                        Name =:= "self")).

scan(Input) ->
    case ?LEXER:string(Input) of
        {ok, Tokens, Lines} -> {ok, fix_tokens(Tokens, []), Lines};
        Other               -> Other
    end.

parse(Input) ->
    {ok, Tokens, _} = scan(Input),
    error_logger:info_msg("~p~n", [Tokens]),
    ?PARSER:parse(Tokens).

fix_tokens([], Acc) ->
    lists:reverse(Acc);
fix_tokens([Preceding, {'*', TokenLine, '*'}|Rest], Acc) ->
    Acc1 = case check_preceding(Preceding) of
        true  -> [{'*', TokenLine, '*'},Preceding|Acc];
        false -> [{wildcard, TokenLine, wildcard},Preceding|Acc]
    end,
    fix_tokens(Rest, Acc1);
fix_tokens([{name,TokenLine,"processing"},{'-',_},{name,_,"instruction"}|Rest],
           Acc) ->
    fix_tokens(Rest, [{'processing-instruction', TokenLine}|Acc]);
fix_tokens([Preceding, {name, TokenLine, Name}=Tok|Rest], Acc)
        when ?IS_OPERATOR(Name) ->
    case check_preceding(Preceding) of
        true ->
            fix_tokens([{list_to_atom(Name), TokenLine}|Rest],
                       [Preceding|Acc]);
        false ->
            fix_tokens(Rest, [Tok,Preceding|Acc])
    end;
fix_tokens([{name, TokenLine, Name}, {'::',_}=Tok | Rest], Acc)
        when ?IS_AXIS(Name) ->
    fix_tokens([Tok|Rest], [{axis, TokenLine, list_to_atom(Name)}|Acc]);
fix_tokens([{name, TokenLine, Name}, {'(',_}=Tok | Rest], Acc)
        when ?IS_NODETYPE(Name) ->
    fix_tokens([Tok|Rest], [{node, TokenLine, list_to_atom(Name)}|Acc]);
fix_tokens([{name, TokenLine, Name}, {'(',_}=Tok | Rest], Acc) ->
    fix_tokens([Tok|Rest], [{function, TokenLine, Name}|Acc]);
fix_tokens([Token|Rest], Acc) ->
    fix_tokens(Rest, [Token|Acc]).

check_preceding({Token, _, _}) -> check_preceding(Token);
check_preceding({Token, _}) -> check_preceding(Token);
check_preceding('@')   -> false;
check_preceding('::')  -> false;
check_preceding('(')   -> false;
check_preceding('[')   -> false;
check_preceding('*')   -> false;
check_preceding('//')  -> false;
check_preceding('/')   -> false;
check_preceding('|')   -> false;
check_preceding('+')   -> false;
check_preceding('-')   -> false;
check_preceding('=')   -> false;
check_preceding('!=')  -> false;
check_preceding('<')   -> false;
check_preceding('<=')  -> false;
check_preceding('>')   -> false;
check_preceding('>=')  -> false;
check_preceding('and') -> false;
check_preceding('or')  -> false;
check_preceding('div') -> false;
check_preceding('mod') -> false;
check_preceding(_)     -> true.
