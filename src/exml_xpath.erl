-module(exml_xpath).

-export([string/1]).

-define(IS_OPERATOR(Name), (Name =:= "and" orelse Name =:= "or" orelse
                            Name =:= "div" orelse Name =:= "mod")).

string(Input) ->
    case exml_xpath_scan:string(Input) of
        {ok, Tokens, Lines} -> {ok, fix_tokens(Tokens, []), Lines};
        Other               -> Other
    end.

fix_tokens([], Acc) ->
    lists:reverse(Acc);
fix_tokens([Preceding, {'*', TokenLine, '*'}|Rest], Acc) ->
    Acc1 = case check_preceding(Preceding) of
        true  -> [{operator, TokenLine, '*'},Preceding|Acc];
        false -> [{name, TokenLine, wildcard},Preceding|Acc]
    end,
    fix_tokens(Rest, Acc1);
fix_tokens([Preceding, {name, TokenLine, Name}=Tok|Rest], Acc)
        when ?IS_OPERATOR(Name) ->
    case check_preceding(Preceding) of
        true ->
            fix_tokens([{operator, TokenLine, list_to_atom(Name)}|Rest],
                       [Preceding|Acc]);
        false ->
            fix_tokens(Rest, [Tok,Preceding|Acc])
    end;
fix_tokens([Token|Rest], Acc) ->
    fix_tokens(Rest, [Token|Acc]).

check_preceding({expression, _, '@'})  -> false;
check_preceding({expression, _, '::'}) -> false;
check_preceding({expression, _, '('})  -> false;
check_preceding({expression, _, '['})  -> false;
check_preceding({operator, _, _})      -> false;
check_preceding(_)                     -> true.
