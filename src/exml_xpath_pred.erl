-module(exml_xpath_pred).

-export([apply/2, pred/1]).
-compile({no_auto_import, [apply/2]}).

-include("exml.hrl").
-include("exml_xpath.hrl").

-define(FILTER(Pred, Args),
        (fun(Elements) ->
                lists:filter(fun(Element) ->
                            NewArgs = [resolve(Arg, Element, Elements) || Arg <- Args],
                            erlang:apply(Pred, [Element|NewArgs])
                    end, Elements) end)).

apply(Elements, Predicates) ->
    Funs = [pred(Pred) || Pred <- Predicates],
    lists:foldl(fun(Fun, Acc) -> Fun(Acc) end, Elements, Funs).

%% Predicates
%% always return a list!!!
pred({number, N}) ->
    fun(Elements) -> [lists:nth(N, Elements)] end;
pred({function, <<"last">>, []}) ->
    fun(Elements) -> [lists:last(Elements)] end;
pred({function, <<"not">>, [Pred]}) ->
    Fun = pred(Pred),
    fun(Elements) -> Elements -- Fun(Elements) end;
pred({function, <<"starts-with">>, [A1, A2]}) ->
    ?FILTER(fun starts_with_fun/3, [A1, A2]);
pred({function, <<"contains">>, [A1, A2]}) ->
    ?FILTER(fun contains_fun/3, [A1, A2]);
pred({comp, '=', A1, A2}) ->
    ?FILTER(fun equals_fun/3, [A1, A2]);
pred({comp, '!=', A1, A2}) ->
    ?FILTER(fun not_equal_fun/3, [A1, A2]);
pred({comp, '<', A1, A2}) ->
    ?FILTER(fun lesser_fun/3, [A1, A2]);
pred({comp, '<=', A1, A2}) ->
    ?FILTER(fun lesser_equal_fun/3, [A1, A2]);
pred({comp, '>', A1, A2}) ->
    ?FILTER(fun greater_fun/3, [A1, A2]);
pred({comp, '>=', A1, A2}) ->
    ?FILTER(fun greater_equal_fun/3, [A1, A2]);
pred({bool, 'or', Pred1, Pred2}) ->
    fun(Elements) ->
            Fun1 = pred(Pred1),
            Fun2 = pred(Pred2),
            Els1 = Fun1(Elements),
            Els2 = Fun2(Elements) -- Els1,
            Els1 ++ Els2
    end;
pred({bool, 'and', Pred1, Pred2}) ->
    fun(Elements) ->
            Fun1 = pred(Pred1),
            Fun2 = pred(Pred2),
            Els1 = Fun1(Elements),
            Els2 = Els1 -- Fun2(Elements),
            Els1 -- Els2
    end;
pred({path, {attr, wildcard, Predicates}}) ->
    apply(?FILTER(fun attr_fun/1, []), Predicates);
pred({path, {attr, Attr, Predicates}}) ->
    apply(?FILTER(fun attr_fun/2, [Attr]), Predicates);
pred(_Other) ->
    fun(Elements) -> Elements end.

%% Functions
resolve({literal, L}, _, _) -> L;
resolve({number, N}, _, _)  -> N;
resolve({negative, N}, Element, Elements) ->
    resolve(N, Element, Elements);
resolve({function, <<"string-length">>, [Arg]}, Element, Elements) ->
    string_length(resolve(Arg, Element, Elements));
resolve({function, <<"normalize-space">>, [Arg]}, Element, Elements) ->
    normalize_space(resolve(Arg, Element, Elements));
resolve({function, <<"count">>, [Arg]}, Element, Elements) ->
    length(resolve(Arg, Element, Elements));
resolve({function, <<"last">>, []}, _Element, Elements) ->
    length(Elements);
resolve({function, <<"name">>, []}, Element, _Elements) ->
    Element#xpathel.name;
resolve({function, <<"position">>, []}, Element, Elements) ->
    string:str(Elements, [Element]);
resolve({function, <<"floor">>, [Arg]}, Element, Elements) ->
    X = resolve(Arg, Element, Elements),
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end;
resolve({function, <<"ceiling">>, [Arg]}, Element, Elements) ->
    X = resolve(Arg, Element, Elements),
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end;
resolve({arith, Op, A1, A2}, Element, Elements) ->
    resolve_apply(arith_fun(Op), [A1, A2], Element, Elements);
resolve({path, _}=Path, Element, _Elements) ->
    exml_xpath_query:q(Element, Path, #st{root=Element});
resolve(Other, _Element, _Elements) ->
    Other.

arith_fun('*') -> fun(X,Y) -> X*Y end;
arith_fun('+') -> fun(X,Y) -> X+Y end;
arith_fun('-') -> fun(X,Y) -> X-Y end;
arith_fun(mod) -> fun(X,Y) -> X rem Y end;
arith_fun('div') -> fun(X,Y) -> X div Y end.

resolve_apply(Fun, Args, Element, Elements) ->
    NewArgs = [resolve(Arg, Element, Elements) || Arg <- Args],
    erlang:apply(Fun, NewArgs).

string_length(Bin) when is_binary(Bin) ->
    byte_size(Bin);
string_length(_) ->
    0.

normalize_space(Bin) when is_binary(Bin) ->
    Stripped = string:strip(binary_to_list(Bin), both, $ ),
    list_to_binary(Stripped);
normalize_space(_) ->
    undefined.

%% Predicates helpers
attr_fun(#xpathel{attrs=[]}) ->
    false;
attr_fun(_) ->
    true.

attr_fun(Element, Attr) ->
    exml_xpath_query:attr(Element, Attr, undefined) =/= undefined.

greater_fun(_, A1, A2) -> A1 > A2.
greater_equal_fun(_, A1, A2) -> A1 >= A2.
lesser_fun(_, A1, A2) -> A1 < A2.
lesser_equal_fun(_, A1, A2) -> A1 =< A2.
equals_fun(_, A1, A2) -> A1 == A2.
not_equal_fun(_, A1, A2) -> A1 /= A2.

starts_with_fun(_, A1, A2) when is_binary(A1), is_binary(A2) ->
    Size = byte_size(A2),
    case binary:match(A1, A2) of
        {0, Size} -> true;
        _         -> false
    end;
starts_with_fun(_, _, _) ->
    false.

contains_fun(_, A1, A2) when is_binary(A1), is_binary(A2) ->
    case binary:match(A1, A2) of
        nomatch -> false;
        _       -> true
    end;
contains_fun(_, _, _) ->
    false.
