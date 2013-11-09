-module(exml_xpath_pred).

-export([apply/2, pred/1]).
-compile({no_auto_import, [apply/2]}).

-include("exml.hrl").
-include("exml_xpath.hrl").

-define(FILTER(Pred), (fun(Elements) -> lists:filter((Pred)(Elements), Elements) end)).

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
    ?FILTER(starts_with_fun(A1, A2));
pred({function, <<"contains">>, [A1, A2]}) ->
    ?FILTER(contains_fun(A1, A2));
pred({comp, '=', A1, A2}) ->
    ?FILTER(equals_fun(A1, A2));
pred({comp, '!=', A1, A2}) ->
    ?FILTER(not_equal_fun(A1, A2));
pred({comp, '<', A1, A2}) ->
    ?FILTER(lesser_fun(A1, A2));
pred({comp, '<=', A1, A2}) ->
    ?FILTER(lesser_equal_fun(A1, A2));
pred({comp, '>', A1, A2}) ->
    ?FILTER(greater_fun(A1, A2));
pred({comp, '>=', A1, A2}) ->
    ?FILTER(greater_equal_fun(A1, A2));
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
    apply(?FILTER(attr_fun()), Predicates);
pred({path, {attr, Attr, Predicates}}) ->
    apply(?FILTER(attr_fun(Attr)), Predicates);
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
resolve({arith, '*', A1, A2}, Element, Elements) ->
    resolve_apply(fun(X,Y) -> X*Y end, [A1, A2], Element, Elements);
resolve({arith, '+', A1, A2}, Element, Elements) ->
    resolve_apply(fun(X,Y) -> X+Y end, [A1, A2], Element, Elements);
resolve({arith, '-', A1, A2}, Element, Elements) ->
    resolve_apply(fun(X,Y) -> X-Y end, [A1, A2], Element, Elements);
resolve({arith, mod, A1, A2}, Element, Elements) ->
    resolve_apply(fun(X,Y) -> X rem Y end, [A1, A2], Element, Elements);
resolve({arith, 'div', A1, A2}, Element, Elements) ->
    resolve_apply(fun(X,Y) -> X div Y end, [A1, A2], Element, Elements);
resolve(Other, Element, _Elements) ->
    exml_xpath_query:q(Element, Other, #st{root=Element}).

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
attr_fun() ->
    fun(_Elements) ->
            fun(#xpathel{attrs=[]}) -> false;
                (_)                 -> true
            end
    end.

attr_fun(Attr) ->
    fun(_Elements) ->
            fun(Element) ->
                    exml_xpath_query:attr(Element, Attr, undefined) =/= undefined
            end
    end.

greater_fun(A1, A2) ->
    fun(Elements) ->
            fun(Element) ->
                    resolve(A1, Element, Elements) > resolve(A2, Element, Elements)
            end
    end.

greater_equal_fun(A1, A2) ->
    fun(Elements) ->
            fun(Element) ->
                    resolve(A1, Element, Elements) >= resolve(A2, Element, Elements)
            end
    end.

lesser_fun(A1, A2) ->
    fun(Elements) ->
            fun(Element) ->
                    resolve(A1, Element, Elements) < resolve(A2, Element, Elements)
            end
    end.

lesser_equal_fun(A1, A2) ->
    fun(Elements) ->
            fun(Element) ->
                    resolve(A1, Element, Elements) =< resolve(A2, Element, Elements)
            end
    end.

equals_fun(A1, A2) ->
    fun(Elements) ->
            fun(Element) ->
                    resolve(A1,Element,Elements) == resolve(A2,Element,Elements)
            end
    end.

not_equal_fun(A1, A2) ->
    fun(Elements) ->
            fun(Element) ->
                    resolve(A1,Element,Elements) /= resolve(A2,Element,Elements)
            end
    end.

starts_with_fun(A1, A2) ->
    fun(Elements) ->
            fun(Element) -> starts_with(resolve(A1, Element, Elements), 
                                        resolve(A2, Element, Elements))
            end
    end.
starts_with(R1, R2) when is_binary(R1), is_binary(R2) ->
    Size = byte_size(R2),
    case binary:match(R1, R2) of
        {0, Size} -> true;
        _         -> false
    end;
starts_with(_, _) ->
    false.

contains_fun(A1, A2) ->
    fun(Elements) ->
            fun(Element) -> contains(resolve(A1, Element, Elements),
                                     resolve(A2, Element, Elements)) end
    end.
contains(R1, R2) when is_binary(R1), is_binary(R2) ->
    case binary:match(R1, R2) of
        nomatch -> false;
        _       -> true
    end;
contains(_, _) ->
    false.
