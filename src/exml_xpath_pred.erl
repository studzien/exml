-module(exml_xpath_pred).

-export([apply/2, pred/1]).
-compile({no_auto_import, [apply/2]}).

-include("exml_xpath.hrl").

-define(FILTER(Pred), (fun(Elements) -> lists:filter(Pred, Elements) end)).

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
pred({comp, '<', A1, A2}) ->
    ?FILTER(lesser_fun(A1, A2));
pred({comp, '>', A1, A2}) ->
    ?FILTER(greater_fun(A1, A2));
pred({path, {attr, wildcard, Predicates}}) ->
    apply(?FILTER(attr_fun()), Predicates);
pred({path, {attr, Attr, Predicates}}) ->
    apply(?FILTER(attr_fun(Attr)), Predicates);
pred(_Other) ->
    fun(Elements) -> Elements end.

%% Predicates helpers
attr_fun() ->
    fun(#xpathel{attrs=[]}) -> false;
       (_)                -> true
    end.

attr_fun(Attr) ->
    fun(Element) ->
            exml_xpath_query:attr(Element, Attr, undefined) =/= undefined
    end.

greater_fun(A1, A2) ->
    fun(Element) -> resolve(A1, Element) > resolve(A2, Element) end.

lesser_fun(A1, A2) ->
    fun(Element) -> resolve(A1, Element) < resolve(A2, Element) end.

equals_fun(A1, A2) ->
    fun(Element) -> resolve(A1, Element) =:= resolve(A2, Element) end.

starts_with_fun(A1, A2) ->
    fun(Element) -> starts_with(resolve(A1, Element), resolve(A2, Element)) end.
starts_with(R1, R2) when is_binary(R1), is_binary(R2) ->
    Size = byte_size(R2),
    case binary:match(R1, R2) of
        {0, Size} -> true;
        _         -> false
    end;
starts_with(_, _) ->
    false.

contains_fun(A1, A2) ->
    fun(Element) -> contains(resolve(A1, Element), resolve(A2, Element)) end.
contains(R1, R2) when is_binary(R1), is_binary(R2) ->
    case binary:match(R1, R2) of
        nomatch -> false;
        _       -> true
    end;
contains(_, _) ->
    false.

%% Functions

resolve({literal, L}, _) -> L;
resolve({number, N}, _)  -> N;
resolve({function, <<"string-length">>, [Arg]}, Element) ->
    string_length(resolve(Arg, Element));
resolve({function, <<"normalize-space">>, [Arg]}, Element) ->
    normalize_space(resolve(Arg, Element));
resolve({function, <<"count">>, [Arg]}, Element) ->
    length(resolve(Arg, Element));
resolve({function, <<"name">>, []}, Element) ->
    Element#xpathel.name;
resolve(Other, Element) -> exml_xpath_query:q(Element, Other, #st{root=Element}).

string_length(Bin) when is_binary(Bin) ->
    byte_size(Bin);
string_length(_) ->
    0.

normalize_space(Bin) when is_binary(Bin) ->
    Stripped = string:strip(binary_to_list(Bin), both, $ ),
    list_to_binary(Stripped);
normalize_space(_) ->
    undefined.
