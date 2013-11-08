-module(exml_xpath_query).

-export([q/2]).

-include("exml.hrl").

-record(st, {root}).

q(Element, Query) ->
    Root = #xmlel{children=[Element]},
    State = #st{root = Root},
    q(Root, Query, State).

q(El, [], _) ->
    El;
q(List, Path, State) when is_list(List) ->
    L = [q(Element, Path, State) || Element <- List],
    lists:flatten(L);
q(_El, {abs_path, AbsPath}, #st{root=R}=State) ->
    error_logger:info_msg("~p~n", [path(AbsPath)]),
    q(R, path(AbsPath), State);
q(El, {path, Path}, State) ->
    q(El, path(Path), State);
q(El, [{all, Path}|Rest], State) ->
    Children = all_elements(El),
    Els = q(Children, path(Path), State),
    q(Els, Rest, State);
q(El, [{element, wildcard}|Rest], State) ->
    q(El#xmlel.children, Rest, State);
q(El, [{element, Name}|Rest], State) ->
    Children = exml_query:subelements(El, Name),
    q(Children, Rest, State);
q(El, [{element, wildcard, Predicates}|Rest], State) ->
    Children = El#xmlel.children,
    Filtered = apply_predicates(Children, Predicates),
    q(Filtered, Rest, State);
q(El, [{element, Name, Predicates}|Rest], State) ->
    Children = exml_query:subelements(El, Name),
    Filtered = apply_predicates(Children, Predicates),
    q(Filtered, Rest, State);
q(El, [{attr, Name}|_Rest], _State) ->
    exml_query:attr(El, Name, []);
q(_, _, _) ->
    [].

path(List) when is_list(List) ->
    lists:flatten(List);
path(Other) ->
    [Other].

all_elements(Element) ->
    lists:flatten(do_all_elements(Element)).

do_all_elements(#xmlel{children=Children}=Element) ->
    Childrens = [do_all_elements(Child) || Child <- Children],
    [Element | Childrens];
do_all_elements(_) ->
    [].

apply_predicates(Elements, Predicates) ->
    Funs = [predicate_fun(Pred) || Pred <- Predicates],
    lists:foldl(fun(Fun, Acc) -> Fun(Acc) end, Elements, Funs).

predicate_fun({number, N}) ->
    fun(Elements) -> lists:nth(N, Elements) end;
predicate_fun({function, <<"last">>, []}) ->
    fun(Elements) -> lists:last(Elements) end;
predicate_fun({function, <<"not">>, [Pred]}) ->
    Fun = predicate_fun(Pred),
    fun(Elements) -> Elements -- Fun(Elements) end;
predicate_fun({comp, '=', A1, A2}) ->
    fun(Elements) ->
            lists:filter(fun(Element) ->
                        resolve(A1, Element) =:= resolve(A2, Element)
                end, Elements)
    end;
predicate_fun({path, {attr, wildcard}}) ->
    fun(Elements) ->
            lists:filter(fun
                    (#xmlel{attrs=[]}) -> false;
                    (_) -> true
                end, Elements)
    end;
predicate_fun({path, {attr, Attr}}) ->
    fun(Elements) ->
            lists:filter(fun(Element) ->
                        exml_query:attr(Element, Attr) =/= undefined
                end, Elements)
    end;
predicate_fun(_Other) ->
    fun(Elements) -> Elements end.

resolve({literal, L}, _) -> L;
resolve({number, N}, _)  -> N;
resolve({function, <<"normalize-space">>, [Arg]}, Element) ->
    case resolve(Arg, Element) of
        Bin when is_binary(Bin) ->
            Stripped = string:strip(binary_to_list(Bin), both, $ ),
            list_to_binary(Stripped);
        _ ->
            undefined
    end;
resolve({function, <<"count">>, [Arg]}, Element) ->
    length(resolve(Arg, Element));
resolve(Other, Element)  -> q(Element, Other, #st{root=Element}).
