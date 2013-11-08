-module(exml_xpath_query).

-export([q/2, q/3]).

-include("exml.hrl").

-record(st, {root, default}).

q(Element, Query) ->
    q(Element, Element, Query, undefined).

q(Element, Query, Default) ->
    Root = #xmlel{children=[Element]},
    State = #st{root = Root,
                default = Default},
    do_q(Root, Query, State).

do_q(El, [], _) ->
    El;
do_q(List, Path, State) when is_list(List) ->
    L = [do_q(Element, Path, State) || Element <- List],
    lists:flatten(L);
do_q(_El, {abs_path, AbsPath}, #st{root=R}=State) ->
    error_logger:info_msg("~p~n", [path(AbsPath)]),
    do_q(R, path(AbsPath), State);
do_q(El, [{elements, Path}|Rest], State) ->
    Children = all_children(El),
    Els = do_q(Children, path(Path), State),
    do_q(Els, Rest, State);
do_q(El, [{element, Name}|Rest], State) ->
    Children = exml_query:subelements(El, Name),
    do_q(Children, Rest, State);
do_q(_, _, #st{default=Default}) ->
    Default.

q(Result, _, [], _) ->
    Result;
q(List, Root, Query, Default) when is_list(List) ->
    [q(Element, Root, Query, Default) || Element <- List];
q(#xmlel{}=Element, Root, {path, Path}, Default) when is_list(Path) ->
    q(#xmlel{children=[Element]}, Root, lists:flatten(Path), Default);
q(#xmlel{}=Element, Root, {path, Path}, Default) ->
    q(#xmlel{children=[Element]}, Root, [Path], Default);
q(#xmlel{}=Element, Root, [{element, Name} | Rest], Default) ->
    Child = exml_query:subelement(Element, Name),
    q(Child, Root, Rest, Default);
q(#xmlel{}=Element, _Root, [{attr, Name}], Default) ->
    exml_query:attr(Element, Name, Default);
q(_, _, _, Default) ->
    Default.

path(List) when is_list(List) ->
    lists:flatten(List);
path(Other) ->
    [Other].

all_children(Element) ->
    lists:flatten(do_all_children(Element)).

do_all_children(#xmlel{children = Children}) ->
    lists:foldl(fun(#xmlel{}=El, Acc) ->
                [El | do_all_children(El)] ++ Acc;
            (_, Acc) ->
                Acc
        end, [], Children);
do_all_children(_) ->
    [].
