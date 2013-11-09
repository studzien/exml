-module(exml_xpath_query).

-export([q/2]).
-export([q/3, attr/3]).

-include("exml.hrl").
-include("exml_xpath.hrl").

q(Element, Query) ->
    Root = #xmlel{children=[Element]},
    NewRoot = build_elements(Root),
    {LUT, Ancestors} = build_lut(NewRoot),
    State = #st{root = NewRoot,
                lut = LUT,
                an = Ancestors},
    q(NewRoot, Query, State).

q(List, [], S) when is_list(List) -> 
    [q(Elem, [], S) || Elem <- List];
q(#xpathel{}=El, [], S) ->
    #xmlel{name=El#xpathel.name,
           attrs=El#xpathel.attrs,
           children=q(El#xpathel.children, [], S)};
q(El, [], _) -> El;

q(List, Path, State) when is_list(List) ->
    L = [q(Element, Path, State) || Element <- List],
    lists:flatten(L);

q(El,  {union, Part1, Part2}, State) ->
    lists:flatten([q(El, Part1, State), q(El, Part2, State)]);

q(_El, {abs_path, AbsPath}, #st{root=R}=State) ->
    P = path(AbsPath),
    error_logger:info_msg("~p~n", [path(AbsPath)]),
    q(R, P, State);

q(El, {path, Path}, State) ->
    P = path(Path),
    q(El, P, State);

q(El, [{all, Path}|Rest], State) ->
    Children = all_elements(El),
    Els = q(Children, path(Path), State),
    q(Els, Rest, State);

q(El, [{element, wildcard}|Rest], State) ->
    q(El#xpathel.children, Rest, State);

q(El, [{element, Name}|Rest], State) ->
    Children = subelements(El, Name),
    q(Children, Rest, State);

q(El, [{element, wildcard, Predicates}|Rest], State) ->
    Children = El#xpathel.children,
    Filtered = exml_xpath_pred:apply(Children, Predicates),
    q(Filtered, Rest, State);

q(El, [{element, Name, Predicates}|Rest], State) ->
    Children = subelements(El, Name),
    Filtered = exml_xpath_pred:apply(Children, Predicates),
    q(Filtered, Rest, State);

q(El, [Tuple|Rest], State) when is_tuple(Tuple), element(1, Tuple) =:= child ->
    q(El, [setelement(1, Tuple, element)|Rest], State);

q(El, [{attr, Name}|_Rest], _State) ->
    attr(El, Name, []);

q(El, [{descendant, wildcard}|Rest], State) ->
    Children = all_children(El),
    q(Children, Rest, State);

q(El, [{descendant, Name}|Rest], State) ->
    Children = name_children(El, Name),
    q(Children, Rest, State);

q(El,[{parent, wildcard}|Rest], State) ->
    q(parent(El, State), Rest, State);

q(El,[{parent, Name}|Rest], State) ->
    case parent(El, State) of
        #xpathel{name=Name}=El -> q(El, Rest, State);
        _                      -> q([], Rest, State)
    end;

q(El,[{ancestor, wildcard}|Rest], State) ->
    Els = lists:filter(fun
                (#xpathel{name=undefined}) -> false;
                (_) -> true
            end, ancestors(El, State)),
    q(Els, Rest, State);

q(El,[{ancestor, Name}|Rest], State) ->
    Els = lists:filter(fun
                (#xpathel{name=N}) when Name =:= N -> true;
                (_) -> false
            end, ancestors(El, State)),
    q(Els, Rest, State);

q(_, _, _) ->
    [].

parent(#xpathel{id=Id}, #st{an=An}=State) ->
    case dict:find(Id, An) of
        {ok, [Parent|_]} -> lut(Parent, State);
        _                -> undefined
    end.

ancestors(#xpathel{id=Id}, #st{an=An}=State) ->
    {ok, Ancestors} = dict:find(Id, An),
    [lut(Ancestor, State) || Ancestor <- Ancestors].

lut(#xpathel{id=Id}, State) ->
    lut(Id, State);
lut(Id, #st{lut=LUT}) ->
    {ok, Value} = dict:find(Id, LUT),
    Value.

path(List) when is_list(List) ->
    lists:flatten(List);
path(Other) ->
    [Other].

subelements(#xpathel{children = Children}, Name) ->
    lists:filter(fun(#xpathel{name = N}) when N =:= Name ->
                        true;
                    (_) ->
                        false
                 end, Children).

attr(#xpathel{attrs = Attrs}, Name, Default) ->
    case lists:keyfind(Name, 1, Attrs) of
        {Name, Value} ->
            Value;
        false ->
            Default
    end.

all_elements(Element) ->
    lists:flatten(do_all_elements(Element)).
do_all_elements(#xpathel{children=Children}=Element) ->
    Childrens = [do_all_elements(Child) || Child <- Children],
    [Element | Childrens];
do_all_elements(_) ->
    [].

all_children(Element) ->
    lists:flatten(do_all_children(Element)).
do_all_children(#xpathel{children=Children}) ->
    Children ++ [do_all_children(Child) || Child <- Children];
do_all_children(_) ->
    [].

name_children(Element, Name) ->
    lists:filter(fun
            (#xpathel{name=N}) when N =:= Name -> true;
            (_) -> false
        end, all_children(Element)).

%% Function used for query preparation
build_elements(Element) ->
    {NewElement, _} = do_build_elements(Element, 1),
    NewElement.

do_build_elements(#xmlel{name=Name, attrs=Attrs, children=Children}, Id) ->
    {NewChildren, NewId} = lists:foldr(fun(Child, {ChildrenAcc, IdAcc}) ->
                    {NewChild, NewIdAcc} = do_build_elements(Child, IdAcc),
                    {[NewChild|ChildrenAcc], NewIdAcc}
            end, {[], Id+1}, Children),
    {#xpathel{id=Id, name=Name, attrs=Attrs, children=NewChildren}, NewId}.

build_lut(Element) ->
    {LUT, Ancestors} = do_build_lut(Element, [], [], []),
    {dict:from_list(LUT), dict:from_list(Ancestors)}.

do_build_lut(#xpathel{id=Id, children=Children}=Element, LUT, Ancestors, CurrentAncestors) ->
    LUT1 = [{Id, Element} | LUT],
    Ancestors1 = [{Id, CurrentAncestors} | Ancestors],
    NewAncestors = [Id | CurrentAncestors],
    lists:foldl(fun(Child, {AccLut, AccAncestors}) ->
                do_build_lut(Child, AccLut, AccAncestors, NewAncestors)
        end, {LUT1, Ancestors1}, Children).
