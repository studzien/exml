-module(exml_xpath_query).

-export([q/2]).
-export([q/3, attr/3]).

-include("exml.hrl").
-include("exml_xpath.hrl").

-define(APPLY(Elements, Name, Predicates),
        exml_xpath_pred:apply(filter_name(Name, Elements), Predicates)). 

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

q(El,{union, Part1, Part2}, State) ->
    lists:flatten([q(El, Part1, State), q(El, Part2, State)]);
q(_El,{abs_path, AbsPath}, #st{root=R}=State) ->
    P = path(AbsPath),
    q(R, P, State);
q(El,{path, Path}, State) ->
    P = path(Path),
    q(El, P, State);
q(El,[{all, Path}|Rest], State) ->
    Children = all_elements(El),
    Els = q(Children, path(Path), State),
    q(Els, Rest, State);
q(El,[{element, {node,text}, _}|Rest], State) ->
    q(cdata(El), Rest, State);
q(El,[{element, Name, Predicates}|Rest], State) ->
    Children = [Child || #xpathel{}=Child <- El#xpathel.children],
    q(?APPLY(Children, Name, Predicates), Rest, State);
q(El,[{attr, Name, Predicates}|Rest], State) ->
    Attrs = attr(El, Name, []),
    Filtered = exml_xpath_pred:apply(Attrs, Predicates),
    q(Filtered, Rest, State);
q(El,[{descendant, Name, Predicates}|Rest], State) ->
    q(?APPLY(descendant(El), Name, Predicates), Rest, State);
q(El,[{'descendant-or-self', Name, Predicates}|Rest], State) ->
    q(?APPLY([El|descendant(El)], Name, Predicates), Rest, State);
q(El,[{parent, Name, Predicates}|Rest], State) ->
    q(?APPLY(parent(El, State), Name, Predicates), Rest, State);
q(El,[{ancestor, Name, Predicates}|Rest], State) ->
    q(?APPLY(ancestor(El, State), Name, Predicates), Rest, State);
q(El,[{'ancestor-or-self', Name, Predicates}|Rest], State) ->
    q(?APPLY([El|ancestor(El, State)], Name, Predicates), Rest, State);
q(El,[{'following-sibling', Name, Predicates}|Rest], State) ->
    q(?APPLY(following_sibling(El, State), Name, Predicates), Rest, State);
q(El,[{'preceding-sibling', Name, Predicates}|Rest], State) ->
    q(?APPLY(preceding_sibling(El, State), Name, Predicates), Rest, State);
q(El,[{following, Name, Predicates}|Rest], State) ->
    q(?APPLY(following(El, State), Name, Predicates), Rest, State);
q(El,[{preceding, Name, Predicates}|Rest], State) ->
    q(?APPLY(preceding(El, State), Name, Predicates), Rest, State);
q(El,[{self, Name, Predicates}|Rest], State) ->
    q(?APPLY([El], Name, Predicates), Rest, State);

q(El, [Tuple|Rest], State) when is_tuple(Tuple), element(1, Tuple) =:= child ->
    q(El, [setelement(1, Tuple, element)|Rest], State);

q(_, _, _) ->
    [].

cdata(#xpathel{children = Children}) ->
    list_to_binary([exml:unescape_cdata(C) || #xmlcdata{} = C <- Children]).

preceding(El, State) ->
    Siblings = preceding_sibling(El, State),
    Parents = case parent(El, State) of
        []       -> [];
        [Parent] -> preceding(Parent, State)
    end,
    Mine = [all_elements(Sibling) || Sibling <- Siblings],
    lists:flatten(Parents ++ Mine).

following(El, State) ->
    Siblings = following_sibling(El, State),
    Parents = case parent(El, State) of
        []       -> [];
        [Parent] -> following(Parent, State)
    end,
    Mine = [all_elements(Sibling) || Sibling <- Siblings],
    lists:flatten(Parents ++ Mine).

siblings(#xpathel{id=Id}, #st{an=An}) ->
    RefSiblings = dict:fetch(Id, An),
    Siblings = dict:filter(fun
                (K,_) when K =:= Id -> false;
                (_,V) when V =:= RefSiblings -> true;
                (_,_) -> false
            end, An),
    dict:to_list(Siblings).

preceding_sibling(#xpathel{id=Id}=El, State) ->
    Preceding = lists:filter(fun
                ({SiblingId,_}) when SiblingId > Id -> false;
                (_) -> true
            end, siblings(El, State)),
    [lut(Id1, State) || {Id1, _} <- Preceding].

following_sibling(#xpathel{id=Id}=El, State) ->
    Following = lists:filter(fun
                ({SiblingId,_}) when SiblingId < Id -> false;
                (_) -> true
            end, siblings(El, State)),
    [lut(Id1, State) || {Id1, _} <- Following].

filter_name(wildcard, Elements) ->
    lists:filter(fun
            (#xpathel{name=undefined}) -> false;
            (_) -> true
        end, Elements);
filter_name(Name, Elements) ->
    lists:filter(fun
            (#xpathel{name=N}) when Name =:= N -> true;
            (_) -> false
        end, Elements).

parent(#xpathel{id=Id}, #st{an=An}=State) ->
    case dict:find(Id, An) of
        {ok, [Parent|_]} -> [lut(Parent, State)];
        _                -> []
    end.

ancestor(#xpathel{id=Id}, State) ->
    ancestor(Id, State);
ancestor(Id, #st{an=An}=State) ->
    {ok, Ancestors} = dict:find(Id, An),
    [lut(Ancestor, State) || Ancestor <- Ancestors].

lut(#xpathel{id=Id}, State) ->
    lut(Id, State);
lut(Id, #st{lut=LUT}) ->
    dict:fetch(Id, LUT).

path(List) when is_list(List) ->
    lists:flatten(List);
path(Other) ->
    [Other].

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
    Childrens = [do_all_elements(Child) || #xpathel{}=Child <- Children],
    [Element | Childrens];
do_all_elements(_) ->
    [].

descendant(Element) ->
    lists:flatten(do_descendant(Element)).
do_descendant(#xpathel{children=Children}) ->
    Children ++ [do_descendant(Child) || #xpathel{}=Child <- Children];
do_descendant(_) ->
    [].

%% Function used for query preparation
build_elements(Element) ->
    {NewElement, _} = do_build_elements(Element, 1),
    NewElement.

do_build_elements(#xmlel{name=Name, attrs=Attrs, children=Children}, Id) ->
    {NewChildren, NewId} = lists:foldl(fun(Child, {ChildrenAcc, IdAcc}) ->
                    {NewChild, NewIdAcc} = do_build_elements(Child, IdAcc),
                    {[NewChild|ChildrenAcc], NewIdAcc}
            end, {[], Id+1}, Children),
    {#xpathel{id=Id,
              name=Name,
              attrs=Attrs,
              children=lists:reverse(NewChildren)}, NewId};
do_build_elements(Other, Id) ->
    {Other, Id}.

build_lut(Element) ->
    {LUT, Ancestors} = do_build_lut(Element, [], [], []),
    {dict:from_list(LUT), dict:from_list(Ancestors)}.

do_build_lut(#xpathel{id=Id, children=Children}=Element,
             LUT, Ancestors, CurrentAncestors) ->
    LUT1 = [{Id, Element} | LUT],
    Ancestors1 = [{Id, CurrentAncestors} | Ancestors],
    NewAncestors = [Id | CurrentAncestors],
    lists:foldl(fun(Child, {AccLut, AccAncestors}) ->
                do_build_lut(Child, AccLut, AccAncestors, NewAncestors)
        end, {LUT1, Ancestors1}, Children);
do_build_lut(_Other, LUT, Ancestors, _CurrentAncestors) ->
    {LUT, Ancestors}.
