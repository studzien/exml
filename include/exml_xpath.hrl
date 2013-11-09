-record(st, {root,
             lut = dict:new(), %% lookup table for elements
             an = dict:new(),
             acc = []}). %% ancestors table

-record(xpathel, {id = 0,
                  name,
                  attrs = [],
                  children = []}).

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
