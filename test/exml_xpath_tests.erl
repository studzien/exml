-module(exml_xpath_tests).

-include_lib("eunit/include/eunit.hrl").
-include("exml.hrl").

-compile(export_all).

%%--------------------------------------------------------------------
%% tests
%%--------------------------------------------------------------------
absolute_path_test() ->
    XML = xml(<<"<AAA>"
                    "<BBB/>"
                    "<CCC/>"
                    "<BBB/>"
                    "<BBB/>"
                    "<DDD>"
                        "<BBB/>"
                    "</DDD>"
                    "<CCC/>"
                "</AAA>">>),

    [XML] = exml_xpath:q(XML, "/AAA"),

    CCC = #xmlel{name = <<"CCC">>},
    [CCC, CCC] = exml_xpath:q(XML, <<"/AAA/CCC">>),
    
    BBB = #xmlel{name = <<"BBB">>},
    [BBB] = exml_xpath:q(XML, "/AAA/DDD/BBB").

all_elements_test() ->
    XML = xml(<<"<AAA>"
                    "<BBB/>"
                    "<CCC/>"
                    "<BBB/>"
                    "<DDD>"
                        "<BBB/>"
                    "</DDD>"
                    "<CCC>"
                        "<DDD>"
                            "<BBB/>"
                            "<BBB/>"
                        "</DDD>"
                    "</CCC>"
                "</AAA>">>),
    BBB = #xmlel{name = <<"BBB">>},
    [BBB, BBB, BBB, BBB, BBB] = exml_xpath:q(XML, "//BBB"),
    [BBB, BBB, BBB] = exml_xpath:q(XML, "//DDD/BBB").
    
wildcard_test() ->
    XML = xml(<<"<AAA>"
                    "<XXX>" 
                        "<DDD>" 
                            "<BBB/>"
                            "<BBB/>"
                            "<EEE/>"
                            "<FFF/>"
                        "</DDD>"
                    "</XXX>"
                    "<CCC>"
                        "<DDD>"
                            "<BBB/>"
                            "<BBB/>"
                            "<EEE/>"
                            "<FFF/>"
                        "</DDD>"
                    "</CCC>"
                    "<CCC>"
                        "<BBB>"
                            "<BBB>"
                                "<BBB/>"
                            "</BBB>"
                        "</BBB>"
                    "</CCC>"
                "</AAA>">>),
    BBB = #xmlel{name = <<"BBB">>},
    EEE = #xmlel{name = <<"EEE">>},
    FFF = #xmlel{name = <<"FFF">>},
    Result1 = exml_xpath:q(XML, "/AAA/CCC/DDD/*"),
    Els1 = [BBB, BBB, EEE, FFF],
    4 = length(Result1),
    [] = Result1 -- Els1,

    Result2 = exml_xpath:q(XML, "/*/*/*/BBB"),
    Els2 = [BBB, BBB, BBB, BBB, BBB#xmlel{children=[BBB]}],
    5 = length(Result2),
    [] = Result2 -- Els2,

    Result3 = exml_xpath:q(XML, "//*"),
    17 = length(Result3).

position_test() ->
    XML = xml(<<"<AAA>" 
                    "<BBB/>"
                    "<BBB/>" 
                    "<BBB/>"
                    "<BBB/>" 
                "</AAA>">>),
    BBB = #xmlel{name = <<"BBB">>},
    [BBB] = exml_xpath:q(XML, "/AAA/BBB[1]"),
    [BBB] = exml_xpath:q(XML, "/AAA/BBB[last()]").

attributes_test() ->
    XML = xml(<<"<AAA>"
                    "<BBB id = \"b1\"/>"
                    "<BBB id = \"b2\"/>"
                    "<BBB name = \"bbb\"/>"
                    "<BBB/>"
                "</AAA>">>),
    [<<"b1">>, <<"b2">>] = exml_xpath:q(XML, "//@id"),

    BBB1 = #xmlel{name = <<"BBB">>, attrs = [{<<"id">>, <<"b1">>}]},
    BBB2 = #xmlel{name = <<"BBB">>, attrs = [{<<"id">>, <<"b2">>}]},
    [BBB1, BBB2] = exml_xpath:q(XML, "//BBB[@id]"),

    BBB3 = #xmlel{name = <<"BBB">>, attrs = [{<<"name">>, <<"bbb">>}]},
    [BBB1, BBB2, BBB3] = exml_xpath:q(XML, "//BBB[@*]"),

    BBB4 = #xmlel{name = <<"BBB">>},
    [BBB4] = exml_xpath:q(XML, "//BBB[not(@*)]").

attributes_values_test() ->
    XML = xml(<<"<AAA>"
                    "<BBB id = \"b1\"/>"
                    "<BBB name = \" bbb \"/>"
                    "<BBB name = \"bbb\"/>"
                "</AAA>">>),
    BBB1 = #xmlel{name = <<"BBB">>, attrs = [{<<"id">>, <<"b1">>}]},
    [BBB1] = exml_xpath:q(XML, "//BBB[@id='b1']"),
    BBB2 = #xmlel{name = <<"BBB">>, attrs = [{<<"name">>, <<"bbb">>}]},
    [BBB2] = exml_xpath:q(XML, "//BBB[@name='bbb']"),
    BBB3 = #xmlel{name = <<"BBB">>, attrs = [{<<"name">>, <<" bbb ">>}]},
    [BBB3, BBB2] = exml_xpath:q(XML, "//BBB[normalize-space(@name)='bbb']").

count_test() ->
    XML = xml(<<"<AAA>"
                    "<CCC>"
                        "<BBB/>"
                        "<BBB/>"
                        "<BBB/>"
                    "</CCC>"
                    "<DDD>"
                        "<BBB/>"
                        "<BBB/>"
                    "</DDD>"
                    "<EEE>"
                        "<CCC/>"
                        "<DDD/>"
                    "</EEE>"
                "</AAA>">>),
    BBB = #xmlel{name = <<"BBB">>},
    DDD = #xmlel{name = <<"DDD">>, children = [BBB, BBB]},
    [DDD] = exml_xpath:q(XML, "//*[count(BBB)=2]"),
    CCC = #xmlel{name = <<"CCC">>, children = [BBB, BBB, BBB]},
    Result = exml_xpath:q(XML, "//*[count(*)=3]"),
    2 = length(Result),
    [#xmlel{name = <<"AAA">>}] = Result -- [CCC].

name_test() ->
    XML = xml(<<"<AAA>"
                    "<BCC>"
                        "<BBB/>"
                        "<BBB/>"
                        "<BBB/>"
                    "</BCC>"
                    "<DDB>"
                        "<BBB/>"
                        "<BBB/>"
                    "</DDB>"
                    "<BEC>"
                        "<CCC/>"
                        "<DBD/>"
                    "</BEC>"
                "</AAA>">>),
    BBB = #xmlel{name = <<"BBB">>},
    [BBB, BBB, BBB, BBB, BBB] = exml_xpath:q(XML, "//*[name()='BBB']"),

    BCC = #xmlel{name = <<"BCC">>, children = [BBB, BBB, BBB]},
    CCC = #xmlel{name = <<"CCC">>},
    DBD = #xmlel{name = <<"DBD">>},
    BEC = #xmlel{name = <<"BEC">>, children = [CCC, DBD]},
    Result1 = exml_xpath:q(XML, "//*[starts-with(name(),'B')]"),
    7 = length(Result1),
    [] = Result1 -- [BCC,BBB,BBB,BBB,BBB,BBB,BEC],

    Result2 = exml_xpath:q(XML, "//*[contains(name(),'C')]"),
    3 = length(Result2),
    [] = Result2 -- [BCC, BEC, CCC].

string_length_test() ->
    XML = xml(<<"<AAA>"
                    "<Q/>"
                    "<SSSS/>"
                    "<BB/>"
                    "<CCC/>"
                    "<DDDDDDDD/>"
                    "<EEEE/>"
                "</AAA>">>),
    Result1 = exml_xpath:q(XML, "//*[string-length(name())=3]"),
    CCC = #xmlel{name = <<"CCC">>},
    2 = length(Result1),
    [#xmlel{name = <<"AAA">>}] = Result1 -- [CCC],

    Result2 = exml_xpath:q(XML, "//*[string-length(name()) < 3]"),
    Q = #xmlel{name = <<"Q">>},
    BB = #xmlel{name = <<"BB">>},
    [Q, BB] = Result2,

    Result3 = exml_xpath:q(XML, "//*[string-length(name()) > 3]"),
    SSSS = #xmlel{name = <<"SSSS">>},
    DDDDDDDD = #xmlel{name = <<"DDDDDDDD">>},
    EEEE = #xmlel{name = <<"EEEE">>},
    [SSSS, DDDDDDDD, EEEE] = Result3.

union_test() ->
    XML = xml(<<"<AAA>"
                    "<BBB/>"
                    "<CCC/>"
                    "<DDD>"
                        "<CCC/>"
                    "</DDD>"
                    "<EEE/>"
                "</AAA>">>),
    Result1 = exml_xpath:q(XML, "//CCC | //BBB"),
    BBB = #xmlel{name = <<"BBB">>},
    CCC = #xmlel{name = <<"CCC">>},
    3 = length(Result1),
    [] = Result1 -- [BBB, CCC, CCC],

    Result2 = exml_xpath:q(XML, "/AAA/EEE | //BBB"),
    EEE = #xmlel{name = <<"EEE">>},
    2 = length(Result2),
    [] = Result2 -- [BBB, EEE],

    Result3 = exml_xpath:q(XML, "/AAA/EEE | //DDD/CCC | /AAA | //BBB"),
    4 = length(Result3),
    [#xmlel{name = <<"AAA">>}] = Result3 -- [BBB, CCC, EEE].

child_test() ->
    XML = xml(<<"<AAA>"
                    "<BBB/>"
                    "<CCC/>"
                "</AAA>">>),
    [#xmlel{name = <<"AAA">>}] = exml_xpath:q(XML, "/AAA"),
    [#xmlel{name = <<"AAA">>}] = exml_xpath:q(XML, "/child::AAA"),
    [#xmlel{name = <<"BBB">>}] = exml_xpath:q(XML, "/AAA/BBB"),
    [#xmlel{name = <<"BBB">>}] = exml_xpath:q(XML, "/child::AAA/child::BBB"),
    [#xmlel{name = <<"BBB">>}] = exml_xpath:q(XML, "/child::AAA/BBB").

descendant_test() ->
    XML = xml(<<"<AAA>"
                    "<BBB>"
                        "<DDD>"
                            "<CCC>"
                                "<DDD/>"
                                "<EEE/>"
                            "</CCC>"
                        "</DDD>"
                    "</BBB>"
                    "<CCC>"
                        "<DDD>"
                            "<EEE>"
                                "<DDD>"
                                    "<FFF/>"
                                "</DDD>"
                            "</EEE>"
                        "</DDD>"
                    "</CCC>"
                "</AAA>">>),
    Result1 = exml_xpath:q(XML, "/descendant::*"),
    11 = length(Result1),

    Result2 = exml_xpath:q(XML, "/AAA/BBB/descendant::*"),
    EEE = #xmlel{name = <<"EEE">>},
    DDD = #xmlel{name = <<"DDD">>},
    CCC = #xmlel{name = <<"CCC">>, children = [DDD, EEE]},
    DDD2 = #xmlel{name = <<"DDD">>, children = [CCC]},
    4 = length(Result2),
    [] = Result2 -- [EEE, DDD, CCC, DDD2],

    Result3 = exml_xpath:q(XML, "//CCC/descendant::*"),
    FFF = #xmlel{name = <<"FFF">>},
    DDD3 = #xmlel{name = <<"DDD">>, children = [FFF]},
    EEE2 = #xmlel{name = <<"EEE">>, children = [DDD3]},
    DDD4 = #xmlel{name = <<"DDD">>, children = [EEE2]},
    6 = length(Result3),
    [] = Result3 -- [FFF, DDD3, EEE2, DDD4, DDD, EEE],

    Result4 = exml_xpath:q(XML, "//CCC/descendant::DDD"),
    3 = length(Result4),
    [] = Result4 -- [DDD, DDD4, DDD3].

parent_test() ->
    XML = xml(<<"<AAA>" 
                    "<BBB>"
                        "<DDD>"
                            "<CCC>"
                                "<DDD/>"
                                "<EEE/>"
                            "</CCC>"
                        "</DDD>"
                    "</BBB>"
                    "<CCC>"
                        "<DDD>"
                            "<EEE>"
                                "<DDD>"
                                    "<FFF/>"
                                "</DDD>"
                            "</EEE>"
                        "</DDD>"
                "</CCC>"
            "</AAA>">>),
    Result = exml_xpath:q(XML, "//DDD/parent::*"),
    4 = length(Result),
    CCC = #xmlel{name = <<"CCC">>, children = [#xmlel{name = <<"DDD">>},
                                               #xmlel{name = <<"EEE">>}]},
    BBB = #xmlel{name = <<"BBB">>, children = [#xmlel{name = <<"DDD">>,
                                                      children = [CCC]}]},
    EEE = #xmlel{name = <<"EEE">>, children = [#xmlel{name = <<"DDD">>,
                                                      children = [#xmlel{name = <<"FFF">>}]}]},  
    CCC1 = #xmlel{name = <<"CCC">>, children = [#xmlel{name = <<"DDD">>,
                                                       children = [EEE]}]},
    [] = Result -- [CCC, BBB, EEE, CCC1].

ancestor_test() ->
    XML = xml(<<"<AAA>"
                    "<BBB>"
                        "<DDD>"
                            "<CCC>"
                                "<DDD/>" 
                                "<EEE/>"
                            "</CCC>" 
                        "</DDD>"
                    "</BBB>"
                    "<CCC>"
                        "<DDD>"
                            "<EEE>"
                                "<DDD>"
                                    "<FFF/>"
                                "</DDD>"
                            "</EEE>"
                        "</DDD>"
                    "</CCC>"
                "</AAA>">>),
    Result = exml_xpath:q(XML, "/AAA/BBB/DDD/CCC/EEE/ancestor::*"),
    CCC = #xmlel{name = <<"CCC">>, children = [#xmlel{name = <<"DDD">>},
                                               #xmlel{name = <<"EEE">>}]},
    DDD = #xmlel{name = <<"DDD">>, children = [CCC]},
    BBB = #xmlel{name = <<"BBB">>, children = [DDD]},
    4 = length(Result),
    [#xmlel{name = <<"AAA">>}] = Result -- [BBB, CCC, DDD],

    Result2 = exml_xpath:q(XML, "//FFF/ancestor::*"),
    DDD1 = #xmlel{name = <<"DDD">>, children = [#xmlel{name = <<"FFF">>}]},
    EEE1 = #xmlel{name = <<"EEE">>, children = [DDD1]},
    DDD2 = #xmlel{name = <<"DDD">>, children = [EEE1]},
    CCC1 = #xmlel{name = <<"CCC">>, children = [DDD2]},
    5 = length(Result2),
    [#xmlel{name = <<"AAA">>}] = Result2 -- [DDD1, EEE1, DDD2, CCC1].

following_sibling_test() ->
    XML = xml(<<"<AAA>"
                    "<BBB>"
                        "<CCC/>"
                        "<DDD/>"
                    "</BBB>"
                    "<XXX>"
                        "<DDD>"
                            "<EEE/>"
                            "<DDD/>"
                            "<CCC/>"
                            "<FFF/>"
                            "<FFF>"
                                "<GGG/>"
                            "</FFF>"
                        "</DDD>"
                    "</XXX>"
                    "<CCC>"
                        "<DDD/>"
                    "</CCC>"
                "</AAA>">>),
    Result1 = exml_xpath:q(XML, "/AAA/BBB/following-sibling::*"),
    2 = length(Result1),
    CCC = #xmlel{name = <<"CCC">>, children = [#xmlel{name = <<"DDD">>}]},
    [#xmlel{name = <<"XXX">>}] = Result1 -- [CCC],

    Result2 = exml_xpath:q(XML, "//CCC/following-sibling::*"),
    3 = length(Result2),
    DDD = #xmlel{name = <<"DDD">>},
    FFF1 = #xmlel{name = <<"FFF">>},
    FFF2 = #xmlel{name = <<"FFF">>, children = [#xmlel{name = <<"GGG">>}]},
    [] = Result2 -- [DDD, FFF1, FFF2].

preceding_sibling_test() ->
    XML = xml(<<"<AAA>" 
                    "<BBB>"
                        "<CCC/>"
                        "<DDD/>"
                    "</BBB>"
                    "<XXX>"
                        "<DDD>"
                            "<EEE/>"
                            "<DDD/>"
                            "<CCC/>"
                            "<FFF/>"
                            "<FFF>"
                                "<GGG/>"
                            "</FFF>"
                        "</DDD>"
                    "</XXX>"
                    "<CCC>"
                        "<DDD/>"
                    "</CCC>"
                "</AAA>">>),

    Result1 = exml_xpath:q(XML, "/AAA/XXX/preceding-sibling::*"),
    BBB = #xmlel{name = <<"BBB">>, children = [#xmlel{name = <<"CCC">>},
                                               #xmlel{name = <<"DDD">>}]},
    [BBB] = Result1,

    Result2 = exml_xpath:q(XML, "//CCC/preceding-sibling::*"),
    EEE = #xmlel{name = <<"EEE">>},
    DDD = #xmlel{name = <<"DDD">>},
    4 = length(Result2),
    [#xmlel{name = <<"XXX">>}] = Result2 -- [BBB, EEE, DDD].

following_test() ->
    XML = xml(<<"<AAA>"
                    "<BBB>"
                        "<CCC/>"
                        "<ZZZ>"
                            "<DDD/>"
                            "<DDD>"
                                "<EEE/>"
                            "</DDD>"
                        "</ZZZ>"
                        "<FFF>"
                            "<GGG/>"
                        "</FFF>"
                    "</BBB>"
                    "<XXX>"
                        "<DDD>"
                            "<EEE/>"
                            "<DDD/>"
                            "<CCC/>"
                            "<FFF/>"
                            "<FFF>"
                                "<GGG/>"
                            "</FFF>"
                        "</DDD>"
                    "</XXX>"
                    "<CCC>"
                        "<DDD/>"
                    "</CCC>"
                "</AAA>">>),
    Result1 = exml_xpath:q(XML, "/AAA/XXX/following::*"),
    DDD = #xmlel{name = <<"DDD">>},
    CCC = #xmlel{name = <<"CCC">>, children = [DDD]},
    [CCC,DDD] = Result1,

    Result2 = exml_xpath:q(XML, "//ZZZ/following::*"),
    12 = length(Result2).

preceding_test() ->
    XML = xml(<<"<AAA>"
                    "<BBB>"
                        "<CCC/>"
                        "<ZZZ>"
                            "<DDD/>"
                        "</ZZZ>"
                    "</BBB>"
                    "<XXX>"
                        "<DDD>"
                            "<EEE/>"
                            "<DDD/>"
                            "<CCC/>"
                            "<FFF/>"
                            "<FFF>"
                                "<GGG/>"
                            "</FFF>"
                        "</DDD>"
                    "</XXX>"
                    "<CCC>"
                        "<DDD/>"
                    "</CCC>"
            "</AAA>">>),
    Result1 = exml_xpath:q(XML, "/AAA/XXX/preceding::*"),
    DDD = #xmlel{name = <<"DDD">>},
    ZZZ = #xmlel{name = <<"ZZZ">>, children = [DDD]},
    CCC = #xmlel{name = <<"CCC">>},
    BBB = #xmlel{name = <<"BBB">>, children = [CCC, ZZZ]},
    4 = length(Result1),
    [] = Result1 -- [DDD, ZZZ, CCC, BBB],

    Result2 = exml_xpath:q(XML, "//GGG/preceding::*"),
    8 = length(Result2),
    FFF = #xmlel{name = <<"FFF">>},
    EEE = #xmlel{name = <<"EEE">>},
    [] = Result2 -- [DDD, ZZZ, CCC, BBB, EEE, DDD, CCC, FFF].

descendant_or_self_test() ->
    XML = xml(<<"<AAA>" 
                    "<BBB>"
                        "<CCC/>"
                        "<ZZZ>"
                            "<DDD/>"
                        "</ZZZ>"
                    "</BBB>"
                    "<XXX>"
                        "<DDD>"
                            "<EEE/>"
                            "<DDD/>"
                            "<CCC/>"
                            "<FFF/>"
                            "<FFF>"
                                "<GGG/>"
                            "</FFF>"
                        "</DDD>"
                    "</XXX>"
                    "<CCC>"
                        "<DDD/>"
                    "</CCC>"
                "</AAA>">>),
    Result1 = exml_xpath:q(XML, "/AAA/XXX/descendant-or-self::*"),
    8 = length(Result1),
    Result2 = exml_xpath:q(XML, "//CCC/descendant-or-self::*"),
    DDD = #xmlel{name = <<"DDD">>},
    CCC1 = #xmlel{name = <<"CCC">>},
    CCC2 = #xmlel{name = <<"CCC">>, children = [DDD]},
    [] = Result2 -- [DDD, CCC1, CCC1, CCC2].

ancestor_or_self_test() ->
    XML = xml(<<"<AAA>"
                    "<BBB>"
                        "<CCC/>"
                        "<ZZZ>"
                            "<DDD/>"
                        "</ZZZ>"
                    "</BBB>"
                    "<XXX>"
                        "<DDD>"
                            "<EEE/>"
                            "<DDD/>"
                            "<CCC/>"
                            "<FFF/>"
                            "<FFF>"
                                "<GGG/>"
                            "</FFF>"
                        "</DDD>"
                    "</XXX>"
                    "<CCC>"
                        "<DDD/>"
                    "</CCC>"
            "</AAA>">>),
    Result1 = exml_xpath:q(XML, "/AAA/XXX/DDD/EEE/ancestor-or-self::*"),
    4 = length(Result1),

    Result2 = exml_xpath:q(XML, "//GGG/ancestor-or-self::*"),
    5 = length(Result2).

axes_union_test() ->
    XML = xml(<<"<AAA>"
                    "<BBB>"
                        "<CCC/>"
                        "<ZZZ/>"
                    "</BBB>"
                    "<XXX>"
                        "<DDD>"
                            "<EEE/>"
                            "<FFF>"
                                "<HHH/>"
                                "<GGG>"
                                    "<JJJ>"
                                        "<QQQ/>"
                                    "</JJJ>"
                                    "<JJJ/>"
                                "</GGG>"
                                "<HHH/>"
                            "</FFF>"
                        "</DDD>"
                    "</XXX>"
                    "<CCC>"
                        "<DDD/>"
                    "</CCC>"
                "</AAA>">>),
    Result = exml_xpath:q(XML, "//GGG/ancestor::* | //GGG/descendant::* | "
                               "//GGG/following::* | //GGG/preceding::* | "
                               "//GGG/self::*"),
    16 = length(Result).

arith_test() ->
    XML = xml(<<"<AAA>"
                    "<BBB/>"
                    "<BBB/>"
                    "<BBB/>"
                    "<BBB/>"
                    "<BBB/>"
                    "<BBB/>"
                    "<BBB/>"
                    "<BBB/>"
                    "<CCC/>"
                    "<CCC/>"
                    "<CCC/>"
                "</AAA>">>),
    BBB = #xmlel{name = <<"BBB">>},
    [BBB, BBB, BBB, BBB] = exml_xpath:q(XML, "//BBB[position() mod 2 = 0]"),
    [BBB, BBB] = exml_xpath:q(XML, "//BBB[position() = floor(last() div 2 + 0.5) or "
                                   "position() = ceiling(last() div 2 + 0.5)]"),
    CCC = #xmlel{name = <<"CCC">>},
    [CCC, CCC] = exml_xpath:q(XML, "//CCC[position() = floor(last() div 2 + 0.5) or "
                              "position() = ceiling(last() div 2 + 0.5)]").
%%--------------------------------------------------------------------
%% helpers
%%--------------------------------------------------------------------

xml(Raw) ->
    {ok, Tree} = exml:parse(Raw),
    Tree.
