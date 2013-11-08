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


%%--------------------------------------------------------------------
%% helpers
%%--------------------------------------------------------------------

xml(Raw) ->
    {ok, Tree} = exml:parse(Raw),
    Tree.
