-module(exml_xpath_tests).

-include_lib("eunit/include/eunit.hrl").
-include("exml.hrl").

-compile(export_all).

%%--------------------------------------------------------------------
%% tests
%%--------------------------------------------------------------------
absolute_path_test() ->
    XML = xml(<<"<AAA>"
                "    <BBB/>"
                "    <CCC/>"
                "    <BBB/>"
                "    <BBB/>"
                "    <DDD> <BBB/> </DDD>"
                "    <CCC/>"
                "</AAA>">>),

    [XML] = exml_xpath:q(XML, "/AAA"),

    CCC = #xmlel{name = <<"CCC">>},
    [CCC, CCC] = exml_xpath:q(XML, <<"/AAA/CCC">>),
    
    BBB = #xmlel{name = <<"BBB">>},
    [BBB] = exml_xpath:q(XML, "/AAA/DDD/BBB").

all_elements_test() ->
    XML = xml(<<"<AAA>"
                "    <BBB/> "
                "    <CCC/> "
                "    <BBB/> "
                "    <DDD> "
                "        <BBB/> "
                "    </DDD> "
                "    <CCC> "
                "        <DDD> "
                "            <BBB/> "
                "            <BBB/> "
                "        </DDD> "
                "    </CCC> "
                "</AAA>">>),
    BBB = #xmlel{name = <<"BBB">>},
    [BBB, BBB, BBB, BBB, BBB] = exml_xpath:q(XML, "//BBB"),
    [BBB, BBB, BBB] = exml_xpath:q(XML, "//DDD/BBB").
    

%%--------------------------------------------------------------------
%% helpers
%%--------------------------------------------------------------------

xml(Raw) ->
    {ok, Tree} = exml:parse(Raw),
    Tree.
