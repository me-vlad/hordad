%%% -------------------------------------------------------------------
%%% File    : hordad_aes_poller_tests
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Tests for hordad_aes_poller
%%%
%%% Created : 2011-02-06 by Max E. Kuznecov <mek@mek.uz.ua>
%%% -------------------------------------------------------------------

-module(hordad_aes_poller_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("hordad_aes_agent/include/hordad_aes_agent.hrl").

default_tag_test_() ->
    {timeout, 60,
     fun() ->
             Tag = hordad_aes_poller:get_default_tag(),
             Node = {{127,0,0,1}, 6699},

             ?assertEqual([Node], hordad_aes_poller:get_nodes(Tag)),
             ?assertMatch([{Node, _, _}], hordad_aes_poller:get_reports(Tag))
     end}.

add_remove_nodes_test_() ->
    {timeout, 60,
     fun() ->
             Tag = "test",

             Node1 = {{127,0,0,1}, 6699},
             Node2 = {{127,0,0,1}, 5555},

             ok = hordad_aes_poller:add_nodes(Tag, [Node1, Node2]),
    
             timer:sleep(10000),
    
             Nodes = hordad_aes_poller:get_nodes(Tag),

             ?assertEqual(true, lists:member(Node1, Nodes)),
             ?assertEqual(true, lists:member(Node2, Nodes)),

             Reports = hordad_aes_poller:get_reports(Tag),

             ?assertEqual(2, length(Reports)),
    
             {Node1, Rep1, _} = hordad_aes_poller:get_report(Tag, Node1),
             {Node2, Rep2, _} = hordad_aes_poller:get_report(Tag, Node2),

             ?assertEqual(available, Rep1#agent_report.status),
             ?assertEqual(down, Rep2#agent_report.status),

             ?assertEqual(ok, hordad_aes_poller:clear_nodes(Tag)),
             ?assertEqual([], hordad_aes_poller:get_nodes(Tag))
     end}.

room_msg_test_() ->
    {timeout, 60,
     fun() ->
             Tag = "room",
             Node1 = {{127,0,0,1}, 6699},

             % Clear all nodes in poller first
             lists:foreach(fun(T) ->
                                   ok = hordad_aes_poller:clear_nodes(T)
                           end, hordad_aes_poller:get_tags()),

             ok = hordad_rooms:join(hordad_aes_poller, self()),
             ok = hordad_aes_poller:add_nodes(Tag, [Node1]),
             ok = receive
                      {hordad_aes_poller, status, Node1, _, available, _} ->
                          ok
                  after 10000 ->
                          timeout
                  end
     end}.
