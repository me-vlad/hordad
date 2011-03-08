%%% -------------------------------------------------------------------
%%% File    : hordad_aes_ag_tests
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Tests for hordad_aes_poller
%%%
%%% Created : 2011-02-06 by Max E. Kuznecov <mek@mek.uz.ua>
%%% -------------------------------------------------------------------

-module(hordad_aes_ag_tests).

-include_lib("eunit/include/eunit.hrl").

add_remove_nodes_test_() ->
    {timeout, 60,
     fun() ->
             Node1 = {{127,0,0,1}, 6699},
             Node2 = {{127,0,0,1}, 5555},

             ok = hordad_aes_ag:add_nodes([Node1, Node2]),
    
             timer:sleep(10000),
    
             Nodes = [N || {N,_} <- hordad_aes_ag:status()],

             ?assertEqual(true, lists:member(Node1, Nodes)),
             ?assertEqual(true, lists:member(Node2, Nodes)),

             up = hordad_aes_ag:status(Node1),
             down = hordad_aes_ag:status(Node2),

             ?assertEqual(ok, hordad_aes_ag:remove_nodes([Node1, Node2])),
             ?assertEqual([], hordad_aes_ag:status())
     end}.

room_msg_test_() ->
    {timeout, 60,
     fun() ->
             Node1 = {{127,0,0,1}, 6699},

             % Clear all nodes in poller first
             ?assertEqual(ok, hordad_aes_ag:remove_nodes([Node1])),

             ok = hordad_rooms:join(hordad_aes_ag, self()),
             ok = hordad_aes_ag:add_nodes([Node1]),

             ok = receive
                      {hordad_aes_ag, status, Node1, down, up, _} ->
                          ok
                  after 10000 ->
                          timeout
                  end
     end}.
