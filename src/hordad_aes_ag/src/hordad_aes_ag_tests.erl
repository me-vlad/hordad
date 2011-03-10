%%% -------------------------------------------------------------------
%%% File    : hordad_aes_ag_tests
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Tests for hordad_aes_poller
%%%
%%% Created : 2011-02-06 by Max E. Kuznecov <mek@mek.uz.ua>
%%% -------------------------------------------------------------------

-module(hordad_aes_ag_tests).

-include_lib("eunit/include/eunit.hrl").

owner_test_() ->
    {timeout, 60,
     fun() ->
             GetNodes = fun() ->
                                [N || {N,_} <- hordad_aes_ag:status()]
                        end,

             Node1 = {{127,0,0,1}, 6699},
             Node2 = {{127,0,0,1}, 5555},
             Node3 = {{127,0,0,1}, 7777},

             ok = hordad_aes_ag:add_nodes([Node1, Node2], custom1),
             ok = hordad_aes_ag:add_nodes([Node2, Node3], custom2),

             %% Now we have:
             %% Node1: [default, custom1]
             %% Node2: [custom1, custom2]
             %% Node3: [custom2]

             N1 = GetNodes(),

             ?assertEqual(true, lists:member(Node1, N1)),
             ?assertEqual(true, lists:member(Node2, N1)),
             ?assertEqual(true, lists:member(Node3, N1)),

             %% Remove node with nonexisting owner
             ?assertEqual(ok, hordad_aes_ag:remove_nodes([Node3], wtf)),
             ?assertEqual(true, lists:member(Node3, GetNodes())),

             %% Now actually remove
             ?assertEqual(ok, hordad_aes_ag:remove_nodes([Node3], custom2)),
             ?assertEqual(false, lists:member(Node3, GetNodes())),

             %% Remove node with multiple owners
             ?assertEqual(ok, hordad_aes_ag:remove_nodes([Node2], custom2)),
             ?assertEqual(true, lists:member(Node2, GetNodes())),
             ?assertEqual(ok, hordad_aes_ag:remove_nodes([Node2], custom1)),
             ?assertEqual(false, lists:member(Node2, GetNodes())),

             ?assertEqual(ok, hordad_aes_ag:remove_nodes([Node1], custom1)),
             ?assertEqual(ok, hordad_aes_ag:remove_nodes([Node1], default)),

             ?assertEqual([], hordad_aes_ag:status())
     end}.

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
