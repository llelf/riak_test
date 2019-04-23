-module(rcb).
-behaviour(riak_test).
-export([confirm/0]).
-include_lib("eunit/include/eunit.hrl").

-define(PROPS,
        [
         {allow_mult, true, false},
         {backend, <<"custom">>, <<"other">>},
         {basic_quorum, true, false},
         {big_vclock, 100, 50},
         {dw, 0, quorum},
         {last_write_wins, true, false},
         {n_val, 2, 3},
         {notfound_ok, false, true},
         {old_vclock, 10000, 86400},
         {pr, 2, 0},
         {pw, all, 0},
         {r, all, quorum},
         {repl, realtime, false},
         {rw, 1, quorum},
         {search, true, false},
         {small_vclock, 10, 50},
         {w, one, quorum},
         {young_vclock, 0, 20}
        ]).

-define(BUCKET, <<"foo">>).

confirm() ->
    N=20,
    [Node1|_] = Nodes = rt:build_cluster(N),
    rt:wait_until_nodes_ready(Nodes),

    Type = <<"s">>,
    Props = [ {allow_mult, false},
              {dvv_enabled, false},
              {last_write_wins, true}
            ],

    rt:create_and_activate_bucket_type(Node1, Type, Props),
    rt:wait_until_bucket_type_status(Type, active, Nodes),

    %% @TODO don't wait???????
    rt:wait_until_bucket_props(Nodes, {Type, <<"bucket">>}, Props),

    Conns = [ rt:pbc(Node) || Node <- Nodes ],

    [ begin
          P = last_write_wins,
          lager:info("[~p][~p/~p] setting ~p=~p", [I,K,Node,P,V]),

          %Node = lists:nth(random:uniform(length(Nodes)), Nodes),
          set(C,P,V)
      end
      ||
        %% [{P,V,_}|_] <- [?PROPS],
        I <- lists:seq(1,1024),

        V <- [true,false],
        K <- lists:seq(1,4),
        {Node,C} <- lists:zip(Nodes,Conns)
    ],

    pass.

%% multiset(Nodes, P, V) ->
%%     implement_me.

set(C, P, V) ->
    %C = rt:pbc(Node),
    riakc_pb_socket:set_bucket(C, ?BUCKET, [{P, V}]).
