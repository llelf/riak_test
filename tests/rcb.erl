-module(rcb).
-behaviour(riak_test).
-export([confirm/0]).
-include_lib("eunit/include/eunit.hrl").

-define(Type,   <<"s">>).
-define(Bucket, {?Type,<<"foo">>}).

confirm() ->
    N=20,
    [Node1|_] = Nodes = rt:build_cluster(N),
    rt:wait_until_nodes_ready(Nodes),

    Props0 = [{prop,0}],

    rt:create_and_activate_bucket_type(Node1, ?Type, Props0),
    rt:wait_until_bucket_type_status(?Type, active, Nodes),

    %% @TODO don't wait???????
    rt:wait_until_bucket_props(Nodes, ?Bucket, Props0),

    Conns = [ rt:pbc(Node) || Node <- Nodes ],

    [ begin
          P = prop,
          lager:info("[~p][~p] setting ~p=~p", [I,K,P,V]),

          %Node = rt:select_random(Nodes),
          ok = multiset(Nodes,Conns,P,V)
      end
      ||
        I <- lists:seq(1,1024),
        V <- [111,222],
        K <- lists:seq(1,4)
        %{Node,C} <- lists:zip(Nodes,Conns)
    ],

    pass.

shuffle(XS) -> lists:sort(fun(_,_) -> random:uniform(2) > 1 end, XS).

multiset(Nodes, OrdConns, P, V) ->
    Me = self(),
    N = length(Nodes),
    {Conns,Ordering} = lists:unzip(shuffle(lists:zip(OrdConns, lists:seq(1,N)))),
    lager:info(" - ~p", [Ordering]),
    [ spawn(fun() -> Me ! set(C,P,V) end) || C <- Conns ],
    [ receive ok -> ok end || _ <- Conns ],
    ok.

set(C, P, V) ->
    %C = rt:pbc(Node),
    riakc_pb_socket:set_bucket(C, ?Bucket, [{P, V}]).

