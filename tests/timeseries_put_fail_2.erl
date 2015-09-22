-module(timeseries_put_fail_2).

%%
%% this test tries to write duff data to a bucket
%%

-behavior(riak_test).

-export([
	 confirm/0
	]).

-import(timeseries_util, [
			  get_ddl/1,
			  get_invalid_obj/0,
			  confirm_put/5
			  ]).

confirm() ->
    ClusterType = single,
    DDL = get_ddl(docs),
    Obj = [get_invalid_obj()],
    Expected = "some error message",
    confirm_put(ClusterType, normal, DDL, Obj, Expected).
