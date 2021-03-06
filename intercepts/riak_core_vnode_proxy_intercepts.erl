%% -------------------------------------------------------------------
%%
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%%-------------------------------------------------------------------

-module(riak_core_vnode_proxy_intercepts).
-compile([export_all, nowarn_export_all]).
-include("intercept.hrl").
-define(M, riak_core_vnode_proxy_orig).

%% @doc force a soft-loaded response for the proxy
soft_load_mbox(_, CRI) ->
    ?I_INFO("returning a soft loaded mbox\n"),
    {soft_loaded, CRI*2, CRI}.

%% @doc force a delayed soft-loaded response for the proxy
timout_mbox_check(_, CRI) ->
    ?I_INFO("returning a soft loaded mbox, but way late\n"),
    %% NOTE: in riak at present the soft-load mbox check timeout is
    %% 100ms. Make this 200ms (TODO maybe use an intercept anon-fun to
    %% make this configurable)
    timer:sleep(200),
    {soft_loaded, CRI*2, CRI}.

