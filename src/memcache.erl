%%==============================================================================
%% Copyright 2013 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

%%%-------------------------------------------------------------------
%%% @doc
%%% The  memcache.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2013, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(memcache).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Management API
-export([start/0, stop/0]).

%% API

%% Storage
-export([set/4, set/5,
         add/4, add/5,
         replace/4, replace/5,
         append/4, append/5,
         prepend/4, prepend/5,
         cas/5, cas/6
        ]).

%% Retrieval
-export([get/2, get/3,
         gets/2, gets/3]).

%% %% Other
%% -export([delete/1,
%%          incr/2, decr/2,
%%          touch/2,
%%          reassign/3,
%%          automove/1,
%%          stats/0, stats/1,
%%          flush_all/0,
%%          version/0,
%%          quit/0
%%         ]).

%% Includes
-include_lib("memcache/src/memcache.hrl").

%% Records

%% Types

%% ===================================================================
%% Management API
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec start() -> ok.
%%--------------------------------------------------------------------
start() ->
    application:start(crypto),
    application:start(jhn),
    application:start(?MODULE).

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok.
%%--------------------------------------------------------------------
stop() ->
    application:stop(?MODULE),
    application:stop(jhn),
    application:stop(crypto).

%% ===================================================================
%% API
%% ===================================================================

%% -------------------------------------------------------------------
%% Storage
%% -------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: set(Pool, Key, Expiration, Data) -> ok | Error.
%% @doc
%%   Equivalent to set(Pool, Key, Expiration, Data, []).
%% @end
%%--------------------------------------------------------------------
-spec set(atom(), key(), expiration(), data()) -> ok | {error, _}.
%%--------------------------------------------------------------------
set(Pool, Key, Expiration, Data) -> set(Pool, Key, Expiration, Data, []).

%%--------------------------------------------------------------------
%% Function: set(Pool, Key, Expiration, Data, Options) -> ok | Error.
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec set(atom(), key(), expiration(), data(), [opt()]) -> ok | {error, _}.
%%--------------------------------------------------------------------
set(Pool, Key, Expiration, Data, Opts) ->
    OptsRecord = parse_opts(Opts),
    Request = memcache_protocol:storage(set, Key, Expiration, Data, OptsRecord),
    memcache_pool_master:request(Pool, Request, OptsRecord).

%%--------------------------------------------------------------------
%% Function: add(Pool, Key, Expiration, Data) -> ok | Error.
%% @doc
%%   Equivalent to add(Pool, Key, Expiration, Data, []).
%% @end
%%--------------------------------------------------------------------
-spec add(atom(), key(), expiration(), data()) -> ok | {error, _}.
%%--------------------------------------------------------------------
add(Pool, Key, Expiration, Data) -> add(Pool, Key, Expiration, Data, []).

%%--------------------------------------------------------------------
%% Function: add(Pool, Key, Expiration, Data, Options) -> ok | Error.
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec add(atom(), key(), expiration(), data(), [opt()]) -> ok | {error, _}.
%%--------------------------------------------------------------------
add(Pool, Key, Expiration, Data, Opts) ->
    OptsRecord = parse_opts(Opts),
    Request = memcache_protocol:storage(add, Key, Expiration, Data, OptsRecord),
    memcache_pool_master:request(Pool, Request, OptsRecord).

%%--------------------------------------------------------------------
%% Function: replace(Pool, Key, Expiration, Data) -> ok | Error.
%% @doc
%%   Equivalent to replace(Pool, Key, Expiration, Data, []).
%% @end
%%--------------------------------------------------------------------
-spec replace(atom(), key(), expiration(), data()) -> ok | {error, _}.
%%--------------------------------------------------------------------
replace(Pool, Key, Expiration, Data) -> replace(Pool, Key, Expiration, Data,[]).

%%--------------------------------------------------------------------
%% Function: replace(Pool, Key, Expiration, Data, Options) -> ok | Error.
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec replace(atom(), key(), expiration(), data(), [opt()]) -> ok | {error, _}.
%%--------------------------------------------------------------------
replace(Pool, Key, Expiration, Data, Opts) ->
    OptsRecord = parse_opts(Opts),
    Request =
        memcache_protocol:storage(replace, Key, Expiration, Data, OptsRecord),
    memcache_pool_master:request(Pool, Request, OptsRecord).

%%--------------------------------------------------------------------
%% Function: append(Pool, Key, Expiration, Data) -> ok | Error.
%% @doc
%%   Equivalent to append(Pool, Key, Expiration, Data, []).
%% @end
%%--------------------------------------------------------------------
-spec append(atom(), key(), expiration(), data()) -> ok | {error, _}.
%%--------------------------------------------------------------------
append(Pool, Key, Expiration, Data) -> append(Pool, Key, Expiration, Data, []).

%%--------------------------------------------------------------------
%% Function: append(Pool, Key, Expiration, Data, Options) -> ok | Error.
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec append(atom(), key(), expiration(), data(), [opt()]) -> ok | {error, _}.
%%--------------------------------------------------------------------
append(Pool, Key, Expiration, Data, Opts) ->
    OptsRecord = parse_opts(Opts),
    Request =
        memcache_protocol:storage(append, Key, Expiration, Data, OptsRecord),
    memcache_pool_master:request(Pool, Request, OptsRecord).

%%--------------------------------------------------------------------
%% Function: prepend(Pool, Key, Expiration, Data) -> ok | Error.
%% @doc
%%   Equivalent to prepend(Pool, Key, Expiration, Data, []).
%% @end
%%--------------------------------------------------------------------
-spec prepend(atom(), key(), expiration(), data()) -> ok | {error, _}.
%%--------------------------------------------------------------------
prepend(Pool, Key, Expiration, Data) -> prepend(Pool, Key, Expiration, Data,[]).

%%--------------------------------------------------------------------
%% Function: prepend(Pool, Key, Expiration, Data, Options) -> ok | Error.
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec prepend(atom(), key(), expiration(), data(), [opt()]) -> ok | {error, _}.
%%--------------------------------------------------------------------
prepend(Pool, Key, Expiration, Data, Opts) ->
    OptsRecord = parse_opts(Opts),
    Request =
        memcache_protocol:storage(prepend, Key, Expiration, Data, OptsRecord),
    memcache_pool_master:request(Pool, Request, OptsRecord).

%%--------------------------------------------------------------------
%% Function: cas(Pool, Key, Expiration, Data) -> ok | Error.
%% @doc
%%   Equivalent to cas(Pool, Key, Expiration, Data, []).
%% @end
%%--------------------------------------------------------------------
-spec cas(atom(), key(), expiration(), data(), binary()) -> ok | {error, _}.
%%--------------------------------------------------------------------
cas(Pool, Key, Expiration, Data, Unique) ->
    cas(Pool, Key, Expiration, Data, Unique, []).

%%--------------------------------------------------------------------
%% Function: cas(Pool, Key, Expiration, Data, Options) -> ok | Error.
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec cas(atom(), key(), expiration(), data(), binary(), [opt()]) ->
          ok | {error, _}.
%%--------------------------------------------------------------------
cas(Pool, Key, Expiration, Data, Unique, Opts) ->
    OptsRecord = parse_opts(Opts),
    Request = memcache_protocol:cas(Key, Expiration, Data, Unique, OptsRecord),
    memcache_pool_master:request(Pool, Request, OptsRecord).

%% -------------------------------------------------------------------
%% Retrieval
%% -------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: get(Pool, Keys) -> {ok, Data} | Error
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec  get(atom(), [key()])-> {ok, data()} | {error, _}.
%%--------------------------------------------------------------------
get(Pool, Keys) -> get(Pool, Keys, []).

%%--------------------------------------------------------------------
%% Function: get(Pool, Keys, Options) -> {ok, Data} | Error
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec  get(atom(), [key()], [opt()])-> {ok, data()} | {error, _}.
%%--------------------------------------------------------------------
get(Pool, Keys, Opts) ->
    OptsRecord = parse_opts(Opts),
    Request = memcache_protocol:retrieval(get, Keys, OptsRecord),
    memcache_pool_master:request(Pool, Request, OptsRecord).

%%--------------------------------------------------------------------
%% Function: gets(Pool, Keys) -> {ok, Data} | Error
%% @doc
%%   
%%   cas is returned.
%% @end
%%--------------------------------------------------------------------
-spec  gets(atom(), [key()])-> {ok, data()} | {error, _}.
%%--------------------------------------------------------------------
gets(Pool, Keys) -> gets(Pool, Keys, []).

%%--------------------------------------------------------------------
%% Function: gets(Pool, Keys, Options) -> {ok, Data} | Error
%% @doc
%%   
%%   cas is returned.
%% @end
%%--------------------------------------------------------------------
-spec  gets(atom(), [key()], [opt()])-> {ok, data()} | {error, _}.
%%--------------------------------------------------------------------
gets(Pool, Keys, Opts) ->
    OptsRecord = parse_opts(Opts),
    Request = memcache_protocol:retrieval(gets, Keys, OptsRecord),
    memcache_pool_master:request(Pool, Request, OptsRecord).

%% -------------------------------------------------------------------
%% Other
%% -------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
%-spec  -> .
%%--------------------------------------------------------------------

%% ===================================================================
%% Internal functions.
%% ===================================================================

parse_opts(_) -> #opts{}.
