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
         prepend/4, prepend/5
        ]).

%% Retrieval
-export([get/2, get/3]).

%% Other
-export([delete/2,
         incr/5, incr/6,
         decr/5, decr/6,
         quit/1,
         flush/2,
         noop/1,
         version/1
         %% ,
         %% stats/0, stats/1
        ]).

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

%% -------------------------------------------------------------------
%% Other
%% -------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: delete(Pool, Keys) -> [ok | Error].
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec  delete(atom(), [key()])-> [{ok, data()} | {error, _}].
%%--------------------------------------------------------------------
delete(Pool, Keys) ->
    [memcache_pool_master:request(Pool,
                                  memcache_protocol:delete(Key),
                                  #opts{}) ||
        Key <- Keys].

%%--------------------------------------------------------------------
%% Function: incr(Pool, Key, Delta, Initial, Expiration) -> ok | Error.
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec  incr(atom(), key(), pos_integer(), pos_integer(), expiration()) ->
           ok | {error, _}.
%%--------------------------------------------------------------------
incr(Pool, Key, Delta, Initial, Expiration) ->
    incr(Pool, Key, Delta, Initial, Expiration, []).

%%--------------------------------------------------------------------
%% Function: incr(Pool, Key, Delta, Initial, Expiration, Options) -> ok | Error.
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec  incr(atom(), key(), pos_integer(), pos_integer(),expiration(),[opt()]) ->
           ok | {error, _}.
%%--------------------------------------------------------------------
incr(Pool, Key, Delta, Initial, Expiration, Opts) ->
    OptsRecord = parse_opts(Opts),
    Request =
        memcache_protocol:change(incr, Key, Delta, Initial, Expiration, Opts),
    memcache_pool_master:request(Pool, Request, OptsRecord).

%%--------------------------------------------------------------------
%% Function: decr(Pool, Key, Delta, Initial, Expiration) -> ok | Error.
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec  decr(atom(), key(), pos_integer(), pos_integer(), expiration()) ->
           ok | {error, _}.
%%--------------------------------------------------------------------
decr(Pool, Key, Delta, Initial, Expiration) ->
    decr(Pool, Key, Delta, Initial, Expiration, []).

%%--------------------------------------------------------------------
%% Function: decr(Pool, Key, Delta, Initial, Expiration, Options) -> ok | Error.
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec  decr(atom(), key(), pos_integer(), pos_integer(),expiration(),[opt()]) ->
           ok | {error, _}.
%%--------------------------------------------------------------------
decr(Pool, Key, Delta, Initial, Expiration, Opts) ->
    OptsRecord = parse_opts(Opts),
    Request =
        memcache_protocol:change(decr, Key, Delta, Initial, Expiration, Opts),
    memcache_pool_master:request(Pool, Request, OptsRecord).

%%--------------------------------------------------------------------
%% Function: quit(Pool) -> ok.
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec  quit(atom()) -> ok.
%%--------------------------------------------------------------------
quit(Pool) ->
    memcache_pool_master:request(Pool, memcache_protocol:quit(), #opts{}).

%%--------------------------------------------------------------------
%% Function: flush(Pool, Expiration) -> ok.
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec  flush(atom(), expiration()) -> ok.
%%--------------------------------------------------------------------
flush(Pool, Expiration) ->
    memcache_pool_master:request(Pool,
                                 memcache_protocol:flush(Expiration),
                                 #opts{}).

%%--------------------------------------------------------------------
%% Function: noop(Pool) -> ok.
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec  noop(atom()) -> ok.
%%--------------------------------------------------------------------
noop(Pool) ->
    memcache_pool_master:request(Pool, memcache_protocol:noop(), #opts{}).

%%--------------------------------------------------------------------
%% Function: noop(Pool) -> ok.
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec  version(atom()) -> ok.
%%--------------------------------------------------------------------
version(Pool) ->
    memcache_pool_master:request(Pool, memcache_protocol:version(), #opts{}).

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
