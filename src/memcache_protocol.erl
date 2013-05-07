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
%%% The protocol encoding/decoding for Zookeeper.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2013, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(memcache_protocol).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions
-export([storage/5]).

%% Includes
-include_lib("memcache/src/memcache.hrl").

%% Types

%% Exported Types

%% Records

%% Defines
-define(REQUEST_MAGIC, 16#80).
-define(RESPONSE_MAGIC, 16#81).

-define(get, 16#00).
-define(set, 16#01).
-define(add, 16#02).
-define(replace, 16#03).
-define(delete, 16#04).
-define(increment, 16#05).
-define(decrement, 16#06).
-define(quit, 16#07).
-define(flush, 16#08).
-define(getq, 16#09).
-define(noop, 16#0A).
-define(version, 16#0B).
-define(getk, 16#0C).
-define(getkq, 16#0D).
-define(append, 16#0E).
-define(prepend, 16#0F).
-define(stat, 16#10).
-define(setq, 16#11).
-define(addq, 16#12).
-define(replaceq, 16#13).
-define(deleteq, 16#14).
-define(incrementq, 16#15).
-define(decrementq, 16#16).
-define(quitq, 16#17).
-define(flushq, 16#18).
-define(appendq, 16#19).
-define(prependq, 16#1A).


%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: storage(Type, Key, Expiration, Data, Opts) -> Binary
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec storage(atom(), key(), expiration(), data(), #opts{}) -> binary().
%%--------------------------------------------------------------------
storage(_Type, Key, _Expiration, _Data, _Opts = #opts{}) ->
    _EKey = encode_key(Key, binary),
    <<?REQUEST_MAGIC>>.

%%--------------------------------------------------------------------
%% Function: 
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------

%% ===================================================================
%% Internal functions.
%% ===================================================================

header(Magic, OpCode, KeyLength, ExtrasLength, BodyLength, Opaque, CAS) ->
    DataType = <<0:8>>, %% Reserved for future use
    Reserved = <<0:16>>, %% Reserved for future use
    <<Magic:8, OpCode:8, KeyLength:16,
      ExtrasLength:8, DataType:8, Reserved:16,
      BodyLength:32,
      Opaque:32,
      CAS:32>>.

encode_key(_, _) -> <<>>.

encode_flags(_, _) -> <<>>.

encode_expiration(_, _) -> <<>>.

encode_noreply(_, _) -> <<>>.
