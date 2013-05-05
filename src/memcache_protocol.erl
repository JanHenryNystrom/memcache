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
-define(TERMINATOR, <<"\r\n">>).

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: storage(Type, Key, Expiration, Data, OptsRecord) -> IOList
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec storage(atom(), key(), expiration(), data(), #opts{}) -> iolist().
%%--------------------------------------------------------------------
storage(Type, Key, Expiration, Data, Opts = #opts{protocol = text}) ->
    Payload = term_to_binary(Data, [compressed]),
    [atom_to_binary(Type, latin1),
     encode_key(Key, text),
     encode_flags(Opts, text),
     encode_expiration(Expiration, text),
     byte_size(Payload),
     encode_noreply(Opts, text),
     ?TERMINATOR,
     Payload,
     ?TERMINATOR
     ].

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

encode_key(_, _) -> <<>>.

encode_flags(_, _) -> <<>>.

encode_expiration(_, _) -> <<>>.

encode_noreply(_, _) -> <<>>.
