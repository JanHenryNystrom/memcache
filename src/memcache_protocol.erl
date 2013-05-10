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
%%% The protocol encoding/decoding for memcached.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2013, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(memcache_protocol).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions
-export([storage/5,
         retrieval/3]).

%% Includes
-include_lib("memcache/src/memcache.hrl").

%% Types

%% Exported Types

%% Records

%% Defines
-define(REQUEST_MAGIC, 16#80).
-define(RESPONSE_MAGIC, 16#81).

-define(GET, 16#00).
-define(SET, 16#01).
-define(ADD, 16#02).
-define(REPLACE, 16#03).
-define(DELETE, 16#04).
-define(INCREMENT, 16#05).
-define(DECREMENT, 16#06).
-define(QUIT, 16#07).
-define(FLUSH, 16#08).
-define(GETQ, 16#09).
-define(NOOP, 16#0A).
-define(VERSION, 16#0B).
-define(GETK, 16#0C).
-define(GETKQ, 16#0D).
-define(APPEND, 16#0E).
-define(PREPEND, 16#0F).
-define(STAT, 16#10).
-define(SETQ, 16#11).
-define(ADDQ, 16#12).
-define(REPLACEQ, 16#13).
-define(DELETEQ, 16#14).
-define(INCREMENTQ, 16#15).
-define(DECREMENTQ, 16#16).
-define(QUITQ, 16#17).
-define(FLUSHQ, 16#18).
-define(APPENDQ, 16#19).
-define(PREPENDQ, 16#1A).


%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: storage(Type, Key, Expiration, Value, Opts) -> Binary
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec storage(atom(), key(), expiration(), data(), #opts{}) -> binary().
%%--------------------------------------------------------------------
storage(Type, Key, Expiration, Value, Opts) ->
    request(Type, Key, Expiration, Value, Opts).

%%--------------------------------------------------------------------
%% Function: retrieval(Type, Key, Opts) -> Binary
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec retrieval(atom(), key(), #opts{}) -> binary().
%%--------------------------------------------------------------------
retrieval(Type, Key, Opts) ->
    request(Type, Key, <<>>, <<>>, Opts).

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

request(Type, Key, Expiration, Value, Opts) ->
    {Key1, KeyLength} = key(Key),
    {Extras, ExtrasLength} = extras(Type, Expiration, Opts),
    Header = header(?REQUEST_MAGIC,
                    opcode(Type, Opts),
                    KeyLength,
                    ExtrasLength,
                    ExtrasLength + KeyLength + byte_size(Value),
                    opaque(Opts),
                    cas(Opts)),
    Body = body(Type, Key1, Extras, Value),
    <<Header/binary, Body/binary>>.


header(Magic, OpCode, KeyLength, ExtrasLength, BodyLength, Opaque, CAS) ->
    DataType = <<0:8>>, %% Reserved for future use
    Reserved = <<0:16>>, %% Reserved for future use
    <<Magic:8, OpCode:8, KeyLength:16,
      ExtrasLength:8, DataType:8, Reserved:16,
      BodyLength:32,
      Opaque:32,
      CAS:32>>.

key(Key) when is_binary(Key), byte_size(Key) =< 250 -> {Key, byte_size(Key)};
key(Term) ->
    case term_to_binary(Term, [compressed]) of
        Binary when byte_size(Binary) =< 250 -> {Binary, byte_size(Binary)};
        Binary -> {crypto:sha244(Binary), 244}
    end.

extras(get, <<>>, _) -> {<<>>, 0};
extras(set, Expiration, #opts{flags = Flags}) ->
    {<<Flags/binary, Expiration:32>>, 64};
extras(add, Expiration, #opts{flags = Flags}) ->
    {<<Flags/binary, Expiration:32>>, 64};
extras(replace, Expiration, #opts{flags = Flags}) ->
    {<<Flags/binary, Expiration:32>>, 64};
extras(delete, _, _) ->
    {<<>>, 0};
extras(incr, Expiration, #opts{delta = Delta, initial = Initial}) ->
    {<<Delta:64, Initial:64, Expiration:32>>, 160};
extras(decr, Expiration, #opts{delta = Delta, initial = Initial}) ->
    {<<Delta:64, Initial:64, Expiration:32>>, 160};
extras(quit, _, _) ->
    {<<>>, 0};
extras(flush, <<>>, _) ->
    {<<>>, 0};
extras(flush, Expiration, _) ->
    {<<Expiration:32>>, 32};
extras(noop, _, _) ->
    {<<>>, 0};
extras(version, _, _) ->
    {<<>>, 0};
extras(append, _, _) ->
    {<<>>, 0};
extras(prepend, _, _) ->
    {<<>>, 0};
extras(stat, _, _) ->
    {<<>>, 0}.


body(get, Key, <<>>, <<>>) -> Key;
body(set, Key, Extras, Value) -> <<Extras/binary, Key/binary, Value/binary>>;
body(add, Key, Extras, Value) -> <<Extras/binary, Key/binary, Value/binary>>;
body(replace, Key, Extras, Value) -> <<Extras/binary, Key/binary,Value/binary>>;
body(delete, Key, <<>>, <<>>) -> Key;
body(incr, Key, Extras, <<>>) -> <<Extras/binary, Key/binary>>;
body(decr, Key, Extras, <<>>) -> <<Extras/binary, Key/binary>>;
body(quit, <<>>, <<>>, <<>>) -> <<>>;
body(flush, <<>>, Extras, <<>>) -> Extras;
body(noop, <<>>, <<>>, <<>>) -> <<>>;
body(version, <<>>, <<>>, <<>>) -> <<>>;
body(append, Key, <<>>, Value) -> <<Key/binary, Value/binary>>;
body(prepend, Key, <<>>, Value) -> <<Key/binary, Value/binary>>;
body(stat, <<>>, <<>>, <<>>) -> <<>>;
body(stat, Key, <<>>, <<>>) -> Key.

opcode(get, #opts{quiet = false}) -> ?GET;
opcode(get, #opts{quiet = true}) -> ?GETQ;
opcode(set, #opts{quiet = false}) -> ?SET;
opcode(set, #opts{quiet = true}) -> ?SETQ;
opcode(add, #opts{quiet = false}) -> ?ADD;
opcode(add, #opts{quiet = true}) -> ?ADDQ;
opcode(replace, #opts{quiet = false}) -> ?REPLACE;
opcode(replace, #opts{quiet = true}) -> ?REPLACEQ;
opcode(delete, #opts{quiet = false}) -> ?DELETE;
opcode(delete, #opts{quiet = true}) -> ?DELETEQ;
opcode(incr, #opts{quiet = false}) -> ?INCREMENT;
opcode(incr, #opts{quiet = true}) -> ?INCREMENTQ;
opcode(decr, #opts{quiet = false}) -> ?DECREMENT;
opcode(decr, #opts{quiet = true}) -> ?DECREMENTQ;
opcode(quit, #opts{quiet = false}) -> ?QUIT;
opcode(quit, #opts{quiet = true}) -> ?QUITQ;
opcode(flush, #opts{quiet = false}) -> ?FLUSH;
opcode(flush, #opts{quiet = true}) -> ?FLUSHQ;
opcode(noop, _) -> ?NOOP;
opcode(version, _) -> ?VERSION;
opcode(append, #opts{quiet = false}) -> ?APPEND;
opcode(append, #opts{quiet = true}) -> ?APPENDQ;
opcode(prepend, #opts{quiet = false}) -> ?PREPEND;
opcode(prepend, #opts{quiet = true}) -> ?PREPENDQ;
opcode(stat, _) -> ?STAT.

opaque(#opts{opaque = Opaque}) -> Opaque.

cas(#opts{cas = CAS}) -> CAS.

parse(Message, OpCode) ->
    case Message of
        <<?RESPONSE_MAGIC:8, OpCode:8, KeyLength:16,
          ExtrasLength:8, _DataType:8, 0:16/unsigned,
          BodyLength:32, Opaque:32, CAS:32, T/binary>> ->
            ValueLength = BodyLength - ExtrasLength - KeyLength,
            <<Extras:ExtrasLength/bytes,
              Key:KeyLength/bytes,
              Value:ValueLength/bytes>> = T,
            {ok, Opaque, CAS, Key, Extras, Value};
        <<?RESPONSE_MAGIC:8, OpCode:8, _KeyLength:16,
          ExtrasLength:8, _DataType:8, Status:16/unsigned,
          BodyLength:32, _Opaque:32, _CAS:32, T/binary>> ->
            ErrorMsgLength = BodyLength - ExtrasLength,
            <<_:ExtrasLength/bytes, ErrorMsg:ErrorMsgLength/bytes>> = T,
            {error, Status, ErrorMsg}
    end.
