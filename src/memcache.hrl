%% -*-erlang-*-
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

%% Records
-record(pool_spec, {name :: atom(),
                    timeout :: timeout(),
                    size :: pos_integer(),
                    hosts :: [{string(), non_neg_integer()}]}).

-record(req, {sync = false :: boolean(),
              from :: undefined | jhn_server:from(),
              type ::atom(),
              payload :: _}).

-record(opts, {quiet = false :: boolean(),
               multi = false :: boolean(),
               opaque = <<0:32>> :: binary(),
               cas = <<0:32>> :: binary(),
               flags = <<>> :: binary(),
               key = false :: boolean(),
               delta  = 0 ::  integer(),
               initial = 0 :: pos_integer()
              }).

%% Types
-type key() :: _.
-type data() :: _.
-type expiration() :: _.
-type opt() :: quiet | key | {cas, binary()}.
