-module(behaviour_supervisor_bridge).
-behaviour(supervisor_bridge).

%% API
-export([start_link/0]).

%% supervisor_bridge callbacks
-export([init/1, terminate/2]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%% API

%% @doc
%% Starts the supervisor bridge
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}

start_link() ->
	supervisor_bridge:start_link({local, ?SERVER}, ?MODULE, []).

%%% supervisor_bridge callbacks

%% @private
%% @doc
%% Creates a supervisor_bridge process, linked to the calling process,
%% which calls Module:init/1 to start the subsystem. To ensure a
%% synchronized start-up procedure, this function does not return
%% until Module:init/1 has returned.
%%
%% @spec init(Args) -> {ok, Pid, State} |
%%                     ignore |
%%                     {error, Reason}

init([]) ->
	case 'AModule':start_link() of
		{ok, Pid} ->
			{ok, Pid, #state{}};
		Error ->
			Error
	end.

%% @private
%% @doc
%% This function is called by the supervisor_bridge when it is about
%% to terminate. It should be the opposite of Module:init/1 and stop
%% the subsystem and do any necessary cleaning up.The return value is
%% ignored.
%%
%% @spec terminate(Reason, State) -> void()

terminate(_Reason, _State) ->
	'AModule':stop(),
	ok.

