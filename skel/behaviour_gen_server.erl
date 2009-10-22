-module(behaviour_gen_server).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

-type(state() :: #state{}).

%%% API

%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}

-spec(start_link/0 :: () -> {'ok', pid()} | 'ignore' | {'error', any()}).

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%% gen_server callbacks

%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}

-spec(init/1 :: (Args :: list()) ->
		{'ok', State :: state()} |
		{'ok', State :: state(), Timeout :: integer()} |
		'ignore' |
		{'stop', Reason :: any()}).

init([]) ->
	{ok, #state{}}.

%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}

-spec(handle_call/3 :: (Requests :: any(), From :: any(), State :: state()) ->
		{'reply', Reply :: any(), State :: state()} |
		{'reply', Reply :: any(), State :: state(), Timeout :: integer()} |
		{'noreply', State :: state()} |
		{'noreply', State :: state(), Timeout :: any()} |
		{'stop', Reason :: any(), Reply :: any(), State :: state()} |
		{'stop', Reason :: any(), State :: state()}).

handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}

-spec(handle_cast/2 :: (Msg :: any(), State :: state()) ->
		{'noreply', State :: state()} |
		{'noreply', State :: state(), Timeout :: integer()} |
		{'stop', Reason :: any(), State :: state()}).

handle_cast(_Msg, State) ->
	{noreply, State}.

%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}

-spec(handle_info/2 :: (Info :: any(), State :: state()) ->
		{'noreply', State :: state()} |
		{'noreply', State :: state(), Timeout :: integer()} |
		{'stop', Reason :: any(), State :: state()}).

handle_info(_Info, State) ->
	{noreply, State}.

%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()

-spec(terminate/2 :: (Reason :: any(), State :: state()) -> any()).

terminate(_Reason, _State) ->
	ok.

%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}

-spec(code_change/3 :: (OldVsn :: any(), State :: state(), Extra :: any()) ->
		{'ok', NewState :: state()}).

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%% Internal functions

