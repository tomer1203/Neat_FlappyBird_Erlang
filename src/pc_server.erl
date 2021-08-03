%%%-------------------------------------------------------------------
%%% @author Omri, Tomer
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03.8  2021 12:26
%%%-------------------------------------------------------------------
-module(pc_server).
-author("Omri, Tomer").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(pc_server_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #pc_server_state{}} | {ok, State :: #pc_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Genotype_list]) ->PID_genotype_map =#{},
  [maps:put(G,spawn_monitor(neuralNetwork:start_link()),PID_genotype_map)  || G <-Genotype_list],

  {ok, #pc_server_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #pc_server_state{}) ->
  {reply, Reply :: term(), NewState :: #pc_server_state{}} |
  {reply, Reply :: term(), NewState :: #pc_server_state{}, timeout() | hibernate} |
  {noreply, NewState :: #pc_server_state{}} |
  {noreply, NewState :: #pc_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #pc_server_state{}} |
  {stop, Reason :: term(), NewState :: #pc_server_state{}}).
handle_call(_Request, _From, State = #pc_server_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #pc_server_state{}) ->
  {noreply, NewState :: #pc_server_state{}} |
  {noreply, NewState :: #pc_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #pc_server_state{}}).
handle_cast(_Request, State = #pc_server_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #pc_server_state{}) ->
  {noreply, NewState :: #pc_server_state{}} |
  {noreply, NewState :: #pc_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #pc_server_state{}}).
handle_info(_Info, State = #pc_server_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #pc_server_state{}) -> term()).
terminate(_Reason, _State = #pc_server_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #pc_server_state{},
    Extra :: term()) ->
  {ok, NewState :: #pc_server_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #pc_server_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

put_and_send(G,PID,Map) ->
  maps:put(G,spawn_monitor(neuralNetwork:start_link()),Map),
  PID ! {start_simulation,self(),G,Pipe_list}