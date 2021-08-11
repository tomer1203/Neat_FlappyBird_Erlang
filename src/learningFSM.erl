%%%-------------------------------------------------------------------
%%% @author omri
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Aug 2021 19:55
%%%-------------------------------------------------------------------
-module(learningFSM).
-author("omri").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(lerningFSM_state, {}).

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
  {ok, State :: #lerningFSM_state{}} | {ok, State :: #lerningFSM_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #lerningFSM_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #lerningFSM_state{}) ->
  {reply, Reply :: term(), NewState :: #lerningFSM_state{}} |
  {reply, Reply :: term(), NewState :: #lerningFSM_state{}, timeout() | hibernate} |
  {noreply, NewState :: #lerningFSM_state{}} |
  {noreply, NewState :: #lerningFSM_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #lerningFSM_state{}} |
  {stop, Reason :: term(), NewState :: #lerningFSM_state{}}).
handle_call(_Request, _From, State = #lerningFSM_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #lerningFSM_state{}) ->
  {noreply, NewState :: #lerningFSM_state{}} |
  {noreply, NewState :: #lerningFSM_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #lerningFSM_state{}}).
handle_cast({network_evaluation,PC_PID,Fitness_list}, State = #lerningFSM_state{}) ->
  Top=top_genotypes(Fitness_list),gen_server:cast(PC_PID,{network_feedback,Top}),{noreply, State};

handle_cast(_Request, State = #lerningFSM_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #lerningFSM_state{}) ->
  {noreply, NewState :: #lerningFSM_state{}} |
  {noreply, NewState :: #lerningFSM_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #lerningFSM_state{}}).
handle_info(_Info, State = #lerningFSM_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #lerningFSM_state{}) -> term()).
terminate(_Reason, _State = #lerningFSM_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #lerningFSM_state{},
    Extra :: term()) ->
  {ok, NewState :: #lerningFSM_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #lerningFSM_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


top_genotypes(FitList)->
  SortedFitList=lists:sort(fun({KeyA,ValA}, {KeyB,ValB}) -> {ValA,KeyA} >= {ValB,KeyB} end, FitList),Top_fit=top_fit(SortedFitList,length(SortedFitList)).
top_fit(T,25)->T;
top_fit([_|T],N)-> top_fit(T,N-1).