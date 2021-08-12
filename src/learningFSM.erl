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
-export([start_link/0,start/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(learningFSM_state, {ets_maps=#{},pc_names=[]}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
start() ->
  gen_server:start({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #learningFSM_state{}} | {ok, State :: #learningFSM_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
%%TODO - get a map from Pids to atom <1.32.2> -> pc1
%% get list of pids!
init([Pc_Names, Name_to_atom]) ->Map= create_ets_map(Pc_Names,Name_to_atom,#{}),
  {ok, #learningFSM_state{pc_names = Pc_Names, ets_maps = Map}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #learningFSM_state{}) ->
  {reply, Reply :: term(), NewState :: #learningFSM_state{}} |
  {reply, Reply :: term(), NewState :: #learningFSM_state{}, timeout() | hibernate} |
  {noreply, NewState :: #learningFSM_state{}} |
  {noreply, NewState :: #learningFSM_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #learningFSM_state{}} |
  {stop, Reason :: term(), NewState :: #learningFSM_state{}}).
handle_call(_Request, _From, State = #learningFSM_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #learningFSM_state{}) ->
  {noreply, NewState :: #learningFSM_state{}} |
  {noreply, NewState :: #learningFSM_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #learningFSM_state{}}).
handle_cast({network_evaluation,PC_PID,Fitness_list}, State = #learningFSM_state{}) ->
  Keep_kill_List=top_genotypes(Fitness_list),gen_server:cast(PC_PID,{network_feedback,self(),Keep_kill_List}),{noreply, State};

handle_cast({update_generation,From,List_of_gen},State) ->
  Map_ets=State#learningFSM_state.ets_maps,
  Gen_ets = maps:get(From,Map_ets),
  ets:delete_all_objects(Gen_ets),
  % clear the old ets and replace whit a new type of gens.
  update_ets(Gen_ets,List_of_gen),
  % send list of gens ti the neighbours.
  send_to_neighbours(From,List_of_gen,State#learningFSM_state.pc_names),
  {noreply, State};



handle_cast(_Request, State = #learningFSM_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #learningFSM_state{}) ->
  {noreply, NewState :: #learningFSM_state{}} |
  {noreply, NewState :: #learningFSM_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #learningFSM_state{}}).
handle_info(_Info, State = #learningFSM_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #learningFSM_state{}) -> term()).
terminate(_Reason, _State = #learningFSM_state{}) ->
  io:format("Closing learning fsm~n"),
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #learningFSM_state{},
    Extra :: term()) ->
  {ok, NewState :: #learningFSM_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #learningFSM_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

% sort from worst to the best
top_genotypes(FitList)->
  SortedFitList=lists:sort(fun({KeyA,ValA}, {KeyB,ValB}) -> {ValA,KeyA} >= {ValB,KeyB} end, FitList),make_keep_kill_List(SortedFitList,length(SortedFitList),[]).
make_keep_kill_List(_ ,0 ,List) -> List;
make_keep_kill_List([{PID,_}|T], N ,List) when N =< 75 -> make_keep_kill_List(T, N-1 ,List ++[{PID,0,false}]);
make_keep_kill_List([{PID,_}|T], N ,List) when N > 75 -> make_keep_kill_List(T, N-1 ,List ++[{PID,4,true}]).%TODO: CHANGE TO BACK RECURSION

% get ets and list and enter all the element from the list to the ets.
update_ets(_,[]) -> ok;
update_ets(Gen_ets,[{Key,Value}|T]) -> ets:insert(Gen_ets,{Key,Value}),
  update_ets(Gen_ets,T).

% send all the neighbours the new gens.
send_to_neighbours(From,List_of_gen,Pc_names)->[ gen_server:cast({global,Pc},{neighbor_ets_update,self(),List_of_gen})|| Pc<-Pc_names,Pc =/= From].


create_ets_map([],_,Map)->Map;
create_ets_map([H|T],Name_to_atom,Map)->Pc_ets = ets:new(maps:get(H,Name_to_atom),[set]),
  New_map=maps:put(H,Pc_ets,Map), create_ets_map(T,Name_to_atom,New_map).

