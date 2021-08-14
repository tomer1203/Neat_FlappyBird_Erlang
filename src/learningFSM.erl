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

-include("Constants.hrl").

%% API
-export([start_link/4,start/4]).
-export([lfsm_rpc/1]).
-export([create_ets_map/3]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(learningFSM_state, {number_of_pc = 4,ets_maps=#{},pc_names=[],fitness_list =[],number_of_nn = 100}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)

start_link(Number_of_Pcs,Pc_Names, Name_to_atom,Number_of_networks) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Number_of_Pcs, Pc_Names, Name_to_atom,Number_of_networks], []).
start(Number_of_Pcs,Pc_Names, Name_to_atom,Number_of_networks) ->
  gen_server:start({local, ?SERVER}, ?MODULE, [Number_of_Pcs, Pc_Names, Name_to_atom,Number_of_networks], []).

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
init([Number_of_Pcs, Pc_Names, Name_to_atom,Number_of_networks]) ->
  process_flag(trap_exit, false),
  io:format("LFSM UP~n"),
  Map= create_ets_map(Pc_Names,Name_to_atom,#{}),
  {ok, #learningFSM_state{number_of_pc = Number_of_Pcs,pc_names = Pc_Names, ets_maps = Map, fitness_list = [],number_of_nn = Number_of_networks}}.

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
handle_cast({network_evaluation,PC_PID,Fitness_list}, State = #learningFSM_state{fitness_list = Acc_Fitness_list,number_of_pc = Number_of_pc,number_of_nn = Number_of_nn }) ->
  io:format("got network evaluation function~n"),
  io:format("the name of the server is : : : ~p~n",[PC_PID]),
  All_Fitness_lists = lists:append(Fitness_list,Acc_Fitness_list),
  New_Fitness_list =  case length(All_Fitness_lists) of
                        Number_of_nn->

                          Keep_List=top_genotypes(All_Fitness_lists,Number_of_nn,State#learningFSM_state.ets_maps),
                          % TODO change to rcp call ! !
                          %io:format("nana bana i do what i whena~p~n",[{pid_to_name(PC_PID),pc_server,pc_rpc,[{network_feedback,self(),Keep_List}]}]),
                          rpc:call(?PC1,pc_server,pc_rpc,[pc1,{network_feedback,self(),Keep_List}]),
                          rpc:call(?PC2,pc_server,pc_rpc,[pc2,{network_feedback,self(),Keep_List}]),
                          rpc:call(?PC3,pc_server,pc_rpc,[pc3,{network_feedback,self(),Keep_List}]),
                          rpc:call(?PC4,pc_server,pc_rpc,[pc4,{network_feedback,self(),Keep_List}]),[];
                    %%      gen_server:cast(PC_PID,{network_feedback,self(),Keep_List}),[];
                        _ -> All_Fitness_lists
  end,

  {noreply, State#learningFSM_state{fitness_list = New_Fitness_list}};

handle_cast({update_generation,From,List_of_gen},State) ->
  Map_ets=State#learningFSM_state.ets_maps,
  Gen_ets = maps:get(From,Map_ets),
  ets:delete_all_objects(Gen_ets),
  % clear the old ets and replace whit a new type of gens.
  update_ets(Gen_ets,List_of_gen),
  % send list of gens ti the neighbours.
  %send_to_neighbours(From,List_of_gen,State#learningFSM_state.pc_names),
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
top_genotypes(FitList,Number_of_networks,Map_ets)->
  SortedFitList=lists:sort(fun({KeyA,ValA}, {KeyB,ValB}) -> {ValA,KeyA} >= {ValB,KeyB} end, FitList),
  make_keep_List(SortedFitList,length(SortedFitList),[],Number_of_networks,Map_ets).

%%make_keep_kill_List(_ ,0 ,List) -> List;
%%make_keep_kill_List([{PID,_}|T], N ,List) when N =< 75 -> make_keep_kill_List(T, N-1 ,List ++[{PID,0,false}]);
%%make_keep_kill_List([{PID,_}|T], N ,List) when N > 75 -> make_keep_kill_List(T, N-1 ,List ++[{PID,4,true}]).%TODO: CHANGE TO BACK RECURSION

make_keep_List(_ ,0 ,List,_Number_of_networks,_Map_ets) -> List;
make_keep_List([{_PID,_}|T], N ,List,Number_of_networks,Map_ets) when N =< round((?DIVIDE_BY-1)*Number_of_networks/?DIVIDE_BY) -> make_keep_List(T, N-1 ,List,Number_of_networks,Map_ets);
make_keep_List([{PID,_}|T], N ,List,Number_of_networks,Map_ets) when N > round((?DIVIDE_BY-1)*Number_of_networks/?DIVIDE_BY) -> make_keep_List(T, N-1 ,List ++[{PID,true,ets_lookup(PID,Map_ets)}],Number_of_networks,Map_ets).%TODO: CHANGE TO BACK RECURSION

% get ets and list and enter all the element from the list to the ets.
update_ets(_,[]) -> ok;
update_ets(Gen_ets,[{Key,Value}|T]) -> ets:insert(Gen_ets,{Key,Value}),
  update_ets(Gen_ets,T).

% send all the neighbours the new gens.
send_to_neighbours(From,List_of_gen,Pc_names)->
  [ rpc:call(pid_to_name(Pc),pc_server,pc_rpc,[Pc,{neighbor_ets_update,self(),From,List_of_gen}])|| Pc<-Pc_names,Pc =/= From]. %gen_server:cast({global,Pc},{neighbor_ets_update,self(),List_of_gen})


create_ets_map([],_,Map)->Map;
create_ets_map([H|T],Name_to_atom,Map)->io:format("name of new Ets ~p ~n",[maps:get(H,Name_to_atom)]), Pc_ets = ets:new(maps:get(H,Name_to_atom),[set,public]),
  New_map=maps:put(H,Pc_ets,Map), create_ets_map(T,Name_to_atom,New_map).

lfsm_rpc(Message)->
  gen_server:cast(learningFSM,Message).

pid_to_name(PID) -> case PID of
                      pc1 -> ?PC1;
                      pc2 -> ?PC2;
                      pc3 -> ?PC3;
                      pc4 -> ?PC4;
                      _ -> exit("wrong pc name!")
                    end.

ets_lookup(Network_PID,Neighbours_map_ets) ->
  Ets_list = maps:to_list(Neighbours_map_ets),
  {Key,ETS}=hd(Ets_list),
  case ets:lookup(ETS,Network_PID) of
    []-> ets_lookup(Network_PID,maps:remove(Key,Neighbours_map_ets));
    [{_Key,Gen}] -> {Key,Gen};
    G -> exit(lists:flatten(io_lib:format("The gen is not in neighbours ets ! ~p~n",[G])))
  end .