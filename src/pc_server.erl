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
-include("Constants.hrl").

%% API
-export([start_link/9,start/9]).
-export([pc_rpc/2, serialize/1, deserialize/1]).
-export([nn_monitor/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
code_change/3]).
-define(SERVER, ?MODULE).


%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
%%-spec(start_link(Name::atom(),Pc_num::integer(), Learning_pid::pid(), Number_of_networks ::integer(),Num_Layers::integer(),Num_Neurons_Per_Layer::integer(),Neighbors_Ets_Map::term()) ->
%%  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Name,Pc_num, Learning_pid, Number_of_networks,Num_Layers,Num_Neurons_Per_Layer,Pc_Names, Name_to_atom,DefGenList) ->
  gen_server:start_link({local, Name}, ?MODULE, [Name,Pc_num, Learning_pid, Number_of_networks,Num_Layers,Num_Neurons_Per_Layer,Pc_Names, Name_to_atom,DefGenList], []).

%%-spec(start(Name::atom(),Pc_num :: integer(),Learning_pid::pid(), Number_of_networks ::integer(),Num_Layers::integer(),Num_Neurons_Per_Layer::integer(),Neighbors_Ets_Map::term()) ->
%%  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start(Name,Pc_num,Learning_pid,Number_of_networks,Num_Layers,Num_Neurons_Per_Layer,Pc_Names, Name_to_atom,DefGenList) ->
  gen_server:start({local, Name}, ?MODULE, [Name,Pc_num,Learning_pid,Number_of_networks,Num_Layers,Num_Neurons_Per_Layer,Pc_Names, Name_to_atom,DefGenList], []).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(N :: integer()) ->
  {ok, State :: #pc_server_state{}} | {ok, State :: #pc_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).

init([Name,Pc_num,Learning_pid,Number_of_networks,Num_Layers,Num_Neurons_Per_Layer,Pc_Names, Name_to_atom,DefGenList]) ->
  process_flag(trap_exit, false),
  GenList=case DefGenList of
    [] ->make_gen_list(Number_of_networks,Num_Layers,Num_Neurons_Per_Layer);
    _->[deserialize(Flat_gen) || Flat_gen <- DefGenList]
  end,
  Networks = construct_networks(Name, Pc_num,Number_of_networks,GenList),
  Gen_ets = ets:new(gen_ets,[set]),
  Fitness_ets = ets:new(fitness_ets,[set]),
  [ets:insert(Gen_ets,{Pid,Graph})||{Pid,Graph}<-Networks],
  sync_ets(Gen_ets,Learning_pid,Name),
  Neighbors_map_ets= learningFSM:create_ets_map(Pc_Names,Name_to_atom,#{}),
  % build neighbor ets

  {ok, #pc_server_state{name = Name,pc_num = Pc_num, learning_pid = Learning_pid,
    number_of_networks = Number_of_networks,gen_ets = Gen_ets,
    fitness_ets = Fitness_ets, remaining_networks = Number_of_networks,neighbours_map_ets = Neighbors_map_ets}}.
%% Neighbors_Ets_Map
%% Pids -> empty ets
%%
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

handle_cast({start_simulation,_From,Pipe_list}, State = #pc_server_state{gen_ets = Gen_ets})->
  First_key = ets:first(Gen_ets),
  start_networks(First_key,Pipe_list,Gen_ets,State#pc_server_state.name),
  {noreply, State#pc_server_state{pipe_list = Pipe_list}};

% A message from one of the neural networks to the Pc updating it how the simulation went
handle_cast({finished_simulation,_From,_Time}, _State = #pc_server_state{remaining_networks = 0})->
  exit("Pc exiting received too many finished simulation messages");
handle_cast({finished_simulation,From,Time}, State = #pc_server_state{remaining_networks = 1,fitness_ets = Fitness_ets})->
  ets:insert(Fitness_ets,{From,Time}),
  rpc:call(?GRAPHICS_NODE,learningFSM,lfsm_rpc,[{network_evaluation,State#pc_server_state.name,ets:tab2list(Fitness_ets)}]),
  {noreply, State#pc_server_state{remaining_networks = 0}};
handle_cast({finished_simulation,From,Time}, State = #pc_server_state{remaining_networks = Remaining_networks,fitness_ets = Fitness_ets})->
  ets:insert(Fitness_ets,{From,Time}),
  {noreply, State#pc_server_state{remaining_networks = Remaining_networks-1}};

handle_cast({neighbor_ets_update,_From,Neighbor,List_of_gen}, State = #pc_server_state{})->
  spawn(fun()->update_ets(List_of_gen,Neighbor,State#pc_server_state.neighbours_map_ets)end),
  {noreply,State};


% from learning fsm
handle_cast({network_keep,_From}, State = #pc_server_state{gen_ets = Gen_ets, pipe_list=Pips,learning_pid = Learning_FSM_Pid,name = Pc_Name})->
  Keep_PID_list = [Pid||{Pid,_G} <- ets:tab2list(Gen_ets)],
  send_keep(Keep_PID_list,Pips),
  spawn(fun()->sync_ets(Gen_ets,Learning_FSM_Pid,Pc_Name)end),
  % send message finish
  {noreply, State#pc_server_state{generation=wait,remaining_networks = State#pc_server_state.number_of_networks}};

handle_cast({network_feedback,From,TopGens}, State = #pc_server_state{name = Pc_Name,gen_ets = Gen_ets,learning_pid = Learning_FSM_Pid})when From =:= State#pc_server_state.learning_pid->
  % get list of all available networks(the ones that were killed), a list of the networks to keep and a list of the genotypes to mutate
  {Keep_map,Mutate_list} = parseKeepList(Gen_ets,State#pc_server_state.neighbours_map_ets,TopGens),
  {Kill_PID_List,Keep_PID_list}=mutate_and_restart_networks(Pc_Name,Keep_map,Mutate_list,Gen_ets,State#pc_server_state.pipe_list),
  spawn(fun()->sync_ets(Gen_ets,Learning_FSM_Pid,Pc_Name)end),
  case State#pc_server_state.generation of
    graphics->
      send_keep(Keep_PID_list,State#pc_server_state.pipe_list),
      start_simulation_kill(Kill_PID_List,State#pc_server_state.pipe_list),
      NewState=State#pc_server_state{generation=wait};
      _ -> NewState=State#pc_server_state{generation=mutation, keep_list = {Keep_PID_list,Kill_PID_List}}
  end,{noreply, NewState};

% from graphics
handle_cast({run_generation,_From, Pipe_list}, State)->
  case State#pc_server_state.generation of
      mutation->
        {Keep_PID_list,Kill_PID_List}=State#pc_server_state.keep_list,
        send_keep(Keep_PID_list,Pipe_list),
        start_simulation_kill(Kill_PID_List,Pipe_list),
        NewState=State#pc_server_state{generation=wait,remaining_networks = State#pc_server_state.number_of_networks,pipe_list = Pipe_list};
      _ -> NewState=State#pc_server_state{generation=graphics, pipe_list = Pipe_list,remaining_networks = State#pc_server_state.number_of_networks}
  end,{noreply, NewState};

handle_cast({network_down,_,_Nn_Pid},State = #pc_server_state{gen_ets = _Gen_ets,fitness_ets = _Fitness_ETS,pipe_list = _Pipes,name = _Name})->
%%  io:format("network_down recognized starting restart~n"),
%%  [{_Key,Gen}] = ets:lookup(Gen_ets,Nn_Pid),
%%  ets:delete(Gen_ets,Nn_Pid),
%%  ets:delete(Fitness_ETS,Nn_Pid),
%%  Result = ets:lookup(Gen_ets,Nn_Pid),
%%  io:format("Result of delete ~p ~n",[Result]),
%%  Self = self(),
%%  {ok,New_Nn_pid} = neuralNetwork:start(get_nn_name(Name,erlang:unique_integer()),Self),
%%  spawn(?MODULE, nn_monitor, [New_Nn_pid,Self]),
%%  ets:insert(Gen_ets,{New_Nn_pid,Gen}),
%%  ets:insert(Fitness_ETS,{New_Nn_pid,0}),
%%  gen_statem:cast(New_Nn_pid,{start_simulation,self(),Gen,Pipes,true}),
%%  io:format("network restarted~n"),
  {noreply, State};


handle_cast(_Request, State = #pc_server_state{}) ->io:format("oh no pc message does not fit ~p ~n",[_Request]),
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #pc_server_state{}) ->
  {noreply, NewState :: #pc_server_state{}} |
  {noreply, NewState :: #pc_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #pc_server_state{}}).
% A debug message, it is ignored for now
handle_info({finished_constructing,_From}, State = #pc_server_state{})-> {noreply,State};
handle_info(_Info, State = #pc_server_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #pc_server_state{}) -> term()).
terminate(Reason, _State = #pc_server_state{gen_ets = Gen_ETS}) ->
  io:format("closing Pc ~p Server for reason ~p~n",[Reason,_State#pc_server_state.name]),
  Gen_list = ets:tab2list(Gen_ETS),
  [gen_statem:stop(Network_pid)||{Network_pid,_Gen}<-Gen_list].

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
construct_networks(PC_Pid,Pc_num,N,_GenList)->
  construct_networks(PC_Pid,Pc_num,N,self(),_GenList,[]).

construct_networks(_PC_Pid,_Pc_num,0,_Self,_GenList,Acc)-> Acc;
construct_networks(PC_Pid,Pc_num,N,Self,[Gen|T],Acc)->
  {ok,Proc} = neuralNetwork:start(get_nn_name(PC_Pid,N),Self),
  spawn(?MODULE, nn_monitor, [Proc,PC_Pid]),
  construct_networks(PC_Pid,Pc_num,N-1,Self,T,[{Proc,Gen}|Acc]).

% monitors a neural network and waits checks if it is down.
nn_monitor(Proc,PC_pid) ->
  erlang:monitor(process,Proc),
  receive
    {'DOWN', Ref, process, Pid,  normal} ->
      io:format("~p said that ~p died by natural causes~n",[Ref,Pid]),
      gen_server:cast(PC_pid,{network_down,self(),Pid});

    {'DOWN', Ref, process, Pid,  Reason} ->
      io:format("~p said that ~p died by unnatural causes~n~p",[Ref,Pid,Reason]),
      gen_server:cast(PC_pid,{network_down,self(),Pid})
  end.

% opens up the networks
start_networks('$end_of_table',_Pipes,_Gen_ets,_PC_pid)->ok;
start_networks(Key,Pipes,Gen_ets,PC_pid)->
  [{Key,G}]=ets:lookup(Gen_ets,Key),
  case PC_pid of
  pc1->gen_statem:cast(Key,{construct_simulation,self(),G,true});
    _->gen_statem:cast(Key,{construct_simulation,self(),G,false})
  end,
  gen_statem:cast(Key,{start_simulation,self(),Pipes}),
  Next_key = ets:next(Gen_ets,Key),
  start_networks(Next_key,Pipes,Gen_ets,PC_pid).

get_nn_name(PC_Name,N)->list_to_atom(lists:append("nn_",lists:append(atom_to_list(PC_Name), integer_to_list(N)))).

% turn the keep list into a list of the networks to kill the networks to keep and which genotypes
% to mutate and how many times. the sum of the mutate list should be equal to the dead networks.
parseKeepList(Gen_ets,Ets_map_neighbours,KeepList)->parseKeepList(Gen_ets,Ets_map_neighbours,KeepList,#{},[]).
parseKeepList(_Gen_ets,_Ets_map_neighbours,[],KeepAcc,MutateAcc)->{KeepAcc,MutateAcc};
parseKeepList(Gen_ets,Ets_map_neighbours,[{NetworkPid,_Subscribe,{_To,G}}|KeepList],KeepAcc,MutateAcc)->
  case ets:lookup(Gen_ets,NetworkPid) of
    [{Key,Gen}] -> % keep but don't mutate
      parseKeepList(Gen_ets,Ets_map_neighbours,KeepList,maps:put(Key,Gen,KeepAcc),MutateAcc);
    [] -> parseKeepList(Gen_ets,Ets_map_neighbours,KeepList,KeepAcc,[deserialize(G)|MutateAcc])  ;% keep and mutate N-1 times(keep once)
    T -> exit(lists:flatten(io_lib:format("No mach! ! ~p~n",[T])))
    end.


mutate_and_restart_networks(PC_pid,KeepMap,MutateList,Gen_ets,_PipeList) -> mutate_and_restart_networks(PC_pid,KeepMap,MutateList,Gen_ets,ets:tab2list(Gen_ets),_PipeList,[],[]).
mutate_and_restart_networks(_PC_pid,_KeepMap,MutateList,_Gen_ets,[],_PipeList,Kill_PID_List,Keep_PID_list) -> case MutateList of
                                                                          [] -> {Kill_PID_List,Keep_PID_list};
                                                                          _ -> exit("mutate list is not empty")
                                                                        end;
mutate_and_restart_networks(PC_pid,KeepMap,MutateList,Gen_ets, [{Key,_Genotype}|T_ets],_PipeList,Kill_List,Keep_list) ->
  case maps:is_key(Key,KeepMap) of
    false ->
      [G|T_gens] =MutateList,
      ets:delete(Gen_ets,Key),
      assignGenToNetwork(Key,G,Gen_ets,PC_pid),
      mutate_and_restart_networks(PC_pid,KeepMap,T_gens,Gen_ets,T_ets,_PipeList,[Key|Kill_List],Keep_list);
    true ->
      mutate_and_restart_networks(PC_pid,maps:remove(Key,KeepMap),MutateList,Gen_ets,T_ets,_PipeList,Kill_List,[Key|Keep_list])
  end.


assignGenToNetwork(NetworkPid,Gen,Gen_ets,PC_pid)->
  % Copy gen
  Copy_genotype = digraph_utils:subgraph(Gen,digraph:vertices(Gen)),
  genotype:mutator(Copy_genotype,round(abs(rand:normal())*15)),
  % the genotype is inserted as is and when it is mutated in the network it will automatically change since this is an ets.
  ets:insert(Gen_ets,{NetworkPid, Copy_genotype}),
  gen_statem:cast(NetworkPid,{kill,self()}),
  case PC_pid of
    pc1->gen_statem:cast(NetworkPid,{construct_simulation,self(),Copy_genotype,true});
    _->gen_statem:cast(NetworkPid,{construct_simulation,self(),Copy_genotype,false})
  end,
  ok.

% send a keep message to all the keep networks
send_keep(KeepList,PipeList) -> [gen_statem:cast(NetworkPid,{keep,self(),PipeList,true})||NetworkPid<-KeepList].

start_simulation_kill([],_)->ok;
start_simulation_kill([H|T],Pips)->
  gen_statem:cast(H,{start_simulation,self(),Pips}),
  start_simulation_kill(T,Pips).

sync_ets(Gen_ets,_Learning_FSM_Pid,Pc_pid) ->
  Pid_Gen_List = ets:tab2list(Gen_ets),
  Fully_Serialized_ETS = [{Network_Pid,serialize(G)}||{Network_Pid,G}<-Pid_Gen_List],
  rpc:call(?GRAPHICS_NODE,learningFSM,lfsm_rpc,[{update_generation,Pc_pid,Fully_Serialized_ETS}]).

update_ets(List_of_gen,Neighbor,Neighbors_Ets_map)->
  Gen_ets= maps:get(Neighbor,Neighbors_Ets_map),
  ets:delete_all_objects(Gen_ets),
  [ets:insert(Gen_ets,{NetPid,deserialize(Flat_Gen)}) || {NetPid,Flat_Gen} <- List_of_gen].

serialize({digraph, V, E, N, B}) ->
  {ets:tab2list(V),
    ets:tab2list(E),
    ets:tab2list(N),
    B}.

deserialize({VL, EL, NL, B}) ->
  DG = {digraph, V, E, N, B} = case B of
                                 true -> digraph:new();
                                 false -> digraph:new([acyclic])
                               end,
  ets:delete_all_objects(V),
  ets:delete_all_objects(E),
  ets:delete_all_objects(N),
  ets:insert(V, VL),
  ets:insert(E, EL),
  ets:insert(N, NL),
  DG.

make_gen_list(Number_of_networks,Num_Layers,Num_Neurons_Per_Layer) ->
  make_gen_list(Number_of_networks,Num_Layers,Num_Neurons_Per_Layer,[]).
make_gen_list(0,_Num_Layers,_Num_Neurons_Per_Layer,GenAcc) -> GenAcc;
make_gen_list(Number_of_networks,Num_Layers,Num_Neurons_Per_Layer,GenAcc) ->
  G = genotype:test_Genotype(Num_Layers,Num_Neurons_Per_Layer),
  make_gen_list(Number_of_networks-1,Num_Layers,Num_Neurons_Per_Layer,[G|GenAcc]).

pc_rpc(Pc,Message)->
  gen_server:cast(Pc,Message).