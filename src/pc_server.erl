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
-export([start_link/6,start/6]).

-export([nn_monitor/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
code_change/3]).
-define(SERVER, ?MODULE).


%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(Name::atom(),Pc_num::integer(), Learning_pid::pid(), Number_of_networks ::integer(),Num_Layers::integer(),Num_Neurons_Per_Layer::integer()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Name,Pc_num, Learning_pid, Number_of_networks,Num_Layers,Num_Neurons_Per_Layer) ->
  gen_server:start_link({local, Name}, ?MODULE, [Pc_num, Learning_pid, Number_of_networks,Num_Layers,Num_Neurons_Per_Layer], []).

-spec(start(Name::atom(),Pc_num :: integer(),Learning_pid::pid(), Number_of_networks ::integer(),Num_Layers::integer(),Num_Neurons_Per_Layer::integer()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start(Name,Pc_num,Learning_pid,Number_of_networks,Num_Layers,Num_Neurons_Per_Layer) ->
  gen_server:start({local, Name}, ?MODULE, [Pc_num,Learning_pid,Number_of_networks,Num_Layers,Num_Neurons_Per_Layer], []).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(N :: integer()) ->
  {ok, State :: #pc_server_state{}} | {ok, State :: #pc_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Pc_num,Learning_pid,Number_of_networks,Num_Layers,Num_Neurons_Per_Layer]) ->
  %PID_genotype_map =#{},
  Networks = construct_networks(Pc_num,Number_of_networks,Num_Layers,Num_Neurons_Per_Layer),
  %[maps:put(G,spawn_monitor(neuralNetwork:start_link()),PID_genotype_map)  || G <-Genotype_list],
  %ets:insert(tableEx9, {K,V})
  Gen_ets = ets:new(gen_ets,[set]),
  Fitness_ets = ets:new(fitness_ets,[set]),
  [ets:insert(Gen_ets,{Pid,Graph})||{Pid,Graph}<-Networks],
  io:format("Number of networks ~p~n",[Number_of_networks]),
  {ok, #pc_server_state{pc_num = Pc_num, learning_pid = Learning_pid,
    number_of_networks = Number_of_networks,gen_ets = Gen_ets,
    fitness_ets = Fitness_ets, remaining_networks = Number_of_networks}}.

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
  start_networks(First_key,Pipe_list,Gen_ets),
  {noreply, State};

% A message from one of the neural networks to the Pc updating it how the simulation went
handle_cast({finished_simulation,From,Time}, State = #pc_server_state{remaining_networks = 0})->
  exit("Pc exiting received too many finished simulation messages");
handle_cast({finished_simulation,From,Time}, State = #pc_server_state{remaining_networks = 1,fitness_ets = Fitness_ets})->
  ets:insert(Fitness_ets,{From,Time}),
  io:format("remaining networks ~p~n",[1]),

  %TODO: Send fitness scores to learning fsm(either through messages or through ets)
  %io:format("Fitness ETS: ~p~n length of list:~p~n",[ets:tab2list(Fitness_ets),length(ets:tab2list(Fitness_ets))]),
  gen_server:cast(State#pc_server_state.learning_pid,{network_evaluation,self(),ets:tab2list(Fitness_ets)}),
  {noreply, State#pc_server_state{remaining_networks = 0}};
handle_cast({finished_simulation,From,Time}, State = #pc_server_state{remaining_networks = Remaining_networks,fitness_ets = Fitness_ets})->
  ets:insert(Fitness_ets,{From,Time}),
  %io:format("remaining networks ~p~n number of networks~p~n",[Remaining_networks,State#pc_server_state.number_of_networks]),
  {noreply, State#pc_server_state{remaining_networks = Remaining_networks-1}};
% from learning fsm
handle_cast({network_feedback,From,TopGens}, State = #pc_server_state{gen_ets = Gen_ets})when From =:= State#pc_server_state.learning_pid->
  % get list of all available networks(the ones that were killed), a list of the networks to keep and a list of the genotypes to mutate

  [{Pid,_,_Fit}|_T] = TopGens,
  [{_K,G}] = ets:lookup(Gen_ets,Pid),
  io:format("printing graph~n"),
  plot_graph:to_file(G,"graph.png","png"),
  {KeepList,KillList,MutateList}=parseKeepList(Gen_ets,TopGens),
  %io:format("got feedback from learning fsm:~p~nlength of list~p~n",[TopGens,length(TopGens)]),
  case State#pc_server_state.generation of
    graphics->
      sendKeepAndKillMessage(KeepList,KillList,State#pc_server_state.pipe_list,Gen_ets),
      mutate_and_restart_networks(KillList,MutateList,State#pc_server_state.gen_ets,State#pc_server_state.pipe_list),
      NewState=State#pc_server_state{generation=wait};
      _ -> NewState=State#pc_server_state{generation=mutation, keep_list = {KeepList,KillList,MutateList}} % TODO- function start simulation, mutate, update ets ! !
  end,{noreply, NewState};

% from graphics
handle_cast({run_generation,From, Pipe_list}, State)->

  io:format("got feedback from graphics~p~n",[State#pc_server_state.number_of_networks]),

  case State#pc_server_state.generation of
      mutation->
        {KeepList,KillList,MutateList}=State#pc_server_state.keep_list,
        sendKeepAndKillMessage(KeepList,KillList,Pipe_list,State#pc_server_state.gen_ets),
        %io:format("sending to mutate and restart: kill list= ~p~nMutateList=~p~n",[length(KillList),length(MutateList)]),
        mutate_and_restart_networks(KillList,MutateList,State#pc_server_state.gen_ets,Pipe_list),
        NewState=State#pc_server_state{generation=wait,remaining_networks = State#pc_server_state.number_of_networks};
      _ -> NewState=State#pc_server_state{generation=graphics, pipe_list = Pipe_list,remaining_networks = State#pc_server_state.number_of_networks}
  end,{noreply, NewState};

handle_cast(_Request, State = #pc_server_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #pc_server_state{}) ->
  {noreply, NewState :: #pc_server_state{}} |
  {noreply, NewState :: #pc_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #pc_server_state{}}).
% A debug message, it is ignored for now
handle_info({finished_constructing,From}, State = #pc_server_state{})-> {noreply,State};
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
construct_networks(Pc_num,N,Num_Layers,Num_Neurons_Per_Layer)->
  construct_networks(Pc_num,N,self(),Num_Layers,Num_Neurons_Per_Layer,[]).

construct_networks(_Pc_num,0,_Self,_Num_Layers,_Num_Neurons_Per_Layer,Acc)-> Acc;
construct_networks(Pc_num,N,Self,Num_Layers,Num_Neurons_Per_Layer,Acc)->
  G = genotype:test_Genotype(Num_Layers,Num_Neurons_Per_Layer),
  {ok,Proc} = neuralNetwork:start(get_nn_name(Pc_num,N),Self),
  spawn(?MODULE, nn_monitor, [Proc]),
  construct_networks(Pc_num,N-1,Self,Num_Layers,Num_Neurons_Per_Layer,[{Proc,G}|Acc]).

% monitors a neural network and waits checks if it is down.
nn_monitor(Proc) ->
  erlang:monitor(process,Proc),
  receive
    {'DOWN', Ref, process, Pid,  normal} ->
      io:format("~p said that ~p died by natural causes~n",[Ref,Pid]);%TODO: if network is down we need to restart it!
    {'DOWN', Ref, process, Pid,  Reason} ->
      io:format("~p said that ~p died by unnatural causes~n~p",[Ref,Pid,Reason]) %TODO: if network is down we need to restart it!
  end.

% opens up the networks
start_networks('$end_of_table',_Pipes,_Gen_ets)->ok;
start_networks(Key,Pipes,Gen_ets)->
  [{Key,G}]=ets:lookup(Gen_ets,Key),
  % TODO: last value is sub2 graphics! this needs to change to false eventually and it is currently true for debugging
  gen_statem:cast(Key,{start_simulation,self(),G,Pipes,true}),
  Next_key = ets:next(Gen_ets,Key),
  start_networks(Next_key,Pipes,Gen_ets).

get_nn_name(Pc_num,N)->list_to_atom(lists:append("nn_",integer_to_list(Pc_num*100+N))).
%put_and_send(G,PID,Map,Pipe_list) ->ok.

% turn the keep list into a list of the networks to kill the networks to keep and which genotypes
% to mutate and how many times. the sum of the mutate list should be equal to the dead networks.
parseKeepList(Gen_ets,KeepList)->parseKeepList(Gen_ets,KeepList,[],[],[]).
parseKeepList(_Gen_ets,[],KeepAcc,KillAcc,MutateAcc)->{KeepAcc,KillAcc,MutateAcc};
parseKeepList(Gen_ets,[{NetworkPid,Keep,Subscribe}|KeepList],KeepAcc,KillAcc,MutateAcc)->
  case Keep of
    0 -> % kill network
      parseKeepList(Gen_ets,KeepList,KeepAcc,[NetworkPid|KillAcc],MutateAcc);
    1 -> % keep but don't mutate
      parseKeepList(Gen_ets,KeepList,[{NetworkPid,Subscribe}|KeepAcc],KillAcc,MutateAcc);
    N -> % keep and mutate N-1 times(keep once)
      case ets:lookup(Gen_ets,NetworkPid) of
          []-> io:format("ETS:LOOKUP FAILED: NetworkPid= ~p, ETS length=~p,~nETS=~p~n",[NetworkPid,length(ets:tab2list(Gen_ets)), ets:tab2list(Gen_ets)]);
          _->ok
      end,

      [{Key,Gen}] = ets:lookup(Gen_ets,NetworkPid),
      parseKeepList(Gen_ets,KeepList,[{NetworkPid,Subscribe}|KeepAcc],KillAcc,[{Gen,N-1}|MutateAcc])
  end.

mutate_and_restart_networks([],[],_Gen_ets,_PipeList)->ok;
mutate_and_restart_networks(KillList,[{_Gen,0}|MutateList],Gen_ets,PipeList)->mutate_and_restart_networks(KillList,MutateList,Gen_ets,PipeList);
mutate_and_restart_networks([],MutateList,Gen_ets,_PipeList)->
  exit(lists:flatten(io_lib:format("there are more mutations than networks to put them in ~p~p", [MutateList,Gen_ets])));
mutate_and_restart_networks(KillList,[],Gen_ets,PipeList)->exit("there are more dead networks than mutations to put in them");
mutate_and_restart_networks([NetworkPid|KillList],[{Gen,N}|MutateList],Gen_ets,PipeList)->
  assignGenToNetwork(NetworkPid,Gen,Gen_ets,PipeList),
  mutate_and_restart_networks(KillList,[{Gen,N-1}|MutateList],Gen_ets,PipeList).

assignGenToNetwork(NetworkPid,Gen,Gen_ets,PipeList)->
  % Copy gen
  Copy_genotype = digraph_utils:subgraph(Gen,digraph:vertices(Gen)),
  genotype:mutator(Copy_genotype,round(abs(rand:normal())*15)),
  % the genotype is inserted as is and when it is mutated in the network it will automatically change since this is an ets.
  % TODO: Notice that you can't send the ets to neighbors immediatly since we first need to let the network mutate it.
  ets:insert(Gen_ets,{NetworkPid, Copy_genotype}),
  % TODO: the last value is Sub2graphics and it is currently true only for debugging and needs to be changed in the future..
  gen_statem:cast(NetworkPid,{start_simulation,self(),Copy_genotype,PipeList,true}),
  ok.
sendKeepAndKillMessage(KeepList,KillList,PipeList,Gen_Ets)->
  % send a keep message to all the keep networks
  [gen_statem:cast(NetworkPid,{keep,self(),PipeList,Subscribe})||{NetworkPid,Subscribe}<-KeepList],
  % send a kill message to all the kill networks
  [kill_network_and_gen(NetworkPid,Gen_Ets)||NetworkPid<-KillList].

% kills the network and deletes the gen from the ets(the new gen will be inserted when we restart the network)
kill_network_and_gen(NetworkPid,Gen_Ets)->
  ets:delete(Gen_Ets,NetworkPid),
  gen_statem:cast(NetworkPid,{kill,self()}).