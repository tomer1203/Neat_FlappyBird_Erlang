%%%-------------------------------------------------------------------
%%% @author Omri, Tomer
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Aug 2021 19:55
%%%-------------------------------------------------------------------
-module(learningFSM).
-author("Omri, Tomer").

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

-record(learningFSM_state, {number_of_pc = 4,ets_maps=#{},pc_names=[],fitness_list =[],number_of_nn = 100, reboot=[], checkpoint = #{}, run_time_state= wait, pips = []}).

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

%% get list of pids!
init([Number_of_Pcs, Pc_Names, Name_to_atom,Number_of_networks]) ->
  put(?PC1,?PC1),
  put(?PC2,?PC2),
  put(?PC3,?PC3),
  put(?PC4,?PC4),
  process_flag(trap_exit, false),
  net_kernel:monitor_nodes(true),
  timer:sleep(250),
  net_kernel:connect_node(?PC1),
  timer:sleep(250),
  net_kernel:connect_node(?PC2),
  timer:sleep(250),
  net_kernel:connect_node(?PC3),
  timer:sleep(250),
  net_kernel:connect_node(?PC4),
  timer:sleep(250),
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

% each pc sends one network evaluation message which is the accumulated messages from all the networks of that pc.
% when the learning fsm gets all of it's messages it sends back the feedback to the networks
handle_cast({network_evaluation,PC_PID,Fitness_list},
    State = #learningFSM_state{fitness_list = Acc_Fitness_list,reboot = Reboot,number_of_nn = Number_of_nn,
      checkpoint = Map,run_time_state =Run_time_state, number_of_pc = Number_of_pc }) ->
  NewMap=case maps:is_key(PC_PID,Map) of
           true -> Map;
           false -> maps:put(PC_PID,true,Map)
         end,
  All_Fitness_lists = lists:append(Fitness_list,Acc_Fitness_list),
  case maps:size(NewMap) of
    Number_of_pc ->
                case Reboot of
                  []->
                    Keep_List=top_genotypes(All_Fitness_lists,Number_of_nn,State#learningFSM_state.ets_maps),
                    rpc:call(get(?PC1),pc_server,pc_rpc,[pc1,{network_feedback,self(),Keep_List}]),
                    rpc:call(get(?PC2),pc_server,pc_rpc,[pc2,{network_feedback,self(),Keep_List}]),
                    rpc:call(get(?PC3),pc_server,pc_rpc,[pc3,{network_feedback,self(),Keep_List}]),
                    rpc:call(get(?PC4),pc_server,pc_rpc,[pc4,{network_feedback,self(),Keep_List}]),
                    {noreply, State#learningFSM_state{fitness_list = [],checkpoint = #{}}};

                  _ -> case Run_time_state of
                         run_generation ->
                                 Pcs =[?PC1,?PC2,?PC3,?PC4],
                                  keep_and_start(Pcs,State#learningFSM_state.pips,Number_of_nn,Reboot),
                                  {noreply, State#learningFSM_state{reboot=[],fitness_list = [],checkpoint = #{},run_time_state = wait}};% not empty
                         _ ->{noreply, State#learningFSM_state{fitness_list = [],checkpoint = #{},run_time_state = network_evaluation}}
                       end
                end;
      _->   {noreply, State#learningFSM_state{fitness_list = All_Fitness_lists, checkpoint = NewMap}}
  end;

% update the learning fsm ets tables
handle_cast({update_generation,From,List_of_gen},State) ->
  Map_ets=State#learningFSM_state.ets_maps,
  Gen_ets = maps:get(From,Map_ets),
  ets:delete_all_objects(Gen_ets),
  % clear the old ets and replace whit a new type of gens.
  update_ets(Gen_ets,List_of_gen),
  % send list of gens ti the neighbours.
  {noreply, State};

% when there is a node down we reach this function eventually which is responsible
% to restarting only one pc process(meaning that it might get called multiple times).
% it also doen't restart the pc immediatly but rather sets up the restart to happen when
% all other simulations are finished
handle_cast({restart_pc,PC_down}, State = #learningFSM_state{checkpoint = Map}) ->
  Gen_map = State#learningFSM_state.ets_maps,
  {Name,Number} = case PC_down of
    ?PC1 -> {pc1,1};
    ?PC2 -> {pc2,2};
    ?PC3 -> {pc3,3};
    ?PC4 -> {pc4,4}
  end,
  ETS=maps:get(Name,Gen_map),
  Flat_ets =ets:tab2list(ETS),
  GenList=[FLat_gen||{_key,FLat_gen} <- Flat_ets],
  SizeMap = maps:size(Map),
  if
    SizeMap > 2 -> gen_server:cast(learningFSM,{network_evaluation,name_to_sname(PC_down),[]}),
      {noreply, State#learningFSM_state{reboot =[{PC_down,Name,Number,GenList}|State#learningFSM_state.reboot]}};
    true -> NewMap=maps:put(name_to_sname(PC_down),reboot,Map),
      {noreply, State#learningFSM_state{checkpoint = NewMap,reboot =[{PC_down,Name,Number,GenList}|State#learningFSM_state.reboot]}}
  end;


% a message from the graphics that tells that the graphics finished the current generations display.
% this opens up the possibilty to restart if also the learning fsm is ready
handle_cast({run_generation,_From,PipeList}, State = #learningFSM_state{reboot = Reboot,number_of_nn = Number_of_nn, run_time_state =Run_time_state}) ->
  Pcs =[?PC1,?PC2,?PC3,?PC4],
  case Reboot of
    []  ->  {noreply, State};
    _   ->
      if
        Run_time_state =:= network_evaluation ->
          keep_and_start(Pcs,PipeList,Number_of_nn,Reboot),
          {noreply, State#learningFSM_state{reboot=[], run_time_state = wait}};
        true -> {noreply, State#learningFSM_state{pips =PipeList, run_time_state = run_generation}}
      end
  end;


handle_cast(_Request, State = #learningFSM_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #learningFSM_state{}) ->
  {noreply, NewState :: #learningFSM_state{}} |
  {noreply, NewState :: #learningFSM_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #learningFSM_state{}}).

% all good
handle_info({nodeup,_PC},State)->
  {noreply, State};

% all bad..
% this is the first function in a long series called when a pc is down
% it finds which pc is going to take the responsibilty of the fallen pc and then gives responsibilty to the next functions
handle_info({nodedown,PC},State)-> % if a node is down, check which PC, move responsibilities to different PC and update monitors
  Connection_Map = #{?PC2=>?PC3,?PC3=>?PC4,?PC4=>?PC1},
  io:format("node down ~p --> ~p ---> ~p ~n",[PC, maps:get(PC,Connection_Map),get(maps:get(PC,Connection_Map))]),
  find_and_replace_pc_name([{?PC1,get(?PC1)},{?PC2,get(?PC2)},{?PC3,get(?PC3)},{?PC4,get(?PC4)}],PC,get(maps:get(PC,Connection_Map))),
  {noreply, State};
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

% create a list of only the genes to save
make_keep_List(_ ,0 ,List,_Number_of_networks,_Map_ets) -> List;
make_keep_List([{_PID,_}|T], N ,List,Number_of_networks,Map_ets) when N =< round((?DIVIDE_BY-1)*Number_of_networks/?DIVIDE_BY) -> make_keep_List(T, N-1 ,List,Number_of_networks,Map_ets);
make_keep_List([{PID,_}|T], N ,List,Number_of_networks,Map_ets) when N > round((?DIVIDE_BY-1)*Number_of_networks/?DIVIDE_BY) -> make_keep_List(T, N-1 ,List ++[{PID,true,ets_lookup(PID,Map_ets)}],Number_of_networks,Map_ets).

% get ets and list and enter all the element from the list to the ets.
update_ets(_,[]) -> ok;
update_ets(Gen_ets,[{Key,Value}|T]) -> ets:insert(Gen_ets,{Key,Value}),
  update_ets(Gen_ets,T).

create_ets_map([],_,Map)->Map;
create_ets_map([H|T],Name_to_atom,Map)->Pc_ets = ets:new(maps:get(H,Name_to_atom),[set,public]),
  New_map=maps:put(H,Pc_ets,Map), create_ets_map(T,Name_to_atom,New_map).

% RPC
lfsm_rpc(Message)->
  gen_server:cast(learningFSM,Message).

% smart ets lookup which also searches in the neighbors ets
ets_lookup(Network_PID,Neighbours_map_ets) ->
  Ets_list = maps:to_list(Neighbours_map_ets),
  {Key,ETS}=hd(Ets_list),
  case ets:lookup(ETS,Network_PID) of
    []-> ets_lookup(Network_PID,maps:remove(Key,Neighbours_map_ets));
    [{_Key,Gen}] -> {Key,Gen};
    G -> exit(lists:flatten(io_lib:format("The gen is not in neighbours ets ! ~p~n",[G])))
  end .

% find the next open pc to take the load of the fallen pc
find_and_replace_pc_name([],_Closed_Pc,_New_Pc)->ok;
find_and_replace_pc_name([{PcKey,PcNode}|T],Closed_Pc,NewPc)->
  case PcNode of
    Closed_Pc ->
      put(PcKey, NewPc),
      graphics!{nodedown,Closed_Pc,NewPc},
      gen_server:cast(learningFSM,{restart_pc, PcKey}),
      find_and_replace_pc_name(T,Closed_Pc,NewPc);
    _-> % pc is still up
      find_and_replace_pc_name(T,Closed_Pc,NewPc)
  end.

% yep. pretty self explanatory
name_to_sname(Pc) ->
  case Pc of
    ?PC1 -> pc1;
    ?PC2 -> pc2;
    ?PC3 -> pc3;
    ?PC4 -> pc4
  end.

% send a keep(start next simulation without killing anyone) to all existing pc and a start message to new pc
% used to safely transition between two generations in the event of a pc nodedown
keep_and_start([],_PipeList,_Number_of_nn,[]) -> ok;
keep_and_start([Pc|T],PipeList,Number_of_nn,[])->
  rpc:cast(Pc,pc_server,pc_rpc,[name_to_sname(Pc),{network_keep,self()}]),
  keep_and_start(T,PipeList,Number_of_nn,[]);

keep_and_start(Pcs,PipeList,Number_of_nn,[H|T])->
  {PC_down,Name,Number,GenList}=H,
  rpc:call(get(PC_down),pc_server,start,[Name,Number,self(),round(Number_of_nn/4),?NUMBER_OF_LAYERS,?NUMBER_OF_NEURONS_PER_LAYER,?PC_LIST,graphics:generate_map(pc1_,?PC_LIST,?ETS_NAME_LIST),GenList]),
  rpc:call(get(PC_down), pc_server,pc_rpc,[Name,{start_simulation,self(),PipeList}]),
  keep_and_start([Pc||Pc<-Pcs, Pc =/= H],PipeList,Number_of_nn,T).
