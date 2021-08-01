%%%-------------------------------------------------------------------
%%% @author Omri & Tomer
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21.7 2021 16:54
%%%-------------------------------------------------------------------
-module(neuralNetwork).
-author("Omri & Tomer").

-behaviour(gen_statem).

%% API
-export([start_link/0,idle/3,construct_network/2]).

%% gen_statem callbacks
-export([init/1, format_status/2, state_name/3, handle_event/4, terminate/3,
  code_change/4, callback_mode/0]).

-define(SERVER, ?MODULE).

-record(nn_state, {pcPID,genotype,pipList,actuatorPID,sensorsPIDs,simulation}).
-record(neuron, {type,id=erlang:unique_integer(),layer,af=tanh,bias=rand:uniform()}).
-record(neuron_data,{
  id,                 % the id of the current neuron
  in_pids,            % the pid of all inputs of the current neuron
  out_pids,           % the pid of all outputs of the current neuron
  remaining_in_pids,  % the pids which have not yet sent an input to the neuron
  bias,               % the bias of the neuron calculation
  af,                 % the activation function used in this neuron
  acc=0                % An Accumulator for the neuron
}).
%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_link() ->
  gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init({PC_PID}) ->
  {ok, idle, #nn_state{pcPID = PC_PID}}.

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() ->
  handle_event_function.

%% @private
%% @doc Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
format_status(_Opt, [_PDict, _StateName, _State]) ->
  Status = some_term,
  Status.

%% @private
%% @doc There should be one instance of this function for each possible
%% state name.  If callback_mode is state_functions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
state_name(_EventType, _EventContent, State = #nn_state{}) ->
  NextStateName = next_state,
  {next_state, NextStateName, State}.

idle(cast,{start_simulation,Pc_PID,Genotype,Pipe_list},State) when Pc_PID =:=State#nn_state.pcPID ->
  NextStateName = construction,
  %TODO- mutator.
  Actuator_PID=spawn(fun()->construct_network(Genotype,self())end),
  %TODO- send to pc the new genotype.
  New_stat=State#nn_state{pipList = Pipe_list,genotype = Genotype,actuatorPID=Actuator_PID},
  {next_state, NextStateName, New_stat}.


construction(cast,{finished_constructing,_,SensorsPIDs},State)  ->
  NextStateName = simulation,New_stat =State#nn_state{sensorsPIDs = SensorsPIDs},
  {next_state, NextStateName, New_stat}.


simulation(cast,{start_simulation,Pc_PID},State) when Pc_PID =:= State#nn_state.pcPID -> ok .



%TODO - evaluation state
%create_network(cast,{},State)->


%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
handle_event(_EventType, _EventContent, _StateName, State = #nn_state{}) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, _State = #nn_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #nn_state{}, _Extra) ->
  {ok, StateName, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

% construct the neural network(genotype->phenotype), send to Network message:finished_constructing, then beaver like neuron.
construct_network(G,NnPID)->
  [Actuator]=genotype:get_actuator(G),
  SelfPID=self(),
  NumOfLayer=(Actuator#neuron.layer) - 1,
  IdToPIDs=#{Actuator#neuron.id => SelfPID,nnPID=>NnPID},
  NewIdToPIDs=construct_all_layers(G,NumOfLayer,IdToPIDs),
  SensorsPIDs=get_sensor_PIDs(G,NewIdToPIDs),
  configure_nn(G,NewIdToPIDs),
  In_pids=get_weights_map(G,Actuator,NewIdToPIDs),
  NeuronActuator=#neuron_data{id=SelfPID,in_pids=In_pids,out_pids=[NnPID],remaining_in_pids=In_pids,bias=Actuator#neuron.bias,af=Actuator#neuron.af},
  NnPID ! {finished_constructing,SelfPID,SensorsPIDs},
  neuron:loop(NeuronActuator).

%TODO: number of layer is int ? or not?
% construct all the layer frm the actuator until the sensor return map of ID -> PID.
construct_all_layers(_,-1,IdToPIDs)->IdToPIDs;
construct_all_layers(G,NumOfLayer,IdToPIDs)->
  ListOfNods=genotype:get_layer(G,NumOfLayer),NewIdToPIDs=construct_layer(ListOfNods,IdToPIDs),construct_all_layers(G,NumOfLayer-1,NewIdToPIDs).

%create  layer of process form layer in graph.
construct_layer([],PIDs)->PIDs;
construct_layer([H|T],PIDs) -> NewPIDs= construct_node(H,PIDs),construct_layer(T,NewPIDs).

%construct one node form node in the graph to process,return map of ID -> PID.
construct_node(N,IdToPIDs)->
                           NewPID=spawn_link(fun()->neuron:init()end),
                           NewIdToPIDs=maps:put(N#neuron.id,NewPID,IdToPIDs),NewIdToPIDs.



configure_nn(G,IdToPIDs)-> ListOfNodes=genotype:get_nodes(G),configure_nn(G,IdToPIDs,ListOfNodes).
configure_nn(_,_,[])->ok;
configure_nn(G,IdToPIDs,[H|T])-> if
                                      H#neuron.type=:=sensor ->
                                         NnPid=maps:get(nnPID,IdToPIDs),
                                         OutPIDs=get_out_PIDs(G,H,IdToPIDs),
                                         HeadPid=maps:get(H#neuron.id,IdToPIDs),
                                         Neuron=#neuron_data{id =HeadPid,in_pids=#{NnPid => 1},out_pids=OutPIDs,remaining_in_pids=#{NnPid => 1},bias=H#neuron.bias,af=none},
                                         HeadPid ! {configure_neuron,self(),Neuron},configure_nn(G,IdToPIDs,T);
                                      H#neuron.type=:=actuator->
                                        configure_nn(G,IdToPIDs,T);
                                      true ->
                                        In_pids=get_weights_map(G,H,IdToPIDs),
                                        OutPIDs=get_out_PIDs(G,H,IdToPIDs),
                                        HeadPid=maps:get(H#neuron.id,IdToPIDs),
                                        Neuron=#neuron_data{id =HeadPid,in_pids=In_pids,out_pids=OutPIDs,remaining_in_pids=In_pids,bias=H#neuron.bias,af=H#neuron.af},
                                        HeadPid ! {configure_neuron,self(),Neuron},configure_nn(G,IdToPIDs,T)
                                      end.

%the function get Graph, Node and maps of PIDs, key:ID. return list of all node neighbours PIDs.
get_out_PIDs(G,N,PIDs)->NeighboursOut=digraph:out_neighbours(G,N#neuron.id),get_out_PIDs(G,PIDs,NeighboursOut,[]).
get_out_PIDs(_,_,[],OutPIDs)->OutPIDs;
get_out_PIDs(G,PIDs,[H|T],OutPIDs)->get_out_PIDs(G,PIDs,T,OutPIDs ++[maps:get(H,PIDs)]).

%return list of sensor PIDS.
get_sensor_PIDs(G,IdToPIDs) -> Sensor=genotype:get_sensors(G),get_sensor_PIDs(G,IdToPIDs,Sensor,[]).
get_sensor_PIDs(_,_,[],SensorsPIDs)->SensorsPIDs;
get_sensor_PIDs(G,IdToPIDs,[H|T],SensorsPIDs) ->
  PID=maps:get(H#neuron.id,IdToPIDs),
  get_sensor_PIDs(G,IdToPIDs,T,SensorsPIDs++[PID]).

%return map of all the in edges {from,to} weight, key: from id, value: weight.
get_weights_map(G,Node,IdToPIDs) -> Edges=digraph:in_edges(G,Node#neuron.id),Weights=#{} ,get_weights_map(G,IdToPIDs,Edges,Weights).
get_weights_map(_,_,[],WeightsMap) -> WeightsMap;
get_weights_map(G,IdToPIDs,[H|T],WeightsMap) ->
  {_,Node1ID,_,Weight}= digraph:edge(G,H),case Weight =:= [] of
                                          true->io:format("egde is: ~p~n,Weight: ~p~n",[digraph:edge(G,H),Weight]),erlang:error("invalid Weight");
                                          false->ok
                                        end,PID=maps:get(Node1ID,IdToPIDs),WeightsMap_new= maps:put(PID,Weight,WeightsMap),get_weights_map(G,IdToPIDs,T,WeightsMap_new).