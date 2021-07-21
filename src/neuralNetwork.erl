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
-export([start_link/0,idle/3]).

%% gen_statem callbacks
-export([init/1, format_status/2, state_name/3, handle_event/4, terminate/3,
  code_change/4, callback_mode/0]).

-define(SERVER, ?MODULE).

-record(nn_state, {pcPID,genotype,pipList,actuatorPID,simulation}).
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

idle(cast,{start_simulation,Pc_PID,Genotype,Pipe_list},State)  when Pc_PID =:=State#nn_state.pcPID ->
  NextStateName = construction,Actuator_PID=spawn(fun()->construct_network(Genotype,self())end),
  New_stat=State#nn_state{pipList = Pipe_list,genotype = Genotype,actuatorPID=Actuator_PID},
  {next_state, NextStateName, New_stat}.

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
construct_network(G,NnPIDs)->
  Actuator=genotype:get_actuator(G),
  SelfPIDs=self(),
  NumOfLayer=Actuator#neuron.layer-1,PIDs=#{Actuator#neuron.id => SelfPIDs,inSensor=>NnPIDs},
  PIDs=construct_layers(G,NumOfLayer,PIDs),
  SensorsPIDs=get_sensor_PIDs(G,PIDs),
  NnPIDs ! {finished_constructing,self(),SensorsPIDs},
  In_pids=genotype:get_weights_map(G,Actuator),
  Neuron=#neuron_data{id=Actuator#neuron.id,in_pids=In_pids,out_pids=[NnPIDs],remaining_in_pids=[NnPIDs],bias=Actuator#neuron.bias,af=Actuator#neuron.af},
  neuron:loop(Neuron).

% construct all the layer frm the actuator until the sensor return map of ID -> PID.
construct_layers(G,0,PIDs)->ListOfNods=genotype:get_layer(G,0),NewPIDs=tl([construct_node(G,N,PIDs)||N<-ListOfNods]),NewPIDs;
construct_layers(G,NumOfLayer,PIDs)->
  ListOfNods=genotype:get_layer(G,NumOfLayer),NewPIDs=tl([construct_node(G,N,PIDs)||N<-ListOfNods]),construct_layers(G,NumOfLayer-1,NewPIDs).

%construct one node form node in the graph to process,return map of ID -> PID.
construct_node(G,N,PIDs)-> if
                             N#neuron.type=:=sensor ->
                               Id=N#neuron.id,
                               OutPIDs=get_out_PIDs(G,N,PIDs),
                               Neuron=#neuron_data{id =Id,in_pids=#{neuralNetwork => 1},out_pids=OutPIDs,remaining_in_pids=OutPIDs,bias=0,af=none},
                               NewPDISs=spawn_link(fun()->neuron:loop(Neuron)end),
                               NewPIDs=maps:put(Id,NewPDISs,PIDs),NewPIDs;
                             true->
                               Id=N#neuron.id,
                               In_pids=genotype:get_weights_map(G,N),
                               OutPIDs=get_out_PIDs(G,N,PIDs),
                               Neuron=#neuron_data{id =Id,in_pids=In_pids,out_pids=OutPIDs,remaining_in_pids=OutPIDs,bias=N#neuron.bias,af=N#neuron.af},
                               NewPDISs=spawn_link(fun()->neuron:loop(Neuron)end),
                               NewPIDs=maps:put(Id,NewPDISs,PIDs),NewPIDs
                           end.

%the function get Graph, Node and maps of PIDs, key:ID. return list of all node neighbours PIDs.
get_out_PIDs(G,N,PIDs)->NeighboursOut=digraph:out_neighbours(G,N),get_out_PIDs(G,PIDs,NeighboursOut,[]).
get_out_PIDs(_,_,[],OutPIDs)->OutPIDs;
get_out_PIDs(G,PIDs,[H|T],OutPIDs)->get_out_PIDs(G,PIDs,T,OutPIDs ++[maps:get(H#neuron.id,PIDs)]).

%return list of sensor PIDS.
get_sensor_PIDs(G,PIDs) -> Sensor=genotype:get_sensors(G),get_sensor_PIDs(G,PIDs,Sensor,[]).
get_sensor_PIDs(_,_,[],SensorsPIDs)->SensorsPIDs;
get_sensor_PIDs(G,PIDs,[H|T],SensorsPIDs) ->
  PID=maps:get(H#neuron.id,PIDs),
  get_sensor_PIDs(G,PIDs,T,SensorsPIDs++[PID]).