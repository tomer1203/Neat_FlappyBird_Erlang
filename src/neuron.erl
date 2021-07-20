%%%-------------------------------------------------------------------
%%% @author tomer
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jul 2021 15:29
%%%-------------------------------------------------------------------
-module(neuron).
-author("tomer").

%% API
-export([]).
-record(neuron_data,{
  id,                 % the id of the current neuron
  in_pids,            % the pid of all inputs of the current neuron
  out_pids,           % the pid of all outputs of the current neuron
  remaining_in_pids,  % the pids which have not yet sent an input to the neuron
  bias,               % the bias of the neuron calculation
  af,                 % the activation function used in this neuron
  acc                 % An Accumulator for the neuron
}).

init()->ok.

loop(State = #neuron_data{})->
  receive
    {neuron_send, From, Value}->
      % check if From is A valid input source
      In_Pids_Map = State#neuron_data.remaining_in_pids,
      case maps:is_key(From,In_Pids_Map) of
        false->exit("unautherized sender");
        true->ok
      end,
      % Add The data into the accumulation
      Acc = State#neuron_data.acc + maps:get(From,In_Pids_Map),
      Reduced_in_pids = maps:remove(From,In_Pids_Map),
      % if all inputs have been collected(map is empty)->
      case maps:size(Reduced_in_pids) of
         0->
          % activate the af function on the acc
          Result = activation_function(State#neuron_data.af, Acc+State#neuron_data.bias),
          % send result to all output recipients
          [Out_Pid!{neuron_send,self(),Result}||Out_Pid =  State#neuron_data.out_pids],
          % restart the neuron to starting position
          loop(State#neuron_data{acc=0,remaining_in_pids = State#neuron_data.in_pids});
        _ ->
          loop(State#neuron_data{acc = Acc,remaining_in_pids = Reduced_in_pids})
      end;
    _ ->
      exit("Invalid Message")
  end.


activation_function(af_atom, Value)->Value.