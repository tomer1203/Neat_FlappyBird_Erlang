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
-export([loop/1]).
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
        false->erlang:error("unautherized sender");
        true->ok
      end,
      % Add The data into the accumulation
      Weight = maps:get(From,In_Pids_Map),
      Acc = State#neuron_data.acc + Weight*Value,
      Reduced_in_pids = maps:remove(From,In_Pids_Map),
      % if all inputs have been collected(map is empty)->
      case maps:size(Reduced_in_pids) of
         0->
          % activate the af function on the acc
          Result = activation_function(State#neuron_data.af, Acc+State#neuron_data.bias),
          % send result to all output recipients
          [Out_Pid!{neuron_send, self(), Result}||Out_Pid <- State#neuron_data.out_pids],
          % restart the neuron to starting position
          loop(State#neuron_data{acc=0,remaining_in_pids = State#neuron_data.in_pids});
        _ ->
          loop(State#neuron_data{acc = Acc,remaining_in_pids = Reduced_in_pids})
      end;
    _ ->
      erlang:error("Invalid Message")
  end.


activation_function(none, Value)->Value;
activation_function(tanh,Value)->   math:tanh(Value);
activation_function(cos,Value)->    math:cos(Value);
activation_function(sin,Value)->    math:sin(Value);
activation_function(sign,0)->     0;
activation_function(sign,Value)->
  case Value > 0 of
    true -> 1;
    false -> -1
  end;
% The bin/1 function converts Val into a binary value, 1 if Val > 0, and 0 if Val = < 0.
activation_function(bin,Val)->
  case Val > 0 of
    true -> 1;
    false -> 0
  end;
%The trinary/1 function converts Val into a trinary value.
activation_function(trinary,Val)->
  if
    (Val < 0.33) and (Val > -0.33) -> 0;
    Val >= 0.33 -> 1;
    Val =< -0.33 -> -1
  end;
activation_function(multiquadric,Val)->
  math:pow(Val*Val + 0.01,0.5);
activation_function(absolute,Val)->
  abs(Val);
activation_function(linear,Val)->
  Val;
activation_function(quadratic,Val)->
  activation_function(sign,Val)*Val*Val;
activation_function(gaussian,Val)->
  activation_function(gaussian,2.71828183,Val);
activation_function(sqrt,Val)->
  activation_function(sign,Val)*math:sqrt(abs(Val));

activation_function(log,Val)->
  case Val == 0 of
    true -> 0;
    false -> activation_function(sign,Val)*math:log(abs(Val))
  end;
activation_function(sigmoid,Val)->
  V = case Val > 10 of
        true -> 10;
        false ->
          case Val < -10 of
            true -> -10;
            false -> Val
          end
      end,
  2/(1+math:pow(2.71828183,-V)) - 1;
activation_function(sigmoid1,Val)->
  Val/(1+abs(Val));

%The avg/1 function accepts a List for a parameter, and then returns the average of the list to the caller.
activation_function(avg,List)->
  lists:sum(List)/length(List);
activation_function(std,List)->
  Avg = activation_function(avg,(List)),
  activation_function(std,List,Avg,[]).

activation_function(std,[Val|List],Avg,Acc)->
  activation_function(std,List,Avg,[math:pow(Avg-Val,2)|Acc]);
activation_function(std,[],_Avg,Acc)->
  Variance = lists:sum(Acc)/length(Acc),
  math:sqrt(Variance).

activation_function(gaussian,Const,Val)->
  V = case Val > 10 of
        true -> 10;
        false ->
          case Val < -10 of
            true -> -10;
            false -> Val
          end
      end,
  math:pow(Const,-V*V).

%The std/1 function accepts a List for a parameter, and then returns to the caller the standard deviation of the list.