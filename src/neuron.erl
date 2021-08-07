%%%-------------------------------------------------------------------
%%% @author Omri, Tomer
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jul 2021 15:29
%%%-------------------------------------------------------------------
-module(neuron).
-author("Omri, Tomer").



%% API
-export([init/0,loop/1,test/0]).
-include("Constants.hrl").


% A test function used to test the functionality of the neuron
test()->
  PID = spawn(fun()-> init() end),
  Initial_State = #neuron_data{id = PID, in_pids = #{1=>0.1,2=>0.2,3=>0.3}, out_pids = [self(),self()], bias = 7,af = log,acc = 0},
  PID!{configure_neuron,self(),Initial_State},
  PID!{neuron_send,3,6},
  PID!{neuron_send,1,7},
  PID!{neuron_send,2,8}.

% this is the function that should be spawned. when a configuration is received it starts the main loop
init()->
  receive
    {configure_neuron,_From,#neuron_data{id = ID, in_pids = In_pids, out_pids = Out_pids, remaining_in_pids = _Remaining_in_pids, bias = Bias,af = AF,acc = Acc}} ->
      loop(#neuron_data{id = ID, in_pids = In_pids, out_pids = Out_pids, remaining_in_pids = In_pids, bias = Bias,af = AF,acc = Acc})
  end.

% responsible for all the neurons calculations,
% Accumulates the weighted inputs and sends the result when all inputs arrived
% neurons function =>
% Result = activation_function(sum_i(Weight_i * Input_i) + bias) = activation_function(W*In + b)
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
           %io:format("--Iam= ~p, Result= ~p~n Weights= ~p~n",[State#neuron_data.id,Result,State#neuron_data.in_pids]),
          % send result to all output recipients
          [Out_Pid!{neuron_send, self(), Result}||Out_Pid <- State#neuron_data.out_pids],
          % restart the neuron to starting position
          loop(State#neuron_data{acc=0,remaining_in_pids = State#neuron_data.in_pids});
        _ ->
          loop(State#neuron_data{acc = Acc,remaining_in_pids = Reduced_in_pids})
      end;
    {kill,From}->exit("normal");%TODO: check if this one works(this might not close all the other process since the reason is normal
    M ->
      erlang:error(lists:flatten(io_lib:format("Invalid Message: ~p", [M])))
  end.

% a long list of possible activation function as suggested
% from "Handbook of neuroevolution through Erlang by Gene I sher".
activation_function(none, Value)->Value;
activation_function(tanh,Value)-> math:tanh(Value);
activation_function(cos,Value)->  math:cos(Value);
activation_function(sin,Value)->  math:sin(Value);
activation_function(sign,0)->     0;
activation_function(sign,Value)->
  case Value > 0 of
    true -> 1;
    false -> -1
  end;
% The bin/1 function converts Value into a binary value, 1 if Value > 0, and 0 if Value = < 0.
activation_function(bin,Value)->
  case Value > 0 of
    true -> 1;
    false -> 0
  end;
%The trinary/1 function converts Value into a trinary value.
activation_function(trinary,Value)->
  if
    (Value < 0.33) and (Value > -0.33) -> 0;
    Value >= 0.33 -> 1;
    Value =< -0.33 -> -1
  end;
activation_function(multiquadric,Value)->
  math:pow(Value*Value + 0.01,0.5);
activation_function(absolute,Value)->
  abs(Value);
activation_function(linear,Value)->
  Value;
activation_function(quadratic, Value = Value)->
  activation_function(sign,Value)*Value*Value;
activation_function(gaussian,Value)->
  activation_function(gaussian,2.71828183,Value);
activation_function(sqrt,Value)->
  activation_function(sign,Value)*math:sqrt(abs(Value));
activation_function(log,Value)->
  case Value == 0 of
    true -> 0;
    false -> activation_function(sign,Value)*math:log(abs(Value))
  end;
activation_function(sigmoid,Value)->
  V = case Value > 10 of
        true -> 10;
        false ->
          case Value < -10 of
            true -> -10;
            false -> Value
          end
      end,
  2/(1+math:pow(2.71828183,-V)) - 1;
activation_function(sigmoid1,Value)->
  Value/(1+abs(Value));

%The avg/1 function accepts a List for a parameter, and then returns the average of the list to the caller.
activation_function(avg,List)->
  lists:sum(List)/length(List);

%The std/1 function accepts a List for a parameter, and then returns to the caller the standard deviation of the list.
activation_function(std,List)->
  Avg = activation_function(avg,(List)),
  activation_function(std,List,Avg,[]).
activation_function(std,[Value|List],Avg,Acc)->
  activation_function(std,List,Avg,[math:pow(Avg-Value,2)|Acc]);
activation_function(std,[],_Avg,Acc)->
  Variance = lists:sum(Acc)/length(Acc),
  math:sqrt(Variance).

activation_function(gaussian,Const,Value)->
  V = case Value > 10 of
        true -> 10;
        false ->
          case Value < -10 of
            true -> -10;
            false -> Value
          end
      end,
  math:pow(Const,-V*V).

