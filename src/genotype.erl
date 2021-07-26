%%%-------------------------------------------------------------------
%%% @author omrag
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21.7 2021 10:12
%%%-------------------------------------------------------------------
-module(genotype).
-author("omrag").

%% API
-export([construct_Genotype/3,get_sensors/1,get_actuator/1,get_layer/2,test_Genotype/2,get_layer/1,rand_neutron/1]).

-record(neuron,
 {type,
  id=erlang:unique_integer(),
  layer,
  af=tanh,
  bias=rand:uniform()}).
%for test only
test_Genotype(NumOfLayers,NumOfNeurons) ->
  ListOfSensors=[#neuron{type = sensor,layer = 0,id=sensor_1},#neuron{type = sensor,layer = 0,id=sensor_2},#neuron{type = sensor,layer = 0,id=sensor_3}],
  Genotype=construct_Genotype(ListOfSensors,NumOfLayers,NumOfNeurons),Genotype.

%construct graph with X layers and etch two neighbours layer are connect between them.
construct_Genotype(ListOfSensors,NumOfLayers,NumOfNeurons)->
  G=digraph:new([acyclic]),
  add_sensors(G,ListOfSensors),
  add_layers(G,NumOfLayers,NumOfNeurons),
  add_actuator(G,NumOfLayers),G.

%Adds
add_sensors(G,[H|T]) -> digraph:add_vertex(G,H), add_sensors(G,T);
add_sensors(_,[]) -> ok.

add_actuator(G,NumOfLayers) -> Actuator= #neuron{type =actuator,layer=NumOfLayers+1},
  digraph:add_vertex(G,Actuator),
  PrevLayer=get_layer(G,NumOfLayers),
  [digraph:add_edge(G,X,Actuator,rand:uniform()) ||  X<-PrevLayer].

%add all the layer between the sensors ans the actuator.
add_layers(G,NumOfLayers,NumOfNeurons) -> add_layers(G,1,NumOfLayers,NumOfNeurons).
add_layers(G,NumOfLayers,NumOfLayers,NumOfNeurons)-> CurrentLayer= create_layer_of_Neuron(G,NumOfLayers,NumOfNeurons),
  PrevLayer=get_layer(G,NumOfLayers-1),
  [digraph:add_edge(G,X,Y,rand:uniform()) ||  X<-PrevLayer, Y <-CurrentLayer];
add_layers(G,NumOfCurrentLayer,NumOfLayers,NumOfNeurons) ->
  CurrentLayer= create_layer_of_Neuron(G,NumOfCurrentLayer,NumOfNeurons),
  PrevLayer=get_layer(G,NumOfCurrentLayer-1),
  [digraph:add_edge(G,X,Y,rand:uniform()) ||  X<-PrevLayer, Y <-CurrentLayer],
  add_layers(G,NumOfCurrentLayer+1,NumOfLayers,NumOfNeurons).

%Gets
%return list of the sensors.
get_sensors(G) -> Vertices=digraph:vertices(G),[N||N <- Vertices, N#neuron.type =:= sensor].
%return the actuator.
get_actuator(G) -> Vertices=digraph:vertices(G),[N||N <- Vertices, N#neuron.type =:= actuator].
%return list of neuron that in the same layer.
get_layer(G, NumOfLayer) -> Vertices=digraph:vertices(G),[N||N <- Vertices, N#neuron.layer =:= NumOfLayer].


% create list of neurons, the size is the number of neurons in one layer .
create_layer_of_Neuron(G,NumOfLayer,NumOfNeurons)->create_layer_of_Neuron(G,[],NumOfLayer,NumOfNeurons).
create_layer_of_Neuron(_,List,_,0) ->List;
create_layer_of_Neuron(G,List,NumOfLayer,NumOfNeurons) ->
  Neuron = #neuron{type = neuron,layer=NumOfLayer},
  digraph:add_vertex(G,Neuron),
  create_layer_of_Neuron(G,List ++ [Neuron],NumOfLayer,NumOfNeurons-1).

get_layer(Node)->Node#neuron.layer.

%%%===================================================================
%%%     genotype-mutator
%%%===================================================================

% the function return random neutron.
rand_neutron(G) ->
  Neurons = digraph:vertices(G),
  LengthNeurons=length(Neurons),
  Index = rand:uniform(LengthNeurons),
  SelectedNeuron= lists:nth(Index ,Neurons),
  SelectedNeuron.
%TODO
%%Choose a random neuron A, check if it has a bias in its weights list, if it does
%%not, add the bias value. If the neuron already has a bias value, do nothing.
%dd_bias(G)

%TODO
%%Choose a random neuron A, check if it has a bias in its weights list, if it does,
%%remove it. If the neuron does not have a bias value, do nothing.
%remove_bias(G)

%%Choose a random neuron A, and perturb each weight in its weight list with a
%%probability of 1/sqrt(length(weights)), with the perturbation intensity randomly
%%chosen between -Pi/2 and Pi/2.
mutate_weights(G)->
  SelectedNeutron = rand_neutron(G),



%%Choose a random neuron A, and reset all its synaptic weights to random values
%%ranging between -Pi/2 and Pi/2.
%reset_weights()


%%Choose a random neuron A, and change its activation function to a new random
%%activation function chosen from the af_list in the constraint record belonging to
%%the NN.
%mutate_af()



%%Choose a random neuron A, and an element B, and then add a connection from
%%element B (possibly an existing sensor) to neuron A.
%add_inlink()