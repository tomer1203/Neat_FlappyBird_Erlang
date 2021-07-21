%%%-------------------------------------------------------------------
%%% @author omrag
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. יולי 2021 10:12
%%%-------------------------------------------------------------------
-module(construct_Genotype).
-author("omrag").

%% API
-export([construct_Genotype/3,get_sensors/1,get_actuator/1,get_layer/2,get_weights_map/2,test_Genotype/2]).

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
  [digraph:add_edge(G,X,Y) ||  X<-PrevLayer, Y <-CurrentLayer];
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
%return map of all the in edges {from,to} weight, key: from id, value: weight.
get_weights_map(G,Node) -> Edges=digraph:in_edges(G,Node),Weights=#{} ,get_weights_map(G,Edges,Weights).
get_weights_map(_,[],Weights) -> Weights;
get_weights_map(G,[H|T],Weights) ->
{_,Node1,_,Weight}= digraph:edge(G,H),Weights_new= maps:put(Node1#neuron.id,Weight,Weights),get_weights_map(G,T,Weights_new).

% create list of neurons, the size is the number of neurons in one layer .
create_layer_of_Neuron(G,NumOfLayer,NumOfNeurons)->create_layer_of_Neuron(G,[],NumOfLayer,NumOfNeurons).
create_layer_of_Neuron(_,List,_,0) ->List;
create_layer_of_Neuron(G,List,NumOfLayer,NumOfNeurons) ->
  Neuron = #neuron{type = neuron,layer=NumOfLayer},
  digraph:add_vertex(G,Neuron),
  create_layer_of_Neuron(G,List ++ [Neuron],NumOfLayer,NumOfNeurons-1).
