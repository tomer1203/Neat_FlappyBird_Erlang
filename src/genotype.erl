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

-include("Constants.hrl").

%% API
-export([construct_Genotype/3,get_sensors/1,get_actuator/1,get_layer/2,test_Genotype/2,
  get_layer/1,get_label/2,rand_neutron/1,get_nodes/1,getEdges/1,mutator/2,remove_node/2]).

-export([
add_bias/1,
remove_bias/1,
mutate_weights/1,
mutate_af/1,
remove_neuron/1,
add_link/1,
remove_inlink/1,
remove_outlink/1,add_neuron/1]).
%for test only
test_Genotype(NumOfLayers,NumOfNeurons) ->
  ListOfSensors=[#neuron{type = sensor,layer = 0,id=sensor_1},#neuron{type = sensor,layer = 0,id=sensor_2},#neuron{type = sensor,layer = 0,id=sensor_3},#neuron{type = sensor,layer = 0,id=sensor_4}],
  Genotype=construct_Genotype(ListOfSensors,NumOfLayers,NumOfNeurons),Genotype.

%construct graph with X layers and etch two neighbours layer are connect between them.
construct_Genotype(ListOfSensors,NumOfLayers,NumOfNeurons)->
  G=digraph:new([acyclic]),
  add_sensors(G,ListOfSensors),
  add_layers(G,NumOfLayers,NumOfNeurons),
  add_actuator(G,NumOfLayers),G.

%Adds
add_sensors(G,[H|T]) -> digraph:add_vertex(G,H#neuron.id,H), add_sensors(G,T);
add_sensors(_,[]) -> ok.

add_actuator(G,NumOfLayers) -> Actuator= #neuron{type =actuator,layer=NumOfLayers+1},
  digraph:add_vertex(G,Actuator#neuron.id,Actuator),
  PrevLayer=get_layer(G,NumOfLayers),
  [digraph:add_edge(G,X#neuron.id,Actuator#neuron.id,rand:normal()) ||  X<-PrevLayer].

%add all the layer between the sensors ans the actuator.
add_layers(G,NumOfLayers,NumOfNeurons) -> add_layers(G,1,NumOfLayers,NumOfNeurons).
add_layers(G,NumOfLayers,NumOfLayers,NumOfNeurons)-> CurrentLayer= create_layer_of_Neuron(G,NumOfLayers,NumOfNeurons),
  PrevLayer=get_layer(G,NumOfLayers-1),
  [digraph:add_edge(G,X#neuron.id,Y#neuron.id,rand:normal()) ||  X<-PrevLayer, Y <-CurrentLayer];
add_layers(G,NumOfCurrentLayer,NumOfLayers,NumOfNeurons) ->
  CurrentLayer= create_layer_of_Neuron(G,NumOfCurrentLayer,NumOfNeurons),
  PrevLayer=get_layer(G,NumOfCurrentLayer-1),
  [digraph:add_edge(G,X#neuron.id,Y#neuron.id,rand:normal()) ||  X<-PrevLayer, Y <-CurrentLayer],
  add_layers(G,NumOfCurrentLayer+1,NumOfLayers,NumOfNeurons).


% create list of neurons, the size is the number of neurons in one layer .
create_layer_of_Neuron(G,NumOfLayer,NumOfNeurons)->create_layer_of_Neuron(G,[],NumOfLayer,NumOfNeurons).
create_layer_of_Neuron(_,List,_,0) ->List;
create_layer_of_Neuron(G,List,NumOfLayer,NumOfNeurons) ->
  Neuron = #neuron{type = neuron,layer=NumOfLayer},
  digraph:add_vertex(G,Neuron#neuron.id,Neuron),
  create_layer_of_Neuron(G,List ++ [Neuron],NumOfLayer,NumOfNeurons-1).




%Gets
%return list of the sensors.
get_sensors(G) -> Vertices=get_nodes(G),[N||N <- Vertices, N#neuron.type =:= sensor].
%return the actuator.
get_actuator(G) -> Vertices=get_nodes(G),[N||N <- Vertices, N#neuron.type =:= actuator].
%return list of neuron that in the same layer.
get_layer(G, NumOfLayer) -> Vertices=get_nodes(G),[N||N <- Vertices, N#neuron.layer =:= NumOfLayer].
% return all the nodes in layers that larger then NumOfLayer.
get_layers_from(G,NumOfLayer) -> Vertices=get_nodes(G),[N||N <- Vertices, N#neuron.layer > NumOfLayer].
% return all the nodes in layers that smaller then NumOfLayer.
get_layers_until(G,NumOfLayer) -> Vertices=get_nodes(G),[N||N <- Vertices, N#neuron.layer < NumOfLayer].
% return the number of layer in the graph.
get_num_layers(G) -> [Actuator]=get_actuator(G), Actuator#neuron.layer.
get_layer(Node)->Node#neuron.layer.
get_nodes(G)->
  VerL=digraph:vertices(G),
  [get_label(G,V) || V <- VerL].
get_in_neighbours(G,N)->Neighbours=digraph:in_neighbours(G,N#neuron.id),[get_label(G,X)||X<-Neighbours].
get_out_neighbours(G,N)->Neighbours=digraph:out_neighbours(G,N#neuron.id),[get_label(G,X)||X<-Neighbours].
get_label(G,V)->{_,Node}=digraph:vertex(G,V),Node.
%update the data in  a node from the graph.
set_node(G,Node)->digraph:add_vertex(G,Node#neuron.id,Node).
% set new weight for exist edge (['$e' | N])
set_edge_weight(G,Edge,NewWeight) -> {E,Node1ID,Node2ID,_}= digraph:edge(G,Edge) ,digraph: add_edge(G,E,Node1ID,Node2ID,NewWeight).
% set add a  number to weight for exist edge (['$e' | N])
mod_edge_weight(G,Edge,NewWeight ) -> {E,Node1ID,Node2ID,Weight}= digraph:edge(G,Edge) ,digraph: add_edge(G,E,Node1ID,Node2ID,NewWeight+Weight).
%add edge form X to Y whit random weight.
add_edge(G,X,Y)->X_neighbours=digraph:out_neighbours(G,X#neuron.id),
  Cond=[ID||ID<-X_neighbours,Y#neuron.id=:=ID],
  case Cond of
    []->digraph:add_edge(G,X#neuron.id,Y#neuron.id,rand:uniform());
    _->already_neighbours
end.
add_edge_rand_out(G,N)->
  Nodes_from = get_layers_from(G,N#neuron.layer),
  Rand_node_output=rand_element(Nodes_from),
  add_edge(G,N,Rand_node_output).
add_edge_rand_in(G,N)->
  Nodes_until = get_layers_until(G,N#neuron.layer),
  Rand_node_input=rand_element(Nodes_until),
  add_edge(G,Rand_node_input,N).

getEdges(G)->
  B=digraph:edges(G),
  [digraph:edge(G,E) || E <- B].

get_in_edges(G, Node) -> digraph:in_edges(G,Node#neuron.id).
get_out_edges(G, Node) -> digraph:out_edges(G,Node#neuron.id).
% delete a Node form the graph and update the layer in the graph.
remove_node(G,N) ->
  Layer = get_layer(N),
  NeighboursIn=digraph:in_neighbours(G,N#neuron.id),
  NeighboursOut=digraph:out_neighbours(G,N#neuron.id),
  [add_edge_rand_out(G,get_label(G,Nei))||Nei <- NeighboursIn],
  digraph:del_vertex(G,N#neuron.id),
  NodeInLayer =get_layer(G,Layer),
  io:format("noods~p,remove!!~p",[NodeInLayer,N#neuron.id]),
  if
    NodeInLayer =:= [] -> io:format("update!~p",[NodeInLayer]),update_layer_remove(G,Layer+1);
    true -> ok
  end,
  [add_edge_rand_in(G,get_label(G,NeiOut))||NeiOut <- NeighboursOut,digraph:in_neighbours(G,NeiOut) =:=[]].

remove_edge(G,E) ->
  {_,NID1,NID2,_} = digraph:edge(G,E),
  N1out=digraph:out_neighbours(G,NID1),
  N2in=digraph:in_neighbours(G,NID2),
  NumOfNeighbours1=length(N1out),
  NumOfNeighbours2=length(N2in),
  if
    ((NumOfNeighbours1>1) and (NumOfNeighbours2>1)) -> digraph:del_edge(G,E);
    true -> ok
  end .
% after remove a node update all layer index that is larger then L in graph G. (Layer L is empty)
update_layer_remove(G,L) ->
  [Actuator] =get_actuator(G),
  N=Actuator#neuron.layer,
  update_layer_remove(G,L,N).
update_layer_remove(G,N,N) ->
  NodesInLayer = get_layer(G,N),
  [set_node(G,Node#neuron{layer = N-1 })||Node<-NodesInLayer];
update_layer_remove(G,L,N) ->
  NodesInLayer = get_layer(G,L),
  [set_node(G,Node#neuron{layer = L-1 })||Node<-NodesInLayer],
  update_layer_remove(G,L+1,N).
% after add a node update all layer index that is larger then L in graph G. (Layer L is new)
update_layer_add(G,L) ->
  [Actuator] =get_actuator(G),
  N=Actuator#neuron.layer,
  update_layer_add(G,L,N).
update_layer_add(G,N,N) ->
  NodesInLayer = get_layer(G,N),
  [set_node(G,Node#neuron{layer = N+1 })||Node<-NodesInLayer];
update_layer_add(G,L,N) ->
  NodesInLayer = get_layer(G,L),
  [set_node(G,Node#neuron{layer = L+1 })||Node<-NodesInLayer],
  update_layer_add(G,L+1,N).

%%%===================================================================
%%%     genotype-mutator
%%%===================================================================
% add all the the in_neighbours of A as out_neighbours in graph G.
%%connect_outport_inport(G,A,B) ->
%%  ANeighbours = get_in_neighbours(G,A),
%%  [add_edge(G,B,N)||N<-ANeighbours].
%%
%%% add all the the out_neighbours of A as in_neighbours in graph G.
%%connect_inport_outport(G,A,B) ->
%%  ANeighbours = get_out_neighbours(G,A),
%%  [add_edge(G,N,B)||N<-ANeighbours].

% the function return random neutron. node == record
rand_neutron(G) ->
  Nodes = get_nodes(G),
  Neurons =[N||N<-Nodes, N#neuron.type =:= neuron],
  LengthNeurons=length(Neurons),
  Index = rand:uniform(LengthNeurons),
  SelectedNeuron= lists:nth(Index ,Neurons),
  SelectedNeuron.

% the function return random node.
rand_node(G) ->
  Nodes = get_nodes(G),
  LengthNeurons=length(Nodes),
  Index = rand:uniform(LengthNeurons),
  SelectedNode= lists:nth(Index ,Nodes),
  SelectedNode.

% the function return random element form list of elements.
rand_element(List) ->
  LengthNeurons=length(List),
  Index = rand:uniform(LengthNeurons),
  SelectedNode= lists:nth(Index ,List),
  SelectedNode.

% get the length of the list and return True in  probability of 1/sqrt(N)
probability_choice_sq(N) -> Ens = rand:uniform(100), P=100/math:pow(N,1/2),if
                                                  Ens =< P-> true;
                                                  true -> false
                                                end.
% chosen between -Pi/2 and Pi/2
probability_choice_half() ->Ens = rand:uniform(100), if
                                                      Ens =< 50 ->rand:uniform( 3);
                                                      true -> - rand:uniform(3)
                                                    end.

rand_af()->
  ListOfAf = ?ACTIVATION_FUNCTION_LIST,
  LengthAf = length(ListOfAf),
  Index = rand:uniform(LengthAf),
  SelectedAf= lists:nth(Index ,ListOfAf),SelectedAf.

%%Choose a random neuron A, check if it has a bias in its weights list, if it does
%%not, add the bias value. If the neuron already has a bias value, do nothing.
add_bias(G) -> SelectedNode = rand_neutron(G),set_node(G,SelectedNode#neuron{bias = rand:uniform()}).

%%Choose a random neuron A, check if it has a bias in its weights list, if it does,
%%remove it. If the neuron does not have a bias value, do nothing.
remove_bias(G) -> SelectedNode = rand_neutron(G),set_node(G,SelectedNode#neuron{bias = 0}).

%%Choose a random neuron A, and perturb each weight in its in_weight list with a
%%probability of 1/sqrt(length(weights)), with the perturbation intensity randomly
%%chosen between -Pi/2 and Pi/2.
mutate_weights(G)->
  SelectedNeuron = rand_neutron(G),
  ListOfWeights = get_in_edges(G,SelectedNeuron),
  mutate_weights(G,ListOfWeights).
mutate_weights(_,[])->ok;
mutate_weights(G,[H|T]) ->
  N=length([H|T]),
  Pro = probability_choice_sq(N),
  if
    Pro =:= true -> mod_edge_weight(G,H,probability_choice_half()),mutate_weights(G,T);
    true -> mutate_weights(G,T)
  end.


%TODO- 3. reset_weights(G)
%%Choose a random neuron A, and reset all its synaptic weights to random values
%%ranging between -Pi/2 and Pi/2.
%reset_weights(G) -> SelectedNeuron = rand_neutron(G),


%%Choose a random neuron A, and change its activation function to a new random
%%activation function chosen from the af_list in the constraint record belonging to
%%the NN.
mutate_af(G) ->
  SelectedNeuron = rand_neutron(G),
  NewNode = SelectedNeuron#neuron{af=rand_af()},
  set_node(G,NewNode).


%%Choose a random neuron A, and an element B, and then add a connection from
%%element B (possibly an existing sensor) to neuron A.
add_link(G) ->
  SelectedNodeA= rand_node(G),
  SelectedNodeB= rand_node(G),
  if
    SelectedNodeA#neuron.layer < SelectedNodeB#neuron.layer -> add_edge(G,SelectedNodeA,SelectedNodeB); %TODO- check if the edge is already exist!!!
    SelectedNodeB#neuron.layer < SelectedNodeA#neuron.layer -> add_edge(G,SelectedNodeB,SelectedNodeA);
    true -> ok
  end.

%%Choose a random neuron A in the NN, and remove it from the topology. Then
%%fix the presynaptic neuron B’s and postsynaptic neuron C’s outports and inports
%%respectively to accommodate the removal of the connection with neuron A.
remove_neuron(G) ->
  SelectedNode = rand_neutron(G),
  remove_node(G,SelectedNode).



%%Create a new neuron A, giving it a unique id and positioning it in a randomly
%%selected layer of the NN. Then give the neuron A a randomly chosen activation
%%function. Then choose a random neuron B in the NN and connect neuron A’s
%%inport to the neuron B’s outport. Then choose a random neuron C in the NN
%%and connect neuron A’s outport to the neuron C’s inport.

add_neuron(G) ->
  NumOfLayers=get_num_layers(G) ,
  SelectedLayer =rand:uniform(NumOfLayers),
  Neuron = #neuron{type = neuron,layer=SelectedLayer, af=rand_af(),bias = rand:uniform()},
  Nodes_until = get_layers_until(G,SelectedLayer),
  Rand_node_input=rand_element(Nodes_until),
  if
    SelectedLayer =:= NumOfLayers ->
      update_layer_add(G,SelectedLayer),
      set_node(G,Neuron),
      [Actuator]=get_actuator(G),
      add_edge(G,Neuron,Actuator);  %update the actuator layer, new layer is added
    true ->
      set_node(G,Neuron),
      add_edge_rand_out(G,Neuron)
%%      Nodes_from = get_layers_from(G,SelectedLayer),
%%      Rand_node_output=rand_element(Nodes_from),
%%      add_edge(G,Neuron,Rand_node_output)
  end,
  add_edge(G,Rand_node_input,Neuron).
%TODO- need to select NodeA and NodeB and copy edge.

%%remove_inlink:
%%Choose a random neuron A, and disconnect it from a randomly chosen element
%%in its input_idps list.
remove_inlink(G) ->
  SelectedNeuron = rand_neutron(G),
  InEdges=get_in_edges(G,SelectedNeuron),
  Length =length(InEdges),
  if
    Length >= 2 -> SelectedEdge = rand_element(InEdges),remove_edge(G,SelectedEdge);
    true -> ok
  end.

%%17. remove_outlink:
%%Choose a random neuron A, and disconnect it from a randomly chosen element
%%in its output_ids list.
remove_outlink(G) ->
  SelectedNeuron = rand_neutron(G),
  LayerNeuron = get_layer(SelectedNeuron),
  NumOfLayers=get_num_layers(G),
  OutEdges=get_out_edges(G,SelectedNeuron),
  Length =length(OutEdges),
  if
    (Length >= 2) and (LayerNeuron < (NumOfLayers - 1))-> SelectedEdge = rand_element(OutEdges),remove_edge(G,SelectedEdge);
    true -> ok
  end.

mutator(_,0) -> ok;
mutator(G,N) -> Index= rand:uniform(9),
  case Index of
    1 -> add_bias(G);
    9 -> add_bias(G);
    2 -> remove_bias(G);
    3 -> mutate_weights(G);

    4 -> mutate_af(G);
    5 -> mutate_af(G);
    %5 -> remove_neuron(G);
    6 -> add_link(G);
    7 -> remove_inlink(G);
    8 -> remove_outlink(G)
    %9 -> add_neuron(G)
  end,
  mutator(G,N-1).




