%%%-------------------------------------------------------------------
%%% @author Omri gil and Tomer Shaked
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Oct 2020 15:55
%%%-------------------------------------------------------------------
-module(plot_graph).
-author("Omri, Tomer").

%% API
-export([ to_dot/2,to_file/3]).
-include_lib("wx/include/wx.hrl").
-include("Constants.hrl").


%%create graph pic to show

to_file(Graph, File, Format) ->
  Y = erlang:timestamp(),
  {A1,A2,A3}=Y,
  DotFile = lists:concat([File, ".dot-", A1, "-", A2, "-", A3]),
  to_dot(Graph, DotFile),
  DotCommant = lists:concat(["dot -T", Format, " -o", File, " ", DotFile]),
  X=os:cmd(DotCommant),
  file:delete(DotFile),
  X.
plotEdge(Edge,IODevice)->
  {_,Node1ID,Node2ID,Weight}=Edge,

  io:format(IODevice, "  ~p ~s ~p [label=~p] ;~n",[Node1ID, "->", Node2ID,float_to_list(Weight,[{decimals,3}])]).
plotNode(Node,IODevice)->

  ID=Node#neuron.id,
  Label = Node#neuron.type,
  Layer = Node#neuron.layer,
  AF = Node#neuron.af,
  case Node#neuron.bias of
    0->Bias = integer_to_list(Node#neuron.bias);
    _-> Bias = float_to_list(Node#neuron.bias,[{decimals,3}])
  end,
  io:format(IODevice, "  ~p  [label=\"~p\\n~s\\nBias: ~s\\nLayer: ~p\\nAF: ~p\",shape=circle,fillcolor=chartreuse1,style=filled,width=1,height=1,fixedsize=true];~n",[ID,Node#neuron.id,Label,Bias,AF,Layer]).
to_dot(Graph, File) ->
  % open file
  {ok, IODevice} = file:open(File, [write]),
  Nodes = genotype:get_nodes(Graph),
  Edges = genotype:getEdges(Graph),

  % print graph
  io:format(IODevice, "~s ~s {~n", ["digraph", "G"]),
  io:format(IODevice, "rankdir=LR;~n", []),
  io:format(IODevice, "nodesep=0.001;center=true;~n", []),
  io:format(IODevice, "dpi= 100 ;ratio=\"fill\";size=\"14.7,7.3!\";margin=0;", []),
  % all nodes
  [plotNode(Node,IODevice)||Node<- Nodes],
  [plotEdge(Edge,IODevice)||Edge<-Edges],

  io:format(IODevice, "{ rank=min", []),
  [ io:format(IODevice, ";~p", [Name])||#neuron{id=Name}<-genotype:get_sensors(Graph)],
  io:format(IODevice, "}~n", []),
  io:format(IODevice, "{ rank=max", []),
  [ io:format(IODevice, ";~p", [Name])||#neuron{id=Name}<-genotype:get_actuator(Graph)],
  io:format(IODevice, "}~n", []),

  % close file
  io:format(IODevice, "}~n", []),
  file:close(IODevice).

