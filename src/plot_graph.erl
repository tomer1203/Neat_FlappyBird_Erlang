%%%-------------------------------------------------------------------
%%% @author hananel assor, dor avni
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Oct 2020 15:55
%%%-------------------------------------------------------------------
-module(graph_visualization).
-author("chass").

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
  %file:delete(DotFile),
  X.
plotEdge(Edge,IODevice)->
  {_,Node1ID,Node2ID,Weight}=Edge,
  case is_integer(Node1ID) of
    true -> Name1 = integer_to_list(Node1ID);
    false -> Name1 = Node1ID
  end,
  case is_integer(Node2ID) of
    true -> Name2 = integer_to_list(Node2ID);
    false -> Name2 = Node2ID
  end,
  io:format(IODevice, "  ~p ~s ~p [label=~p] ;~n",[Node1ID, "->", Node2ID,float_to_list(Weight,[{decimals,3}])]).
plotNode(Node,IODevice)->
  case is_integer(Node#neuron.id) of
    true -> Name = integer_to_list(Node#neuron.id);
    false -> Name = Node#neuron.id
  end,

  Label = Node#neuron.type,
  AF = Node#neuron.af,
  Bias = float_to_list(Node#neuron.bias,[{decimals,3}]),
  io:format(IODevice, "  ~p  [label=\"~s\\nBias: ~s\\nAF: ~s\",shape=circle,fillcolor=chartreuse1,style=filled,width=1,height=1,fixedsize=true];~n",[Node#neuron.id,Label,Bias,AF]).
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

