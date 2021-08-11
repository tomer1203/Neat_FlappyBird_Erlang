%%%-------------------------------------------------------------------
%%% @author Omri, Tomer
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Aug 2021 21:03
%%%-------------------------------------------------------------------
-module(main).
-author("Omri, Tomer").

%% API
-export([test/1,test2/1,loop/1]).
test(Name)->
  %Pipes = simulation:generate_pipes(5),
  %graphics:start(Pipes),
  %neuralNetwork:start(Name,{self()}),
  G = genotype:test_Genotype(10,10),
%%  E = graph_visualization:getEdgesList(G),
%%  V = graph_visualization:getVerticesList(G),
    plot_graph:to_file( G ,Name, "png").


loop(N)->
  io:format("~p~n",[N]),
  loop(N+1).
test2(C)->
  N = 1000,
  Pipes = simulation:generate_pipes(10),
  graphics:start(N),
  pc_server:start(pc1,C,self(),N,2,2),
  gen_server:cast(pc1,{start_simulation,self(),Pipes}).