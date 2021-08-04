%%%-------------------------------------------------------------------
%%% @author tomer
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Aug 2021 21:03
%%%-------------------------------------------------------------------
-module(main).
-author("tomer").

%% API
-export([test/1]).
test(Name)->
  %Pipes = graphics:generate_pipes(5),
  %graphics:start(Pipes),
  %neuralNetwork:start(Name,{self()}),
  G = genotype:test_Genotype(2,2),
%%  E = graph_visualization:getEdgesList(G),
%%  V = graph_visualization:getVerticesList(G),
    plot_graph:to_file( G ,Name, "png").