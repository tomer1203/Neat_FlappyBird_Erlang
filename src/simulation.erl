%%%-------------------------------------------------------------------
%%% @author tomer
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Jul 2021 15:34
%%%-------------------------------------------------------------------
-module(simulation).
-author("tomer").

%% API
-export([simulate_a_frame/2,test/0]).
-include("Constants.hrl").
%  <- y==0                    |       |
%                             |       |
%                            _|       |_
%  height     = top      ->  ||_______||
%
%                             ^PIPE_GAP
%  height+gap = bottom   ->    _______
%                            ||       ||
%                             |       |
%                             |       |
%<----------- x ----------->  |       |
-record(pipe_rec,{height,x,passed}).

-record(bird_rec,{y,vel,angle,jump_height}).
-record(bird_graphics_rec,{y,angle}).

-record(sim_state,{tick_time,
                   bird,              % the bird running in the current simulation
                   visible_pipeList,  % the pipes which are visible on screen
                   extra_pipeList,    % pipes in reserve for when the visible pipes move out of the screen
                   used_pipeList}).   % the already used pipes, we keep those pipes so that we can refresh the reserve from the used pipes when we run out
test()->

  {Collide1,Sim1} = simulate_a_frame(#sim_state{
                      tick_time = 1,
                      bird = #bird_rec{
                        y = 120,
                        vel = 0,
                        angle = 0,
                        jump_height = 100},
                      visible_pipeList = [#pipe_rec{
                                            height = 100,
                                            x = 240,
                                            passed = false},
                                          #pipe_rec{
                                            height = 98,
                                            x = 400,
                                            passed = false}],
                      extra_pipeList = [],
                      used_pipeList = [#pipe_rec{
                        height = 33,
                        x = -45,
                        passed = true}]},true),
  io:format("~n~n-----1------~p~n",[{Collide1,Sim1}]),
  {Collide2,Sim2} = simulate_a_frame(Sim1,false),
  io:format("~n~n-----2------~p~n",[{Collide2,Sim2}]),
  {Collide3,Sim3} = simulate_a_frame(Sim2,false),
  io:format("~n~n-----3------~p~n",[{Collide3,Sim3}]),
  {Collide4,Sim4} = simulate_a_frame(Sim3,false),
  io:format("~n~n-----4------~p~n",[{Collide4,Sim4}]),
  {Collide5,Sim5} = simulate_a_frame(Sim4,false),
  io:format("~n~n-----5------~p~n",[{Collide5,Sim5}]),
  {Collide6,Sim6} = simulate_a_frame(Sim5,false),
  io:format("~n~n-----6------~p~n",[{Collide6,Sim6}]),
  {Collide7,Sim7} = simulate_a_frame(Sim6,false),
  io:format("~n~n-----7------~p~n",[{Collide7,Sim7}]),
  {Collide8,Sim8} = simulate_a_frame(Sim7,false),
  io:format("~n~n-----8------~p~n",[{Collide8,Sim8}]).
simulate_a_frame(Simulation_State = #sim_state{},Jump)->
  Tick_time = if
    Jump =:=1 -> 1;
    true      -> Simulation_State#sim_state.tick_time+1
  end,
  % move bird
  Moved_bird = bird_move(Simulation_State#sim_state.bird,Jump,Tick_time),
  % move pipes
  Moved_pipes = pipe_move(Simulation_State#sim_state.visible_pipeList),

  [First_Pipe|Rest] = Moved_pipes,
  io:format("Test= ~p~n",[((not First_Pipe#pipe_rec.passed) and (First_Pipe#pipe_rec.x < ?BIRD_X_LOCATION))]),

  % if we passed the first pipe
  {VIS,RES,USE} = if
    ((not First_Pipe#pipe_rec.passed) and (First_Pipe#pipe_rec.x < ?BIRD_X_LOCATION))->
      All_pipes = [First_Pipe#pipe_rec{passed = true}|Rest],
      io:format("All_pipes= ~p~n",[All_pipes]),
      %% ADD PIPE %%
      % if reserve List is empty
      {Resrve_List,Used_list} = if
        length(Simulation_State#sim_state.extra_pipeList) =:= 0 ->
          {lists:reverse(Simulation_State#sim_state.used_pipeList),[]};
        true                                                    ->
          {Simulation_State#sim_state.extra_pipeList,Simulation_State#sim_state.used_pipeList}
      end,
      [First_Reserve|Rest_Reserve] = Resrve_List,
      %  add the new pipe from the reserve list
      Vis_Pipe_List = lists:append([All_pipes,[First_Reserve#pipe_rec{x=?WIN_WIDTH,passed = false}]]),
      {Vis_Pipe_List,Rest_Reserve,[Used_list]};
    true->
      io:format("moved pipes= ~p~n",[Moved_pipes]),
      {Moved_pipes,Simulation_State#sim_state.extra_pipeList,Simulation_State#sim_state.used_pipeList}
  end,
  io:format("VIS= ~p~n",[VIS]),

  % if there is a pipe off-screen remove it
  {New_visible_pipeList,New_Used_pipes} = case First_Pipe#pipe_rec.x+?PIPE_WIDTH < 0 of
    true->  [A|B] = VIS,io:format("used pipes: ~p~n",[[A|USE]]),{B,[A|USE]};
    false-> {VIS,USE}
  end,
  % check collision
  Ground_collision = world_collision_detection(Moved_bird),
  Collide = if
    Ground_collision =:= true ->true;
    true                      -> pipe_collision_detection(Moved_bird,New_visible_pipeList)
  end,
  % return if collided and new sim state
 {Collide,#sim_state{tick_time = Tick_time, bird = Moved_bird, visible_pipeList = New_visible_pipeList, extra_pipeList = RES, used_pipeList = New_Used_pipes}}.

bird_move(Bird,Jump,Tick_time)->
  {Vel,Jump_height} = case Jump of
    true->  {-10.5,Bird#bird_rec.y};
    false-> {Bird#bird_rec.vel,Bird#bird_rec.jump_height}
  end,
  Displacement = Vel*Tick_time+0.5*(3)*math:pow(Tick_time,2),
  Displacement2 = if
    Displacement >=16 -> 16;
    Displacement < 0  -> Displacement - 2;
    true              -> Displacement
  end,
  io:format("~p~n",[Displacement2]),
  Y = Bird#bird_rec.y + Displacement2,
  Tilt = Bird#bird_rec.angle,
  Tilt2 = if
    (Displacement2 < 0) or (Y < (Jump_height + 50))->
      T = if
        Tilt < ?MAX_ROTATION-> ?MAX_ROTATION;
        true-> Tilt
      end,T;
    true->
      T = if
        Tilt > -90 -> Tilt - ?ROT_VEL;
        true       -> Tilt
      end,T
  end,
  #bird_rec{y = Y,vel = Vel,angle = Tilt2,jump_height = Jump_height}.
pipe_move(Pipes)->
  io:format("move pipes length = ~p~n",[length(Pipes)]),
  F = [#pipe_rec{height = Height,x = X-?X_VELOCITY,passed = Passed}||#pipe_rec{height=Height,x=X,passed = Passed}<-Pipes],
io:format("move pipes length = ~p~n",[length(F)]),F.


% pipe.x < bird.x + birdRadius and bird.x - birdRadius < pipe.x + pipeWidth and
% pipe.height > bird.y + birdRadius or bird.y - birdRadius > pipe.height + pipeGap
pipe_collision_detection(Bird,[])->exit("no pipes visible");
pipe_collision_detection(Bird,Pipes)->
  [P|_R] = Pipes,
  Pipe_x = P#pipe_rec.x,
  Pipe_height = P#pipe_rec.height,
  Bird_x = ?BIRD_X_LOCATION,
  Bird_y = Bird#bird_rec.y,
  if
    ((Pipe_x < Bird_x + ?BIRD_RADIUS) and (Bird_x - ?BIRD_RADIUS < Pipe_x + ?PIPE_WIDTH)) and
    ((Pipe_height > Bird_y + ?BIRD_RADIUS) or (Bird_y - ?BIRD_RADIUS > Pipe_height + ?PIPE_GAP))  -> true;
    true                                                                                          -> false
  end.

world_collision_detection(Bird)->
  case ((Bird#bird_rec.y > (?BG_HEIGHT - ?BASE_HEIGHT)) or (Bird#bird_rec.y < 0)) of
    true->true;
    false-> false
  end.