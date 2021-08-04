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
-export([simulate_a_frame/2,feature_extraction/1,initiate_simulation/1,simulate_pipes/1,test/0]).
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
initiate_simulation(Pipes)->
  [First_pipe| Rest] = Pipes,
  #sim_state{
    tick_time = 1,
    bird = #bird_rec{
      y = ?BIRD_Y_LOCATION,
      vel = 0,
      angle = 0,
      jump_height = 0},
    visible_pipeList = [First_pipe],
    extra_pipeList = Rest,
    used_pipeList = []}.
feature_extraction(Simulation_State = #sim_state{})->
  Bird_Y           = Simulation_State#sim_state.bird#bird_rec.y,
  Bird_Y_vel       = Simulation_State#sim_state.bird#bird_rec.vel,
  First_pipe = hd(Simulation_State#sim_state.visible_pipeList),
  Distance_to_pipe = First_pipe#pipe_rec.x - ?BIRD_X_LOCATION,
  PipeHeight       = First_pipe#pipe_rec.height,
  [Bird_Y, Bird_Y_vel, Distance_to_pipe, PipeHeight].
simulate_pipes(Pipe_State = #pipes_graphics_rec{})->
  % move pipes
  Moved_pipes = pipe_move(Pipe_State#pipes_graphics_rec.visible_pipeList),

  [First_Pipe|Rest] = Moved_pipes,

  % if we passed the first pipe
  {VIS,RES,USE} = if
                    ((not First_Pipe#pipe_rec.passed) and (First_Pipe#pipe_rec.x < ?BIRD_X_LOCATION))->
                      All_pipes = [First_Pipe#pipe_rec{passed = true}|Rest],
                      %% ADD PIPE %%
                      % if reserve List is empty
                      {Resrve_List,Used_list} = if
                                                  length(Pipe_State#pipes_graphics_rec.extra_pipeList) =:= 0 ->
                                                    {lists:reverse(Pipe_State#pipes_graphics_rec.used_pipeList),[]};
                                                  true                                                    ->
                                                    {Pipe_State#pipes_graphics_rec.extra_pipeList,Pipe_State#pipes_graphics_rec.used_pipeList}
                                                end,
                      [First_Reserve|Rest_Reserve] = Resrve_List,
                      %  add the new pipe from the reserve list
                      Vis_Pipe_List = lists:append([All_pipes,[First_Reserve#pipe_rec{x=?WIN_WIDTH,passed = false}]]),
                      {Vis_Pipe_List,Rest_Reserve,Used_list};
                    true->
                      {Moved_pipes,Pipe_State#pipes_graphics_rec.extra_pipeList,Pipe_State#pipes_graphics_rec.used_pipeList}
                  end,

  % if there is a pipe off-screen remove it
  {New_visible_pipeList,New_Used_pipes} = case First_Pipe#pipe_rec.x+?PIPE_WIDTH < 0 of
                                            true->  [A|B] = VIS,{B,[A|USE]};
                                            false-> {VIS,USE}
                                          end,
  #pipes_graphics_rec{visible_pipeList = New_visible_pipeList, extra_pipeList = RES, used_pipeList = New_Used_pipes}.

simulate_a_frame(Simulation_State = #sim_state{},Jump)->
  Tick_time = if
    Jump =:= true -> 1;
    true      -> Simulation_State#sim_state.tick_time+1
  end,
  % move bird
  Moved_bird = bird_move(Simulation_State#sim_state.bird,Jump,Tick_time),
  % move pipes
  Moved_pipes = pipe_move(Simulation_State#sim_state.visible_pipeList),

  [First_Pipe|Rest] = Moved_pipes,

  % if we passed the first pipe
  {VIS,RES,USE} = if
    ((not First_Pipe#pipe_rec.passed) and (First_Pipe#pipe_rec.x < ?BIRD_X_LOCATION))->
      All_pipes = [First_Pipe#pipe_rec{passed = true}|Rest],
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
      {Vis_Pipe_List,Rest_Reserve,Used_list};
    true->
      {Moved_pipes,Simulation_State#sim_state.extra_pipeList,Simulation_State#sim_state.used_pipeList}
  end,

  % if there is a pipe off-screen remove it
  {New_visible_pipeList,New_Used_pipes} = case First_Pipe#pipe_rec.x+?PIPE_WIDTH < 0 of
    true->  [A|B] = VIS,{B,[A|USE]};
    false-> {VIS,USE}
  end,
  % check collision
  Ground_collision = world_collision_detection(Moved_bird),
  Collide = if
    Ground_collision =:= true ->true;
    true                      -> pipe_collision_detection(Moved_bird,New_visible_pipeList)
  end,
  % return if collided and new sim state
  Bird_graphics = #bird_graphics_rec{y = Moved_bird#bird_rec.y,angle = Moved_bird#bird_rec.angle},
  New_simulation_state = #sim_state{total_time = Simulation_State#sim_state.total_time+1, tick_time = Tick_time, bird = Moved_bird, visible_pipeList = New_visible_pipeList, extra_pipeList = RES, used_pipeList = New_Used_pipes},
 {Collide,Bird_graphics,New_simulation_state}.

bird_move(Bird,Jump,Tick_time)->
  {Vel,Jump_height} = case Jump of
    true->  {-10.5,Bird#bird_rec.y};
    false-> {Bird#bird_rec.vel,Bird#bird_rec.jump_height}
  end,
  Displacement = Vel*Tick_time+0.5*(?GRAVITY)*math:pow(Tick_time,2),
  Displacement2 = if
    Displacement >=16 -> 16;
    Displacement < 0  -> Displacement - 2;
    true              -> Displacement
  end,
  Y = Bird#bird_rec.y + Displacement2,
  Tilt = Bird#bird_rec.angle,
  Tilt2 = if
    (Displacement2 < 0) -> %or (Y < (Jump_height + 50))
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
  F = [#pipe_rec{height = Height,x = X-?X_VELOCITY,passed = Passed}||#pipe_rec{height=Height,x=X,passed = Passed}<-Pipes], F.


% pipe.x < bird.x + birdRadius and bird.x - birdRadius < pipe.x + pipeWidth and
% pipe.height > bird.y + birdRadius or bird.y - birdRadius > pipe.height + pipeGap
pipe_collision_detection(_Bird,[])->exit("no pipes visible");
pipe_collision_detection(Bird,Pipes)->
  [P|_R] = Pipes,
  Pipe_x = P#pipe_rec.x,
  Pipe_height = P#pipe_rec.height,
  Bird_x = ?BIRD_X_LOCATION,
  Bird_y = Bird#bird_rec.y,
  if
    ((Pipe_x < Bird_x + ?BIRD_WIDTH) and (Bird_x < Pipe_x + ?PIPE_WIDTH)) and
    ((Pipe_height > Bird_y) or (Bird_y + ?BIRD_HEIGHT > Pipe_height + ?PIPE_GAP))  -> true;
    true                                                                                          -> false
  end.

world_collision_detection(Bird)->
  case ((Bird#bird_rec.y+2*?BIRD_RADIUS > (?BG_HEIGHT - ?BASE_HEIGHT)) or (Bird#bird_rec.y < 0)) of
    true->true;
    false-> false
  end.