%%%-------------------------------------------------------------------
%%% @author tomer
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Jul 2021 12:52
%%%-------------------------------------------------------------------
-author("tomer").

%% simulation records
-record(pipe_rec,{height,x,passed}).

-record(bird_rec,{y,vel,angle,jump_height}).
-record(bird_graphics_rec,{y,angle}).

-record(sim_state,{tick_time,
  bird,              % the bird running in the current simulation
  visible_pipeList,  % the pipes which are visible on screen
  extra_pipeList,    % pipes in reserve for when the visible pipes move out of the screen
  used_pipeList}).   % the already used pipes, we keep those pipes so that we can refresh the reserve from the used pipes when we run out

-define(WIN_WIDTH,600).
-define(WIN_HEIGHT,800).
-define(FLOOR,730).

-define(MAX_ROTATION,25).
-define(ROT_VEL,10).
-define(ANIMATION_TIME,2).

-define(PIPE_GAP,200).
-define(X_VELOCITY,5).
-define(BIRD_X_LOCATION,230).
-define(BIRD_Y_LOCATION,400).% this is the starting y location


-define(BG_WIDTH,564).%288
-define(BG_HEIGHT,1024).%512
-define(BASE_WIDTH,672).%336
-define(BASE_HEIGHT,224).%112
-define(PIPE_WIDTH,104).%52
-define(PIPE_HEIGHT,640).%320
-define(BIRD_WIDTH,68).%34
-define(BIRD_HEIGHT,48).%24
-define(BIRD_RADIUS,13).% averaged width and height to 26