%%%-------------------------------------------------------------------
%%% @author tomer
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Jul 2021 12:52
%%%-------------------------------------------------------------------
-author("tomer").

%% CONSTANTS %%
% Node Communication Constants %
-define(GRAPHICS_NODE, 'pc1@omri-VirtualBox').
-define(PC1, 'pc1@omri-VirtualBox').
-define(PC2, 'pc1@omri-VirtualBox').
-define(PC3, 'pc1@omri-VirtualBox').
-define(PC4, 'pc1@omri-VirtualBox'). %132.72.104.

% World Constants %
% these are not really used since the more accurate measurement is the actual graphics size
-define(WIN_WIDTH,600).
-define(WIN_HEIGHT,800).
-define(FLOOR,730).
-define(Timer,40).% Graphics Update Timer, default: 67

% Bird Constants %
-define(BIRD_X_LOCATION,120).%230
-define(BIRD_Y_LOCATION,400).% this is the starting y location
-define(MAX_ROTATION,25).
-define(ROT_VEL,10).
-define(ANIMATION_TIME,2).

% Pipe Constants %
-define(PIPE_GAP,200).
-define(X_VELOCITY,5).
-define(GRAVITY,4). % 3
-define(JUMP_VELOCITY,-10.5). % -10.5
-define(PIPE_MIN_DISTANCE_FROM_EDGES,50).
-define(PIPE_MAX_HEIGHT,?BG_HEIGHT-?BASE_HEIGHT-?PIPE_GAP-?PIPE_MIN_DISTANCE_FROM_EDGES).
-define(NUMBER_OF_PIPES,20).
-define(END_OF_THE_WORLD,5000).

% Graphics Constants %
-define(BG_WIDTH,564).%288
-define(BG_HEIGHT,1024).%512
-define(BASE_WIDTH,672).%336
-define(BASE_HEIGHT,224).%112
-define(PIPE_WIDTH,104).%52
-define(PIPE_HEIGHT,640).%320
-define(BIRD_WIDTH,68).%34
-define(BIRD_HEIGHT,48).%24
-define(BIRD_RADIUS,26).% averaged width and height to 26

% Genotype Constants %
%-define(ACTIVATION_FUNCTION_LIST,[gaussian, tanh, cos, sin, sign, bin, trinary, multiquadric, absolute, linear, quadratic, gaussian, sqrt, log, sigmoid]).
-define(ACTIVATION_FUNCTION_LIST,[tanh]).
-define(NUMBER_OF_MUTATION, 3).

-define(NUMBER_OF_SUBSCRIBED_BIRDS,100).
-define(DIVIDE_BY,4). % controls how many survive for example for divide by 4 and 100 nn you get 25 survive and 75 dead

%% SIMULATION RECORDS %%
-record(pipe_rec,{height,x,passed}).

% record used for simulating the birds movement
-record(bird_rec,{
  y,           % Y coordinate of the bird
  vel,         % Birds Y velocity
  angle,       % Angle of the bird
  jump_height  % the height the bird last jumped
}).

% a compressed record holding only the data used for the birds graphics
-record(bird_graphics_rec,{
  y = ?BIRD_Y_LOCATION,     % Y coordinate of the bird
  angle = 0  % Angle of the bird
}).
-record(pipes_graphics_rec,{
  visible_pipeList,  % the pipes which are visible on screen
  extra_pipeList,    % pipes in reserve for when the visible pipes move out of the screen
  used_pipeList      % the already used pipes, we keep those pipes so that we can refresh the reserve from the used pipes when we run out
}).
% a record used for the whole simulation. this record represents the state of the simulation in a single frame
-record(sim_state,{
  total_time = 0,        % the time passed since the start of the simulation(counted in frames)
  tick_time,         % the time passed since the last jump accured(counted in frames)
  bird,              % the bird running in the current simulation
  visible_pipeList,  % the pipes which are visible on screen
  extra_pipeList,    % pipes in reserve for when the visible pipes move out of the screen
  used_pipeList}).   % the already used pipes, we keep those pipes so that we can refresh the reserve from the used pipes when we run out


%% GRAPHICS RECORDS %%
-record(graphics_state, {
  frame, panel, dc, paint,
  pc_list, %TODO: maybe change this to a constant list or something
  simulation_finished,super_graphics,
  number_of_nn,
  debug_const_pipe_list,
  time = 0, collide = false, base_state, bird_queue = queue:new(), pipes_state, current_bird_list = [],
  bmpRMap,bmpB1Map,bmpB2Map,bmpB3Map,bmpB4Map,bmpPipeMap,bmpBaseMap,bmpLogoMap}).
-record(base_state,{
  x1 = 0,
  x2 = ?BASE_WIDTH
}).


%% NEURAL NETWORK RECORDS %%
-record(nn_state, {
  pcPID,
  genotype,
  pipList,
  actuatorPID,
  sensorsPIDs,
  simulation,
  finish_and_start = wait,
  sub2graphics = false,
  require_mutation = false
}).
-record(neuron, {type,id=erlang:unique_integer(),layer,af=sin,bias=rand:uniform()}).
-record(neuron_data,{
  id,                 % the id of the current neuron
  in_pids,            % the pid of all inputs of the current neuron
  out_pids,           % the pid of all outputs of the current neuron
  remaining_in_pids,  % the pids which have not yet sent an input to the neuron
  bias,               % the bias of the neuron calculation
  af,                 % the activation function used in this neuron
  acc=0                % An Accumulator for the neuron
}).

%% PC SERVER RECORDS %%
-record(pc_server_state, {
  name,
  pc_num,
  number_of_networks,
  gen_ets,
  neighbours_map_ets,
  fitness_ets,
  remaining_networks,
  learning_pid,
  generation=wait,
  pipe_list,
  keep_list
}).
