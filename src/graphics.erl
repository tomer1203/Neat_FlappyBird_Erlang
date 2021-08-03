%%%-------------------------------------------------------------------
%%% @author tomer
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jul 2021 18:14
%%%-------------------------------------------------------------------
-module(graphics).
-author("tomer").

-behaviour(wx_object).

-include_lib("wx/include/wx.hrl").
-include("Constants.hrl").
-export([start/1,all/2]).
-export([init/1,handle_event/2,handle_sync_event/3,handle_info/2]).
-export([generate_pipes/1]).

-define(SERVER, ?MODULE).


%%%-------------------------------------------------------------------
all(Name,G)->
    Pipes = generate_pipes(5),
    start(Pipes),
    neuralNetwork:start(Name,{self()}),
    G_mutated = genotype:mutator(G,3),
    gen_statem:cast(Name,{start_simulation,self(),G_mutated,Pipes}).
start(Pipes) ->
    wx_object:start({local,?SERVER},?MODULE,[Pipes],[]).


init([Pipes]) ->
    % graphics
    WxServer = wx:new(),
    Frame = wxFrame:new(WxServer, ?wxID_ANY, "FLappy Bird", [{size,{?BG_WIDTH, ?BG_HEIGHT}}]),
    Panel  = wxPanel:new(Frame),
    %DC=wxPaintDC:new(Panel),
    %Paint = wxBufferedPaintDC:new(Panel),
    % create bitmap to all images
    {BmpRmap,BmpB1Map,BmpB2Map,BmpB3Map,BmpPipeMap,BmpBaseMap}=createBitMaps(),

    % connect panel
    wxFrame:show(Frame),
    erlang:send_after(?Timer, self(), timer),

    wxPanel:connect(Panel, paint, [callback]),
    wxPanel:connect (Panel, left_down),
    wxPanel:connect (Panel, right_down),
    wxFrame:connect(Frame, close_window),

%%    DC2=wxPaintDC:new(Panel),
%%    wxDC:clear(DC2),
%%    wxDC:drawBitmap(DC2,BmpRmap,{0,0}),
%%    wxDC:drawBitmap(DC2,BmpB1Map,{0,0}),

    % Generate random pipes
    [Pipe|Extras] = Pipes,

    %Pipe = generate_pipes(1),
    %Extras = generate_pipes(20),
    SimState = #sim_state{
        tick_time = 1,
        bird = #bird_rec{
            y = ?BIRD_Y_LOCATION,
            vel = 0,
            angle = 0,
            jump_height = 0},
        visible_pipeList = Pipe,
        extra_pipeList = Extras,
        used_pipeList = []},
    NewBase = #base_state{x1 = 0,x2 = ?BASE_WIDTH},
    %dc=DC, paint = Paint
    {Frame,#graphics_state{frame = Frame, panel = Panel, simulation = SimState,pipes_state = #pipes_graphics_rec{visible_pipeList = [Pipe],extra_pipeList = Extras,used_pipeList = []},base_state = NewBase, bmpRMap = BmpRmap,bmpB1Map = BmpB1Map,bmpB2Map = BmpB2Map,bmpB3Map = BmpB3Map,bmpPipeMap = BmpPipeMap,bmpBaseMap = BmpBaseMap}}.

%%%-------------------------------------------------------------------

handle_event(#wx{event = #wxClose{}},State = #graphics_state {frame = Frame}) -> % close window event
    io:format("Exiting\n"),
    wxWindow:destroy(Frame),
    wx:destroy(),
    {stop,normal,State}.

% This Is the main Loop for the graphics
handle_info({bird_update,_From,{Collide,Bird_loc}},State=#graphics_state{bird_list = BirdList})->
    New_BirdList = queue:in({Collide,Bird_loc},BirdList),
    NewState = State#graphics_state{bird_list = New_BirdList},
    {noreply, NewState};
handle_info(timer, State=#graphics_state{frame = Frame,simulation = Simulation,base_state = Base_location_rec,bird_list = Bird_list,pipes_state = Pipe_state,time = Time}) ->  % refresh screen for graphics
    wxWindow:refresh(Frame), % refresh screen

    %io:format("timer event~n"),
%%    {Collide2,_, NewSimulationState2} = case Simulation#sim_state.tick_time =:= 9 of
%%        true ->  {Collide,Graphics_Bird,NewSimulationState} = simulation:simulate_a_frame(Simulation,true),{Collide,Graphics_Bird,NewSimulationState};
%%        false->  {Collide,Graphics_Bird,NewSimulationState} = simulation:simulate_a_frame(Simulation,false),{Collide,Graphics_Bird,NewSimulationState}
%%    end,
%%    receive
%%        {bird_update,_From,{_Collide3,_New_simulation_state}}->io:format("ok going~n")
%%    end,
    case queue:is_empty(Bird_list) of
         false ->
             {{value,{Collide,Bird_loc}},NewBirdList} = queue:out(Bird_list),
             NewPipes = simulation:simulate_pipes(Pipe_state),
             NewBase  = #base_state{x1 = move_base(Base_location_rec#base_state.x1) , x2 = move_base(Base_location_rec#base_state.x2)},
             NewState = State#graphics_state{current_bird_state =  Bird_loc,pipes_state = NewPipes,collide = Collide,time = Time+1,base_state = NewBase,bird_list = NewBirdList};
        true ->
            NewBase  = Base_location_rec,
            NewState = State
    end,
    erlang:send_after(?Timer,self(),timer),
    % MoveBase
%%    NewBase = #base_state{x1 = move_base(Base_location_rec#base_state.x1) , x2 = move_base(Base_location_rec#base_state.x2)},
%%    NewState =State#graphics_state{simulation = NewSimulationState2,collide = Collide2,time = Time+1,base_state = NewBase},
    %io:format("Frame Count= ~p~n", [Time]),
    %{noreply, State#graphics_state{simulation = Simulation,collide = true,time = Time+1,base_state = Base_location_rec}}.
    {noreply, NewState}.

    %io:format("timer event~n"),
%%    {Collide2,_, NewSimulationState2} = case Simulation#sim_state.tick_time =:= 9 of
%%        true ->  {Collide,Graphics_Bird,NewSimulationState} = simulation:simulate_a_frame(Simulation,true),{Collide,Graphics_Bird,NewSimulationState};
%%        false->  {Collide,Graphics_Bird,NewSimulationState} = simulation:simulate_a_frame(Simulation,false),{Collide,Graphics_Bird,NewSimulationState}
%%    end,

handle_sync_event(#wx{event=#wxPaint{}}, _,  _State = #graphics_state{panel = Panel,simulation = SimState,current_bird_state = Bird,pipes_state = Pipes_state,base_state = Base_rec,collide = Collide,time = Time, bmpRMap = BmpRmap,bmpB1Map = BmpB1Map,bmpB2Map = BmpB2Map,bmpB3Map = BmpB3Map,bmpPipeMap = BmpPipeMap,bmpBaseMap = BmpBaseMap}) ->
%%    ok;
    DC2=wxPaintDC:new(Panel),
    wxDC:clear(DC2),
    wxDC:drawBitmap(DC2,BmpRmap,{0,0}),


    %io:format("updating screen~n"),
    Yrounded = round(Bird#bird_graphics_rec.y),
    draw_bird(DC2,BmpB1Map,BmpB2Map,BmpB3Map,?BIRD_X_LOCATION,Yrounded,Bird#bird_graphics_rec.angle,Time),
    [draw_pipe(DC2,BmpPipeMap,Pipe#pipe_rec.x,Pipe#pipe_rec.height)||Pipe <- Pipes_state#pipes_graphics_rec.visible_pipeList],
    draw_base(DC2, BmpBaseMap, Base_rec#base_state.x1, Base_rec#base_state.x2),
% TODO: debug: shows a bird on the top right part of the screen every time that a collision happens
    if
        Collide =:= true ->wxDC:drawBitmap(DC2,BmpB1Map,{0,20});
        true-> ok
    end,
    wxPaintDC:destroy(DC2);

handle_sync_event(_Event,_,State) ->
    {noreply, State}.

draw_bird(PaintPanel,BmpBird1Map,BmpBird2Map,BmpBird3Map,X,Y,Tilt,Time)->
    BirdBmp = case round(Time/?ANIMATION_TIME) rem 3 of
                  0->BmpBird1Map;
                  1->BmpBird2Map;
                  2->BmpBird3Map
              end,
    BirdIm = wxBitmap:convertToImage(BirdBmp),
    RotBirdIm = wxImage:rotate(BirdIm,angle2radians(Tilt), {0,0}),% if direction is not left, rotate the image
    RotBmpBird = wxBitmap:new(RotBirdIm),
    wxDC:drawBitmap(PaintPanel, RotBmpBird, {X,Y}),
    wxImage:destroy(BirdIm),
    wxImage:destroy(RotBirdIm),
    wxBitmap:destroy(RotBmpBird).
draw_pipe(PaintPanel,BmpPipeMap,X, Height)->
    PipeIm = wxBitmap:convertToImage(BmpPipeMap),
    RotPipeIm = wxImage:rotate(PipeIm,angle2radians(180), {0,0}),% if direction is not left, rotate the image
    RotBmpPipeMap = wxBitmap:new(RotPipeIm),
    wxImage:destroy(PipeIm),
    wxImage:destroy(RotPipeIm),
    wxDC:drawBitmap(PaintPanel, RotBmpPipeMap, {X, Height-?PIPE_HEIGHT}),
    wxBitmap:destroy(RotBmpPipeMap),
    wxDC:drawBitmap(PaintPanel, BmpPipeMap, {X, Height+?PIPE_GAP}).
draw_base(PaintPanel, BmpBaseMap, X1, X2)->
    wxDC:drawBitmap(PaintPanel,BmpBaseMap,{X1,?BG_HEIGHT - ?BASE_HEIGHT}),
    wxDC:drawBitmap(PaintPanel,BmpBaseMap,{X2,?BG_HEIGHT - ?BASE_HEIGHT}).

createBitMaps() ->         % create bitmap to all images
    Rmap = wxImage:new("../Images/bg.png"),
    Rmapc = wxImage:scale(Rmap,?BG_WIDTH,?BG_HEIGHT),
    BmpRMap = wxBitmap:new(Rmapc),
    wxImage:destroy(Rmap),
    wxImage:destroy(Rmapc),
    B1map = wxImage:new("../Images/bird1_fix.png"),
    B1mapSc = wxImage:scale(B1map,?BIRD_WIDTH,?BIRD_HEIGHT),
    BmpB1Map = wxBitmap:new(B1mapSc),
    wxImage:destroy(B1map),
    wxImage:destroy(B1mapSc),
    B2map = wxImage:new("../Images/bird2_fix.png"),
    B2mapSc = wxImage:scale(B2map,?BIRD_WIDTH,?BIRD_HEIGHT),
    BmpB2Map = wxBitmap:new(B2mapSc),
    wxImage:destroy(B2map),
    wxImage:destroy(B2mapSc),
    B3map = wxImage:new("../Images/bird3_fix.png"),
    B3mapSc = wxImage:scale(B3map,?BIRD_WIDTH,?BIRD_HEIGHT),
    BmpB3Map = wxBitmap:new(B3mapSc),
    wxImage:destroy(B3map),
    wxImage:destroy(B3mapSc),
    Pipemap = wxImage:new("../Images/pipe_fix.png"),
    PipemapSc = wxImage:scale(Pipemap,?PIPE_WIDTH,?PIPE_HEIGHT),
    BmpPipeMap = wxBitmap:new(PipemapSc),
    wxImage:destroy(Pipemap),
    wxImage:destroy(PipemapSc),
    Basemap = wxImage:new("../Images/base.png"),
    BasemapSc = wxImage:scale(Basemap,?BASE_WIDTH,?BASE_HEIGHT),
    BmpBaseMap = wxBitmap:new(BasemapSc),
    wxImage:destroy(Basemap),
    wxImage:destroy(BasemapSc),
    {BmpRMap,BmpB1Map,BmpB2Map,BmpB3Map,BmpPipeMap,BmpBaseMap}.

angle2radians(Angle)->Angle*math:pi()*2/360.

move_base(X) when X + ?BASE_WIDTH - ?X_VELOCITY < 0 ->?BASE_WIDTH-8;
move_base(X)->X - ?X_VELOCITY.

generate_pipes(N)->generate_pipes(N,[]).
generate_pipes(0,Acc)->Acc;
generate_pipes(N,Acc)->Height = rand:uniform(?PIPE_MAX_HEIGHT-?PIPE_MIN_DISTANCE_FROM_EDGES)+?PIPE_MIN_DISTANCE_FROM_EDGES,
    generate_pipes(N-1,[#pipe_rec{height = Height,x = ?BG_WIDTH-?PIPE_WIDTH,passed = false}|Acc]).
