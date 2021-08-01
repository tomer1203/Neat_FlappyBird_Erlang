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
-export([start/0]).
-export([init/1,handle_event/2,handle_sync_event/3,handle_info/2]).
-define(max_x, 1344).
-define(max_y,890).
-define(Timer,67).%67

-define(SERVER, ?MODULE).


%%%-------------------------------------------------------------------

start() ->
    wx_object:start({local,?SERVER},?MODULE,[],[]).


init([]) ->


    % graphics
    WxServer = wx:new(),
    Frame = wxFrame:new(WxServer, ?wxID_ANY, "FLappy Bird", [{size,{?BG_WIDTH, ?BG_HEIGHT}}]),
    Panel  = wxPanel:new(Frame),
    DC=wxPaintDC:new(Panel),
    Paint = wxBufferedPaintDC:new(Panel),
    % create bitmap to all images
    {BmpRmap,BmpB1Map,BmpB2Map,BmpB3Map,BmpPipeMap,BmpBaseMap}=createBitMaps(),


    % connect panel
    wxFrame:show(Frame),
    erlang:send_after(?Timer, self(), timer),

    wxPanel:connect(Panel, paint, [callback]),
    wxPanel:connect (Panel, left_down),
    wxPanel:connect (Panel, right_down),
    wxFrame:connect(Frame, close_window),

    DC2=wxPaintDC:new(Panel),
    wxDC:clear(DC2),
    wxDC:drawBitmap(DC2,BmpRmap,{0,0}),
    wxDC:drawBitmap(DC2,BmpB1Map,{0,0}),

    % Generate random pipes
    Pipe = generate_pipes(1),
    Extras = generate_pipes(20),
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


    {Frame,#graphics_state{frame = Frame, panel = Panel, dc=DC, paint = Paint,simulation = SimState, bmpRMap = BmpRmap,bmpB1Map = BmpB1Map,bmpB2Map = BmpB2Map,bmpB3Map = BmpB3Map,bmpPipeMap = BmpPipeMap,bmpBaseMap = BmpBaseMap}}.

%%%-------------------------------------------------------------------
generate_pipes(N)->generate_pipes(N,[]).
generate_pipes(0,Acc)->Acc;
generate_pipes(N,Acc)->Height = rand:uniform(?PIPE_MAX_HEIGHT-?PIPE_MIN_DISTANCE_FROM_EDGES)+?PIPE_MIN_DISTANCE_FROM_EDGES,
    generate_pipes(N-1,[#pipe_rec{height = Height,x = 0,passed = false}|Acc]).

handle_event(#wx{event = #wxClose{}},State = #graphics_state {frame = Frame}) -> % close window event
    io:format("Exiting\n"),
    wxWindow:destroy(Frame),
    wx:destroy(),
    {stop,normal,State}.



% This Is the main Loop for the graphics
handle_info(timer, State=#graphics_state{frame = Frame,simulation = Simulation,time = Time}) ->  % refresh screen for graphics
    wxWindow:refresh(Frame), % refresh screen
    erlang:send_after(?Timer,self(),timer),
    {Collide2,_, NewSimulationState2} = case Simulation#sim_state.tick_time =:= 13 of
        true ->  {Collide,Graphics_Bird,NewSimulationState} = simulation:simulate_a_frame(Simulation,true),{Collide,Graphics_Bird,NewSimulationState};
        false->  {Collide,Graphics_Bird,NewSimulationState} = simulation:simulate_a_frame(Simulation,false),{Collide,Graphics_Bird,NewSimulationState}
    end,


    NewState =State#graphics_state{simulation = NewSimulationState2,collide = Collide2,time = Time+1},
    {noreply, NewState}.

handle_sync_event(#wx{event=#wxPaint{}}, _,  _State = #graphics_state{panel = Panel,simulation = SimState,collide = Collide,time = Time, bmpRMap = BmpRmap,bmpB1Map = BmpB1Map,bmpB2Map = BmpB2Map,bmpB3Map = BmpB3Map,bmpPipeMap = BmpPipeMap,bmpBaseMap = BmpBaseMap}) ->
    DC2=wxPaintDC:new(Panel),
    wxDC:clear(DC2),
    io:format("It's a me ~p~n",[SimState#sim_state.visible_pipeList]),
    wxDC:drawBitmap(DC2,BmpRmap,{0,0}),
    wxDC:drawBitmap(DC2,BmpBaseMap,{0,?BG_HEIGHT - ?BASE_HEIGHT}),
    draw_bird(DC2,BmpB1Map,BmpB2Map,BmpB3Map,?BIRD_X_LOCATION,round(SimState#sim_state.bird#bird_rec.y),SimState#sim_state.bird#bird_rec.angle,Time),
    [draw_pipe(DC2,BmpPipeMap,Pipe#pipe_rec.x,Pipe#pipe_rec.height)||Pipe <- SimState#sim_state.visible_pipeList],

    % TODO: debug: shows a bird on the top right part of the screen every time that a collision happens
    if
        Collide =:= true ->wxDC:drawBitmap(DC2,BmpB1Map,{0,20});
        true-> ok
    end,
    {noreply, _State};

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
    wxDC:drawBitmap(PaintPanel, RotBmpBird, {X,Y}).
draw_pipe(PaintPanel,BmpPipeMap,X, Height)->
    PipeIm = wxBitmap:convertToImage(BmpPipeMap),
    RotPipeIm = wxImage:rotate(PipeIm,angle2radians(180), {0,0}),% if direction is not left, rotate the image
    RotBmpPipeMap = wxBitmap:new(RotPipeIm),
    wxDC:drawBitmap(PaintPanel, RotBmpPipeMap, {X, Height-?PIPE_HEIGHT}),
    wxDC:drawBitmap(PaintPanel, BmpPipeMap, {X, Height+?PIPE_GAP}).


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