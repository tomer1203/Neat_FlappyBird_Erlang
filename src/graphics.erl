%%%-------------------------------------------------------------------
%%% @author Omri, Tomer
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jul 2021 18:14
%%%-------------------------------------------------------------------
-module(graphics).
-author("Omri, Tomer").

-behaviour(wx_object).

-include_lib("wx/include/wx.hrl").
-include("Constants.hrl").
-export([start/3]).
-export([init/1,handle_event/2,handle_sync_event/3,handle_info/2,handle_cast/2]).
-export([graphics_reduce/1]).

-define(SERVER, ?MODULE).


%%%-------------------------------------------------------------------
%%all(Name)->
%%    Pipes = simulation:generate_pipes(5),
%%    start(pc1,1,1),
%%    neuralNetwork:start(Name,self()),
%%    %G_mutated = genotype:mutator(G,3),
%%    G = genotype:test_Genotype(3,5),
%%    gen_statem:cast(Name,{start_simulation,self(),G,Pipes,true}).
start(Name,C,N) ->
    Pipes = simulation:generate_pipes(?NUMBER_OF_PIPES),
    wx_object:start({local,?SERVER},?MODULE,[Pipes,[pc1]],[]),
    %TODO: probably won't work with multiple nodes
    Graphics_reduce_pid = spawn_link(graphics,graphics_reduce,[N]),
    register(graphics_proxy,Graphics_reduce_pid),
    % TODO: start more than one pc
    {ok,Learning_pid} =  learningFSM:start(),
    pc_server:start(Name,1,Learning_pid,N,2,5),
    gen_server:cast(Name,{start_simulation,self(),Pipes}).

init([Pipes,PC_list]) ->
    % graphics
    WxServer = wx:new(),
    Frame = wxFrame:new(WxServer, ?wxID_ANY, "FLappy Bird", [{size,{?BG_WIDTH, ?BG_HEIGHT}}]),
    Panel  = wxPanel:new(Frame,[{size, {?BG_WIDTH, ?BG_HEIGHT}}]),
    %DC=wxPaintDC:new(Panel),
    %Paint = wxBufferedPaintDC:new(Panel),
    % create bitmap to all images
    Button = wxButton:new(Frame, ?wxID_ANY, [{label, "Start"}]),
    Button2 = wxButton:new(Frame, ?wxID_ANY, [{label, "finish"}]),
    MainSizer = wxBoxSizer:new(?wxHORIZONTAL),

    UiSizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(MainSizer, Panel,[{flag,?wxEXPAND}]),
    wxSizer:add(UiSizer, Button,[{flag,?wxALL bor ?wxEXPAND},{border, 5}]),
    wxSizer:add(UiSizer, Button2,[{flag,?wxEXPAND bor ?wxALL},{border,5}]),
    wxSizer:add(MainSizer, UiSizer),

    {BmpRmap,BmpB1Map,BmpB2Map,BmpB3Map,BmpPipeMap,BmpBaseMap,BmpLogoMap}=createBitMaps(),

    wxWindow:setSizer(Frame, MainSizer),
    wxSizer:setSizeHints(MainSizer, Frame),

    % connect panel
    wxFrame:show(Frame),
    erlang:send_after(?Timer, self(), timer),

    wxPanel:connect(Panel, paint, [callback]),
%%    wxPanel:connect (Panel, left_down),
%%    wxPanel:connect (Panel, right_down),
    wxFrame:connect(Frame, close_window),
    wxButton:connect(Button, command_button_clicked),
    %wxButton:connect(Button2, command_button_clicked, [{callback, fun handle_click2/2}]),

    % Generate random pipes
    [Pipe|Extras] = Pipes,

    %Pipe = generate_pipes(1),
    %Extras = generate_pipes(20),
    NewBase = #base_state{x1 = 0,x2 = ?BASE_WIDTH},
    %dc=DC, paint = Paint
    {Frame,#graphics_state{
        frame = Frame,
        panel = Panel,
        pc_list = PC_list,
        simulation_finished = false,
        super_graphics = false,
        debug_const_pipe_list = Pipes,
        pipes_state = #pipes_graphics_rec{visible_pipeList = [Pipe],extra_pipeList = Extras,used_pipeList = []},
        base_state = NewBase,
        bmpRMap = BmpRmap,bmpB1Map = BmpB1Map,bmpB2Map = BmpB2Map,bmpB3Map = BmpB3Map,bmpPipeMap = BmpPipeMap,bmpBaseMap = BmpBaseMap,bmpLogoMap = BmpLogoMap,
        current_bird_list = []}}.

%%%-------------------------------------------------------------------

handle_click(#wx{obj = Button},State) ->
    io:format("Start Button clicked~p~n",[State#graphics_state.pc_list]),
    NewState = State#graphics_state{super_graphics = true},
    {noreply, NewState}.
handle_click2(#wx{obj = Button},_Event) ->
    io:format("Stop Button clicked~n").
handle_event(#wx{obj = Button, event = #wxCommand{type = command_button_clicked}},State) ->
    io:format("Start Button clicked~p~n",[State#graphics_state.pc_list]),
    NewState = State#graphics_state{super_graphics = not State#graphics_state.super_graphics},
    {noreply, NewState};
handle_event(#wx{event = #wxClose{}},State = #graphics_state {frame = Frame}) -> % close window event
    io:format("Exiting\n"),
    wxWindow:destroy(Frame),
    wx:destroy(),
    {stop,normal,State}.


% the locations of all birds in the last iteration
handle_cast({bird_locations,Bird_List},State=#graphics_state{bird_queue = Bird_queue})->
    New_bird_queue = queue:in(Bird_List,Bird_queue),
    NewState = State#graphics_state{bird_queue = New_bird_queue},
    {noreply, NewState};

handle_cast({new_generation},State=#graphics_state{})->
    io:format("graphics: starting new generation~n"),
    % TODO: generate new pipes

    % TODO: restart simulation
    NewState = State#graphics_state{ simulation_finished = true},
%%    PipeList = simulation:generate_pipes(?NUMBER_OF_PIPES),
%%    [H_pipe|T_pipes] = PipeList,
%%    NewState = State#graphics_state{ pipes_state = #pipes_graphics_rec{visible_pipeList = [H_pipe],extra_pipeList = T_pipes,used_pipeList = []}},
%%    graphics_proxy!{new_generation,?NUMBER_OF_SUBSCRIBED_BIRDS},
%%    [gen_server:cast(PC,{run_generation,self(),PipeList})||PC<-State#graphics_state.pc_list],
    % TODO: send to pc an ok message
    io:format("pc list ~p~n",[State#graphics_state.pc_list]),

    {noreply, NewState}.
%%handle_info({bird_update,_From,{Collide,Bird_loc}},State=#graphics_state{bird_list = BirdList})->
%%    New_BirdList = queue:in({Collide,Bird_loc},BirdList),
%%    NewState = State#graphics_state{bird_list = New_BirdList},
%%    {noreply, NewState};

% This Is the main Loop for the graphics
handle_info(timer, State=#graphics_state{frame = Frame,base_state = Base_location_rec,bird_queue = Bird_queue,pipes_state = Pipe_state,time = Time}) ->  % refresh screen for graphics
    wxWindow:refresh(Frame), % refresh screen
    %io:format("timer event~n"),
%%    {Collide2,_, NewSimulationState2} = case Simulation#sim_state.tick_time =:= 9 of
%%        true ->  {Collide,Graphics_Bird,NewSimulationState} = simulation:simulate_a_frame(Simulation,true),{Collide,Graphics_Bird,NewSimulationState};
%%        false->  {Collide,Graphics_Bird,NewSimulationState} = simulation:simulate_a_frame(Simulation,false),{Collide,Graphics_Bird,NewSimulationState}
%%    end,
%%    receive
%%        {bird_update,_From,{_Collide3,_New_simulation_state}}->io:format("ok going~n")
%%    end,

%%    io:format("Bird List ~p~n",[Bird_queue]),
    case queue:is_empty(Bird_queue) of
        true ->
            if
                State#graphics_state.simulation_finished =:= true ->
                    PipeList = simulation:generate_pipes(?NUMBER_OF_PIPES),
                    [H_pipe | T_pipes] =PipeList,
%%                    [H_pipe | T_pipes] =State#graphics_state.debug_const_pipe_list,
                    NewState = State#graphics_state{simulation_finished = false, pipes_state = #pipes_graphics_rec{visible_pipeList = [H_pipe], extra_pipeList = T_pipes, used_pipeList = []}},
                    graphics_proxy ! {new_generation, ?NUMBER_OF_SUBSCRIBED_BIRDS},
                    [gen_server:cast(PC, {run_generation, self(), PipeList}) || PC <- State#graphics_state.pc_list];
%%                    [gen_server:cast(PC, {run_generation, self(), PipeList}) || PC <- State#graphics_state.pc_list];
                true -> NewState = State
            end,
            NewBase  = Base_location_rec;
        false ->
            {{value,Bird_list},NewBirdQueue} = queue:out(Bird_queue),
             NewPipes = simulation:simulate_pipes(Pipe_state),
             NewBase  = #base_state{x1 = move_base(Base_location_rec#base_state.x1) , x2 = move_base(Base_location_rec#base_state.x2)},
             NewState = State#graphics_state{current_bird_list =  Bird_list,pipes_state = NewPipes,time = Time+1,base_state = NewBase,bird_queue = NewBirdQueue}

    end,
    erlang:send_after(?Timer,self(),timer),
%%    NewState = State,
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

handle_sync_event(#wx{event=#wxPaint{}}, _,  _State = #graphics_state{panel = Panel,current_bird_list = Bird_list,pipes_state = Pipes_state,base_state = Base_rec,collide = Collide,time = Time, bmpRMap = BmpRmap,bmpB1Map = BmpB1Map,bmpB2Map = BmpB2Map,bmpB3Map = BmpB3Map,bmpPipeMap = BmpPipeMap,bmpBaseMap = BmpBaseMap}) ->
%%    ok;
    DC2=wxPaintDC:new(Panel),
    wxDC:clear(DC2),
    wxDC:drawBitmap(DC2,BmpRmap,{0,0}),
    wxDC:drawBitmap(DC2,_State#graphics_state.bmpLogoMap,{round((?BG_WIDTH/2) - 306/2),50+round(math:sin(Time/10)*10)}),

    %io:format("updating screen~n"),
    %Yrounded = round(Bird#bird_graphics_rec.y),
    %io:format("Bird_list:~p~n",[Bird_list]),
    [draw_bird(DC2,BmpB1Map,BmpB2Map,BmpB3Map,?BIRD_X_LOCATION,round(Y),Tilt,Time,_State#graphics_state.super_graphics)||{_,#bird_graphics_rec{y=Y,angle = Tilt}}<- Bird_list],
    %draw_bird(DC2,BmpB1Map,BmpB2Map,BmpB3Map,?BIRD_X_LOCATION,Yrounded,Bird#bird_graphics_rec.angle,Time),
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
draw_bird(PaintPanel,BmpBird1Map,BmpBird2Map,BmpBird3Map,X,Y,Tilt,Time,Super_graphics)->
    BirdBmp = case round(Time/?ANIMATION_TIME) rem 3 of
                  0->BmpBird1Map;
                  1->BmpBird2Map;
                  2->BmpBird3Map
              end,
    case Super_graphics of
        true ->

            BirdIm = wxBitmap:convertToImage(BirdBmp),
            RotBirdIm = wxImage:rotate(BirdIm,angle2radians(Tilt), {0,0}),% if direction is not left, rotate the image
            RotBmpBird = wxBitmap:new(RotBirdIm),
            wxDC:drawBitmap(PaintPanel, RotBmpBird, {X,Y}),
            wxImage:destroy(BirdIm),
            wxImage:destroy(RotBirdIm),
            wxBitmap:destroy(RotBmpBird);
        false ->
            wxDC:drawBitmap(PaintPanel, BirdBmp, {X,Y})
    end.
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

    Logomap = wxImage:new("../Images/Flappy_Logo.png"),
    LogomapSc = wxImage:scale(Logomap,306,81),
    BmpLogoMap = wxBitmap:new(LogomapSc),
    wxImage:destroy(Logomap),
    wxImage:destroy(LogomapSc),
    {BmpRMap,BmpB1Map,BmpB2Map,BmpB3Map,BmpPipeMap,BmpBaseMap,BmpLogoMap}.

angle2radians(Angle)->Angle*math:pi()*2/360.

move_base(X) when X + ?BASE_WIDTH - ?X_VELOCITY < 0 ->?BASE_WIDTH-8;
move_base(X)->X - ?X_VELOCITY.


% this is a sort of a proxy for the communication with all the neural networks
% it accumulates the inputs from the networks and only when it has all birds from all the network
graphics_reduce(N)->graphics_reduce([],1,N,N).

graphics_reduce(Bird_list,Frame_number,0,Next_N)->
    wx_object:cast(graphics,{bird_locations,Bird_list}),
    case Next_N of
        0 -> io:format("All birds Dead waiting for next generation~n"),
            wx_object:cast(graphics,{new_generation}),
            receive
                {new_generation,New_N}->io:format("restarting graphics~n"), graphics_reduce([],1,New_N,New_N)
            end;
        N -> graphics_reduce([],Frame_number+1,N,N)
    end;
graphics_reduce(Bird_List,Frame_number,N,Next_N)->
    receive
        {bird_update,_From,Frame_number,{Collide,Bird_graphics}}->
            %io:format("received message from: ~p frame count: ~p left to receive:~p~n",[_From,Frame_number,N]),
            New_Birdlist = [{Collide,Bird_graphics}|Bird_List],
            case Collide of
                true->  graphics_reduce(New_Birdlist,Frame_number,N-1,Next_N-1);
                false-> graphics_reduce(New_Birdlist,Frame_number,N-1,Next_N)
            end;
        {bird_update,_From,Number,{Collide,Bird_graphics}} when Number<Frame_number->
            io:format("message slowing graphics down removed~n"),
            graphics_reduce(Bird_List,Frame_number,N,Next_N);
        {kill,_From}->ok
%%    after 1000->
%%        io:format("message was missing from graphics. removing one bird. Frame: ~p Remaining:~p~n",[Frame_number,N]),
%%        flush_messages(),
%%        graphics_reduce(Bird_List,Frame_number,0,0)

    end.
flush_messages() ->
    receive
        _ -> flush_messages()
    after 0 ->
        ok
    end.