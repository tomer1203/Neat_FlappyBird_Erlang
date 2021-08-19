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
-export([start/1]).
-export([graphics_rpc/1,graphics_reduce_rpc/1]).
-export([init/1,handle_event/2,handle_sync_event/3,handle_info/2,handle_cast/2,terminate/2]).
-export([graphics_reduce/1]).
-export([generate_map/3]).

-define(SERVER, ?MODULE).


%%%-------------------------------------------------------------------
start(N) ->
    Pipes = simulation:generate_pipes(?NUMBER_OF_PIPES),
    wx_object:start({local,?SERVER},?MODULE,[Pipes,[pc1,pc2,pc3,pc4],N],[]).

initialize_system(N,Pipes)->
    put(?PC1,?PC1),
    put(?PC2,?PC2),
    put(?PC3,?PC3),
    put(?PC4,?PC4),
    PC_List = [pc1,pc2,pc3,pc4],
    ETS_Name_List = [pc1_ets,pc2_ets,pc3_ets,pc4_ets],
    Graphics_reduce_pid = spawn_link(graphics,graphics_reduce,[round(N/4)]),
    register(graphics_proxy,Graphics_reduce_pid),
    {ok,Learning_pid} =  learningFSM:start_link(length(?PC_LIST),?PC_LIST,generate_map(lfsm_,PC_List,ETS_Name_List),N),
    DefGenList=[],
    rpc:call(get(?PC1),pc_server,start,[pc1,1,Learning_pid,round(N/length(PC_List)) ,?NUMBER_OF_LAYERS,?NUMBER_OF_NEURONS_PER_LAYER,PC_List,generate_map(pc1_,PC_List,ETS_Name_List),DefGenList]),
    rpc:call(get(?PC2),pc_server,start,[pc2,2,Learning_pid,round(N/length(?PC_LIST)),?NUMBER_OF_LAYERS,?NUMBER_OF_NEURONS_PER_LAYER,?PC_LIST,generate_map(pc2_,?PC_LIST,?ETS_NAME_LIST),DefGenList]),
    rpc:call(get(?PC3),pc_server,start,[pc3,3,Learning_pid,round(N/length(?PC_LIST)),?NUMBER_OF_LAYERS,?NUMBER_OF_NEURONS_PER_LAYER,?PC_LIST,generate_map(pc3_,?PC_LIST,?ETS_NAME_LIST),DefGenList]),
    rpc:call(get(?PC4),pc_server,start,[pc4,4,Learning_pid,round(N/length(?PC_LIST)),?NUMBER_OF_LAYERS,?NUMBER_OF_NEURONS_PER_LAYER,?PC_LIST,generate_map(pc4_,?PC_LIST,?ETS_NAME_LIST),DefGenList]),
    rpc:call(get(?PC1), pc_server,pc_rpc,[pc1,{start_simulation,self(),Pipes}]),
    rpc:call(get(?PC2), pc_server,pc_rpc,[pc2,{start_simulation,self(),Pipes}]),
    rpc:call(get(?PC3), pc_server,pc_rpc,[pc3,{start_simulation,self(),Pipes}]),
    rpc:call(get(?PC4), pc_server,pc_rpc,[pc4,{start_simulation,self(),Pipes}]).



init([Pipes,PC_list,N]) ->
%%    initialize_system(N,Pipes),
    % graphics
    WxServer = wx:new(),
    Frame = wxFrame:new(WxServer, ?wxID_ANY, "FLappy Bird", [{size,{?BG_WIDTH, ?BG_HEIGHT}}]),
    Panel  = wxPanel:new(Frame,[{size, {?BG_WIDTH, ?BG_HEIGHT}}]),


    Button = wxButton:new(Frame, 10, [{label, "Start"}]),
    Button1 = wxButton:new(Frame, 11, [{label, "Toggle graphics"}]),
    Button2 = wxButton:new(Frame, 12, [{label, "Enchance graphics"}]),
    Font = wxFont:new(42, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD),
    Font1 = wxFont:new(16, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD),
    Text = wxStaticText:new(Frame, 13,"Gen: 0",[{size,{250,70}}]),
    Text1 = wxStaticText:new(Frame, 13,"Frame Count= 0",[{size,{250,70}}]),
    wxStaticText:setFont(Text,Font),
    wxStaticText:setFont(Text1,Font1),
%%    wxStaticText:setLabel(Text,""),
    MainSizer = wxBoxSizer:new(?wxHORIZONTAL),

    UiSizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(MainSizer, Panel,[{flag,?wxEXPAND}]),
    wxSizer:add(UiSizer, Text,[{flag,?wxALL bor ?wxEXPAND},{border, 5}]),
    wxSizer:add(UiSizer, Text1,[{flag,?wxALL bor ?wxEXPAND},{border, 5}]),
    wxSizer:add(UiSizer, Button,[{flag,?wxALL bor ?wxEXPAND},{border, 5}]),
    wxSizer:add(UiSizer, Button1,[{flag,?wxALL bor ?wxEXPAND},{border, 5}]),
    wxSizer:add(UiSizer, Button2,[{flag,?wxALL bor ?wxEXPAND},{border, 5}]),
    wxSizer:add(MainSizer, UiSizer),

    % create bitmap to all images
    {BmpRmap,BmpB1Map,BmpB2Map,BmpB3Map,BmpPipeMap,BmpBaseMap,BmpLogoMap}=createBitMaps(),

    wxWindow:setSizer(Frame, MainSizer),
    wxSizer:setSizeHints(MainSizer, Frame),

    % connect panel
    wxFrame:show(Frame),
    erlang:send_after(?Timer, self(), timer),

    wxPanel:connect(Panel, paint, [callback]),
    wxFrame:connect(Frame, close_window),
    wxButton:connect(Button, command_button_clicked),
    wxButton:connect(Button1, command_button_clicked),
    wxButton:connect(Button2, command_button_clicked),
%%    wxWindow:connect(Panel, command_button_clicked),

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
        started = false,
        super_graphics = false,
        debug_const_pipe_list = Pipes,
        number_of_nn = N,
        pipes_state = #pipes_graphics_rec{visible_pipeList = [Pipe],extra_pipeList = Extras,used_pipeList = []},
        base_state = NewBase,text = Text,text1 = Text1,num_of_generations = 0, frame_count = 0,
        bmpRMap = BmpRmap,bmpB1Map = BmpB1Map,bmpB2Map = BmpB2Map,bmpB3Map = BmpB3Map,bmpPipeMap = BmpPipeMap,bmpBaseMap = BmpBaseMap,bmpLogoMap = BmpLogoMap,
        current_bird_list = []}}.

%%%-------------------------------------------------------------------

handle_event(#wx{id =ID,obj = _Button, event = #wxCommand{type = command_button_clicked}},State) ->
    io:format("Button clicked~p~n",[ID]),
    NewState = case ID of
        10 -> % Start
            case State#graphics_state.started of
                false -> initialize_system(State#graphics_state.number_of_nn,State#graphics_state.debug_const_pipe_list),
                         State#graphics_state{started = true,num_of_generations = 1};
                true  -> State
            end;
        11 -> State#graphics_state{active_graphics = not State#graphics_state.active_graphics};
        12 -> State#graphics_state{super_graphics = not State#graphics_state.super_graphics}

    end,
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
    NewState = State#graphics_state{ simulation_finished = true,num_of_generations = State#graphics_state.num_of_generations+1},
    {noreply, NewState};

handle_cast(Input,State)->
    io:format("no fitting cast function Input= ~p ~n State= ~p~n",[Input,State]),{noreply, State}.



handle_info({nodedown,OldPc,NewPc},State)-> % if a node is down, check which PC, move responsibilities to different PC and update monitors
    io:format("graphics, nodedown ~p~n",[NewPc]),
    put(OldPc,NewPc),
    {noreply, State};

% This Is the main Loop for the graphics
handle_info(timer, State=#graphics_state{frame = Frame,base_state = Base_location_rec,bird_queue = Bird_queue,pipes_state = Pipe_state,time = Time}) ->  % refresh screen for graphics
    wxWindow:refresh(Frame), % refresh screen
    case queue:is_empty(Bird_queue) of
        true ->
            if
                State#graphics_state.simulation_finished =:= true ->
                    PipeList = State#graphics_state.debug_const_pipe_list,
                    [H_pipe | T_pipes] =PipeList,
                    NewState = State#graphics_state{simulation_finished = false, pipes_state = #pipes_graphics_rec{visible_pipeList = [H_pipe], extra_pipeList = T_pipes, used_pipeList = []},frame_count = 0},
                    graphics_proxy ! {new_generation, round(State#graphics_state.number_of_nn / 4)},
                    rpc:cast(get(?PC1),pc_server,pc_rpc,[pc1,{run_generation, self(), PipeList}]),
                    rpc:cast(get(?PC2),pc_server,pc_rpc,[pc2,{run_generation, self(), PipeList}]),
                    rpc:cast(get(?PC3),pc_server,pc_rpc,[pc3,{run_generation, self(), PipeList}]),
                    rpc:cast(get(?PC4),pc_server,pc_rpc,[pc4,{run_generation, self(), PipeList}]),
                    gen_server:cast(learningFSM,{run_generation,self(),PipeList});

                true -> NewState = State
            end,
            Base_location_rec;
        false ->
            {{value,Bird_list},NewBirdQueue} = queue:out(Bird_queue),
             NewPipes = simulation:simulate_pipes(Pipe_state),
            case State#graphics_state.active_graphics of
                true -> NewBase  = #base_state{x1 = move_base(Base_location_rec#base_state.x1) , x2 = move_base(Base_location_rec#base_state.x2)};
                false -> NewBase = Base_location_rec
            end,

             NewState = State#graphics_state{current_bird_list =  Bird_list,pipes_state = NewPipes,time = Time+1,base_state = NewBase,bird_queue = NewBirdQueue,frame_count = State#graphics_state.frame_count+1}
    end,
    case State#graphics_state.active_graphics of
        true -> erlang:send_after(?Timer,self(),timer);
        false-> erlang:send_after(1,self(),timer) % when graphics is disabled you can run the update loop way faster
    end,
    {noreply, NewState}.

handle_sync_event(#wx{event=#wxPaint{}}, _,  State = #graphics_state{panel = Panel,current_bird_list = Bird_list,pipes_state = Pipes_state,base_state = Base_rec,collide = _Collide,time = Time, bmpRMap = BmpRmap,bmpB1Map = BmpB1Map,bmpB2Map = BmpB2Map,bmpB3Map = BmpB3Map,bmpPipeMap = BmpPipeMap,bmpBaseMap = BmpBaseMap}) ->
    DC2=wxPaintDC:new(Panel),
    wxDC:clear(DC2),
    wxDC:drawBitmap(DC2,BmpRmap,{0,0}),
    wxStaticText:setLabel(State#graphics_state.text,lists:append("Gen: ",lists:flatten(io_lib:format("~p", [State#graphics_state.num_of_generations])))),
    wxStaticText:setLabel(State#graphics_state.text1,lists:append("Frame Count= ",lists:flatten(io_lib:format("~p", [State#graphics_state.frame_count])))),
    case State#graphics_state.active_graphics of
        true->case State#graphics_state.started of
                  true ->[draw_bird(DC2,BmpB1Map,BmpB2Map,BmpB3Map,?BIRD_X_LOCATION,round(Y),Tilt,Time,State#graphics_state.super_graphics)||{_,#bird_graphics_rec{y=Y,angle = Tilt}}<- Bird_list],
                      [draw_pipe(DC2,BmpPipeMap,Pipe#pipe_rec.x,Pipe#pipe_rec.height)||Pipe <- Pipes_state#pipes_graphics_rec.visible_pipeList];
                  false -> ok
              end,
            wxDC:drawBitmap(DC2,State#graphics_state.bmpLogoMap,{round((?BG_WIDTH/2) - 306/2),50+round(math:sin(Time/10)*10)}),
            draw_base(DC2, BmpBaseMap, Base_rec#base_state.x1, Base_rec#base_state.x2);
        false->ok
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

terminate(_Reason, State = #graphics_state{}) ->
    io:format("killing graphics"),
    graphics_proxy!{kill,self()},
    rpc:cast(get(?PC1),pc_server,stop,[pc1]),
    rpc:cast(get(?PC2),pc_server,stop,[pc2]),
    rpc:cast(get(?PC3),pc_server,stop,[pc3]),
    rpc:cast(get(?PC4),pc_server,stop,[pc4]),
    gen_server:stop(learningFSM),
    unregister(graphics_proxy),
    wxFrame:destroy(State#graphics_state.frame).

% create bitmap to all images
createBitMaps() ->
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
    rpc:call(?GRAPHICS_NODE,graphics,graphics_rpc,[{bird_locations,Bird_list}]),
    case Next_N of
    0 -> io:format("All birds Dead waiting for next generation~n"),
        rpc:call(?GRAPHICS_NODE,graphics,graphics_rpc,[{new_generation}]),
        receive
            {new_generation,New_N}->graphics_reduce([],1,New_N,New_N)
        end;
        N -> graphics_reduce([],Frame_number+1,N,N)
    end;
graphics_reduce(Bird_List,Frame_number,N,Next_N)->
    receive
        {bird_update,_From,Frame_number,{Collide,Bird_graphics}}->
            New_Birdlist = [{Collide,Bird_graphics}|Bird_List],
            case Collide of
                true->  graphics_reduce(New_Birdlist,Frame_number,N-1,Next_N-1);
                false-> graphics_reduce(New_Birdlist,Frame_number,N-1,Next_N)
            end;
        {bird_update,_From,Number,{_Collide,_Bird_graphics}} when Number<Frame_number->
            graphics_reduce(Bird_List,Frame_number,N,Next_N);
        {kill,_From}->ok
%%    after 2000->
%%        io:format("message was missing from graphics. removing one bird. Frame: ~p Remaining:~p~n",[Frame_number,N]),
%%        %flush_messages(),
%%        graphics_reduce(Bird_List,Frame_number,N-1,Next_N-1)

    end.
graphics_rpc(Pass2Graphics)->
    wx_object:cast(graphics,Pass2Graphics).

graphics_reduce_rpc(Pass2GraphicsReduce)->
    graphics_proxy!Pass2GraphicsReduce.

append_atoms(Atom1,Atom2)->list_to_atom(lists:append(atom_to_list(Atom1),atom_to_list(Atom2))).

generate_map(Header,Keys,List_of_footers)->
    generate_map(Header,Keys,List_of_footers,#{}).
generate_map(_Header,[],[],Map_Acc)->Map_Acc;
generate_map(Header,[Key|KeyT],[Footer|FooterT],Map_Acc)->
    Map_Acc2 = maps:put(Key,append_atoms(Header,Footer),Map_Acc),
    generate_map(Header,KeyT,FooterT,Map_Acc2).