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
-compile(export_all).
-define(max_x, 1344).
-define(max_y,890).
-define(Timer,67).%67

-define(SERVER, ?MODULE).

-record(state, {frame, panel, dc, paint, list,
    simulation,collide = false,time = 0,
    bmpRMap,bmpB1Map,bmpB2Map,bmpB3Map,bmpB4Map,bmpPipeMap,bmpBaseMap}).
%%%-------------------------------------------------------------------
start2() ->
    lists:foreach(fun test_ets/1,
        [set, ordered_set, bag, duplicate_bag]).
test_ets(Mode) ->
    TableId = ets:new(test, [set]),
    ets:insert(TableId, {a,1}),
    ets:insert(TableId, {b,2}),
    ets:insert(TableId, {a,1}),
    ets:insert(TableId, {a,3}),
    TableId2 = ets:new(test2, [set]),
    ets:insert(TableId2, {c,TableId}),
    List = ets:tab2list(TableId2),
    io:format("~-13w => ~p~n", [Mode, List]),
    ets:delete(TableId).

test_loop(Wx_pid)->
    io:format("wx pid hopefully: ~p~n",[Wx_pid]),
    Wx_pid!hello_world.
start3()->
    {_,_,_,Wx_pid} = wx_object:start({local,?SERVER},?MODULE,[],[]),
    PID = spawn(fun()->test_loop(Wx_pid) end).
start() ->
    A = wx_object:start({local,?SERVER},?MODULE,[],[]),
    io:format("Look here: ~p~n",[A]).



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
    Pipes = [#pipe_rec{
        height = 200,
        x = 550,
        passed = false}],
    Extras = [#pipe_rec{
        height = 400,
        x = 550,
        passed = false},
    #pipe_rec{
        height = 150,
        x = 550,
        passed = false},
    #pipe_rec{
        height = 500,
        x = 550,
        passed = false}],
    SimState = #sim_state{
        tick_time = 1,
        bird = #bird_rec{
            y = ?BIRD_Y_LOCATION,
            vel = 0,
            angle = 0,
            jump_height = 0},
        visible_pipeList = Pipes,
        extra_pipeList = Extras,
        used_pipeList = []},


    {Frame,#state{frame = Frame, panel = Panel, dc=DC, paint = Paint,simulation = SimState, bmpRMap = BmpRmap,bmpB1Map = BmpB1Map,bmpB2Map = BmpB2Map,bmpB3Map = BmpB3Map,bmpPipeMap = BmpPipeMap,bmpBaseMap = BmpBaseMap}}.

%%%-------------------------------------------------------------------

handle_event(#wx{event = #wxClose{}},State = #state {frame = Frame}) -> % close window event
    io:format("Exiting\n"),
    wxWindow:destroy(Frame),
    wx:destroy(),
    {stop,normal,State};

handle_event(hello_world,State = #state {frame = Frame}) -> % close window event
    io:format("YES!!!!\n"),
    wxWindow:destroy(Frame),
    wx:destroy(),
    {stop,normal,State}.
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
angle2radians(Angle)->Angle*math:pi()*2/360.
%%handle_event(#wx{event = #wxMouse{type=left_down, x=X, y=Y}},State) -> % when left click has been pressed, activate navigation function
%%    spawn(main,main_navigation,[X,Y,get(?PC1),get(?PC2),get(?PC3),get(?PC4)]), % spawn the process in the corresponding PC
%%    {noreply,State};
%%
%%handle_event(#wx{event = #wxMouse{type=right_down, x=X, y=Y}},State) -> % when the right click was pressed, activate a function that returns the color of the light that was pressed
%%    if
%%        X >= 780, Y =< 545 -> rpc:call(get(?PC1),server,print_light,[X,Y]);
%%        X >= 780, Y >= 545 -> rpc:call(get(?PC4),server,print_light,[X,Y]);
%%        X =< 780, Y =< 545 -> rpc:call(get(?PC2),server,print_light,[X,Y]);
%%        X =< 780, Y >= 545 -> rpc:call(get(?PC3),server,print_light,[X,Y]);
%%        true -> error
%%    end,
%%    {noreply,State}.

handle_info(timer, State=#state{frame = Frame,simulation = Simulation,time = Time}) ->  % refresh screen for graphics
    wxWindow:refresh(Frame), % refresh screen
    erlang:send_after(?Timer,self(),timer),
    %NewState = State#state{simulation = State#state.simulation#sim_state{bird = State#state.simulation#sim_state.bird#bird_rec{y=State#state.simulation#sim_state.bird#bird_rec.y-1}}},
    {Collide, NewSimulationState2} = case Simulation#sim_state.tick_time =:= 13 of
        true ->  {Collide,NewSimulationState} = simulation:simulate_a_frame(Simulation,true);
        false->  {Collide,NewSimulationState} = simulation:simulate_a_frame(Simulation,false)
    end,

    %{_,NewSimulationState} = simulation:simulate_a_frame2(Simulation,false),
    io:format("NewSimulationState: ~p~n",[NewSimulationState]),

    NewState =State#state{simulation = NewSimulationState2,collide = Collide,time = Time+1},
    {noreply, NewState}.

handle_sync_event(#wx{event=#wxPaint{}}, _,  _State = #state{panel = Panel,simulation = SimState,collide = Collide,time = Time, bmpRMap = BmpRmap,bmpB1Map = BmpB1Map,bmpB2Map = BmpB2Map,bmpB3Map = BmpB3Map,bmpPipeMap = BmpPipeMap,bmpBaseMap = BmpBaseMap}) ->
    DC2=wxPaintDC:new(Panel),
    wxDC:clear(DC2),
    io:format("It's a me ~p~n",[SimState#sim_state.visible_pipeList]),
    wxDC:drawBitmap(DC2,BmpRmap,{0,0}),
    wxDC:drawBitmap(DC2,BmpBaseMap,{0,?BG_HEIGHT - ?BASE_HEIGHT}),
    %wxDC:drawBitmap(DC2,BmpB1Map,{?BIRD_X_LOCATION,round(SimState#sim_state.bird#bird_rec.y)}),
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


%%printsmoke('$end_of_table',_,_) -> ok; % this function paints the smoke after an accident
%%printsmoke(Key,Panel,BmpSmoke) ->
%%    DI =wxClientDC:new(Panel),
%%    case ets:member(smoke,Key) of
%%        true -> [{_,{A,B}}] = ets:lookup(smoke,Key),
%%            wxDC:drawBitmap(DI, BmpSmoke, {A - 10, B - 10}),
%%            printsmoke(ets:next(smoke,Key),Panel,BmpSmoke);
%%        _-> printsmoke(ets:first(smoke),Panel,BmpSmoke)
%%    end.
%%
%%printCars('$end_of_table',_,_,_,_,_,_,_) -> ok; % this function paints all of the cars
%%printCars(Key,Panel,BmpCar1,BmpCar2,BmpCar3,BmpCar1b,BmpCar2b,BmpCar3b) ->
%%    [{_,[{A,B},D,_,Type,_],_,_,_,_,_,Nav}] = ets:lookup(cars,Key),
%%    DI =wxClientDC:new(Panel),
%%    case Type of
%%        red -> case D of
%%                   left -> case Nav of
%%                               null-> wxDC:drawBitmap(DI, BmpCar1, {A, B}); % if car navigation status is null, print the original color
%%                               _->   wxDC:drawBitmap(DI, BmpCar1b, {A, B}) % if car is navigating, print blue color
%%                           end;
%%
%%                   down ->case Nav of
%%                              null-> Im = wxBitmap:convertToImage(BmpCar1), Im2 = wxImage:rotate(Im,-300,{A,B}), % if direction is not left, rotate the image
%%                                  BitIm = wxBitmap:new(Im2), wxDC:drawBitmap(DI, BitIm, {A, B});
%%                              _-> Im = wxBitmap:convertToImage(BmpCar1b), Im2 = wxImage:rotate(Im,-300,{A,B}),
%%                                  BitIm = wxBitmap:new(Im2), wxDC:drawBitmap(DI, BitIm, {A, B})
%%                          end;
%%
%%                   right ->case Nav of
%%                               null-> Im = wxBitmap:convertToImage(BmpCar1), Im2 = wxImage:rotate(Im,600,{A,B}),% if direction is not left, rotate the image
%%                                   BitIm = wxBitmap:new(Im2), wxDC:drawBitmap(DI, BitIm, {A, B});
%%                               _->  Im = wxBitmap:convertToImage(BmpCar1b), Im2 = wxImage:rotate(Im,600,{A,B}),
%%                                   BitIm = wxBitmap:new(Im2), wxDC:drawBitmap(DI, BitIm, {A, B})
%%                           end;
%%
%%                   up ->case Nav of
%%                            null-> Im = wxBitmap:convertToImage(BmpCar1), Im2 = wxImage:rotate(Im,300,{A,B}),% if direction is not left, rotate the image
%%                                BitIm = wxBitmap:new(Im2), wxDC:drawBitmap(DI, BitIm, {A, B});
%%                            _-> Im = wxBitmap:convertToImage(BmpCar1b), Im2 = wxImage:rotate(Im,300,{A,B}),
%%                                BitIm = wxBitmap:new(Im2), wxDC:drawBitmap(DI, BitIm, {A, B})
%%                        end;
%%
%%
%%                   _ ->  ets:delete(cars,Key)
%%               end;
%%        grey -> case D of
%%                    left -> case Nav of
%%                                null-> wxDC:drawBitmap(DI, BmpCar2, {A, B});
%%                                _->   wxDC:drawBitmap(DI, BmpCar2b, {A, B})
%%                            end;
%%
%%                    down ->case Nav of
%%                               null-> Im = wxBitmap:convertToImage(BmpCar2), Im2 = wxImage:rotate(Im,-300,{A,B}),% if direction is not left, rotate the image
%%                                   BitIm = wxBitmap:new(Im2), wxDC:drawBitmap(DI, BitIm, {A, B});
%%                               _-> Im = wxBitmap:convertToImage(BmpCar2b), Im2 = wxImage:rotate(Im,-300,{A,B}),
%%                                   BitIm = wxBitmap:new(Im2), wxDC:drawBitmap(DI, BitIm, {A, B})
%%                           end;
%%
%%                    right ->case Nav of
%%                                null-> Im = wxBitmap:convertToImage(BmpCar2), Im2 = wxImage:rotate(Im,600,{A,B}),% if direction is not left, rotate the image
%%                                    BitIm = wxBitmap:new(Im2), wxDC:drawBitmap(DI, BitIm, {A, B});
%%                                _->  Im = wxBitmap:convertToImage(BmpCar2b), Im2 = wxImage:rotate(Im,600,{A,B}),
%%                                    BitIm = wxBitmap:new(Im2), wxDC:drawBitmap(DI, BitIm, {A, B})
%%                            end;
%%
%%                    up ->case Nav of
%%                             null-> Im = wxBitmap:convertToImage(BmpCar2), Im2 = wxImage:rotate(Im,300,{A,B}),% if direction is not left, rotate the image
%%                                 BitIm = wxBitmap:new(Im2), wxDC:drawBitmap(DI, BitIm, {A, B});
%%                             _-> Im = wxBitmap:convertToImage(BmpCar2b), Im2 = wxImage:rotate(Im,300,{A,B}),
%%                                 BitIm = wxBitmap:new(Im2), wxDC:drawBitmap(DI, BitIm, {A, B})
%%                         end;
%%
%%
%%                    _ -> ets:delete(cars,Key)
%%                end;
%%        yellow ->  case D of
%%                       left -> case Nav of
%%                                   null-> wxDC:drawBitmap(DI, BmpCar3, {A, B});
%%                                   _->   wxDC:drawBitmap(DI, BmpCar3b, {A, B})
%%                               end;
%%
%%                       down ->case Nav of
%%                                  null-> Im = wxBitmap:convertToImage(BmpCar3), Im2 = wxImage:rotate(Im,-300,{A,B}),% if direction is not left, rotate the image
%%                                      BitIm = wxBitmap:new(Im2), wxDC:drawBitmap(DI, BitIm, {A, B});
%%                                  _-> Im = wxBitmap:convertToImage(BmpCar3b), Im2 = wxImage:rotate(Im,-300,{A,B}),
%%                                      BitIm = wxBitmap:new(Im2), wxDC:drawBitmap(DI, BitIm, {A, B})
%%                              end;
%%
%%                       right ->case Nav of
%%                                   null-> Im = wxBitmap:convertToImage(BmpCar3), Im2 = wxImage:rotate(Im,600,{A,B}),% if direction is not left, rotate the image
%%                                       BitIm = wxBitmap:new(Im2), wxDC:drawBitmap(DI, BitIm, {A, B});
%%                                   _->  Im = wxBitmap:convertToImage(BmpCar3b), Im2 = wxImage:rotate(Im,600,{A,B}),
%%                                       BitIm = wxBitmap:new(Im2), wxDC:drawBitmap(DI, BitIm, {A, B})
%%                               end;
%%
%%                       up ->case Nav of
%%                                null-> Im = wxBitmap:convertToImage(BmpCar3), Im2 = wxImage:rotate(Im,300,{A,B}),% if direction is not left, rotate the image
%%                                    BitIm = wxBitmap:new(Im2), wxDC:drawBitmap(DI, BitIm, {A, B});
%%                                _-> Im = wxBitmap:convertToImage(BmpCar3b), Im2 = wxImage:rotate(Im,300,{A,B}),
%%                                    BitIm = wxBitmap:new(Im2), wxDC:drawBitmap(DI, BitIm, {A, B})
%%                            end;
%%
%%
%%                       _ -> ets:delete(cars,Key)
%%                   end;
%%        _ ->  ets:delete(cars,Key)
%%    end,
%%    case ets:member(cars,Key) of
%%        true -> printCars(ets:next(cars,Key),Panel,BmpCar1,BmpCar2,BmpCar3,BmpCar1b,BmpCar2b,BmpCar3b);
%%        _ -> ok
%%    end.
%%
%%
%%
%%handle_info(timer, State=#state{frame = Frame}) ->  % refresh screen for graphics
%%
%%    spawn(main,update_ets,[get(?PC1),?Home]), % spawn update ets functions
%%    spawn(main,update_ets,[get(?PC2),?Home]),
%%    spawn(main,update_ets,[get(?PC3),?Home]),
%%    spawn(main,update_ets,[get(?PC4),?Home]),
%%
%%    wxWindow:refresh(Frame), % refresh screen
%%    erlang:send_after(?Timer,self(),timer),
%%    {noreply, State};
%%
%%handle_info({nodeup,PC},State)->
%%    io:format("~p nodeup ~n",[PC]),
%%    {noreply, State};
%%
%%handle_info({nodedown,PC},State)-> % if a node is down, check which PC, move responsibilities to different PC and update monitors
%%    io:format("~p nodedown ~n",[PC]),
%%    case PC of
%%        ?PC1 -> backup_pc(?PC1,get(?PC2)),
%%            rpc:call(get(?PC2),server,update_monitor,[pc_1]), % update the monitor in the others PCs that node is down
%%            rpc:call(get(?PC3),server,update_monitor,[pc_1]),
%%            rpc:call(get(?PC4),server,update_monitor,[pc_1]),
%%            move_car(?PC1,ets:first(cars))     ;
%%
%%        ?PC2 ->backup_pc(?PC2,get(?PC3)),
%%            rpc:call(get(?PC1),server,update_monitor,[pc_2]),% update the monitor in the others PCs that node is down
%%            rpc:call(get(?PC3),server,update_monitor,[pc_2]),
%%            rpc:call(get(?PC4),server,update_monitor,[pc_2]),
%%            move_car(?PC2,ets:first(cars));
%%
%%        ?PC3 ->backup_pc(?PC3,get(?PC4)),
%%            rpc:call(get(?PC2),server,update_monitor,[pc_3]),% update the monitor in the others PCs that node is down
%%            rpc:call(get(?PC1),server,update_monitor,[pc_3]),
%%            rpc:call(get(?PC4),server,update_monitor,[pc_3]),
%%            move_car(?PC3,ets:first(cars));
%%
%%        ?PC4 ->backup_pc(?PC4,get(?PC1)),
%%            rpc:call(get(?PC2),server,update_monitor,[pc_4]),% update the monitor in the others PCs that node is down
%%            rpc:call(get(?PC3),server,update_monitor,[pc_4]),
%%            rpc:call(get(?PC1),server,update_monitor,[pc_4]),
%%            move_car(?PC4,ets:first(cars))
%%
%%    end,
%%    {noreply, State}.
%%
%%handle_cast({delete_car, Pid},State) -> % delete car from ets
%%    ets:delete(cars,Pid),
%%    {noreply,State};
%%
%%handle_cast({smoke,Car1,L1,Car2,L2},State) -> % insert smoke location to the ets
%%    ets:insert(smoke,{Car1,L1}),
%%    ets:insert(smoke,{Car2,L2}),
%%    {noreply,State};
%%
%%
%%handle_cast({del_smoke,_},State) -> % delete smoke from ets
%%    ets:delete_all_objects(smoke),
%%    {noreply,State}.


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


% this function requests the ets from each server and calls to function to update ets
%%update_ets(PC,Home) ->
%%    List=
%%        try
%%            rpc:call(PC,server,update_car_location,[]) % call update car location function in server to get list of cars and locations
%%        catch _:_ -> problem
%%        end,
%%    case List of
%%        {ok, List1} -> list_to_ets(List1); % if the list returned normally, convert it to ets
%%        Else-> io:format("there is a problem~n"),io:format("~p~n",[Else]),
%%            Res = net_adm:ping(PC), % else, check if relevant pc is connected
%%            case Res of
%%                pong -> rpc:call(PC, erlang, disconnect_node, [Home]); % if it is, disconnect it
%%                _-> ok
%%            end,
%%            ok
%%    end.
%%
%%% this function adds the ets received to main ets
%%list_to_ets('$end_of_table') ->
%%    ok;
%%list_to_ets(List) ->
%%    lists:foreach(fun(Key_Value) -> ets:insert(cars, Key_Value) end, List).
%%
%%
%%delete_car(Pid) -> wx_object:cast(main,{delete_car, Pid}). % create delete car event
%%start_smoke(Car1,L1,Car2,L2) -> wx_object:cast(main,{smoke,Car1,L1,Car2,L2}).% create start smoke event
%%del_smoke(Pid) -> wx_object:cast(main,{del_smoke,Pid}).% create delete smoke event
%%
%%% this function checks which PC is down and moves all car from fallen PC to new PC
%%move_car(_,'$end_of_table') -> ok;
%%move_car(PcDown,Key) ->
%%    [{_,Location,Name,Start,Type,Con,PC,Nav}] = ets:lookup(cars,Key),
%%
%%    case PcDown of
%%        ?PC1 -> if
%%                    PC == ?PC1  -> rpc:call(get(?PC2),server,moved_car,[Name,Type,Start,Location,Con,get(?PC2),Nav]), % call to function in server
%%                        Next = ets:next(cars,Key), % get next car
%%                        ets:delete(cars,Key), % delete car from ets
%%                        move_car(PcDown,Next) ; % move next car
%%                    true ->move_car(PcDown,ets:next(cars,Key))
%%                end;
%%
%%        ?PC2 -> if
%%
%%                    PC == ?PC2 -> rpc:call(get(?PC3),server,moved_car,[Name,Type,Start,Location,Con,get(?PC3),Nav]),
%%                        Next = ets:next(cars,Key),
%%                        ets:delete(cars,Key),
%%                        move_car(PcDown,Next);
%%                    true -> move_car(PcDown,ets:next(cars,Key))
%%                end;
%%
%%        ?PC3 -> if
%%
%%                    PC == ?PC3 -> rpc:call(get(?PC4),server,moved_car,[Name,Type,Start,Location,Con,get(?PC4),Nav]),
%%                        Next = ets:next(cars,Key),
%%                        ets:delete(cars,Key),
%%                        move_car(PcDown,Next) ;
%%                    true -> move_car(PcDown,ets:next(cars,Key))
%%                end;
%%
%%        ?PC4 -> if
%%
%%                    PC == ?PC4 -> rpc:call(get(?PC1),server,moved_car,[Name,Type,Start,Location,Con,get(?PC1),Nav]),
%%                        Next = ets:next(cars,Key),
%%                        ets:delete(cars,Key),
%%                        move_car(PcDown,Next) ;
%%                    true -> move_car(PcDown,ets:next(cars,Key))
%%                end
%%    end.
%%
%%% this function which PCs are combined with the fallen PC and backs them up
%%backup_pc(PCDown,NewPC) ->
%%    L = [?PC1,?PC2,?PC3,?PC4],
%%    L2 = [PC||PC <-L, get(PC) == PCDown], % make list of all PCs that are combined with the fallen PC
%%    Fun = fun(E) -> put(E,NewPC) end,
%%    lists:foreach(Fun,L2), ok. % combine said PCs with the backup PC
%%
%%%this function is the navigation system
%%main_navigation(X,Y,PC1,_,_,_) ->
%%    Result =  check_cars(ets:first(cars)), % check if there is a car that was already selected
%%    case Result of
%%        null -> %  in case there isn't a car that is already selected, pick the closest car
%%            search_close_car(ets:first(cars),{X,Y});
%%
%%        % in case there isn't a car that already selected, search a close junction
%%        Pid -> io:format("Pid was found: ~p~n",[Pid]), Result2 = rpc:call(PC1,server,server_search_close_junc,[X,Y]),
%%
%%            case Result2 of
%%                null -> io:format("error in navigation, cant find close junction");
%%                Dest ->io:format("Dest was found: ~p~n",[Dest]), [{_,[_,_,_,_,_],_,_,_,_,PC,_}] = ets:lookup(cars,Pid), % in case there is a close junction update the car destination
%%                    rpc:call(PC,server,update_car_nav,[Pid,Dest])
%%
%%            end
%%    end,
%%    ok.
%%
%%% this function checks if there is a close car and if so it changes the navigation element in the ets
%%search_close_car('$end_of_table',_)  -> io:format("error in Nav, cant find close car~n");
%%search_close_car(Key,{X,Y}) ->
%%    Ans = ets:member(cars,Key),
%%    if % check if car is in ets
%%        Ans == true -> [{_,[{X2,Y2},_,_,_,_],_,_,_,_,PC,_}] = ets:lookup(cars,Key), % if so, take its coordinates and PC
%%            Next = ets:next(cars,Key) ;
%%        true-> {X2,Y2} = {0,0} ,Next = ets:first(cars), PC = null, % else, start function with first car in ets
%%            search_close_car(ets:first(cars),{X,Y})
%%    end,
%%    D = math:sqrt(math:pow(X-X2,2) + math:pow(Y-Y2,2)), % check the distance of the car from the click
%%    if
%%        D =< 70 -> rpc:call(PC,ets,update_element,[cars,Key,[{8,in_process}]]); % if the distance is small enough, call relevant PC to update the nav element in ets
%%        true -> search_close_car(Next,{X,Y}) % else, check next car
%%    end.
%%
%%%this function check if there is a car that already selected
%%check_cars('$end_of_table') -> io:format("there is no car in process ~n"), null;
%%check_cars(Key) ->  [{_,[_,_,_,_,_],_,_,_,_,_,Nav}] = ets:lookup(cars,Key),
%%    case Nav of % check if car has already been selected
%%        in_process -> Key; % if so, return its pid
%%        _-> check_cars(ets:next(cars,Key)) % else, check next car
%%    end.
%%
%%% this function goes over the car ets and checks whether the car processes are still alive
%%main_cars_mon('$end_of_table',PC1,PC2,PC3,PC4) -> main_cars_mon(ets:first(cars),PC1,PC2,PC3,PC4);
%%main_cars_mon(Key,PC1,PC2,PC3,PC4)->
%%
%%    Ans = ets:member(cars,Key), % check if the car is in ets and get next element in ets
%%    if
%%        Ans == true -> Next = ets:next(cars,Key);
%%        true -> Next = ets:first(cars),
%%            main_cars_mon(ets:first(cars),PC1,PC2,PC3,PC4)
%%    end,
%%    [{_,_,Name,_,_,_,PC,_}] = ets:lookup(cars,Key),
%%    Res =  check_PC(Key,PC,PC1,PC2,PC3,PC4), % check if car is alive in its PC
%%    case Res of
%%        {_,true} -> main_cars_mon(Next,PC1,PC2,PC3,PC4); % if it is, check next car
%%        {PC,false}-> % if it isn't, check if the car is still in ets
%%            timer:sleep(2000),
%%
%%            case ets:member(cars,Key) of
%%                true -> rpc:call(PC,server,deleteCar,[Key]), % if it is, delete it and start a new car
%%
%%                    case PC of
%%                        PC1 -> main_search_close_car(ets:first(cars),{1344,93}), rpc:call(PC,server,start_car,[Name,20,[{1344,93},left,r1,yellow,st],PC]);
%%                        PC2 -> main_search_close_car(ets:first(cars),{0,417}), rpc:call(PC,server,start_car,[Name,10,[{0,417},right,r3,red,st],PC]);
%%                        PC3 -> main_search_close_car(ets:first(cars),{0,651}),rpc:call(PC,server,start_car,[Name,20,[{0,651},right,r9,grey,st],PC]);
%%                        PC4 -> main_search_close_car(ets:first(cars),{0,651}), rpc:call(PC,server,start_car,[Name,20,[{0,651},right,r9,grey,st],PC])
%%                    end,
%%                    main_cars_mon(Next,PC1,PC2,PC3,PC4);
%%                _ -> main_cars_mon(Next,PC1,PC2,PC3,PC4) % if it isn't, check next car
%%            end;
%%        _ -> main_cars_mon(ets:first(cars),PC1,PC2,PC3,PC4)
%%    end.
%%
%%% this function checks if a car is alive on its PC
%%check_PC(Key,PC_to_check,PC1,PC2,PC3,PC4) ->
%%    Res = net_adm:ping(PC_to_check), % check if the PC the car is on is alive
%%    case Res of
%%        pong -> case PC_to_check of % if it is, call to check if the car process is alive
%%                    PC1 -> {PC1,rpc:call(PC1, erlang, is_process_alive, [Key])};
%%                    PC2 -> {PC2,rpc:call(PC2, erlang, is_process_alive, [Key])};
%%                    PC3 -> {PC3,rpc:call(PC3, erlang, is_process_alive, [Key])};
%%                    PC4 -> {PC4,rpc:call(PC4, erlang, is_process_alive, [Key])}
%%
%%
%%                end;
%%
%%        _-> case PC_to_check of % if the PC is not alive, check the backup PC
%%                PC1 -> check_PC(Key,PC2,PC1,PC2,PC3,PC4);
%%                PC2 -> check_PC(Key,PC3,PC1,PC2,PC3,PC4);
%%                PC3 -> check_PC(Key,PC4,PC1,PC2,PC3,PC4);
%%                PC4 -> check_PC(Key,PC1,PC1,PC2,PC3,PC4)
%%            end
%%    end.
%%
%%% this function searches a close car, if there is a close car, the function waits 0.5 sec and start over
%%main_search_close_car('$end_of_table',_)  -> ok;
%%main_search_close_car(Key,{X,Y}) ->
%%    Ans = ets:member(cars,Key),
%%    if
%%        Ans == true -> [{_,[{X2,Y2},_,_,_,_],_,_,_,_,_,_}] = ets:lookup(cars,Key),
%%            Next = ets:next(cars,Key) ;
%%        true-> {X2,Y2} = {0,0} ,Next = ets:first(cars),
%%            main_search_close_car(ets:first(cars),{X,Y})
%%    end,
%%    D = math:sqrt(math:pow(X-X2,2) + math:pow(Y-Y2,2)),
%%    if
%%        D =< 100 -> timer:sleep(500),main_search_close_car(Key,{X,Y});
%%        true -> main_search_close_car(Next,{X,Y})
%%    end.