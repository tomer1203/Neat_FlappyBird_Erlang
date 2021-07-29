%%%-------------------------------------------------------------------
%%% @author tomer
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Jul 2021 15:13
%%%-------------------------------------------------------------------
-module(testing_wx).
-author("tomer").

%% API
-export([]).
-include_lib("wx/include/wx.hrl").

-export([test/0]).

-define(max_x, 1344).
-define(max_y,890).

-export([start/0, handleButtonStart/2]).

-record(data, {env, file}).

%% Will get the pid of server
%% will send the information on button pressing
start() ->

  %%Frame and components build
  WX = wx:new(),
  Frame = wxFrame:new(wx:null(), 1, "Top Frame"),
  TopTxt = wxStaticText:new(Frame, ?wxID_ANY, "Analog Neuron final Project"), %%?wxID_ANY
  Button = zxw:png_button(Frame, 7192, "bg.png"),
%%  when WxParent      :: wx:wx_object(),
%%  ID            :: integer(),
%%  ImageFilePath :: string(),
%%  Button        :: wx:wx_object().

  %L Components
%%  TextConfiguration = wxStaticText:new(Frame, ?wxID_ANY, "Program Configuration"), %%?wxID_ANY
%%  TextSetNumNeurons = wxStaticText:new(Frame, ?wxID_ANY, "Enter number of Neurons per Layer"), %%?wxID_ANY
%%  TextCtrlNeurons = wxTextCtrl:new(Frame, ?wxID_ANY,  [{value, "example:4 3 6 7"}]),
%%  ButtonBuild = wxButton:new(Frame, ?wxID_ANY, [{label, "Build"}]), %{style, ?wxBU_LEFT}
%%  FilePickerInput = wxFilePickerCtrl:new(Frame, ?wxID_ANY),
%%  ButtonStart = wxButton:new(Frame, ?wxID_ANY, [{label, "Start"}]),

  %Buttons
%%  wxButton:connect(ButtonStart, command_button_clicked, [{callback, fun handleButtonStart/2}, {userData, #data{env = wx:get_env(), file=FilePickerInput}}]),
  %R Components
%%  TextNet = wxStaticText:new(Frame, ?wxID_ANY, "Net Description"), %%?wxID_ANY

  %% panel for picture
  Panel = wxPanel:new(Frame),
  %% bitmap
%%  PictureDraw = wxImage:new("bg.png"),
%%  Picture = wxBitmap:new(PictureDraw),
%%  wxPanel:connect(Panel, paint, [{callback,fun(WxData, _)->panelPictureUpdate(Picture, WxData)end}]),


  %3 Components
%%  TextOutput = wxStaticText:new(Frame, ?wxID_ANY, "Program Output"), %%?wxID_ANY



  %%Font set
%%  Font = wxFont:new(20, ?wxFONTFAMILY_ROMAN, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_NORMAL),
%%  wxTextCtrl:setFont(TopTxt, Font),
%%  Font2 = wxFont:new(18, ?wxFONTFAMILY_ROMAN, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_NORMAL),
%%  wxTextCtrl:setFont(TextConfiguration, Font2),
%%  wxTextCtrl:setFont(TextOutput, Font2),
%%  wxTextCtrl:setFont(TextNet, Font2),
%%
%%  Font3 = wxFont:new(12, ?wxFONTFAMILY_ROMAN, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_NORMAL),
%%  wxTextCtrl:setFont(TextSetNumNeurons, Font3),


  %%Sizer Attachment
%%  MainSizer = wxBoxSizer:new(?wxVERTICAL),
%%  MainSizer2 = wxBoxSizer:new(?wxHORIZONTAL),
%%  MainSizerL = wxBoxSizer:new(?wxVERTICAL),
%%  MainSizerR = wxBoxSizer:new(?wxVERTICAL),
%%  MainSizer3 = wxBoxSizer:new(?wxVERTICAL),
%%
%%  wxSizer:add(MainSizer, TopTxt, [{flag, ?wxALIGN_TOP bor ?wxALIGN_CENTER}, {border, 5}]),
%%  wxSizer:add(MainSizer, MainSizer2), %,[{flag, ?wxALIGN_CENTER}]),
%%  wxSizer:add(MainSizer, MainSizer3),
%%  wxSizer:add(MainSizer2, MainSizerL, [{border, 5}]),%{flag, ?wxALIGN_LEFT},
%%  wxSizer:add(MainSizer2, MainSizerR, [{border, 5}]),%{flag, ?wxALIGN_RIGHT},

  %% Assign to L
%%  lists:foreach(fun(X)-> wxSizer:add(MainSizerL, X, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]) end,
%%    [TextConfiguration, TextSetNumNeurons, TextCtrlNeurons, ButtonBuild, FilePickerInput, ButtonStart]),
  %wxSizer:add(MainSizerL, TextConfiguration, [{flag, ?wxALL bor ?wxEXPAND}, {border, 5}]),
  %wxSizer:add(MainSizerL, TextSetNumNeurons, [{flag, ?wxALL bor ?wxEXPAND}, {border, 5}]),
  %wxSizer:add(MainSizerL, TextCtrlL, [{flag, ?wxALL bor ?wxEXPAND}, {border, 5}]),

  %% Assign to R
%%  wxSizer:add(MainSizerR, TextNet, [{flag, ?wxALL bor ?wxALIGN_CENTRE }, {border, 8}]),
%%  wxSizer:add(MainSizerR, Panel, [{flag, ?wxEXPAND}]),%, {proportion, 1}, {border, 8}]),

  %% Assign to 3
%%  wxSizer:add(MainSizer3, TextOutput, [{flag, ?wxALL bor ?wxALIGN_CENTRE }, {border, 8}]),


%%  wxWindow:setSizer(Frame, MainSizer),
  %%Show Frame
  wxFrame:show(Frame).

handleButtonStart(WxData,_)->
  %Get the userdata
  Data=WxData#wx.userData,
  wx:set_env(Data#data.env),
  FilePicker = Data#data.file,
  %Use the info
  Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Print"),
  Text=io_lib:format("The file is: ~p~n", [wxFilePickerCtrl:getPath(FilePicker)]),
  wxStaticText:new(Frame, ?wxID_ANY, Text),
  wxFrame:show(Frame).
% upload the picture to the panel
panelPictureUpdate(Picture, #wx{obj =Panel} ) ->
  %% display picture
  DC = wxPaintDC:new(Panel),
  wxDC:drawBitmap(DC, Picture, {0,0}),
  wxPaintDC:destroy(DC),
  ok.
test()->
  WxServer = wx:new(),
  Frame = wxFrame:new(WxServer, ?wxID_ANY, "MAP", [{size,{?max_x, ?max_y}}]),
  Panel  = wxPanel:new(Frame),
  %DC=wxPaintDC:new(Panel),
  %Paint = wxBufferedPaintDC:new(Panel),
  wxImage:initStandardHandlers(),
  BmpRmap=createBitMaps(),
  wxFrame:show(Frame),
  %wxPanel:connect(Panel, paint, [callback]),
  %wxPanel:connect (Panel, left_down),
  %wxPanel:connect (Panel, right_down),
  %wxFrame:connect(Frame, close_window),
  DC2=wxPaintDC:new(Panel),
  wxDC:clear(DC2),
  wxDC:drawBitmap(DC2,BmpRmap,{0,0}).

createBitMaps() ->         % create bitmap to all images
  Rmap = wxImage:new("bg.png"),
  Rmapc = wxImage:scale(Rmap,?max_x,?max_y),
  BmpRmap = wxBitmap:new(Rmapc),
  wxImage:destroy(Rmap),
  wxImage:destroy(Rmapc),BmpRmap.
