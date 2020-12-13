-module(cauder_wx_map).

%% API
-export([create/1, update/2]).

-include_lib("wx/include/wx.hrl").
-include("cauder.hrl").
-include("cauder_wx.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Creates the <i>map info</i> panel and populates it.

-spec create(Parent) -> Window when
  Parent :: wxWindow:wxWindow(),
  Window :: wxWindow:wxWindow().

create(Parent) ->
  Win = wxPanel:new(Parent),

  Sizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Win, [{label, "Map Info"}]),
  wxWindow:setSizer(Win, Sizer),

  Expand = [{proportion, 1}, {flag, ?wxEXPAND}],



  LeftPanel = wxPanel:new(Win),
  wxBoxSizer:add(Sizer, LeftPanel, Expand),

  LeftSizer = wxBoxSizer:new(?wxHORIZONTAL),
  wxPanel:setSizer(LeftPanel, LeftSizer),

  wxBoxSizer:add(LeftSizer, create_map(LeftPanel), Expand),

  % -----

  wxBoxSizer:addSpacer(Sizer, ?SPACER_SMALL),

  % -----

  RightPanel = wxPanel:new(Win),
  wxBoxSizer:add(Sizer, RightPanel, Expand),

  RightSizer = wxBoxSizer:new(?wxVERTICAL),
  wxPanel:setSizer(RightPanel, RightSizer),

  wxBoxSizer:add(RightSizer, create_history_map(RightPanel), Expand),

  Win.


%%------------------------------------------------------------------------------
%% @doc Updates the <i>map info</i> panel according to the given new state,
%% by comparing it with the given old state.

-spec update(OldState, NewState) -> ok when
  OldState :: cauder_wx:state(),
  NewState :: cauder_wx:state().

update(OldState, NewState) ->
  update_map(OldState, NewState),
  update_history_map(OldState, NewState).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-spec create_map(Parent) -> Window when
  Parent :: wxWindow:wxWindow(),
  Window :: wxWindow:wxWindow().

create_map(Parent) ->
  Win = wxPanel:new(Parent),

  Sizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Win, [{label, "Map"}]),
  wxPanel:setSizer(Win, Sizer),

  MapArea = wxListBox:new(Win, ?SYSTEM_Map),
  wxBoxSizer:add(Sizer, MapArea, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}, {border, ?SPACER_SMALL}]),

  Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),
  wxListBox:setFont(MapArea, Font),


  Win.


-spec update_map(OldState, NewState) -> ok when
  OldState :: cauder_wx:state(),
  NewState :: cauder_wx:state().

update_map(#wx_state{system = #sys{map = Map}}, #wx_state{system = #sys{map = Map}}) ->
  ok;
update_map(_, #wx_state{system = undefined}) ->
  wxListBox:clear(cauder_wx:find(?SYSTEM_Map, wxListBox)),
  ok;
update_map(_, #wx_state{system = #sys{map = []}}) ->
  wxListBox:clear(cauder_wx:find(?SYSTEM_Map, wxListBox)),
  ok;
update_map(_, #wx_state{system = #sys{map = Map}}) ->
  MapArea = cauder_wx:find(?SYSTEM_Map, wxListBox),
  wxListBox:freeze(MapArea),
  wxListBox:clear(MapArea),
  lists:foreach(fun(Entry) -> wxListBox:append(MapArea,io_lib:format("~p", [Entry])) end, Map),
  wxListBox:thaw(MapArea).


-spec create_history_map(Parent) -> Window when
  Parent :: wxWindow:wxWindow(),
  Window :: wxWindow:wxWindow().

create_history_map(Parent) ->
  Win = wxPanel:new(Parent),

  Sizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Win, [{label, "History Map"}]),
  wxPanel:setSizer(Win, Sizer),

  HMapArea = wxListBox:new(Win, ?SYSTEM_HMap),
  wxBoxSizer:add(Sizer, HMapArea, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}, {border, ?SPACER_SMALL}]),

  Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),
  wxListBox:setFont(HMapArea, Font),

  Win.

-spec update_history_map(OldState, NewState) -> ok when
  OldState :: cauder_wx:state(),
  NewState :: cauder_wx:state().

update_history_map(#wx_state{system = #sys{hmap = HMap}}, #wx_state{system = #sys{hmap = HMap}}) ->
  ok;
update_history_map(_, #wx_state{system = undefined}) ->
  wxListBox:clear(cauder_wx:find(?SYSTEM_HMap, wxListBox)),
  ok;
update_history_map(_, #wx_state{system = #sys{hmap = []}}) ->
  wxListBox:clear(cauder_wx:find(?SYSTEM_HMap, wxListBox)),
  ok;
update_history_map(_, #wx_state{system = #sys{hmap = HMap}}) ->
  HMapArea = cauder_wx:find(?SYSTEM_HMap, wxListBox),
  wxListBox:freeze(HMapArea),
  wxListBox:clear(HMapArea),
  lists:foreach(fun(Entry) -> wxListBox:append(HMapArea,io_lib:format("~p", [Entry])) end, HMap),
  wxListBox:thaw(HMapArea).
