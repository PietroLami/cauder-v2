-module(cauder_wx_system).

%% API
-export([create/1, update/2]).

-include_lib("wx/include/wx.hrl").
-include("cauder.hrl").
-include("cauder_wx.hrl").


%%%=============================================================================
%%% API
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Creates the <i>system info</i> panel and populates it.

-spec create(Parent) -> Window when
  Parent :: wxWindow:wxWindow(),
  Window :: wxWindow:wxWindow().

create(Parent) ->
  Win = wxPanel:new(Parent),

  Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Win, [{label, "System Info"}]),
  wxWindow:setSizer(Win, Sizer),

  Expand = [{proportion, 1}, {flag, ?wxEXPAND}],

  wxSizer:add(Sizer, create_mail(Win), Expand),

  wxSizer:addSpacer(Sizer, 5),

  Notebook = wxNotebook:new(Win, ?SYSTEM_Notebook),
  wxNotebook:addPage(Notebook, create_trace(Notebook), "Trace"),
  wxNotebook:addPage(Notebook, create_roll_log(Notebook), "Roll Log"),
  wxSizer:add(Sizer, Notebook, Expand),

  Win.


%%------------------------------------------------------------------------------
%% @doc Updates the <i>system info</i> panel according to the given new state,
%% by comparing it with the given old state.

-spec update(OldState, NewState) -> ok when
  OldState :: cauder_wx:state(),
  NewState :: cauder_wx:state().

update(OldState, NewState) ->
  update_mail(OldState, NewState),
  update_trace(OldState, NewState),
  update_roll_log(OldState, NewState).


%%%=============================================================================
%%% Internal functions
%%%=============================================================================


-spec create_mail(Parent) -> Window when
  Parent :: wxWindow:wxWindow(),
  Window :: wxWindow:wxWindow().

create_mail(Parent) ->
  Win = wxPanel:new(Parent),

  Sizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Win, [{label, "Mail"}]),
  wxPanel:setSizer(Win, Sizer),

  MailArea = wxListCtrl:new(Win, [{winid, ?SYSTEM_Mail}, {style, ?wxLC_REPORT bor ?wxLC_SINGLE_SEL}]),
  wxBoxSizer:add(Sizer, MailArea, [{proportion, 1}, {flag, ?wxEXPAND}]),

  Item = wxListItem:new(),
  Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),

  wxListItem:setText(Item, "Dest."),
  wxListItem:setFont(Item, Font),
  wxListCtrl:insertColumn(MailArea, 0, Item),

  wxListItem:setText(Item, "Value"),
  wxListItem:setFont(Item, Font),
  wxListCtrl:insertColumn(MailArea, 1, Item),

  wxListItem:setText(Item, "UID"),
  wxListItem:setFont(Item, Font),
  wxListCtrl:insertColumn(MailArea, 2, Item),

  wxListItem:destroy(Item),

  wxListCtrl:setColumnWidth(MailArea, 0, 75),
  wxListCtrl:setColumnWidth(MailArea, 1, 150),
  wxListCtrl:setColumnWidth(MailArea, 2, 75),

  wxListCtrl:connect(MailArea, command_list_item_activated),

  Win.


-spec update_mail(OldState, NewState) -> ok when
  OldState :: cauder_wx:state(),
  NewState :: cauder_wx:state().

update_mail(#wx_state{system = #sys{mail = Mail}}, #wx_state{system = #sys{mail = Mail}}) ->
  ok;
update_mail(_, #wx_state{system = undefined}) ->
  wxListCtrl:deleteAllItems(cauder_wx:find(?SYSTEM_Mail, wxListCtrl)),
  ok;
update_mail(_, #wx_state{system = #sys{mail = Mail}}) ->
  Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),
  MailArea = cauder_wx:find(?SYSTEM_Mail, wxListCtrl),
  wxListCtrl:freeze(MailArea),
  wxListCtrl:deleteAllItems(MailArea),
  lists:foldl(
    fun(#msg{dest = Dest, val = Value, uid = Uid}, Row) ->
      wxListCtrl:insertItem(MailArea, Row, ""),
      wxListCtrl:setItemFont(MailArea, Row, Font),
      wxListCtrl:setItem(MailArea, Row, 0, integer_to_list(Dest)),
      wxListCtrl:setItem(MailArea, Row, 1, io_lib:format("~p", [Value])),
      wxListCtrl:setItem(MailArea, Row, 2, integer_to_list(Uid)),
      Row + 1
    end, 0, Mail),
  wxListCtrl:thaw(MailArea),
  ok.


%%%=============================================================================


-spec create_trace(Parent) -> Window when
  Parent :: wxWindow:wxWindow(),
  Window :: wxWindow:wxWindow().

create_trace(Parent) ->
  Win = wxPanel:new(Parent),

  Sizer = wxBoxSizer:new(?wxHORIZONTAL),
  wxPanel:setSizer(Win, Sizer),

  TraceArea = wxListBox:new(Win, ?SYSTEM_Trace),
  wxBoxSizer:add(Sizer, TraceArea, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}, {border, ?SPACER_SMALL}]),

  Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),
  wxListBox:setFont(TraceArea, Font),

  Win.


-spec update_trace(OldState, NewState) -> ok when
  OldState :: cauder_wx:state(),
  NewState :: cauder_wx:state().

update_trace(#wx_state{system = #sys{trace = Trace}}, #wx_state{system = #sys{trace = Trace}}) ->
  ok;
update_trace(_, #wx_state{system = undefined}) ->
  wxListBox:clear(cauder_wx:find(?SYSTEM_Trace, wxListBox)),
  ok;
update_trace(_, #wx_state{system = #sys{trace = []}}) ->
  wxListBox:clear(cauder_wx:find(?SYSTEM_Trace, wxListBox)),
  ok;
update_trace(_, #wx_state{system = #sys{trace = Trace}}) ->
  wxNotebook:setSelection(cauder_wx:find(?SYSTEM_Notebook, wxNotebook), ?SYSTEM_Notebook_Trace),
  TraceArea = cauder_wx:find(?SYSTEM_Trace, wxListBox),
  wxListBox:freeze(TraceArea),
  wxListBox:clear(TraceArea),
  Entries = lists:map(fun lists:flatten/1, lists:map(fun cauder_pp:trace_entry/1, Trace)),
  lists:foreach(fun(Entry) -> wxListBox:append(TraceArea, Entry) end, Entries),
  wxListBox:thaw(TraceArea).


%%%=============================================================================


-spec create_roll_log(Parent) -> Window when
  Parent :: wxWindow:wxWindow(),
  Window :: wxWindow:wxWindow().

create_roll_log(Parent) ->
  Win = wxPanel:new(Parent),

  Sizer = wxBoxSizer:new(?wxHORIZONTAL),
  wxPanel:setSizer(Win, Sizer),

  RollLogArea = wxListBox:new(Win, ?SYSTEM_RollLog),
  wxBoxSizer:add(Sizer, RollLogArea, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}, {border, ?SPACER_SMALL}]),

  Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),
  wxListBox:setFont(RollLogArea, Font),

  Win.


-spec update_roll_log(OldState, NewState) -> ok when
  OldState :: cauder_wx:state(),
  NewState :: cauder_wx:state().

update_roll_log(#wx_state{system = #sys{roll = RollLog}}, #wx_state{system = #sys{roll = RollLog}}) ->
  ok;
update_roll_log(_, #wx_state{system = undefined}) ->
  wxListBox:clear(cauder_wx:find(?SYSTEM_RollLog, wxListBox)),
  ok;
update_roll_log(_, #wx_state{system = #sys{roll = []}}) ->
  wxListBox:clear(cauder_wx:find(?SYSTEM_RollLog, wxListBox)),
  ok;
update_roll_log(_, #wx_state{system = #sys{roll = RollLog}}) ->
  wxNotebook:setSelection(cauder_wx:find(?SYSTEM_Notebook, wxNotebook), ?SYSTEM_Notebook_RollLog),
  RollLogArea = cauder_wx:find(?SYSTEM_RollLog, wxListBox),
  wxListBox:freeze(RollLogArea),
  wxListBox:clear(RollLogArea),
  lists:foreach(fun(Entry) -> wxListBox:append(RollLogArea, Entry) end, RollLog),
  wxListBox:thaw(RollLogArea).

