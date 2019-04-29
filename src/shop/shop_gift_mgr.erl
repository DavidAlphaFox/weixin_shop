%%----------------------------------------------------
%% 商店礼包系统
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(shop_gift_mgr).
-behaviour(gen_server).
-export([start_link/0
        ,create_gift/3
        ,get_help_info/1
        ,help/2
        ,reward/2
        ,steal/2
        ,get_gift_list/1
        ,get_friend_gift_list/1
        ,get_steal_info/1

    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {}).

-include("common.hrl").
-include("role.hrl").
-include("shop.hrl").
-include("all_pb.hrl").
-include("error_msg.hrl").
-include_lib("stdlib/include/ms_transform.hrl").



-define(gift_reward_time, 86400 * 2).
-define(gift_protect_time, 600).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% 创建一个礼包
create_gift(Role = #role{role_id = RoleId, gift_id = GiftId, name = Name, icon = Icon}, Id, DisCount) ->
    case ets:lookup(shop, Id) of
        [#shop{price = Price}] ->
            Now = date:unixtime(),
            Gift = #shop_gift{id = {RoleId, GiftId}, auto_id = GiftId, role_id = RoleId, name = Name, icon = Icon, start_time = Now, time = Now + ?gift_reward_time, discount = DisCount, item_id = Id, all_gold = trunc(Price * 0.2)},
            ets:insert(shop_gift, Gift),
            {ok, Role#role{gift_id = GiftId + 1}};
        _ ->
            {ok, Role}
    end.


%% 获取助力信息
get_help_info(Id) ->
    case ets:lookup(shop_gift, Id) of
        [#shop_gift{role_id = RoleId, auto_id = GiftId, icon = RoleIcon, name = Name, time = Time, help_list = List, status = Status, discount = Discount, item_id = ItemId, all_gold = Gold}] ->
            case ets:lookup(shop, ItemId) of
                [Item = #shop{buy_num = List}] ->
                    {Discount, Num} = lists:keyfind(Discount, 1, List),
                    Now = date:unixtime(),
                    Left = erlang:max(Time - Now, 0),
                    {ok, #p_gift_info{role_id = RoleId, gift_id = GiftId, role_icon = RoleIcon, role_name = Name, left_time = Left, role_list = List, status = Status, item = Item, discount = Discount, buy_num = Num, all_gold = Gold}};
                _ ->
                    {false, ?error_busy}
            end;
        _ ->
            {false, ?error_busy}
    end.

%% 获取偷取信息
get_steal_info(Id) ->
    case ets:lookup(shop_gift, Id) of
        [#shop_gift{steal_list = List}] -> List;
        _ -> []
    end.

%% 助力
help(Role = #role{name = Name, role_id = RoleId, icon = Icon}, Id) ->
    case catch gen_server:call(?MODULE, {help, Id, #p_help_role{role_name = Name, role_id = RoleId, icon = Icon, time = date:unixtime()}}) of
        {ok, HelpTime} -> 
            NewRole = role_lib:add_value(Role, ?daily_help),
            {ok, HelpTime, NewRole};
        _ ->
            {false, ?error_busy}
    end.

%% 领取奖励
reward(Role = #role{role_id = RoleId}, Id) ->
    case catch gen_server:call(?MODULE, {reward, RoleId, Id}) of
        {ok, Gold} ->
            {ok, NewRole} = role_lib:add_gold(Role, Gold),
            {ok, Gold, NewRole};
        {false, Reason} ->
            {false, Reason}
    end.

%% 偷取金币
steal(Role = #role{name = Name, role_id = RoleId, icon = Icon}, Id) ->
    case catch gen_server:call(?MODULE, {steal, Id, #p_help_role{role_name = Name, role_id = RoleId, icon = Icon, time = date:unixtime()}}) of
        {ok, Gold} -> 
            {ok, NewRole} = role_lib:add_gold(Role, Gold),
            {ok, Gold, NewRole};
        _ ->
            {false, ?error_busy}
    end.

%% 获取自己礼包列表
get_gift_list(_Role = #role{role_id = RoleId}) ->
    case catch ets:match_object(shop_gift, #shop_gift{role_id = RoleId,  _ = '_'}) of
        List when is_list(List) ->
            Now = date:unixtime(),
            [#p_gift_simple{role_id = RoleId, gift_id = Id, status = Status, time = max(0, Time - Now)}||#shop_gift{auto_id = Id, status = Status, time = Time} <-List];
        _ ->
            []
    end.

%% 获取好友的礼包礼包
get_friend_gift_list(_Role = #role{role_id = RoleId}) ->
    case db:get_all("select role_id from role where parent_id = ?", [RoleId]) of
        {ok, List} ->
            NewList = do_firend_gift(List, []),
            Now = date:unixtime(),
            do_steal_list(NewList, Now, RoleId, []);
        _ ->
            []
    end.

do_steal_list([#shop_gift{time = Time, role_id = RoleId, name = Name, icon = Icon, auto_id  = Id, steal_list = StealList, all_gold = Gold, steal_gold = Gold1} | L], Now, RoleId, List) ->
    NewList = case lists:keyfind(RoleId, #p_help_role.role_id, StealList) of
        false ->
            case Gold > Gold1 * 2 of
                true ->
                    case Now > Time + ?gift_protect_time of
                        true ->
                            [#p_gift_simple{role_id = RoleId, gift_id = Id, name = Name, icon = Icon, status = 0, time = max(0, Time + ?gift_protect_time - Now)} | List];
                        _ ->
                            List
                    end;
                _ ->
                    List
            end;
        _ ->
            List
    end,
    do_steal_list(L, Now, RoleId, NewList);
do_steal_list([], _, _, List) -> List.



do_firend_gift([[RoleId] | L], List) -> 
    NewList = case catch ets:match_object(shop_gift, #shop_gift{role_id = RoleId,  status = 0,  _ = '_'}) of
        List1 when is_list(List1) ->
            List1 ++ List;
        _ ->
            List
    end,
    do_firend_gift(L, NewList);
do_firend_gift([], List) -> List.



init([]) ->
    ?INFO("[~w] 正在启动", [?MODULE]),
    process_flag(trap_exit, true),
    ets:new(shop_gift, [public, named_table, set, {keypos, #shop_gift.id}]),
    dets:open_file(shop_gift, [{file, "./dets/shop_gift.dets"}, {keypos, #shop_gift.id}, {type, set}]),
    ets:from_dets(shop_gift, shop_gift),
    State = #state{},
    ?INFO("[~w] 启动完成", [?MODULE]),
    {ok, State}.

%% 助力
handle_call({help, Id, HelpRole}, _From, State) ->
    Now = date:unixtime(),
    Reply = case ets:lookup(shop_gift, Id) of
        [Gift = #shop_gift{time = Time, help_list = List}]  when Time > Now ->
            N = sys_rand:rand(10, 300), 
            NewGift = Gift#shop_gift{time = max(Now, Time - N * 60), help_list = [HelpRole#p_help_role{help_time = N * 60} | List]},
            ets:insert(shop_gift, NewGift),
            {ok, N * 60};
        _ ->
            {false, ?error_act}
    end,
    {reply, Reply, State};

%% 领取奖励
handle_call({reward, RoleId, Id}, _From, State) ->
    Reply = case ets:lookup(shop_gift, Id) of
        [Gift = #shop_gift{time = 0, role_id = RoleId, status = 0, all_gold = Gold, steal_gold = Gold1}]   ->
            ets:insert(shop_gift, Gift#shop_gift{status = 1}),
            {ok, Gold - Gold1};
        _ ->
            {false, ?error_act}
    end,
    {reply, Reply, State};

%% 偷取金币
handle_call({steal, Id, Role}, _From, State) ->
    Now = date:unixtime(),
    Reply = case ets:lookup(shop_gift, Id) of
        [Gift = #shop_gift{time = Time, steal_list = List, status = 0, all_gold = Gold, steal_gold = Gold1}] when Now >= Time + ?gift_protect_time ->
            case Gold1 >= trunc(0.5 * Gold) of
                true -> 
                    {false, ?error_act};
                _ ->
                    Add = sys_rand:rand(1, 10),
                    ets:insert(shop_gift, Gift#shop_gift{steal_gold = Gold1 + Add, steal_list = [Role#p_help_role{help_time = Add} | List]}),
                    {ok, Add}
            end;
        _ ->
            {false, ?error_act}
    end,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?INFO("[~w] 正在关闭...", [?MODULE]),
    dets:delete_all_objects(shop_gift),
    ets:to_dets(shop_gift, shop_gift),
    dets:close(shop_gift),
    ?INFO("[~w] 关闭完成", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.





