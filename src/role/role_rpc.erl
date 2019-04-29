%%----------------------------------------------------
%% @doc 人物协议处理
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(role_rpc).
-export([
        handle/3

    ]).

-include("common.hrl").
-include("role.hrl").
-include("all_pb.hrl").
-include("error_msg.hrl").

%% 购买商品
handle(1102, #m_1102_tos{shop_id = ShopId, discount = DisCount, luck_num = NumList, charge_type = ChargeType, buy_num = BuyNum, flag = Flag}, Role) ->
    case shop_mgr:buy_item(Role, ShopId, DisCount, NumList, ChargeType, BuyNum, Flag) of
        {ok, NewRole, Info} ->
            {ok, #m_1102_toc{info = Info, charge_type = ChargeType}, NewRole};
        {false, Reason} ->
            {false, Reason}
    end;

%% 查看商场
handle(1103, #m_1103_tos{type = Type, page = Page}, _Role) ->
    List = shop_mgr:get_shop_list(Type, Page),
    {reply, #m_1103_toc{list = List}};

%% 助力
handle(1105, #m_1105_tos{role_id = RoleId}, _Role = #role{role_id = RoleId}) ->
    {false, ?error_act};
handle(1105, #m_1105_tos{role_id = RoleId, gift_id = Id}, Role) ->
    case shop_gift_mgr:help(Role, {RoleId, Id}) of
        {ok, Time, NewRole} ->
            {ok, #m_1105_toc{time = Time}, NewRole};
        {false, Reason} ->
            {false, Reason}
    end;

%% 领取礼包
handle(1107, #m_1107_tos{role_id = RoleId, gift_id = Id}, Role = #role{role_id = RoleId}) ->
    case shop_gift_mgr:reward(Role, Id) of
        {ok, Gold, NewRole} ->
            {ok, #m_1107_toc{gold = Gold}, NewRole};
        {false, Reason} ->
            {false, Reason}
    end;

%% 偷取礼包
handle(1108, #m_1108_tos{role_id = RoleId}, _Role = #role{role_id = RoleId}) ->
    {false, ?error_act};
handle(1108, #m_1108_tos{role_id = RoleId, gift_id = Id}, Role) ->
    case shop_gift_mgr:steal(Role, {RoleId, Id}) of
        {ok, Gold, NewRole} ->
            {ok, #m_1108_toc{gold = Gold}, NewRole};
        {false, Reason} ->
            {false, Reason}
    end;

%% 获取自己的礼包
handle(1109, _, Role) ->
    List = shop_gift_mgr:get_gift_list(Role),
    {reply, #m_1109_toc{list = List}};

%% 获取好友的的礼包
handle(1110, _, Role) ->
    List = shop_gift_mgr:get_friend_gift_list(Role),
    {reply, #m_1110_toc{list = List}};

%% 获取偷取信息
handle(1111, #m_1111_tos{gift_id = Id}, _Role = #role{role_id = RoleId}) ->
    List = shop_gift_mgr:get_steal_info({RoleId, Id}),
    {reply, #m_1111_toc{list = List}};

%% 增加收货信息
handle(1112, #m_1112_tos{address = Address}, Role = #role{address = List}) ->
    case erlang:length(List) >= 5 of
        true -> 
            {false, ?error_act};
        _ -> 
            NewList = [Address | List],
            NewList1 = do_address_id(lists:reverse(NewList), 1, []),
            {ok, #m_1112_toc{}, Role#role{address = NewList1}}
    end;


%% 获取收货信息
handle(1113, _, _Role = #role{address = List}) ->
    {reply, #m_1113_toc{list = List}};

%% 修改收货信息
handle(1114, #m_1114_tos{address = Address = #p_address{id = Id}}, Role = #role{address = List}) ->
    NewList = lists:keyreplace(Id, #p_address.id, List, Address),
    {ok, #m_1114_toc{}, Role#role{address = NewList}};

%% 转盘
handle(1115, _, Role = #role{discount_time = Time}) ->
    Now = date:unixtime(),
    case Time > Now of
        true -> {false, ?error_act};
        _ -> 
            {ok,  #m_1115_toc{discount_time = Now + 86400 * 7}, Role#role{discount_time = Now + 86400 * 7}}
    end;

%% 删除收货信息
handle(1116, #m_1116_tos{id = Id}, Role = #role{address = List}) ->
    NewList = lists:keydelete(Id, #p_address.id, List),
    NewList1 = do_address_id(lists:reverse(NewList), 1, []),
    {ok, #m_1116_toc{}, Role#role{address = NewList1}};

%% 设置收货默认信息
handle(1117, #m_1117_tos{id = Id}, Role = #role{address = List}) ->
    case lists:keyfind(Id, #p_address.id, List) of
        Address = #p_address{} ->  
            NewList = lists:keydelete(Id, #p_address.id, List),
            NewList1 = do_address_id([Address | lists:reverse(NewList)], 1, []),
            {ok, #m_1117_toc{}, Role#role{address = NewList1}};
        _ -> {false, ?error_act}
    end;

%% 获取助力信息
handle(1118, #m_1118_tos{role_id = RoleId, gift_id = GiftId}, _Role) ->
    Info = shop_gift_mgr:get_help_info({RoleId, GiftId}),
    {reply, #m_1118_toc{info = Info}};


%% 手机获取验证码, 每天只能验证5次，每次要间隔一分钟
handle(1130, #m_1130_tos{phone = Phone}, Role = #role{phone = ""}) ->
    case erlang:length(Phone) of
        11 ->
            case role_lib:get_value(Role, ?daily_phone) >= 5 of
                true -> {false, ?error_phone_num};
                _ ->
                    Now = date:unixtime(),
                    case get(phone_code) of
                        {_, _, Time} when Time >= Now ->
                            {false, ?error_act};
                        _ ->
                            case db:get_one("select role_id from role where phone = ?", [Phone]) of
                                {ok, _RoleId} when is_integer(_RoleId) ->   %% 已经绑定
                                    {false, ?error_phone_exit};
                                _ ->
                                    Code = sys_rand:rand(1000, 9999),
                                    put(phone_code, {Code, Phone, Now + 60}),
                                    lib_juhe:send_msg(Phone, Code),
                                    NewRole = role_lib:add_value(Role, ?daily_phone),
                                    {ok, #m_1130_toc{}, NewRole}
                            end
                    end
            end;
        _ ->
            {false, ?error_phone}
    end;

%% 绑定手机
handle(1131, #m_1131_tos{code = Code}, Role) ->
    case get(phone_code) of
        {Code, Phone, _} ->
            {ok, NewRole} = role_lib:do_add_gold(Role, 2),
            role_data:save_to_db(NewRole#role{phone = Phone}),
            {ok, #m_1131_toc{type = gold, num = 2, phone = Phone}, NewRole#role{phone = Phone}};
        _ ->
            {false, ?error_phone_code}
    end;


%% 分享签名
handle(1136, #m_1136_tos{url = Url}, _) ->
    case weixin_mgr:client_sign(Url) of
        {ok, Sign, TimeStamp, NonceStr} ->
            Data = #m_1136_toc{sign = erlang:binary_to_list(Sign), timestamp = erlang:integer_to_list(TimeStamp), noncestr = erlang:integer_to_list(NonceStr)},
            {reply, Data};
        _ ->
            {false, ?error_busy}
    end;

%% 前端报错的错误日志
handle(1197, #m_1197_tos{msg = Msg}, #role{role_id = RoleId, name = Name}) ->
    case get(client_error_log) of
        Msg -> ok;
        _ ->
            put(client_error_log, Msg),
            log_db:log(client_error_log, insert, [RoleId, Name, Msg, date:unixtime()])
    end,
    {reply, #m_1197_toc{}};

%% 使用gm命令
%% 充值
%%handle(1198, #m_1198_tos{type = Type, num = Num}, Role = #role{role_id = RoleId}) ->
%%    Now = date:unixtime(),
%%    N = sys_rand:rand(1000, 9999),
%%    Id = lists:concat([RoleId, N, Now]),
%%    case db:exec("insert into charge_log (id, role_id, charge_rmb, type, time, status) values(?, ?, ?, ?, ?, ?)", [Id, RoleId, Num, Type, Now, 0]) of
%%        ok ->
%%            {ok, NewRole} = charge:charge_callback(Role, {Id, Type, Num}),
%%            {ok, #m_1198_toc{}, NewRole};
%%        _ -> {false, ?error_busy}
%%    end;
%%%% 获得数据
%%handle(1199, #m_1199_tos{type = Type, num = Num}, Role) ->
%%    {ok, NewRole} = role_lib:do_add(Role, [{Type, Num}]),
%%    {ok, #m_1199_toc{}, NewRole};
handle(_Cmd, _Data, _Role) ->
    ?ERR("错误的协议数据cmd:~w,data:~w", [_Cmd, _Data]),
    ok.

%%---------------------------------
%% internal function
%%---------------------------------
%% 给地址从新赋值id
do_address_id([Address | L], N, List) -> 
    do_address_id(L, N + 1, [Address#p_address{id = N} | List]);
do_address_id([], _N, List) -> List.
