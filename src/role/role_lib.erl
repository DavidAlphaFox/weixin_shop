%%----------------------------------------------------
%% @doc 人物相关处理工具
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(role_lib).
-export([
        check_id_card/1
        ,do_cost/2
        ,do_add/2
        ,do_cost_coin/2
        ,do_cost_gold/2
        ,do_add_coin/2
        ,do_add_gold/2
        ,send_buff_begin/0
        ,send_buff_flush/0
        ,send_buff_clean/0
        ,get_value/2
        ,get_value/3
        ,set_value/3
        ,add_value/3
        ,add_value/2
        ,check_red_bag/1
        ,notice_red_bag/1
        ,select/4
    ]

).

-include("role.hrl").
-include("common.hrl").
-include("error_msg.hrl").
-include("all_pb.hrl").

%% 身份证验证用的
-define(value_list, [7,9,10,5,8,4,2,1,6,3,7,9,10,5,8,4,2]).


%% 获取人物每日数据,不带默认值，返回0
get_value(_Role = #role{daily_value = List}, Type) ->
    case lists:keyfind(Type, 1, List) of
        {Type, Value} -> Value;
        _ -> 0
    end.

%% 获取人物每日数据,带默认值，返回默认值
get_value(_Role = #role{daily_value = List}, Type, Default) ->
    case lists:keyfind(Type, 1, List) of
        {Type, Value} -> Value;
        _ -> Default
    end.


%% 设置人物每日数据
set_value(Role = #role{daily_value = List}, Type, Value) ->
    NewList = lists:keystore(Type, 1, List, {Type, Value}),
    Role#role{daily_value = NewList}.

%% 增加人物每日数据 仅限数据为integer
add_value(Role, Type) ->
    add_value(Role, Type, 1).

%% 增加人物每日数据 仅限数据为integer
add_value(Role = #role{daily_value = List}, Type, Add) ->
    NewList = case lists:keyfind(Type, 1, List) of
        {Type, Value} -> 
            lists:keyreplace(Type, 1, List, {Type, Value + Add});
        _ ->
            [{Type, Add} | List]
    end,
    Role#role{daily_value = NewList}.


%% 注意 必须要在人物进程调用，并且是begin 后面必须要flush或者clean
%% 消息缓冲推送
send_buff_begin() ->
    case get(send_buff) of
        undefined  ->
            put(send_buff, []);
        _ ->
            ?ERR("缓冲池已经开始了", []),
            ok
    end.

send_buff_flush() ->
    case get(socket_pid) of
        Pid when is_pid(Pid) ->
            case get(send_buff) of
                List when is_list(List)->
                    [Pid ! {tcp_send, Data} || Data<-List],
                    put(send_buff, undefined);
                _ ->
                    ?ERR("缓冲池没有数据", []),
                    ok
            end;
        _ ->
            ?ERR("不在人物进程不能调用", [])
    end.

send_buff_clean() ->
    put(send_buff, undefined).


%% 扣除指定资产
%% 礼券
do_cost_coin(Role = #role{coin = Value}, Cost) when Value >= Cost-> 
    sys_conn:pack_send(1101, #m_1101_toc{list = [#p_assets{type = coin, num = Value - Cost}]}),
    {ok, Role#role{coin = Value - Cost}};
do_cost_coin(_, _) ->
    {false, ?error_coin}.
%% 金币
do_cost_gold(Role = #role{gold = Value}, Cost) when Value >= Cost-> 
    sys_conn:pack_send(1101, #m_1101_toc{list = [#p_assets{type = gold, num = Value - Cost}]}),
    {ok, Role#role{gold = Value - Cost}};
do_cost_gold(_, _) ->
    {false, ?error_gold}.

%% 批量扣除资产
do_cost(Role, []) -> {ok, Role};
do_cost(Role, List) -> 
    do_cost(Role, List, []).

do_cost(Role, [], []) -> {ok, Role};
do_cost(Role, [], List) -> 
    sys_conn:pack_send(1101, #m_1101_toc{list = [#p_assets{type = Type, num = Value}||{Type, Value} <-List]}),
    {ok, Role};

%% 礼券
do_cost(Role = #role{coin = OldValue}, [{coin, Value} | L], List) when OldValue >= Value->
    do_cost(Role#role{coin = OldValue - Value}, L, [{coin, OldValue - Value} | List]);
do_cost(_Role, [{coin, _Value} | _L], _)->
    {false, ?error_coin};
%% 金币
do_cost(Role = #role{gold = OldValue}, [{gold, Value} | L], List) when OldValue >= Value->
    do_cost(Role#role{gold = OldValue - Value}, L, [{gold, OldValue - Value} | List]);
do_cost(_Role, [{gold, _Value} | _L], _)->
    {false, ?error_gold};
do_cost(_Role, [{_Type, _} | _L], _List) ->
    {false, ?error_item_num}.



%% 增加指定资产
%% 礼券
do_add_coin(Role = #role{coin = Value}, Add) ->
    sys_conn:pack_send(1101, #m_1101_toc{list = [#p_assets{type = coin, num = Value + Add}]}),
    {ok, Role#role{coin = min(?int_max_num, Value + Add)}}.
%% 金币
do_add_gold(Role = #role{gold = Value}, Add) ->
    sys_conn:pack_send(1101, #m_1101_toc{list = [#p_assets{type = gold, num = Value + Add}]}),
    {ok, Role#role{gold = Value + Add}}.

%% 批量增加资产
do_add(Role, []) -> {ok, Role};
do_add(Role, List) ->
    do_add(Role, List, []).

do_add(Role, [], []) ->  {ok, Role};
do_add(Role, [], List) -> 
    sys_conn:pack_send(1101, #m_1101_toc{list = [#p_assets{type = Type, num = Value}||{Type, Value} <-List]}),
    {ok, Role};

do_add(Role = #role{coin = OldValue}, [{coin, Value} | L], List) -> 
    do_add(Role#role{coin = min(?int_max_num, OldValue + Value)}, L, [{coin, OldValue + Value} | List]);
%% 金币
do_add(Role = #role{gold = OldValue}, [{gold, Value} | L], List) -> 
    do_add(Role#role{gold = OldValue + Value}, L, [{gold, OldValue + Value} | List]);

do_add(Role, [{Type, _} | L], List) ->
    ?ERR("增加未知的资产类型:~w", [Type]),
    do_add(Role, L, List).





%% 身份证验证
check_id_card(Id) ->
    case erlang:length(Id) of
        18 ->
            Year = lists:sublist(Id, 7, 4),
            Month = lists:sublist(Id, 11, 2),
            Day = lists:sublist(Id, 13, 2),
            case check([{date, {Year, Month, Day}}]) of
                true ->
                    Last = do_last(lists:sublist(Id, 17), ?value_list, 0),
                    [lists:last(Id)] =:= Last;
                _ ->
                    false
            end;
        15 ->
            First = lists:sublist(Id, 1, 6),
            Mind = lists:sublist(Id, 7, 2),
            Last = lists:sublist(Id, 9, 7),
            All = First ++ "19" ++ Mind ++ Last,
            Last1 = do_last(All, ?value_list, 0),
            check(All ++ Last1);
        _ ->
            false
    end.

check([]) -> true;
check([{date, {Year, Month, Day}} | L]) -> 
    Year1 = list_to_integer(Year),
    Month1 = list_to_integer(Month),
    Day1 = list_to_integer(Day),
    case catch date:datetime_to_seconds({Year1, Month1, Day1, 0, 0, 0}) of
        Date when is_integer(Date) ->
            check(L);
        _ -> false
    end.

do_last("", [], Num) -> 
    get_rem(Num rem 11);
do_last([N | L], [N1 | L1], Num) -> 
    Add = list_to_integer([N]) * N1,
    do_last(L, L1, Num + Add).

get_rem(0) -> "1";
get_rem(1) -> "0";
get_rem(2) -> "X";
get_rem(3) -> "9";
get_rem(4) -> "8";
get_rem(5) -> "7";
get_rem(6) -> "6";
get_rem(7) -> "5";
get_rem(8) -> "4";
get_rem(9) -> "3";
get_rem(10) -> "1".


%% 通知红包
notice_red_bag(Role) ->
    Id = sys_rand:rand(1000,9999),
    {ok, integer_to_list(Id), Role};
notice_red_bag(_Role = #role{red_openid = ""}) ->
    {false, ?error_red_bag_code};
notice_red_bag(Role = #role{red_openid = OpenId, role_id = RoleID, exchange = Exchange}) ->
    Value = 100,  %% 分
    Now = date:unixtime(),
    ParTraNo = lists:concat([?MachId, Now, sys_rand:rand(100000,999999)]),
    case db:exec("insert into notice_red_bag_log(role_id, cny, order_id, time) value(?, ?, ?, ?)", [RoleID, Value, ParTraNo, Now]) of
        ok -> 
            case send_red_bag(ParTraNo, OpenId, Value) of
                {ok, ParTraNo, WxOrder} ->
                    db:exec("update notice_red_bag_log set status = ?, wx_order = ? where order_id = ?", [1, WxOrder, ParTraNo]),
                    {ok, WxOrder, Role#role{exchange = Exchange + Value}};
                {error, _Reason = 49}->
                    ?ERR("用户[~w]:~ts 提现失败：~w:~w", [RoleID, OpenId, _Reason, Value]),
                    {false, ?error_red_bag_num_limit};
                {error, _Reason = 46}->
                    ?ERR("用户[~w]:~ts 提现失败：~w:~w", [RoleID, OpenId, _Reason, Value]),
                    {false, ?error_red_bag_not_exist};
                {error, _Reason = 50}->
                    ?ERR("用户[~w]:~ts 提现失败：~w:~w", [RoleID, OpenId, _Reason, Value]),
                    {false, ?error_red_bag_account};
                {error, _Reason} when is_integer(_Reason)->
                    ?ERR("用户[~w]:~ts 提现失败：~w:~w", [RoleID, OpenId, _Reason, Value]),
                    {false, ?error_busy};
                _ ->
                    ?ERR("企业付款超时失败~w, ~ts, ~w", [RoleID, ParTraNo, Value]),
                    {false, ?error_busy}
            end;
        _ ->
            {false, ?error_busy}
    end.


check_red_bag(OrderID) ->
    Url = "https://api.mch.weixin.qq.com/mmpaymkttransfers/gettransferinfo",
    MchID = ?RedMachId,
    AppID = ?RedAppId,
    Key = ?RedMachKey,
    Desc = erlang:binary_to_list(unicode:characters_to_binary("是否到账")),
    SS = [
        {appid, AppID}
        ,{mch_id, MchID}
        ,{nonce_str, Desc}
        ,{partner_trade_no, OrderID}
        ,{key, Key}
    ],
    RawSS = util:format_get_params(SS),
    Sign = string:to_upper(erlang:binary_to_list(util:md5(RawSS))),
    [_|T] = lists:reverse(SS),
    Xml = util:format_xml_params(lists:reverse([{sign, Sign}|T])),
    Headers = [{"content-type", "text/xml;charset=utf-8"}],
    ContentType = "text/xml",
    HttpOps = [{ssl, [{certfile, "pem/apiclient_cert.pem"}, {keyfile, "pem/apiclient_key.pem"}]}],
    case httpc:request(post, {Url, Headers, ContentType, Xml}, HttpOps, []) of
        {ok, {_Header, _List, Result}} ->
            case util:get_xml_info(Result, "/xml/result_code") of
                {ok, "SUCCESS"} ->
                    case util:get_xml_info(Result, "/xml/status") of
                        {ok, "SUCCESS"} ->
                            ok;
                        _ ->
                            false
                    end;
                _ ->
                    false
            end;
        _ ->
            false
    end.



%% 公众号发送红包
send_red_bag(ParTraNo, OpenID, Amount) ->
    MchID = ?RedMachId,
    Url = "https://api.mch.weixin.qq.com/mmpaymkttransfers/promotion/transfers",
    IP = sys_env:get_env(ip),
    AppID = ?RedAppId,
    Key = ?RedMachKey,
    NonceStr = string:to_upper(erlang:binary_to_list(util:md5(ParTraNo))),
    Desc = erlang:binary_to_list(unicode:characters_to_binary("感谢您参加，祝您生活愉快")),
    SS = [
        {amount, Amount},
        {check_name, 'NO_CHECK'},
        {desc, Desc},
        {mch_appid, AppID},
        {mchid, MchID},
        {nonce_str, NonceStr},
        {openid, OpenID},
        {partner_trade_no, ParTraNo},
        {spbill_create_ip, IP},
        {key, Key}
    ],
    RawSS = util:format_get_params(SS),
    Sign = string:to_upper(erlang:binary_to_list(util:md5(RawSS))),
    [_|T] = lists:reverse(SS),
    Xml = util:format_xml_params(lists:reverse([{sign, Sign}|T])),
    Headers = [{"content-type", "text/xml;charset=utf-8"}],
    ContentType = "text/xml",
    HttpOps = [{ssl, [{certfile, "pem/apiclient_cert.pem"}, {keyfile, "pem/apiclient_key.pem"}]}],
    case httpc:request(post, {Url, Headers, ContentType, Xml}, HttpOps, []) of
        {ok, {_Header, _List, Result}} ->
            case util:get_xml_info(Result, "/xml/result_code") of
                {ok, "SUCCESS"} ->
                    case util:get_xml_info(Result, "/xml/partner_trade_no") of
                        {ok, ParTraNo} ->
                            case util:get_xml_info(Result, "/xml/payment_no") of
                                {ok, OrderNo} ->
                                    {ok, ParTraNo, OrderNo};
                                _R ->
                                    {ok, ParTraNo}
                            end;
                        _R ->
                            {ok, ParTraNo}
                    end;
                {ok, "FAIL"} ->
                    EC = util:get_xml_info(Result, "/xml/err_code"),
                    case EC of
                        {_, "V2_ACCOUNT_SIMPLE_BAN"} ->
                            {error, 50};
                        {_, "AMOUNT_LIMIT"} ->
                            {error, 53};
                        {_, "NOTENOUGH"} ->
                            {error, 46};
                        {_, "PAYMENT_ACCOUNT_NOT_EXIST"} ->
                            {error, 46};
                        {_, "SENDNUM_LIMIT"} ->
                            {error, 49};
                        {_, "SIGN_ERROR"} ->
                            {error, 73};
                        {_, "CA_ERROR"} ->
                            {error, 52};
                        {_, "REQ_PARAM_XML_ERR"} ->
                            {error, 74};
                        {_, "COUPON_STOCK_ID_EMPTY"} ->
                            {error, 75};
                        {_, "MCH_ID_EMPTY"} ->
                            {error, 76};
                        {_, "CODE_2_ID_ERR"} ->
                            {error, 77};
                        {_, "OPEN_ID_EMPTY"} ->
                            {error, 78};
                        {_, "ERR_VERIFY_SSL_SERIAL"} ->
                            {error, 79};
                        {_, "ERR_VERIFY_SSL_SN"} ->
                            {error, 80};
                        {_, "CA_VERIFY_FAILED"} ->
                            {error, 52};
                        {_, "NO_AUTH"} ->
                            case util:get_xml_info(Result, "/xml/err_code_des") of
                                {ok, [20135,21697,26435,38480,39564,35777,22833,36133,44,
                                        35831,26597,30475,24744,24403,21069,26159,21542,20855,26377,
                                        35813,20135,21697,30340,26435,38480]} ->
                                    {error, 63};
                                _ ->
                                    {error, 50}
                            end;
                        {_, "ILLEGAL_APPID"} ->
                            {error, 81};
                        {_, "MONEY_LIMIT"} ->
                            {error, 53};
                        {_, "SEND_FAILED"} ->
                            {error, 82};
                        {_, "FATAL_ERROR"} ->
                            {error, 83};
                        {_, "OPENID_ERROR"} ->
                            {error, 48};
                        {_, "SYSTEMERROR"} ->
                            {error, 1};
                        {_, _ErrCode} ->
                            case util:get_xml_info(Result, "/xml/err_code_des") of
                                {_, Des} ->
                                    case Des of
                                        [21442,25968,38169,35823,58,36755,20837,30340,29992,25143,111,112,101,110,105,100,26377,35823,46] ->
                                            {error, 48};
                                        [21442,25968,38169,35823,65306,29992,
                                            25143,111,112,101,110,105,100,23383,
                                            27573,24517,22635,65292,24182,19988,
                                            23569,20110,51,50,20010,23383,31526,
                                            46] ->
                                            {error, 48};
                                        [38750,23454,21517,29992,25143,
                                            36134,21495,19981,21487,21457,
                                            25918] ->
                                            {error, 50};
                                        [31995,32479,32321,24537,65292,35831,
                                            31245,21518,20877,35797] ->
                                            {error, 65};
                                        _ ->
                                            ?ERR("提现失败错误~w", [_ErrCode]),
                                            {error, 47}
                                    end;
                                _ ->
                                    ?ERR("提现失败错误~w", [_ErrCode]),
                                    {error, 47}
                            end;
                        _R ->
                            ?ERR("提现失败错误~w", [_R]),
                            {error, 47}
                    end;
                _ ->
                    {error, 1}
            end
    end.



%% 1手所需要的金币
-define(base_select_gold, 10).

%% 检查下前端数据合法性
check([multiple | L], Type, NumList, Multiple) ->
    case Multiple >= 1 of
        true -> 
            check(L, Type, NumList, Multiple);
        _ ->
            false
    end;
check([], _, _, _) -> true.

%% 押注
select(Role = #role{name = Name}, Type, NumList, Multiple) ->
    case check([multiple], Type, NumList, Multiple) of
        true ->
            BaseGold = Multiple * ?base_select_gold,
            Gold = trunc(BaseGold * 1.1) + 1,
            case do_cost_gold(Role, Gold) of
                {ok, NewRole} -> 
                    case notice_red_bag(NewRole) of
                        {ok, OrderID, NewRole1} ->
                            {Status, Num} = do_select(OrderID, Type, NumList),
                            {Status1, Win} = case Status of
                                true -> 
                                    boradcast_mgr:boradcast(util:fbin("恭喜玩家[~ts]在猜~ts游戏中获得~w金币", [Name, type_name(Type), BaseGold * Num])),
                                    {1, BaseGold * Num};
                                _ -> 
                                    {0, 0}
                            end,
                            {ok, OrderID, Status1, Win, NewRole1};
                        {false, Reason} ->
                            {false, Reason}
                    end;
                {false, Reason} ->
                    {false, Reason}
            end;
        _ -> 
            {false, ?error_act}
    end.

type_name(1) -> "尾号";
type_name(2) -> "大小";
type_name(3) -> "单双";
type_name(4) -> "后两位";
type_name(5) -> "后三位".

%% 获取后几位数字列表
get_num_list(OrderID, 1) ->
   Num = erlang:list_to_integer(OrderID) rem 10,
   [Num];
get_num_list(OrderID, 2) ->
   Num1 = erlang:list_to_integer(OrderID) rem 10,
   Num2 = (erlang:list_to_integer(OrderID) rem 100) div 10,
   lists:sort([Num1, Num2]);
get_num_list(OrderID, 3) ->
   Num1 = erlang:list_to_integer(OrderID) rem 10,
   Num2 = (erlang:list_to_integer(OrderID) rem 100) div 10,
   Num3 = (erlang:list_to_integer(OrderID) rem 1000) div 100,
   lists:sort([Num1, Num2, Num3]).


%% 是否中奖 ,返回{true | false, 倍数}
%% 尾号
do_select(OrderID, 1, List) ->
    List1 = get_num_list(OrderID, 1),
    Status = List1 =:= List,
    {Status, 10};
%% 大小
do_select(OrderID, 2, [Num]) ->
    List2 = case Num of
        0 -> [0, 1, 2, 3, 4];
        1 -> [5, 6, 7, 8, 9]
    end,
    List1 = get_num_list(OrderID, 1),
    Status = List2 -- List1 =/= List2,
    {Status, 2};

%% 单双
do_select(OrderID, 3, [Num]) ->
    List2 = case Num of
        0 -> [0, 2, 4, 6, 8];
        1 -> [1, 3, 5, 7, 9]
    end,
    List1 = get_num_list(OrderID, 1),
    Status = List2 -- List1 =/= List2,
    {Status, 2};
%% 后两位
do_select(OrderID, 4, List) ->
    List1 = get_num_list(OrderID, 2),
    Status = List1 =:= lists:sort(List),
    {Status, 50};
%% 后三位
do_select(OrderID, 5, List) ->
    List1 = get_num_list(OrderID, 3),
    Status = List1 =:= lists:sort(List),
    {Status, 150}.


    


