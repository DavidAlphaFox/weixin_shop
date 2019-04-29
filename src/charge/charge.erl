%%----------------------------------------------------
%% @doc  充值处理
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(charge).
-export([do_charge/9
        ,charge_callback/2
        ,web_charge/3
        ,web_send_coin/3
        ,apply_send_coin/3
        ,yb_pay/5
    ]).

-include("role.hrl").
-include("common.hrl").
-include("all_pb.hrl").
-include("error_msg.hrl").



-define(charge_status_start, 0).
-define(charge_status_finish, 1).

%% 支付类型
-define(charge_wxpubpay, 0).    %% 微信官方公众号支付
-define(charge_wft_wx, 1).      %% 威富通公众号支付
-define(charge_yao_zfb, 2).      %% 摇钱树支付宝支付扫码
-define(charge_yao_wx, 3).      %% 摇钱树微信支付
-define(charge_yao_zfb_h5, 4).      %% 摇钱树支付宝h5
-define(charge_wft_wx_sm, 5).      %% 威富通微信扫码
-define(charge_yb_wx_sm, 6).      %% 易宝微信扫码


%% 微信官方支付接口
wxpubpay(OpenID, OrderID, Amount, RoleID, Body) ->
  Url = "https://api.mch.weixin.qq.com/pay/unifiedorder",
  AppID = ?PayAppId,
  MchID = ?MachId,
  IP = sys_env:get_env(ip),
  NotifyUrl = "http://"++IP++":10000/wft",
  Key = ?MachKey,
  Attach = json:encode([{role_id, RoleID}]),
  SS = [
    {appid, AppID},
    {attach, Attach},
    {body, Body},
    {mch_id, MchID},
    {nonce_str, OrderID},
    {notify_url, NotifyUrl},
    {openid, OpenID},
    {out_trade_no, OrderID},
    {spbill_create_ip, IP},
    {total_fee, Amount},
    {trade_type, "JSAPI"},
    {key, Key}
  ],
  RawSS = util:format_get_params(SS),
  Sign = string:to_upper(binary_to_list(util:md5(RawSS))),
  [_ | T] = lists:reverse(SS),
  Xml = util:format_xml_params(lists:reverse([{sign, Sign}|T])),
  Time = date:unixtime(),
  Headers = [{"content-type", "application/json;charset=utf-8"}],
  ContentType = "application/x-www-form-urlencoded",
  case httpc:request(post, {Url, Headers, ContentType, Xml}, [{timeout, 5000}], []) of
    {ok, {_, _, Result}} ->
      case util:get_xml_info(Result, "return_code") of
        {ok, "SUCCESS"} ->
            case util:get_xml_info(Result, "prepay_id") of
                {ok, PrepayID} ->
                    Return = [
                        {appId, unicode:characters_to_binary(AppID)},
                        {nonceStr, unicode:characters_to_binary(OrderID)},
                        {package, unicode:characters_to_binary("prepay_id="++PrepayID)},
                        {signType, unicode:characters_to_binary("MD5")},
                        {timeStamp, unicode:characters_to_binary(integer_to_list(Time))},
                        {key, Key}],
                    Sign2 = string:to_upper(binary_to_list(util:md5(util:format_get_params(Return)))),
                    [_|T2] = lists:reverse(Return),
                    Json = json:encode(lists:reverse([{'paySign', unicode:characters_to_binary(Sign2)} | T2])),
                    {ok, Json};
                _ ->
                    {false, ?error_busy}
            end;
        _ ->
            {ok, Msg} = util:get_xml_info(Result, "return_msg"),
            ?ERR("====:~ts~n",[Msg]),
            {false, ?error_busy}
      end;
    _R ->
      ?ERR("_r:~p",[_R]),
      {false, ?error_busy}
  end.

%% 威富通公众号支付 %% OrderNo订单号 Amount 单位分 Attach::binary()是附加信息json体 例如 {"role_id":20001, "charge_type":red_bag}
wft_gzh(ChannelType, OpenID, OrderNo, Amount, RoleID, Body) ->
    Url = "https://pay.swiftpass.cn/pay/gateway",
    AppID = ?PayAppId,
    MchID = ?MachId,
    Key = ?MachKey,
    IP = sys_env:get_env(ip),
    Service = case ChannelType of
        gzh -> "pay.weixin.jspay";
        sm -> "pay.weixin.native"
    end,
    Amt = integer_to_list(Amount),
    NonceStr = string:to_upper(erlang:binary_to_list(util:md5(OrderNo))),
    NotifyUrl = lists:concat(["http://", IP, ":", 10000, "/wft"]),
    Attach = json:encode([{role_id, RoleID}]),
    SS = case ChannelType of
        gzh ->
            [
                {attach, Attach},
                {body, Body},
                {is_raw, 1},
                {mch_create_ip, IP},
                {mch_id, MchID},
                {nonce_str, NonceStr},
                {notify_url, NotifyUrl},
                {out_trade_no, OrderNo},
                {service, Service},
                {sub_appid, AppID},
                {sub_openid, OpenID},
                {total_fee, Amt},
                {key, Key}
            ];
        sm ->
            [
                {attach, Attach},
                {body, Body},
                {mch_create_ip, IP},
                {mch_id, MchID},
                {nonce_str, NonceStr},
                {notify_url, NotifyUrl},
                {out_trade_no, OrderNo},
                {service, Service},
                {sign_type, "MD5"},
                {total_fee, Amt},
                {key, Key}
            ]
    end,
    RawSS = util:format_get_params(SS),
    Sign = string:to_upper(erlang:binary_to_list(util:md5(RawSS))),
    [_|T] = lists:reverse(SS),
    Xml = util:format_xml_params(lists:reverse([{sign, Sign}|T])),
    Headers = [{"content-type", "text/xml;charset=utf-8"}],
    ContentType = "text/xml",
    case catch httpc:request(post, {Url, Headers, ContentType, Xml}, [{timeout, 5000}], []) of
        {ok, {_Header, _List, Result}} ->
            case util:get_xml_info(Result, "result_code") of
                {ok, "0"} ->
                    StringInfo =  case ChannelType of
                        gzh -> "pay_info";
                        sm -> "code_url"
                    end,
                    case util:get_xml_info(Result, StringInfo) of
                        {ok, PayInfo} ->
                            {ok, PayInfo};
                        {error, ErrorID} ->
                            {false, ErrorID};
                        _Err ->
                            {false, 1}
                    end;
                _->
                    case catch util:get_xml_info(Result, "message") of
                        {ok, Message} ->
                            case OpenID of
                                "" ->
                                    ok;
                                _ ->
                                    ?ERR("威富通接口返回错误描述:~w: ~ts: ~ts", [ChannelType, Message, Xml])
                            end;
                        _ ->
                            case catch util:get_xml_info(Result, "err_msg") of
                                {ok, Message} ->
                                    case OpenID of
                                        "" ->
                                            ok;
                                        _ ->
                                            ?ERR("威富通接口返回错误描述:~w: ~ts: ~ts", [ChannelType, Message, Xml])
                                    end;
                                _ ->
                                    ok
                            end
                    end,
                    {false, 1}
            end;
        _Err ->
            {false, 1}
    end.



%% 摇钱树支付宝微信支付
yao_zfb(ChannelType, Body, Amount, OrderID, SelfIp) ->
    Url = "https://opay.arsomon.com:28443/vipay/reqctl.do",
    MchID = "10000240",
    Key = "a47f3edf4egf65356gad016c4e46e5f4",
    IP = sys_env:get_env(ip),
    Service =  case ChannelType of
        zfb -> "ali.activescan.pay";
        wx -> "wx.js.pay";
        zfb_h5 -> "ali.h5.pay"
    end,
    Amt = float_to_list(Amount/100, [{decimals, 2}]),
    NotifyUrl = lists:concat(["http://", IP, ":", 10000, "/yao_zfb"]),
    SS = 
    case ChannelType of
        zfb_h5 ->
            [
                {amount, Amt},
                {goods, Body},
                {ip, SelfIp},
                {mch_id, MchID},
                {notify_url, NotifyUrl},
                {order_no, OrderID},
                {service, Service},
                {key, Key}
            ];
        _ ->
            [
                {amount, Amt},
                {goods, Body},
                {mch_id, MchID},
                {notify_url, NotifyUrl},
                {order_no, OrderID},
                {service, Service},
                {key, Key}
            ]
    end,
    RawSS = util:format_get_params(SS),
    Sign = string:to_upper(erlang:binary_to_list(util:md5(RawSS))),
    [_|T] = lists:reverse(SS),
    Xml = util:format_xml_params(lists:reverse([{sign, Sign}|T])),
    Headers = [{"content-type", "text/xml;charset=utf-8"}],
    ContentType = "text/xml",
    case catch httpc:request(post, {Url, Headers, ContentType, Xml}, [{timeout, 5000}], []) of
        {ok, {_Header, _List, Result}} ->
            case util:get_xml_info(Result, "res_code") of
                {ok, "100"} ->
                    UrlString =  case ChannelType of
                        zfb -> "code_url";
                        wx -> "url";
                        zfb_h5 -> "pay_url"
                    end,
                    case util:get_xml_info(Result, UrlString) of
                        {ok, CodeUrl} ->
                            {ok, CodeUrl};
                        {error, ErrorID} ->
                            {false, ErrorID};
                        _Err ->
                            {false, 1}
                    end;
                _->
                    case catch util:get_xml_info(Result, "res_msg") of
                        {ok, Message} ->
                            ?ERR("摇钱树接口返回错误描述:~w:~ts", [ChannelType, Message]),
                            {false, 1};
                        _ ->
                            {false, 1}
                    end
            end;
        _R ->
            ?ERR("摇钱树接口返回错误描述:~w: ~w", [ChannelType, _R]),
            {false, 1}
    end.

%% 易宝微信扫码
yb_pay(_OpenID, OrderID, Amount, RoleID, Body) ->
  Url = "https://www.u6u8.com/yskapi/Payment/WxChartPAPayment",
  MchID = "8901108436",
%%  MchID = "4001104637",
  IP = sys_env:get_env(ip),
  NotifyUrl = "http://"++IP++":10000/yb",
  Key = "gHuytl9eEKDihaXcptxqlPqa8Vo9wju5",
  Amt = float_to_list(Amount/100, [{decimals, 2}]),
  Attach = erlang:binary_to_list(json:encode([{role_id, RoleID}])),
  SS = [
    {'P1_bizType', "AppPay"},
    {'P2_orderId', OrderID},
    {'P3_customerNumber', MchID},
    {'P4_payType', "SCAN"},
    {'P5_orderAmount', Amt},
    {'P6_currency', "CNY"},
    {'P7_authcode', "1"},
    {'P8_appType', "WXPAY"},
    {'P9_notifyUrl', NotifyUrl},
    {'P10_successToUr', ""},
    {'P11_orderIp', IP},
    {'P12_goodsName', Body},
    {'P13_goodsDetail', ""},
    {'P14_desc', Attach},
    {key, Key}
  ],
  SS1 = "&" ++ string:join([util:to_list(H) || {_, H} <- SS], "&"),
  Sign = binary_to_list(util:md5(SS1)),
  [_ | T] = lists:reverse(SS),
  Xml = lists:reverse([{sign, Sign}|T]),
  Params = string:join([atom_to_list(Atom) ++ "=" ++ util:to_list(Val) || {Atom, Val} <- Xml], "&"),
%%  Time = date:unixtime(),
  Headers = [{"content-type", "application/json;charset=utf-8"}],
  ContentType = "application/x-www-form-urlencoded",
  case httpc:request(post, {Url, Headers, ContentType, Params}, [{timeout, 5000}], []) of
    {ok, {_, _, Result}} ->
        case json:decode(erlang:list_to_binary(Result), [{object_format, proplist}]) of
            List when is_list(List) ->
                case lists:keyfind(<<"rt2_retCode">>, 1, List) of
                    {_, <<"0000">>} ->
                        case lists:keyfind(<<"rt8_qrcode">>, 1, List) of
                            {_, CodeUrl} ->
                                {ok, binary_to_list(CodeUrl)};
                            _ ->
                                {false, ?error_busy}
                        end;
                    _ ->
                        case lists:keyfind(<<"rt3_retMsg">>, 1, List) of
                            {_, Msg} ->
                                ?ERR("====:~ts~n",[Msg]),
                                {false, ?error_busy};
                            _ ->
                                {false, ?error_busy}
                        end
                end;
            _ ->
                {false, ?error_busy}
        end;
    _ ->
        {false, ?error_busy}
 end.






%% 玩家充值
do_charge(_Role = #role{role_id = RoleId, pay_openid = OpenID, ip = Ip}, ChargeType, Num, Deduction, DisCount, NumList,  Name, ItemId, BuyNum) ->
    Now = date:unixtime(),
    N = sys_rand:rand(1000, 9999),
    Id = lists:concat([RoleId, N, Now]),
    case db:exec("insert into charge_log (id, role_id, charge_rmb, charge_type, gold, time, discount, num_list, item_id, buy_num, status) values(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)", [Id, RoleId, Num, ChargeType, Deduction * 10, date:unixtime(), DisCount, util:term_to_string(NumList), ItemId, BuyNum, 0]) of
        ok ->
            {ok, ""};
%%            case ChargeType of
%%                ?charge_wxpubpay ->
%%                    wxpubpay(OpenID, Id, Num, RoleId, Name);
%%                ?charge_wft_wx ->
%%                    wft_gzh(gzh, OpenID, Id, Num, RoleId, Name);
%%                ?charge_wft_wx_sm ->
%%                    wft_gzh(sm, OpenID, Id, Num, RoleId, Name);
%%                ?charge_yb_wx_sm ->
%%                    yb_pay(OpenID, Id, Num, RoleId, Name);
%%                ?charge_yao_zfb ->
%%                    yao_zfb(zfb, Name, Num, Id, util:to_ip_string(Ip));
%%                ?charge_yao_wx ->
%%                    yao_zfb(wx, Name, Num, Id, util:to_ip_string(Ip));
%%                ?charge_yao_zfb_h5 ->
%%                    yao_zfb(zfb_h5, Name, Num, Id, util:to_ip_string(Ip))
%%            end;
        _ ->
            {false, ?error_busy}
    end.

web_charge(RoleId, Type, Num) ->
    Now = date:unixtime(),
    N = sys_rand:rand(1000, 9999),
    Id = lists:concat([RoleId, N, Now]),
    Num1 = Num * 100,
    case db:exec("insert into charge_log (id, role_id, charge_rmb, charge_type, type, time, status) values(?, ?, ?, ?, ?, ?, ?)", [Id, RoleId, Num1, 99, Type, date:unixtime(), ?charge_status_start]) of
        ok ->
            web_callback:do_pay_charge(Id, RoleId, Type, Num1);
        _ ->
            false
    end.



%% 充值回调
charge_callback(Role = #role{charge = Charge}, {Id, Num}) ->
    case db:get_row("select status,  num_list, discount, item_id from charge_log where id = ?", [Id]) of
        {ok, [?charge_status_start, NumList, DisCount, ItemId]} ->
            Now = date:unixtime(),
            db:exec("update charge_log set status = ? , call_time = ? where id = ?", [?charge_status_finish, Now, Id]),
            account_mgr:charge(Num),
            case role_lib:notice_red_bag(Role) of
                {ok, OrderID, NewRole} ->
                    shop_mgr ! {buy, ItemId, DisCount},
                    Flag = case do_reward(OrderID, DisCount, util:string_to_term(NumList)) of
                        true -> 1;
                        _ ->  0
                    end,
                    {ok, NewRole1 = #role{gift_id = GiftId}} = shop_gift_mgr:create_gift(NewRole, ItemId, DisCount),
                    sys_conn:pack_send(1104, #m_1104_toc{flag = Flag, order_id = OrderID, gift_id = GiftId, price = Num}),
                    {ok, NewRole1#role{charge = Charge + Num, vip = get_vip(Charge + Num)}};
                _ ->
                    {ok, Role}
            end;
        _ ->
            {ok, Role}
    end.


%% 判断是否中奖
do_reward(_OrderID, 100, _NumList) -> true;
do_reward(OrderID, 50, NumList) -> 
    N = lists:last(OrderID),
    lists:member(N, NumList);
do_reward(OrderID, 10, NumList) -> 
    N = lists:last(OrderID),
    lists:member(N, NumList);
do_reward(OrderID, 1, NumList) ->
    [A, B | _] = lists:reverse(OrderID),
    [B, A] =:= NumList.



get_vip(Num) when Num >= 500 -> 2;
get_vip(Num) when Num >= 100 -> 1;
get_vip(_) -> 0.




%% 赠送，扣除指定资产
web_send_coin(RoleID, Type, Num) ->
    case role_data:get_online_role(RoleID) of
        {ok, #online_role{pid = Pid}} ->
            role:apply(async, Pid, {?MODULE, apply_send_coin, [Type, Num]}),
            true;
        _ ->
            case role_data:get_role_from_dets(RoleID) of
                {ok, Role} ->
                    {ok, Role1} = role_var:update_var(Role),
                    {ok, NewRole} = apply_send_coin(Role1, Type, Num),
                    role:do_change({charge, apply_send_coin}, NewRole, Role1),
                    role_data:save_to_db(NewRole),
                    true;
                _ ->
                   false 
            end
    end.

%% 增加资产
apply_send_coin(Role = #role{role_id = RoleId}, Type, Num) when Num > 0 ->
    {ok, NewRole} = case Type of
        1 -> 
            sys_conn:pack_send(1123, #m_1123_toc{type = Type, list = [#p_assets{type = coin, num = Num}]}),
            role_lib:do_add_coin(Role, Num);
        2 ->
            sys_conn:pack_send(1123, #m_1123_toc{type = Type, list = [#p_assets{type = gold, num = Num}]}),
            role_lib:do_add_gold(Role, Num)
    end,
    log_db:log(send_coin_log, insert, [RoleId, Type, Num, date:unixtime()]),
    {ok, NewRole};

%% 扣除资产
apply_send_coin(Role = #role{role_id = RoleId, coin = Coin, gold = Gold}, Type, Num) ->
    NewRole = case Type of
        1 -> Role#role{coin = max(0, Coin + Num)};
        2 -> Role#role{gold = max(0, Gold + Num)}
    end,
    log_db:log(send_coin_log, insert, [RoleId, Type, Num, date:unixtime()]),
    {ok, NewRole}.
    





