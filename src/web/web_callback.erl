%%----------------------------------------------------
%% @doc 后台处理
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(web_callback).
-export([
        do_rpc/1
        ,wft/1
        ,yao_zfb/1
        ,do_pay_charge/3
        ,yb_pay/1
        ,do_get_yb/2
        ,do_pay_charge/1
    ]).

-include("common.hrl").
-include("role.hrl").
-include("all_pb.hrl").
-include_lib("xmerl/include/xmerl.hrl").

do_rpc(Data) ->
    {_, Mod1} = lists:keyfind("mod", 1, Data),
    {_, Function1} = lists:keyfind("function", 1, Data),
    Mod = erlang:list_to_atom(Mod1),
    Function = erlang:list_to_atom(Function1),
    NewArgs = case lists:keyfind("args", 1, Data) of
        {_, Args} ->
            L =
                case is_binary(Args) of
                    true ->
                        json:decode(Args, [{object_format, proplist}]);
                    _ ->
                        json:decode(erlang:list_to_binary(Args), [{object_format, proplist}])
                end,
            lists:map(fun({_, Val}) ->
                case catch erlang:binary_to_integer(Val) of
                    R when is_integer(R) ->
                        R;
                    _ ->
                        case is_integer(Val) of
                            true ->
                                Val;
                            _ ->
                                erlang:binary_to_list(Val)
                        end
                end end, L);
        _ ->
            []
    end,
     ?ERR("Mod:~w, Function:~w, Args:~w", [Mod, Function, NewArgs]),
    case catch erlang:apply(Mod, Function, NewArgs) of
        true ->
            true;
        ok ->
            true;
        false ->
            false;
        {ok, Value} ->
            to_json([{value, Value}]);
        {false, _Reson} ->
            false;
        <<"fail">> ->
            false;
        <<"success">> ->
            true;
        List when is_list(List) ->
            to_json(List);
        R ->
            R
    end.

to_json(List) -> json:encode(List).


%% 微信充值回调
wft(Xml) ->
    {XmlDocs, _Rest} = xmerl_scan:string(Xml),
    case xmerl_xpath:string("/xml/result_code", XmlDocs) of
        [ResultCodeXmlElement] ->
            case ResultCodeXmlElement of
                #xmlElement{content = [#xmlText{value = ResultCode}]} ->
                    case ResultCode =:= "SUCCESS" orelse ResultCode =:= "0" of
                        true ->
                            case xmerl_xpath:string("/xml/out_trade_no", XmlDocs) of
                                [XmlElement] ->
                                    #xmlElement{content = [Content]} = XmlElement,
                                    #xmlText{value = Value} = Content,
                                    case xmerl_xpath:string("/xml/total_fee", XmlDocs) of
                                        [XmlElement1] ->
                                            #xmlElement{content = [Content1]} = XmlElement1,
                                            #xmlText{value = Money} = Content1,
                                            case xmerl_xpath:string("/xml/attach", XmlDocs) of
                                                [#xmlElement{content = [#xmlText{value = Attach1}]}] ->
                                                    Attach =
                                                        case is_binary(Attach1) of
                                                            true ->
                                                                Attach1;
                                                            _ ->
                                                                case catch erlang:list_to_binary(Attach1) of
                                                                    A2 when is_binary(A2) ->
                                                                        A2;
                                                                    _ ->
                                                                        <<"[]">>
                                                                end
                                                        end,
                                                        case xmerl_xpath:string("/xml/transaction_id", XmlDocs) of
                                                            [XmlElement0] -> 
                                                                #xmlElement{content = [Content0]} = XmlElement0,
                                                                #xmlText{value = TranId} = Content0,
                                                                ?ERR("~ts", [TranId]);
                                                            _ -> ok
                                                        end,
                                                    case catch json:decode(Attach, [{object_format, proplist}]) of
                                                        Json when is_list(Json)->
                                                            case lists:keyfind(<<"role_id">>, 1, Json) of
                                                                {_, RoleID} ->
                                                                    case lists:keyfind(<<"charge_type">>, 1, Json) of
                                                                        {_, _Type} ->
                                                                            do_pay_charge(Value, RoleID, list_to_integer(Money));
                                                                        _ ->
                                                                            ?ERR("wft error charge_type:~p", [Json]),
                                                                            <<"fail">>
                                                                    end;
                                                                _ ->
                                                                    ?ERR("wft error charge_type:~p", [Json]),
                                                                    <<"fail">>
                                                            end;
                                                        _ ->
                                                            ?ERR("wft error charge_type:~p", [Attach]),
                                                            <<"fail">>
                                                    end
                                            end;
                                        _ ->
                                            <<"fail">>
                                    end;
                                _ ->
                                    <<"fail">>
                            end;
                        _ ->
                            <<"fail">>
                    end;
                _ ->
                    <<"fail">>
            end;
        _ ->
            <<"fail">>
    end.

do_pay_charge(Id) ->
    case db:get_row("select role_id, charge_rmb, status from charge_log where id = ?", [Id]) of
        {ok, [RoleID,  Money, 0]} ->
            do_pay_charge(Id, RoleID, Money),
            true;
        _ ->
            false
    end.


do_pay_charge(Id, RoleID, Money) ->
    case role_data:get_online_role(RoleID) of
        {ok, #online_role{pid = Pid}} ->
            case is_process_alive(Pid) of
                true ->
                    role:apply(async, Pid, {charge, charge_callback, [{Id, Money}]}),
                    <<"success">>;
                _ ->
                    role_charge_mgr:add_charge(RoleID),
                    ets:delete(online_role, RoleID),
                    Dets = role_data:get_role_dets_name(RoleID),
                    case dets:lookup(Dets, RoleID) of
                        [Role] -> 
                            {ok, Role1} = role_var:update_var(Role),
                            {ok, NewRole} = charge:charge_callback(Role1, {Id, Money}),
                            role_data:save_to_db(NewRole),
                            role:do_change({charge, charge_callback}, NewRole, Role1),
                            role_charge_mgr:delete_charge(RoleID),
                            <<"success">>;
                        _ ->
                            role_charge_mgr:delete_charge(RoleID),
                            <<"fail">>
                    end
            end;
        _ ->
            role_charge_mgr:add_charge(RoleID),
            Dets = role_data:get_role_dets_name(RoleID),
            case dets:lookup(Dets, RoleID) of
                [Role] -> 
                    {ok, Role1} = role_var:update_var(Role),
                    {ok, NewRole} = charge:charge_callback(Role1, {Id, Money}),
                    role_data:save_to_db(NewRole),
                    role:do_change({charge, charge_callback}, NewRole, Role1),
                    role_charge_mgr:delete_charge(RoleID),
                    <<"success">>;
                _ ->
                    role_charge_mgr:delete_charge(RoleID),
                    <<"fail">>
            end
    end.

yao_zfb(Xml) ->
    Xml1 = case is_binary(Xml) of
        true ->
            erlang:binary_to_list(Xml);
        _ ->
            Xml
    end,
    {XmlDocs, _Rest} = xmerl_scan:string(Xml1),
    case xmerl_xpath:string("/xml/status", XmlDocs) of
        [#xmlElement{content = [#xmlText{value = "100"}]}] ->
            case xmerl_xpath:string("/xml/order_no", XmlDocs) of
                [#xmlElement{content = [#xmlText{value = OrderId}]}] ->
                    case xmerl_xpath:string("/xml/amount", XmlDocs) of
                        [#xmlElement{content = [#xmlText{value = Money}]}] ->
                            Rmb = trunc(erlang:list_to_float(Money) * 100),
                            case db:get_row("select role_id, charge_rmb from charge_log where id = ?", [OrderId]) of
                                {ok, [RoleID, Rmb]} ->
                                    do_pay_charge(OrderId, RoleID, Rmb);
                                _ ->
                                    <<"fail">>
                            end;
                        _ ->
                            <<"fail">>
                    end;
                _ ->
                    <<"fail">>
            end;
        _ ->
            <<"fail">>
    end.

%% 易宝回调
yb_pay(Result) ->
    case catch mochiweb_util:parse_qs(Result) of
        B ->
            case catch [C|| {" name", C}<- B] of
                D ->
                    case  catch string:join(D, "") of
                        W ->
                            case catch string:tokens(W, "-\r\n") of
                                S ->
                                    case catch do_get_yb(S, rt4_status) of
                                        {ok, "SUCCESS"} ->
                                            case catch do_get_yb(S, rt2_orderId) of
                                                {ok, OrderId} ->
                                                    case catch do_get_yb(S, rt5_orderAmount) of
                                                        {ok, Amount} ->
                                                            Amount1 = case catch list_to_integer(Amount) of
                                                                Member when is_integer(Member) ->
                                                                    Member;
                                                                _ ->
                                                                    erlang:list_to_float(Amount)
                                                            end,
                                                            Rmb = trunc(Amount1 * 100),
                                                            case catch db:get_row("select role_id, charge_rmb from charge_log where id = ?", [OrderId]) of
                                                                {ok, [RoleID, Rmb]} ->
                                                                    do_pay_charge(OrderId, RoleID, Rmb);
                                                                _ ->
                                                                    <<"fail">>
                                                            end;
                                                        _ ->
                                                            <<"fail">>
                                                    end;
                                                _ ->
                                                    <<"fail">>
                                            end;
                                        _ ->
                                            <<"fail">>
                                    end;
                                _ ->
                                    <<"fail">>
                            end;
                        _ ->
                            <<"fail">>
                    end;
                _ ->
                    <<"fail">>
            end;
        _ ->
            <<"fail">>
    end.
            




do_get_yb([], _) -> false;
do_get_yb(["data\"rt5_orderAmount\"", B | _L], rt5_orderAmount) ->
    {ok, B};
do_get_yb(["data\"rt2_orderId\"", B | _L], rt2_orderId) ->
    {ok, B};
do_get_yb(["data\"rt4_status\"", B | _L], rt4_status) ->
    {ok, B};
do_get_yb(["data\"rt3_systemSerial\"", B | _L], rt3_systemSerial) ->
    {ok, B};
do_get_yb(["data\"rt11_channelOrderNum\"", B | _L], rt11) ->
    {ok, B};
do_get_yb([_ | L], Type) ->
    do_get_yb(L, Type).



