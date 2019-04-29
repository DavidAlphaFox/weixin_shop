%%----------------------------------------------------
%% 商店管理系统
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(shop_mgr).
-behaviour(gen_server).
-export([start_link/0
        ,get_shop_list/2
        ,buy_item/7
        ,do_init/0
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").
-include("role.hrl").
-include("error_msg.hrl").
-include("shop.hrl").
-include("all_pb.hrl").

-record(state, {}).


%% 购买商品
buy_item(Role = #role{gold = Gold, discount_time = DiscountTime}, Id, DisCount, NumList, ChargeType, BuyNum, Flag) ->
    case check(Id, DisCount, NumList, BuyNum, DiscountTime) of
        {ok, Price, Name} ->  
            {Deduction, Num} = case Flag of
                1 ->
                    {min(Gold div 10, trunc(Price * 0.5)), Price - min(Gold div 10, trunc(Price * 0.5))};
                _ ->
                    {0, Price}
            end,
            case charge:do_charge(Role, ChargeType, Num, Deduction, DisCount, NumList, Name, Id, BuyNum) of
                {ok, Info} ->
                    {ok, Role#role{gold = Gold - Deduction * 10}, Info};
                {false, Reason} ->
                    {false, Reason}
            end;
        {false, Reason}-> 
            {false, Reason}
    end.


%% 检查前端数据是否合法
check(Id, DisCount, NumList, BuyNum, DiscountTime) ->
    case ets:lookup(shop, Id) of
        [#shop{discount = List, price = Price, name = Name}] ->
            case lists:member(DisCount, List) of
                true ->
                    Now = date:unixtime(),
                    case Now >= DiscountTime andalso DisCount =/= 100 of
                        true ->
                            {false, ?error_discount_time};
                        _ ->
                            Flag = case DisCount of
                                100 -> true;
                                50 ->  erlang:length(NumList) =:= 5;
                                10 -> erlang:length(NumList) =:= 1;
                                1 -> erlang:length(NumList) =:= 2
                            end,
                            case Flag of
                                true ->
                                    {ok, trunc(Price * DisCount/100 * BuyNum), Name};
                                _ -> 
                                    {false, ?error_act}
                            end
                    end;
                _ ->
                    {false, ?error_act}
            end;
        _ ->
            {false, ?error_busy}
    end.




get_shop_list(Type, Page) ->
    case Type of
        0 -> 
            case catch ets:match_object(shop, #shop{page = Page, _ = '_'}) of
                List when is_list(List) -> 
                    to_p_shop(List);
                _ -> []
            end;
        _ ->
            case catch ets:match_object(shop, #shop{type = Type, _ = '_'}) of
                List when is_list(List) -> 
                    to_p_shop(List);
                _ -> []
            end
    end.

%% 转换前端数据
to_p_shop(List) when is_list(List) ->
    [to_p_shop(Item)|| Item<-List];
to_p_shop(#shop{id = Id, type = Type, name = Name, icon = Icon, price = Price, discount = DisCount, buy_num = List}) ->
    Num = lists:sum([A|| {_, A}<-List]),
    #p_shop{id = Id, type = Type, name = Name, icon = Icon, price = Price, discount = DisCount, buy_num = Num}.





start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ?INFO("[~w] 正在启动", [?MODULE]),
    process_flag(trap_exit, true),
    ets:new(shop, [public, named_table, set, {read_concurrency, true}, {keypos, #shop.id}]),
    do_init(),
    ?INFO("[~w] 启动完成", [?MODULE]),
    State = #state{},
    {ok, State}.


do_init() ->
   Sql = "select * from shop",
    case db:get_all(Sql)  of
        {ok, List} -> 
            Sql1 = "select * from shop_buy",
            List2 = case db:get_all(Sql1) of
                {ok, List1} ->
                    [erlang:list_to_tuple(A)||A <-List1];
                _ ->
                    []
            end,
            do_init(List, List2);
        _ -> []
    end.

do_init([[Id, Type, Name, Icon, Price, DisCount, Page] | L], NumList) ->
    Num = case lists:keyfind(Id, 1, NumList) of
        {Id, Num1, Num2, Num3, Num4} ->
            [{100, Num1}, {50, Num2}, {10, Num3}, {1, Num4}];
        _ ->
            []
    end,
    {ok, DisCount1} = util:string_to_term(DisCount),
    ets:insert(shop, #shop{id = Id, type = Type, name = Name, icon = Icon, price = Price, discount = DisCount1, buy_num = Num, page = Page}),
    do_init(L, NumList);
do_init([], _NumList) -> ok.
 

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% 购买商品成功
handle_info({buy, Id, DisCount}, State) ->
    case ets:lookup(shop, Id) of
        [Shop = #shop{buy_num = List}] ->
            {NewList, Num} = case lists:keyfind(DisCount, 1, List) of
                {DisCount, N} ->
                    {lists:keyreplace(DisCount, 1, List, {DisCount, N + 1}), N + 1};
                _ ->
                    {[{DisCount, 1} | List], 1}
            end,
            Sql = case DisCount of
                100 -> "update shop_buy set price_1 = ? where id = ?";
                50 -> "update shop_buy set price_2 = ? where id = ?";
                10 -> "update shop_buy set price_3 = ? where id = ?";
                1 -> "update shop_buy set price_4 = ? where id = ?"
            end,
            db:exec(Sql, [Num, Id]),
            ets:insert(shop, Shop#shop{buy_num = NewList});
        _ ->
            ok
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?INFO("[~w] 正在关闭...", [?MODULE]),
    ?INFO("[~w] 关闭完成", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
