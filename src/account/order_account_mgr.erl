%%----------------------------------------------------
%% 订单管理系统
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(order_account_mgr).
-behaviour(gen_server).
-export([start_link/0
        ,get_order_list/1
        ,add_order/7
    ]
).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").
-include("all_pb.hrl").

-record(state, {}).

-record(role_order, {
        id = {0, 0}       %% {role_id, type}
        ,num_list = []    %% 所选的押注列表
        ,multiple = 1     %% 押注倍数
        ,order_id = 0     %% 订单id
        ,time = 0         %% 时间
        ,cost = 0         %% 投入
        ,win = 0          %% 产出
    }
).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% 获取订单列表
get_order_list(Id) ->
    List = ets:lookup(order_mgr, Id),
    to_p_order_info(List).

to_p_order_info(#role_order{num_list = NumList, multiple = Multiple, order_id = OrderID, time = Time, cost = Cost, win = Win}) ->
    #p_order_info{order_id = integer_to_list(OrderID), num_list = NumList, multiple = Multiple, cost = Cost, time = Time, win = Win};
to_p_order_info(List) ->
    [to_p_order_info(A) || A <-List].


%% 增加订单信息
add_order(RoleId, Type, NumList, Multiple, OrderID, Win, Cost) ->
    Order = #role_order{id = {RoleId, Type}, num_list = NumList, multiple = Multiple, order_id = list_to_integer(OrderID), win = Win, cost = Cost, time = date:unixtime()},
    ?MODULE ! {add_order, Order}.


init([]) ->
    ?INFO("[~w] 正在启动", [?MODULE]),
    process_flag(trap_exit, true),
    ets:new(order_mgr, [named_table, public, duplicate_bag, {keypos, #role_order.id}]),
    dets:open_file(order_mgr, [{file, "./dets/order_mgr.dets"}, {type, duplicate_bag}, {keypos, #role_order.id}]),
    ets:from_dets(order_mgr, order_mgr),
    erlang:send_after(date:next_diff(3, 0, 0) * 1000, self(), clean),
    State = #state{},
    ?INFO("[~w] 启动完成", [?MODULE]),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({add_order, Order}, State) ->
    ets:insert(order_mgr, Order),
    {noreply, State};

handle_info(clean, State) ->
    Now = date:unixtime(),
    ets:foldl(fun(Data = #role_order{time = Time}, Acc) ->
                case Time + 7 * 86400 =< Now of
                    true ->
                        ets:delete_object(order_mgr, Data);
                    _ ->
                        ok
                end,
                Acc
        end, [], order_mgr),
    erlang:send_after(date:next_diff(3, 0, 0) * 1000, self(), clean),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?INFO("[~w] 正在关闭", [?MODULE]),
    dets:delete_all_objects(order_mgr),
    ets:to_dets(order_mgr, order_mgr),
    dets:close(order_mgr),
    ?INFO("[~w] 关闭完成", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
