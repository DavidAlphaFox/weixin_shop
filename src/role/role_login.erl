%%----------------------------------------------------
%% @doc 人物登陆处理
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(role_login).
-export([
        do/1
    ]).

-include("role.hrl").
-include("common.hrl").
-include("rank.hrl").


%% 只处理玩家
do(Role = #role{login_time = Time, role_id = RoleId}) -> 
    Now = date:unixtime(),
    Role1 = case date:is_same_day(Time, Now) of
        true ->
            Role;
        _ ->
            role:do_zero_flush(Role)
    end,
    RoleEnd = Role1,
    boradcast_mgr:login(RoleEnd),
    role_account_mgr:login(RoleEnd),
    role_data:sync_online_role(RoleEnd),
    db:exec("update role set off = 0 where role_id = ?", [RoleId]),
    RoleEnd#role{login_time = Now, off = 0}.


