%%----------------------------------------------------
%% @doc 玩家推出处理
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(role_out).
-export([do/1]).

-include("role.hrl").
-include("rank.hrl").

do(Role = #role{}) ->
    RoleEnd = Role,
    role_data:sync_out_role(RoleEnd),
    boradcast_mgr:logout(RoleEnd),
    role_data:save(RoleEnd#role{off_time = date:unixtime(), off = 1}, db).


