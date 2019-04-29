%%----------------------------------------------------
%% @doc 人物数据转换
%% 
%% @author weichengjun(527070307@qq.com)
%% @end
%%----------------------------------------------------
-module(role_conver).
-export([
        to_online/1
        ,to_login_role/1
    ]).

-include("role.hrl").
-include("all_pb.hrl").

%% 转换在线数据
to_online(#role{role_id = RoleID, name = Name, icon = Icon, pid = Pid, socket_pid = SocketPid, gold = Gold, screat = Screat}) ->
    #online_role{role_id = RoleID, name = Name, pid = Pid, socket_pid = SocketPid, icon = Icon, gold = Gold, screat = Screat}.

%% 登陆需要数据
to_login_role(#role{role_id = RoleID, open_id = OpenID, name = Name, icon = Icon,  gold = Gold, coin = Coupon, status = Status,  phone = Phone, vip = Vip, discount_time = Time}) ->
  #p_role_info{role_id = RoleID, nick_name = Name, openid = OpenID, icon = Icon,  gold = Gold, coin = Coupon, status = Status, phone = Phone, vip = Vip, discount_time = Time}.  



