%% 按固定格式输出普通信息到控制台
-define(INFO(Msg), logger:info(Msg, [], ?MODULE, ?LINE)).
-define(INFO(F, A), logger:info(F, A, ?MODULE, ?LINE)).
%% 按固定格式输出错误信息到控制台
-define(ERR(Msg), logger:error(Msg, [], ?MODULE, ?LINE)).
-define(ERR(F, A), logger:error(F, A, ?MODULE, ?LINE)).



%% 充值类型
-define(charge_coin, 1).  %% 金币充值


%%修改以下数据需要玩家下线才会生效
%% 登陆公众号
-define(LoginAppID, "wx6550285d3b5d797f").
-define(LoginAppSecret, "b5cb3a1b3b0891aca88106175ac7f4d2").
%% 唯卓
-define(RedMachId, "1511880951").
-define(RedMachKey, "d9oPC3A6VI17J7vxtB3v98DGqUw12N7J").
-define(RedAppId, "wx5f94b1370d1abdb8").
-define(RedAppSecret, "6c7363f45828132c63f2a05577b7d74f").
%% 中传
%% 兴业
%%-define(MachId, "403580132115").
%%-define(MachKey, "13a6bbc11918f778471ae52a5dda8209").
%% 晋商
-define(MachId, "174520003257").
-define(MachKey, "7a465c9d4eb2c9f37a3e2bcfc1e65933").
-define(PayAppId, "wx0ccfe006798c95a0").
-define(PayAppSecret, "d18c09b63d336d69f0bbbd6561fae7ff").

%% int32最大数值
-define(int_max_num, 2147483647).
%% uint32最大数值
-define(uint_max_num, 4294967295).

