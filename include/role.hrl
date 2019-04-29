%% 人物数据结构版本号
-define(role_var, 0).
-record(role, {
        var = ?role_var
        ,role_id = 0
        ,name = ""
        ,pid 
        ,socket_pid
        ,icon = ""          %% 头像
        ,gold = 10000       %% 金币
        ,coin = 0           %% 优惠券
        ,screat = 0         %% 登陆密匙
        ,open_id = ""
        ,ip = ""
        ,parent_id = 0            %% 推荐人id
        ,loop_counter = 0         %% 循环次数
        ,need_sync = false        %% 是否需要保存
        ,sex = 0                  %% 性别
        ,regist_time = 0          %% 注册时间
        ,login_time = 0           %% 最后一次登陆时间
        ,off_time = 0             %% 最后一次离线时间
        ,charge = 0               %% 总充值额(分)
        ,daily_value = []         %% 每日次数记录{key, value}
        ,red_openid = ""          %% 发红包的openid
        ,pay_openid = ""          %% 支付的opendid
        ,phone = ""               %% 手机号
        ,off = 0                  %% 是否下线，0否，1是
        ,exchange = 0             %% 总兑换多少钱（分）
        ,phone_screat = ""        %% 手机密码
        ,status  = 0              %% 场景值
        ,vip = 0                  %% vip等级
        ,gift_id = 1              %% 礼包自增id
        ,address = []             %% 地址列表
        ,discount_time = 0        %% 折扣到期时间
    }
).


%% 每日次数类型
-define(daily_help, 1).     %% 每日助力
-define(daily_phone, 2).    %% 每日手机验证码次数

%% 在线玩家数据结构
-record(online_role, {
        role_id
        ,name = ""
        ,pid
        ,socket_pid
        ,icon = ""
        ,screat = 0
        ,gold = 0
    }
).

