
-record(shop_gift, {
        id = {0, 0}       %% {RoleId, Auto_id}
        ,auto_id = 1      %% 玩家自增id
        ,role_id = 0      %% 玩家id
        ,name = ""        %% 玩家名字
        ,icon = ""        %% 玩家图标
        ,start_time = 0   %% 初始时间
        ,time = 0         %% 可以领取时间
        ,help_list = []   %% 帮助列表
        ,steal_list = []  %% 偷取列表
        ,all_gold = 0     %% 所有金币
        ,reward = 0       %% 是否领取奖励
        ,discount = 0     %% 折扣 
        ,item_id = 0      %% 商品基础id
        ,status = 0       %% 是否领取
        ,steal_gold = 0   %% 被偷的金币
        
    }
).

%% 商品信息
-record(shop, {
        id = 0
        ,type = 0
        ,name = ""
        ,icon = ""
        ,price = 0
        ,discount = []
        ,buy_num = 0
        ,page = 1
    }
).
