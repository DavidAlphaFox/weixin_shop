-- 注册用户
-- 人物数据
DROP TABLE IF EXISTS role;
CREATE TABLE `role` (
  `role_id` int(11) DEFAULT '0'  COMMENT '人物id',
  `openid` varchar(50)  COMMENT '微信openid',
  `name` varchar(50)  COMMENT '名字',
  `icon` varchar(300)  COMMENT '头像',
  `phone` varchar(50)  COMMENT '电话号码',
  `phone_screat` varchar(50)  COMMENT '电话密码',
  `gold` int(11)  DEFAULT '0' COMMENT '金币',
  `coin` int(11)  DEFAULT '0' COMMENT '礼券',
  `parent_id` int(11)  DEFAULT '0' COMMENT '上级id',
  `login_time` int(11)  DEFAULT '0' COMMENT '最后一次登陆的时间戳',
  `off_time` int(11)  DEFAULT '0' COMMENT '最后一次下线的时间戳',
  `charge` int(11)  DEFAULT '0' COMMENT '总充值',
  `regist_time` int(11)  DEFAULT '0' COMMENT '注册时间',
  `off` int(11)  DEFAULT '0' COMMENT '是否下线，0否，1是',
  `exchange` int(11)  DEFAULT '0' COMMENT '总兑换（分）',
  `red_bag` int(11)  DEFAULT '0' COMMENT '红包值（分）',
  `pay_openid` varchar(50)  COMMENT '支付openid',
  `red_openid` varchar(50)  COMMENT '红包openid',
  `info` text  COMMENT '玩家所有信息',
  PRIMARY KEY (`role_id`),
  KEY `openid` (`openid`),
  KEY `parent_id` (`parent_id`),
  KEY `phone` (`phone`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;


-- 充值日志
DROP TABLE IF EXISTS charge_log;
CREATE TABLE `charge_log` (
    `id` varchar(50) NOT NULL  COMMENT '订单id',
    `role_id` int(11) DEFAULT '0'  COMMENT '人物id',
    `charge_rmb` int(11) DEFAULT '0'  COMMENT '充值金额（分）',
    `charge_type` int(11) DEFAULT '0'  COMMENT '充值支付类型，0微信官方，1威富通，2摇钱树支付宝',
    `item_id` int(11) DEFAULT '0'  COMMENT '购买商品id',
    `gold` int(11) DEFAULT '0'  COMMENT '抵扣的金币',
    `discount` int(11) DEFAULT '0'  COMMENT '几折购买',
    `num_list` varchar(50) DEFAULT '0'  COMMENT '购买选择的号码',
    `time` int(11) DEFAULT '0'  COMMENT '充值时间',
    `buy_num` int(11) DEFAULT '0'  COMMENT '购买数量',
    `status` int(11) DEFAULT '0'  COMMENT '订单状态0,未完成 1，成功',
    `call_time` int(11) DEFAULT '0'  COMMENT '充值回调时间',
    PRIMARY KEY (`id`),
    KEY `role_id` (`role_id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;




-- 金币获得和消费日志
DROP TABLE IF EXISTS gold_cost_log;
CREATE TABLE `gold_cost_log` (
    `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '自增id',
    `role_id` int(11) DEFAULT '0'  COMMENT '人物id',
    `type` int(11) DEFAULT '0'  COMMENT '操作类型',
    `value_befor` int(11) DEFAULT '0'  COMMENT '操作前的值',
    `cost_value` int(11) DEFAULT '0'  COMMENT '操作消费的值',
    `add_value` int(11) DEFAULT '0'  COMMENT '操作获得的值',
    `value_after` int(11) DEFAULT '0'  COMMENT '操作后的值',
    `time` int(11) DEFAULT '0'  COMMENT '时间',
    PRIMARY KEY (`id`),
    KEY `role_id` (`role_id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;




-- 玩家注册留存日志
DROP TABLE IF EXISTS role_account_log;
CREATE TABLE `role_account_log` (
    `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '自增id',
    `channel` int(11) DEFAULT '0'  COMMENT '渠道商',
    `registe` int(11) DEFAULT '0'  COMMENT '注册人数',
    `login` int(11) DEFAULT '0'  COMMENT '登陆人数',
    `next_day` int(11) DEFAULT '0'  COMMENT '次日留存',
    `seven_day` int(11) DEFAULT '0'  COMMENT '7日留存',
    `fifteen_day` int(11) DEFAULT '0'  COMMENT '15日留存',
    `time` int(11) DEFAULT '0'  COMMENT '时间',
    PRIMARY KEY (`id`),
    KEY `time` (`time`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;

-- 前端错误日志
DROP TABLE IF EXISTS client_error_log;
CREATE TABLE `client_error_log` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '自增id',
  `role_id` int(11) NOT NULL COMMENT '人物id',
  `name` varchar(50)  COMMENT '名字',
  `msg` text COMMENT '错误日志',
  `time` int(11) NOT NULL COMMENT '时间',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;

-- 在线人数统计
DROP TABLE IF EXISTS role_online_log;
CREATE TABLE `role_online_log` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '自增id',
  `num` int(11) NOT NULL COMMENT '在线人数',
  `time` int(11) NOT NULL COMMENT '时间',
  PRIMARY KEY (`id`),
  KEY `time` (`time`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;

-- 红包通知记录
DROP TABLE IF EXISTS notice_red_bag_log;
CREATE TABLE `notice_red_bag_log` (
  `order_id` varchar(50) NOT NULL COMMENT '订单id',
  `wx_order` varchar(50) DEFAULT "" COMMENT 'wx订单id',
  `role_id` int(11) NOT NULL COMMENT '人物id',
  `cny` int(11) NOT NULL COMMENT '兑换的钱（分）',
  `status` int(11) DEFAULT '0' COMMENT '兑换的钱（分）',
  `time` int(11) NOT NULL COMMENT '时间',
  PRIMARY KEY (`order_id`),
  KEY `role_id` (`role_id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;

-- 总账目统计
DROP TABLE IF EXISTS account_log;
CREATE TABLE `account_log` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '自增id',
  `charge` int(11) NOT NULL COMMENT '充值金额（分）',
  `p` int(11) NOT NULL COMMENT '投入量 金币',
  `w` int(11) NOT NULL COMMENT '产出量 金币',
  `help` int(11) NOT NULL COMMENT '助力红包 （分）',
  `help_num` int(11) NOT NULL COMMENT '助力人数',
  `time` int(11) NOT NULL COMMENT '时间',
  PRIMARY KEY (`id`),
  KEY `time` (`time`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;

-- 商品 
DROP TABLE IF EXISTS shop;
CREATE TABLE `shop` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '自增id',
  `type` int(11) NOT NULL COMMENT '商品类型',
  `name` varchar(50)  COMMENT '名字',
  `icon` varchar(300)  COMMENT '图片地址',
  `price` int(11) NOT NULL COMMENT '商品价格',
  `discount` varchar(300)  COMMENT '折扣列表',
  `page` int(11) NOT NULL COMMENT '第几页',
  PRIMARY KEY (`id`),
  KEY `type` (`type`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;

-- 商店购买情况
DROP TABLE IF EXISTS shop_buy;
CREATE TABLE `shop_buy` (
  `id` int(11) NOT NULL COMMENT '商品id',
  `price_1` int(11) NOT NULL COMMENT '原价购买数量',
  `price_2` int(11) NOT NULL COMMENT '5折购买数量',
  `price_3` int(11) NOT NULL COMMENT '1折购买数量',
  `price_4` int(11) NOT NULL COMMENT '0.1折购买数量',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8mb4;

