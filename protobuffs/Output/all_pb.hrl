-ifndef(M_1001_TOS_PB_H).
-define(M_1001_TOS_PB_H, true).
-record(m_1001_tos, {
    code = erlang:error({required, code}),
    parent_id = erlang:error({required, parent_id}),
    red_code = erlang:error({required, red_code}),
    pay_code = erlang:error({required, pay_code}),
    gift_id = erlang:error({required, gift_id}),
    role_id = erlang:error({required, role_id}),
    flag = erlang:error({required, flag})
}).
-endif.

-ifndef(M_1001_TOC_PB_H).
-define(M_1001_TOC_PB_H, true).
-record(m_1001_toc, {
    info = erlang:error({required, info}),
    id = erlang:error({required, id})
}).
-endif.

-ifndef(M_1002_TOS_PB_H).
-define(M_1002_TOS_PB_H, true).
-record(m_1002_tos, {
    account = erlang:error({required, account})
}).
-endif.

-ifndef(M_1002_TOC_PB_H).
-define(M_1002_TOC_PB_H, true).
-record(m_1002_toc, {
    info = erlang:error({required, info}),
    id = erlang:error({required, id})
}).
-endif.

-ifndef(M_1003_TOS_PB_H).
-define(M_1003_TOS_PB_H, true).
-record(m_1003_tos, {
    role_id = erlang:error({required, role_id}),
    id = erlang:error({required, id})
}).
-endif.

-ifndef(M_1003_TOC_PB_H).
-define(M_1003_TOC_PB_H, true).
-record(m_1003_toc, {
    info = erlang:error({required, info}),
    id = erlang:error({required, id})
}).
-endif.

-ifndef(M_1004_TOS_PB_H).
-define(M_1004_TOS_PB_H, true).
-record(m_1004_tos, {
    phone = erlang:error({required, phone}),
    screat = erlang:error({required, screat})
}).
-endif.

-ifndef(M_1004_TOC_PB_H).
-define(M_1004_TOC_PB_H, true).
-record(m_1004_toc, {
    info = erlang:error({required, info}),
    id = erlang:error({required, id})
}).
-endif.

-ifndef(M_1005_TOS_PB_H).
-define(M_1005_TOS_PB_H, true).
-record(m_1005_tos, {
    phone = erlang:error({required, phone})
}).
-endif.

-ifndef(M_1005_TOC_PB_H).
-define(M_1005_TOC_PB_H, true).
-record(m_1005_toc, {
    
}).
-endif.

-ifndef(M_1006_TOS_PB_H).
-define(M_1006_TOS_PB_H, true).
-record(m_1006_tos, {
    phone = erlang:error({required, phone}),
    screat = erlang:error({required, screat}),
    id = erlang:error({required, id}),
    parent_id = erlang:error({required, parent_id}),
    gift_id = erlang:error({required, gift_id})
}).
-endif.

-ifndef(M_1006_TOC_PB_H).
-define(M_1006_TOC_PB_H, true).
-record(m_1006_toc, {
    info = erlang:error({required, info}),
    id = erlang:error({required, id})
}).
-endif.

-ifndef(P_ROLE_INFO_PB_H).
-define(P_ROLE_INFO_PB_H, true).
-record(p_role_info, {
    role_id = erlang:error({required, role_id}),
    nick_name = erlang:error({required, nick_name}),
    icon = erlang:error({required, icon}),
    gold = erlang:error({required, gold}),
    coin = erlang:error({required, coin}),
    status = erlang:error({required, status}),
    phone = erlang:error({required, phone}),
    openid = erlang:error({required, openid}),
    gift_info,
    vip = erlang:error({required, vip}),
    discount_time = erlang:error({required, discount_time})
}).
-endif.

-ifndef(M_1098_TOS_PB_H).
-define(M_1098_TOS_PB_H, true).
-record(m_1098_tos, {
    
}).
-endif.

-ifndef(M_1098_TOC_PB_H).
-define(M_1098_TOC_PB_H, true).
-record(m_1098_toc, {
    
}).
-endif.

-ifndef(M_1099_TOC_PB_H).
-define(M_1099_TOC_PB_H, true).
-record(m_1099_toc, {
    error_code = erlang:error({required, error_code})
}).
-endif.

-ifndef(M_1101_TOC_PB_H).
-define(M_1101_TOC_PB_H, true).
-record(m_1101_toc, {
    list = []
}).
-endif.

-ifndef(P_ASSETS_PB_H).
-define(P_ASSETS_PB_H, true).
-record(p_assets, {
    type = erlang:error({required, type}),
    num = erlang:error({required, num})
}).
-endif.

-ifndef(P_GIFT_INFO_PB_H).
-define(P_GIFT_INFO_PB_H, true).
-record(p_gift_info, {
    role_id = erlang:error({required, role_id}),
    gift_id = erlang:error({required, gift_id}),
    role_name = erlang:error({required, role_name}),
    left_time = erlang:error({required, left_time}),
    role_list = [],
    status = erlang:error({required, status}),
    item = erlang:error({required, item}),
    discount = erlang:error({required, discount}),
    buy_num = erlang:error({required, buy_num}),
    all_gold = erlang:error({required, all_gold}),
    steal_gold = erlang:error({required, steal_gold}),
    role_icon = erlang:error({required, role_icon})
}).
-endif.

-ifndef(P_HELP_ROLE_PB_H).
-define(P_HELP_ROLE_PB_H, true).
-record(p_help_role, {
    role_id = erlang:error({required, role_id}),
    role_name = erlang:error({required, role_name}),
    icon = erlang:error({required, icon}),
    help_time,
    time = erlang:error({required, time})
}).
-endif.

-ifndef(M_1102_TOS_PB_H).
-define(M_1102_TOS_PB_H, true).
-record(m_1102_tos, {
    shop_id = erlang:error({required, shop_id}),
    discount = erlang:error({required, discount}),
    luck_num = [],
    charge_type = erlang:error({required, charge_type}),
    buy_num = erlang:error({required, buy_num}),
    flag = erlang:error({required, flag})
}).
-endif.

-ifndef(M_1102_TOC_PB_H).
-define(M_1102_TOC_PB_H, true).
-record(m_1102_toc, {
    info = erlang:error({required, info}),
    charge_type = erlang:error({required, charge_type})
}).
-endif.

-ifndef(M_1103_TOS_PB_H).
-define(M_1103_TOS_PB_H, true).
-record(m_1103_tos, {
    type = erlang:error({required, type}),
    page = erlang:error({required, page})
}).
-endif.

-ifndef(M_1103_TOC_PB_H).
-define(M_1103_TOC_PB_H, true).
-record(m_1103_toc, {
    list = []
}).
-endif.

-ifndef(P_SHOP_PB_H).
-define(P_SHOP_PB_H, true).
-record(p_shop, {
    id = erlang:error({required, id}),
    type = erlang:error({required, type}),
    name = erlang:error({required, name}),
    icon = erlang:error({required, icon}),
    price = erlang:error({required, price}),
    discount = [],
    buy_num = erlang:error({required, buy_num})
}).
-endif.

-ifndef(M_1104_TOC_PB_H).
-define(M_1104_TOC_PB_H, true).
-record(m_1104_toc, {
    flag = erlang:error({required, flag}),
    order_id = erlang:error({required, order_id}),
    gift_id = erlang:error({required, gift_id}),
    price = erlang:error({required, price})
}).
-endif.

-ifndef(M_1105_TOS_PB_H).
-define(M_1105_TOS_PB_H, true).
-record(m_1105_tos, {
    role_id = erlang:error({required, role_id}),
    gift_id = erlang:error({required, gift_id})
}).
-endif.

-ifndef(M_1105_TOC_PB_H).
-define(M_1105_TOC_PB_H, true).
-record(m_1105_toc, {
    time = erlang:error({required, time})
}).
-endif.

-ifndef(M_1106_TOC_PB_H).
-define(M_1106_TOC_PB_H, true).
-record(m_1106_toc, {
    type = erlang:error({required, type}),
    name = erlang:error({required, name}),
    msg = erlang:error({required, msg})
}).
-endif.

-ifndef(M_1107_TOS_PB_H).
-define(M_1107_TOS_PB_H, true).
-record(m_1107_tos, {
    role_id = erlang:error({required, role_id}),
    gift_id = erlang:error({required, gift_id})
}).
-endif.

-ifndef(M_1107_TOC_PB_H).
-define(M_1107_TOC_PB_H, true).
-record(m_1107_toc, {
    gold = erlang:error({required, gold})
}).
-endif.

-ifndef(M_1108_TOS_PB_H).
-define(M_1108_TOS_PB_H, true).
-record(m_1108_tos, {
    role_id = erlang:error({required, role_id}),
    gift_id = erlang:error({required, gift_id})
}).
-endif.

-ifndef(M_1108_TOC_PB_H).
-define(M_1108_TOC_PB_H, true).
-record(m_1108_toc, {
    gold = erlang:error({required, gold})
}).
-endif.

-ifndef(M_1109_TOS_PB_H).
-define(M_1109_TOS_PB_H, true).
-record(m_1109_tos, {
    
}).
-endif.

-ifndef(M_1109_TOC_PB_H).
-define(M_1109_TOC_PB_H, true).
-record(m_1109_toc, {
    list = []
}).
-endif.

-ifndef(P_GIFT_SIMPLE_PB_H).
-define(P_GIFT_SIMPLE_PB_H, true).
-record(p_gift_simple, {
    role_id = erlang:error({required, role_id}),
    gift_id = erlang:error({required, gift_id}),
    status = erlang:error({required, status}),
    time = erlang:error({required, time}),
    name,
    icon
}).
-endif.

-ifndef(M_1110_TOS_PB_H).
-define(M_1110_TOS_PB_H, true).
-record(m_1110_tos, {
    
}).
-endif.

-ifndef(M_1110_TOC_PB_H).
-define(M_1110_TOC_PB_H, true).
-record(m_1110_toc, {
    list = []
}).
-endif.

-ifndef(M_1111_TOS_PB_H).
-define(M_1111_TOS_PB_H, true).
-record(m_1111_tos, {
    gift_id = erlang:error({required, gift_id})
}).
-endif.

-ifndef(M_1111_TOC_PB_H).
-define(M_1111_TOC_PB_H, true).
-record(m_1111_toc, {
    list = []
}).
-endif.

-ifndef(M_1112_TOS_PB_H).
-define(M_1112_TOS_PB_H, true).
-record(m_1112_tos, {
    address = erlang:error({required, address})
}).
-endif.

-ifndef(M_1112_TOC_PB_H).
-define(M_1112_TOC_PB_H, true).
-record(m_1112_toc, {
    
}).
-endif.

-ifndef(M_1113_TOS_PB_H).
-define(M_1113_TOS_PB_H, true).
-record(m_1113_tos, {
    
}).
-endif.

-ifndef(M_1113_TOC_PB_H).
-define(M_1113_TOC_PB_H, true).
-record(m_1113_toc, {
    list = []
}).
-endif.

-ifndef(M_1114_TOS_PB_H).
-define(M_1114_TOS_PB_H, true).
-record(m_1114_tos, {
    address = erlang:error({required, address})
}).
-endif.

-ifndef(M_1114_TOC_PB_H).
-define(M_1114_TOC_PB_H, true).
-record(m_1114_toc, {
    
}).
-endif.

-ifndef(P_ADDRESS_PB_H).
-define(P_ADDRESS_PB_H, true).
-record(p_address, {
    id = erlang:error({required, id}),
    name = erlang:error({required, name}),
    address = erlang:error({required, address}),
    phone = erlang:error({required, phone})
}).
-endif.

-ifndef(M_1115_TOS_PB_H).
-define(M_1115_TOS_PB_H, true).
-record(m_1115_tos, {
    
}).
-endif.

-ifndef(M_1115_TOC_PB_H).
-define(M_1115_TOC_PB_H, true).
-record(m_1115_toc, {
    discount_time = erlang:error({required, discount_time})
}).
-endif.

-ifndef(M_1116_TOS_PB_H).
-define(M_1116_TOS_PB_H, true).
-record(m_1116_tos, {
    id = erlang:error({required, id})
}).
-endif.

-ifndef(M_1116_TOC_PB_H).
-define(M_1116_TOC_PB_H, true).
-record(m_1116_toc, {
    
}).
-endif.

-ifndef(M_1117_TOS_PB_H).
-define(M_1117_TOS_PB_H, true).
-record(m_1117_tos, {
    id = erlang:error({required, id})
}).
-endif.

-ifndef(M_1117_TOC_PB_H).
-define(M_1117_TOC_PB_H, true).
-record(m_1117_toc, {
    
}).
-endif.

-ifndef(M_1118_TOS_PB_H).
-define(M_1118_TOS_PB_H, true).
-record(m_1118_tos, {
    role_id = erlang:error({required, role_id}),
    gift_id = erlang:error({required, gift_id})
}).
-endif.

-ifndef(M_1118_TOC_PB_H).
-define(M_1118_TOC_PB_H, true).
-record(m_1118_toc, {
    info = erlang:error({required, info})
}).
-endif.

-ifndef(M_1122_TOS_PB_H).
-define(M_1122_TOS_PB_H, true).
-record(m_1122_tos, {
    type = erlang:error({required, type}),
    num = erlang:error({required, num}),
    charge_type = erlang:error({required, charge_type})
}).
-endif.

-ifndef(M_1122_TOC_PB_H).
-define(M_1122_TOC_PB_H, true).
-record(m_1122_toc, {
    info = erlang:error({required, info}),
    charge_type = erlang:error({required, charge_type})
}).
-endif.

-ifndef(M_1123_TOC_PB_H).
-define(M_1123_TOC_PB_H, true).
-record(m_1123_toc, {
    type = erlang:error({required, type}),
    list = []
}).
-endif.

-ifndef(M_1130_TOS_PB_H).
-define(M_1130_TOS_PB_H, true).
-record(m_1130_tos, {
    phone = erlang:error({required, phone})
}).
-endif.

-ifndef(M_1130_TOC_PB_H).
-define(M_1130_TOC_PB_H, true).
-record(m_1130_toc, {
    
}).
-endif.

-ifndef(M_1131_TOS_PB_H).
-define(M_1131_TOS_PB_H, true).
-record(m_1131_tos, {
    code = erlang:error({required, code})
}).
-endif.

-ifndef(M_1131_TOC_PB_H).
-define(M_1131_TOC_PB_H, true).
-record(m_1131_toc, {
    type = erlang:error({required, type}),
    num = erlang:error({required, num}),
    phone = erlang:error({required, phone})
}).
-endif.

-ifndef(M_1136_TOS_PB_H).
-define(M_1136_TOS_PB_H, true).
-record(m_1136_tos, {
    url = erlang:error({required, url})
}).
-endif.

-ifndef(M_1136_TOC_PB_H).
-define(M_1136_TOC_PB_H, true).
-record(m_1136_toc, {
    sign = erlang:error({required, sign}),
    timestamp = erlang:error({required, timestamp}),
    noncestr = erlang:error({required, noncestr})
}).
-endif.

-ifndef(M_1141_TOS_PB_H).
-define(M_1141_TOS_PB_H, true).
-record(m_1141_tos, {
    
}).
-endif.

-ifndef(M_1141_TOC_PB_H).
-define(M_1141_TOC_PB_H, true).
-record(m_1141_toc, {
    list = []
}).
-endif.

-ifndef(P_NEAR_ROLE_PB_H).
-define(P_NEAR_ROLE_PB_H, true).
-record(p_near_role, {
    role_id = erlang:error({required, role_id}),
    name = erlang:error({required, name}),
    icon = erlang:error({required, icon}),
    luck = erlang:error({required, luck})
}).
-endif.

-ifndef(M_1142_TOS_PB_H).
-define(M_1142_TOS_PB_H, true).
-record(m_1142_tos, {
    type = erlang:error({required, type})
}).
-endif.

-ifndef(M_1142_TOC_PB_H).
-define(M_1142_TOC_PB_H, true).
-record(m_1142_toc, {
    type = erlang:error({required, type}),
    list = []
}).
-endif.

-ifndef(P_ORDER_INFO_PB_H).
-define(P_ORDER_INFO_PB_H, true).
-record(p_order_info, {
    order_id = erlang:error({required, order_id}),
    num_list = [],
    multiple = erlang:error({required, multiple}),
    cost = erlang:error({required, cost}),
    win = erlang:error({required, win}),
    time = erlang:error({required, time})
}).
-endif.

-ifndef(M_1143_TOS_PB_H).
-define(M_1143_TOS_PB_H, true).
-record(m_1143_tos, {
    type = erlang:error({required, type}),
    num = erlang:error({required, num})
}).
-endif.

-ifndef(M_1143_TOC_PB_H).
-define(M_1143_TOC_PB_H, true).
-record(m_1143_toc, {
    num = erlang:error({required, num})
}).
-endif.

-ifndef(M_1148_TOC_PB_H).
-define(M_1148_TOC_PB_H, true).
-record(m_1148_toc, {
    msg = erlang:error({required, msg})
}).
-endif.

-ifndef(M_1197_TOS_PB_H).
-define(M_1197_TOS_PB_H, true).
-record(m_1197_tos, {
    msg = erlang:error({required, msg})
}).
-endif.

-ifndef(M_1197_TOC_PB_H).
-define(M_1197_TOC_PB_H, true).
-record(m_1197_toc, {
    
}).
-endif.

-ifndef(M_1198_TOS_PB_H).
-define(M_1198_TOS_PB_H, true).
-record(m_1198_tos, {
    type = erlang:error({required, type}),
    num = erlang:error({required, num})
}).
-endif.

-ifndef(M_1198_TOC_PB_H).
-define(M_1198_TOC_PB_H, true).
-record(m_1198_toc, {
    
}).
-endif.

-ifndef(M_1199_TOS_PB_H).
-define(M_1199_TOS_PB_H, true).
-record(m_1199_tos, {
    type = erlang:error({required, type}),
    num = erlang:error({required, num})
}).
-endif.

-ifndef(M_1199_TOC_PB_H).
-define(M_1199_TOC_PB_H, true).
-record(m_1199_toc, {
    
}).
-endif.

-ifndef(M_1201_TOS_PB_H).
-define(M_1201_TOS_PB_H, true).
-record(m_1201_tos, {
    type = erlang:error({required, type}),
    start = erlang:error({required, start}),
    num = erlang:error({required, num}),
    up_or_down = erlang:error({required, up_or_down}),
    date = erlang:error({required, date})
}).
-endif.

-ifndef(M_1201_TOC_PB_H).
-define(M_1201_TOC_PB_H, true).
-record(m_1201_toc, {
    list = [],
    num = erlang:error({required, num}),
    value = erlang:error({required, value})
}).
-endif.

-ifndef(P_RANK_INFO_PB_H).
-define(P_RANK_INFO_PB_H, true).
-record(p_rank_info, {
    num = erlang:error({required, num}),
    id = erlang:error({required, id}),
    role_id = erlang:error({required, role_id}),
    name = erlang:error({required, name}),
    icon = erlang:error({required, icon}),
    vip = erlang:error({required, vip}),
    sign = erlang:error({required, sign}),
    value1 = erlang:error({required, value1}),
    value2,
    value3,
    value4,
    value5
}).
-endif.

