"""合併各季 CSV，篩選雙北住宅大樓／華廈／公寓，計算扣除車位後的淨單價；整理車位明細。"""
import re
from pathlib import Path

import numpy as np
import pandas as pd

PING = 3.30579  # 1 坪 = 3.30579 平方公尺
CITY = {"a": "臺北市", "f": "新北市"}
BTYPE = {"住宅大樓": "大樓", "華廈": "華廈", "公寓": "公寓"}
DISTRICTS = {
    "臺北市": ["中正區", "大同區", "中山區", "松山區", "大安區", "萬華區", "信義區", "士林區", "北投區", "內湖區", "南港區", "文山區"],
    "新北市": ["板橋區", "三重區", "中和區", "永和區", "新莊區", "新店區", "樹林區", "鶯歌區", "三峽區", "淡水區",
             "汐止區", "瑞芳區", "土城區", "蘆洲區", "五股區", "泰山區", "林口區", "深坑區", "石碇區", "坪林區",
             "三芝區", "石門區", "八里區", "平溪區", "雙溪區", "貢寮區", "金山區", "萬里區", "烏來區"],
}
TARGETS = {"房地(土地+建物)", "房地(土地+建物)+車位"}
PARK_TYPES = ["坡道平面", "坡道機械", "升降平面", "升降機械", "塔式車位", "一樓平面"]

# 價格可能異於市場行情的交易。增建類（含增建或未登記建物、頂樓加蓋、陽台外推…）
# 舊制只有一個合併勾選項、新制才拆細項，為了前後期一致全部保留。
EXCLUDE_NOTE = re.compile(
    "親友|員工|特殊關係|親等|等親|親屬|夫妻|"
    "預售屋|分件登記|分次登記|配售|區段徵收|合宜住宅|社會住宅|訴訟|拍賣|佔用|占用|"
    "瑕疵|凶宅|非自然死亡|海砂|輻射|"
    "急買急賣|債權債務|抵債|政府機關|協議價購|標售|標讓售|公共設施保留地|"
    "地上權|重建|重劃|都更|畸零地|合建|毛胚|信託"
)
# 車位明細只排除會影響價格的關係交易
EXCLUDE_NOTE_PARK = re.compile("親友|員工|特殊關係|親等|等親|親屬|夫妻|政府機關|協議價購|標售")


def _norm_cols(df):
    df.columns = [re.sub(r"[()（）\s]", "", c) for c in df.columns]
    return df


def _read(path):
    return _norm_cols(pd.read_csv(path, encoding="utf-8-sig", skiprows=[1], dtype=str))


def load_raw(raw_dir: Path):
    mains, parks = [], []
    done = [p for p in raw_dir.iterdir() if (p / ".done").exists()]
    for season_dir in sorted(done, key=lambda p: (int(p.name[:-2]), int(p.name[-1]))):
        for code, city in CITY.items():
            f = season_dir / f"{code}_lvr_land_a.csv"
            if f.exists():
                m = _read(f).assign(縣市=city, 來源季別=season_dir.name)
                mains.append(m)
            f = season_dir / f"{code}_lvr_land_a_park.csv"
            if f.exists():
                parks.append(_read(f).assign(來源季別=season_dir.name))
    main = pd.concat(mains, ignore_index=True)
    park = pd.concat(parks, ignore_index=True)
    # 同一編號出現在多季（更正資料）時保留最新一季
    main = main.drop_duplicates("編號", keep="last")
    return main, park


def _roc_date(s):
    s = s.fillna("").str.strip().str.zfill(7)
    y = pd.to_numeric(s.str[:-4], errors="coerce") + 1911
    m = pd.to_numeric(s.str[-4:-2], errors="coerce")
    d = pd.to_numeric(s.str[-2:], errors="coerce")
    return pd.to_datetime(dict(year=y, month=m, day=d.clip(1, 28)), errors="coerce")


def _num(s):
    return pd.to_numeric(s, errors="coerce")


def _common(df):
    """兩種明細共用：日期、行政區、樣本期間。"""
    df = df.copy()
    df["交易日期"] = _roc_date(df["交易年月日"])
    df["行政區"] = df["鄉鎮市區"].str.strip()
    ok = df["交易日期"].between("2012-08-01", pd.Timestamp.today())
    valid_dist = [c + "|" + d for c, ds in DISTRICTS.items() for d in ds]
    return df[ok & (df["縣市"] + "|" + df["行政區"].fillna("")).isin(valid_dist)]


def clean_houses(main, park, parking):
    funnel = [("原始（雙北、去重後）", len(main))]
    df = _common(main)
    funnel.append(("交易日期有效", len(df)))

    df = df[df["交易標的"].isin(TARGETS)]
    funnel.append(("房地／房地+車位", len(df)))

    df["型態"] = df["建物型態"].fillna("").map(lambda s: next((v for k, v in BTYPE.items() if s.startswith(k)), None))
    df = df[df["型態"].notna()]
    funnel.append(("大樓／華廈／公寓", len(df)))

    use = df["主要用途"].fillna("")
    df = df[use.str.contains("住") | use.str.startswith("見") | (use == "")]
    funnel.append(("主要用途為住宅", len(df)))

    n_bldg = _num(df["交易筆棟數"].str.extract(r"建物(\d+)")[0])
    df = df[n_bldg == 1]
    funnel.append(("單一建物交易", len(df)))

    df = df[~df["備註"].fillna("").str.contains(EXCLUDE_NOTE)]
    funnel.append(("排除特殊交易備註", len(df)))

    # 預售屋交屋過戶在 2017 年前多半沒有備註，改用屋齡一致排除：完工未滿 1 年（含交易日早於完工、
    # 即用簽約日登錄的預售案）一律不計，序列定義為中古屋
    built = _roc_date(df["建築完成年月"].fillna("").str.strip().str.ljust(7, "0").str[:5] + "01")
    age = (df["交易日期"] - built).dt.days / 365.25
    df = df[~(age < 1)]
    funnel.append(("排除屋齡未滿 1 年（新成屋、預售交屋）", len(df)))

    total = _num(df["總價元"])
    area = _num(df["建物移轉總面積平方公尺"])
    p_price = _num(df["車位總價元"]).fillna(0)
    p_area = _num(df["車位移轉總面積平方公尺"]).fillna(0)
    has_park = df["交易標的"].str.endswith("車位") | df["車位類別"].notna()
    bundled = has_park & (p_price <= 0)
    # 車位價格併入總價：用同區同類別同年的車位中位價估算後扣除；車位面積沒填則無法算淨面積
    est = df["編號"].map(_estimate_bundled(df[bundled], park, parking)) * 1e4
    df["車位價來源"] = np.select([bundled, has_park], ["估算", "單獨標價"], "無車位")
    park_price = np.where(bundled, est, np.where(has_park, p_price, 0))
    net_area = area - np.where(has_park, p_area, 0)
    df["淨總價萬"] = (total - park_price) / 1e4
    df["淨坪數"] = net_area / PING
    df["單價萬坪"] = df["淨總價萬"] / df["淨坪數"]
    ok = ~bundled | ((p_area > 0) & est.notna())
    df = df[ok & (df["淨坪數"] >= 3) & (df["淨總價萬"] > 0)]
    funnel.append(("可拆出車位價、面積合理", len(df)))

    df = df[df["單價萬坪"].between(3, 600)]
    df["年"] = df["交易日期"].dt.year
    q1 = df.groupby(["縣市", "行政區", "型態", "年"])["單價萬坪"].transform(lambda s: s.quantile(0.25))
    q3 = df.groupby(["縣市", "行政區", "型態", "年"])["單價萬坪"].transform(lambda s: s.quantile(0.75))
    iqr = q3 - q1
    df = df[df["單價萬坪"].between(q1 - 3 * iqr, q3 + 3 * iqr)]
    funnel.append(("剔除極端值", len(df)))

    built = _roc_date(df["建築完成年月"].fillna("").str.strip().str.ljust(7, "0").str[:5] + "01")
    df["屋齡"] = ((df["交易日期"] - built).dt.days / 365.25).where(lambda s: s.between(0, 80))

    cols = ["縣市", "行政區", "型態", "交易日期", "單價萬坪", "淨總價萬", "淨坪數", "屋齡",
            "總價元", "車位總價元", "車位價來源", "土地位置建物門牌", "移轉層次", "總樓層數", "備註", "編號", "來源季別"]
    return df[cols].sort_values("交易日期").reset_index(drop=True), pd.DataFrame(funnel, columns=["步驟", "筆數"])


def _latest_park(park):
    """同一編號的車位明細若出現在多季，只保留最新一季那一批（同交易內相同車位不可去重）。"""
    order = park["來源季別"].map(lambda s: int(s[:-2]) * 10 + int(s[-1]))
    p = park[order == order.groupby(park["編號"]).transform("max")].copy()
    p["車位類別"] = p["車位類別"].where(p["車位類別"].isin(PARK_TYPES), "其他")
    return p


def _estimate_bundled(bundled, park, parking):
    """逐一車位估價（車位明細每列一個車位）：同縣市行政區類別年份中位數（≥5 筆），不足則用全市同類別同年。"""
    yr = parking["交易日期"].dt.year
    m1 = parking.groupby(["縣市", "行政區", "車位類別", yr])["車位價萬"].agg(["median", "size"])
    m1 = m1.loc[m1["size"] >= 5, "median"]
    m2 = parking.groupby(["縣市", "車位類別", yr])["車位價萬"].median()
    pk = _latest_park(park)
    info = bundled.set_index("編號")[["縣市", "行政區"]].assign(年=bundled["交易日期"].dt.year.values)
    pk = pk[pk["編號"].isin(info.index)].join(info, on="編號")
    est = m1.reindex(pd.MultiIndex.from_frame(pk[["縣市", "行政區", "車位類別", "年"]])).values
    fallback = m2.reindex(pd.MultiIndex.from_frame(pk[["縣市", "車位類別", "年"]])).values
    pk["估價"] = np.where(np.isnan(est), fallback, est)
    return pk.groupby("編號")["估價"].agg(lambda s: s.sum(min_count=len(s)))


def clean_parking(main, park):
    m = _common(main)
    m = m[~m["備註"].fillna("").str.contains(EXCLUDE_NOTE_PARK)]
    p = _latest_park(park).merge(m[["編號", "縣市", "行政區", "交易日期", "交易標的", "建物型態"]], on="編號", how="inner")
    p["車位價萬"] = _num(p["車位價格"]) / 1e4
    p["車位坪數"] = _num(p["車位面積平方公尺"]) / PING
    p = p[p["車位價萬"].between(30, 1500)]
    p["年"] = p["交易日期"].dt.year
    g = p.groupby(["縣市", "行政區", "車位類別", "年"])["車位價萬"]
    q1, q3 = g.transform(lambda s: s.quantile(0.25)), g.transform(lambda s: s.quantile(0.75))
    p = p[p["車位價萬"].between(q1 - 3 * (q3 - q1), q3 + 3 * (q3 - q1))]
    cols = ["縣市", "行政區", "車位類別", "交易日期", "車位價萬", "車位坪數", "車位所在樓層", "交易標的", "建物型態", "編號"]
    return p[cols].sort_values("交易日期").reset_index(drop=True)
