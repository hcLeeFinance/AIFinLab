"""依交易季彙總：行政區 × 型態 × 細分（房屋）、行政區 × 車位類別（車位），並附全市合計。"""
import numpy as np
import pandas as pd

MIN_N = 5  # 單季筆數少於此數視為樣本不足
AGE_BANDS = [(3, 10, "屋齡3–10年"), (10, 20, "屋齡10–20年"), (20, 30, "屋齡20–30年"), (30, np.inf, "屋齡30年以上")]
SIZE_BANDS = [(0, 20, "未滿20坪"), (20, 35, "20–35坪"), (35, 50, "35–50坪"), (50, np.inf, "50坪以上")]
SUBS = ["全部"] + [b[2] for b in AGE_BANDS] + [b[2] for b in SIZE_BANDS]


def _band(values, bands):
    out = pd.Series(None, index=values.index, dtype=object)
    for lo, hi, name in bands:
        out[(values >= lo) & (values < hi)] = name
    return out


def expand_segments(homes):
    """每筆交易展開成「全部」「坪數級距」與（中古屋才有的）「屋齡級距」三列，細分之間各自獨立、不交叉。"""
    size = homes.assign(細分=_band(homes["淨坪數"], SIZE_BANDS))
    age = homes[homes["屋況"] == "中古屋"]
    age = age.assign(細分=_band(age["屋齡"], AGE_BANDS))
    out = pd.concat([homes.assign(細分="全部"), size, age], ignore_index=True)
    return out[out["細分"].notna()]


def _stats(df, keys, col, age=False):
    g = df.groupby(keys, observed=True)[col]
    out = pd.DataFrame({"筆數": g.size(), "中位數": g.median(), "平均數": g.mean(),
                        "P25": g.quantile(0.25), "P75": g.quantile(0.75)})
    if age:
        out["屋齡中位數"] = df.groupby(keys, observed=True)["屋齡"].median()
    return out.reset_index()


def _with_city_total(df, keys, col, age=False):
    """各行政區統計 + 行政區='全市' 的合計列。"""
    by_dist = _stats(df, ["縣市", "行政區"] + keys, col, age)
    by_city = _stats(df, ["縣市"] + keys, col, age).assign(行政區="全市")
    out = pd.concat([by_dist, by_city], ignore_index=True)
    out["樣本不足"] = out["筆數"] < MIN_N
    return out


def _add_quarter(df):
    return df.assign(季=df["交易日期"].dt.to_period("Q").astype(str))


HOUSE_KEYS = ["型態", "細分"]  # 型態：大樓、華廈、公寓（中古屋）、新成屋、預售屋


def aggregate_houses(segs):
    out = _with_city_total(_add_quarter(segs), HOUSE_KEYS + ["季"], "單價萬坪", age=True)
    return out.sort_values(["縣市", "行政區"] + HOUSE_KEYS + ["季"]).reset_index(drop=True)


def aggregate_parking(parking):
    df = _add_quarter(parking)
    out = pd.concat([_with_city_total(df, ["車位類別", "季"], "車位價萬"),
                     _with_city_total(df.assign(車位類別="全部"), ["車位類別", "季"], "車位價萬")],
                    ignore_index=True)
    return out.sort_values(["縣市", "行政區", "車位類別", "季"]).reset_index(drop=True)


def recent_stats(detail, item_cols, value_col, n_quarters=4):
    """最近 n 個交易季合併計算（用原始明細，不是季中位數再取中位數）。"""
    item_cols = [item_cols] if isinstance(item_cols, str) else item_cols
    df = _add_quarter(detail)
    recent = sorted(df["季"].unique())[-n_quarters:]
    df = df[df["季"].isin(recent)]
    if item_cols == ["車位類別"]:
        df = pd.concat([df, df.assign(車位類別="全部")], ignore_index=True)
    return _with_city_total(df, item_cols, value_col), f"{recent[0]}～{recent[-1]}"


def recent_wide(detail, item_col, value_col, n_quarters=4):
    """行政區 × 項目 的中位數與筆數寬表。"""
    stats, label = recent_stats(detail, item_col, value_col, n_quarters)
    med = stats.pivot_table(index=["縣市", "行政區"], columns=item_col, values="中位數")
    cnt = stats.pivot_table(index=["縣市", "行政區"], columns=item_col, values="筆數", aggfunc="sum").add_suffix("_筆數")
    wide = med.round(1).join(cnt.astype("Int64")).reset_index()
    return wide, label


def aggregate_yearly(segs, parking):
    """年度統計（直接用當年全部明細，不是季中位數再平均），供行政區比較與年化漲幅使用。"""
    h = segs.assign(年=segs["交易日期"].dt.year)
    hs = _with_city_total(h, HOUSE_KEYS + ["年"], "單價萬坪")
    p = parking.assign(年=parking["交易日期"].dt.year)
    ps = pd.concat([_with_city_total(p, ["車位類別", "年"], "車位價萬"),
                    _with_city_total(p.assign(車位類別="全部"), ["車位類別", "年"], "車位價萬")],
                   ignore_index=True)
    return hs, ps
