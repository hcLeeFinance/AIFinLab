"""依交易季彙總：行政區 × 型態（房屋）、行政區 × 車位類別（車位），並附全市合計。"""
import pandas as pd

MIN_N = 5  # 單季筆數少於此數視為樣本不足


def _stats(df, keys, col, age=False):
    aggs = dict(筆數=(col, "size"), 中位數=(col, "median"), 平均數=(col, "mean"),
                P25=(col, lambda s: s.quantile(0.25)), P75=(col, lambda s: s.quantile(0.75)))
    if age:
        aggs["屋齡中位數"] = ("屋齡", "median")
    return df.groupby(keys, observed=True).agg(**aggs).reset_index()


def _with_city_total(df, keys, col, age=False):
    """各行政區統計 + 行政區='全市' 的合計列。"""
    by_dist = _stats(df, ["縣市", "行政區"] + keys, col, age)
    by_city = _stats(df, ["縣市"] + keys, col, age).assign(行政區="全市")
    out = pd.concat([by_dist, by_city], ignore_index=True)
    out["樣本不足"] = out["筆數"] < MIN_N
    return out


def _add_quarter(df):
    return df.assign(季=df["交易日期"].dt.to_period("Q").astype(str))


def aggregate_houses(houses):
    out = _with_city_total(_add_quarter(houses), ["型態", "季"], "單價萬坪", age=True)
    return out.sort_values(["縣市", "行政區", "型態", "季"]).reset_index(drop=True)


def aggregate_parking(parking):
    df = _add_quarter(parking)
    out = pd.concat([_with_city_total(df, ["車位類別", "季"], "車位價萬"),
                     _with_city_total(df.assign(車位類別="全部"), ["車位類別", "季"], "車位價萬")],
                    ignore_index=True)
    return out.sort_values(["縣市", "行政區", "車位類別", "季"]).reset_index(drop=True)


def recent_stats(detail, item_col, value_col, n_quarters=4):
    """最近 n 個交易季合併計算（用原始明細，不是季中位數再取中位數）。"""
    df = _add_quarter(detail)
    recent = sorted(df["季"].unique())[-n_quarters:]
    df = df[df["季"].isin(recent)]
    if item_col == "車位類別":
        df = pd.concat([df, df.assign(車位類別="全部")], ignore_index=True)
    return _with_city_total(df, [item_col], value_col), f"{recent[0]}～{recent[-1]}"


def recent_wide(detail, item_col, value_col, n_quarters=4):
    """行政區 × 項目 的中位數與筆數寬表。"""
    stats, label = recent_stats(detail, item_col, value_col, n_quarters)
    med = stats.pivot_table(index=["縣市", "行政區"], columns=item_col, values="中位數")
    cnt = stats.pivot_table(index=["縣市", "行政區"], columns=item_col, values="筆數", aggfunc="sum").add_suffix("_筆數")
    wide = med.round(1).join(cnt.astype("Int64")).reset_index()
    return wide, label


def aggregate_yearly(houses, parking):
    """年度統計（直接用當年全部明細，不是季中位數再平均），供行政區比較與年化漲幅使用。"""
    h = houses.assign(年=houses["交易日期"].dt.year)
    hs = _with_city_total(h, ["型態", "年"], "單價萬坪")
    p = parking.assign(年=parking["交易日期"].dt.year)
    ps = pd.concat([_with_city_total(p, ["車位類別", "年"], "車位價萬"),
                    _with_city_total(p.assign(車位類別="全部"), ["車位類別", "年"], "車位價萬")],
                   ignore_index=True)
    return hs, ps
