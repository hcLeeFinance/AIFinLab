"""產出 Excel 報表與互動式 HTML 儀表板。"""
import json
from pathlib import Path

import pandas as pd

from aggregate import aggregate_yearly, recent_stats, recent_wide
from clean import DISTRICTS

NOTES = [
    "時間軸是「交易日期」所在的季，不是資料公告的期別。最新一季的案件還會在下一期陸續登錄，筆數會偏少。",
    "房屋單價 =（總價 − 車位總價）÷（建物移轉面積 − 車位面積），換算成萬元／坪。",
    "車位價格併在總價裡的交易（大樓約 2～4 成），以同行政區、同車位類別、同年度的車位成交中位數估算後扣除（不足 5 筆改用全市）；車位面積沒填的無法計算淨面積，不列入。直接排除這類交易會讓新北市大樓高估約 3～4%。",
    "本序列是「中古屋」價格：屋齡未滿 1 年（含交易日期早於完工）的交易一律排除。預售屋交屋過戶的價格是數年前簽約時決定，2017 年以前多半沒有備註可辨識，若只靠備註排除，早期新屋占比（新北大樓 2012 年達 44%）會遠高於近年，造成走勢失真；改用屋齡規則可讓各年標準一致。另亦排除備註標示預售屋、分件／分次登記的交易。",
    "只計「房地」或「房地＋車位」的單一建物交易，建物型態為住宅大樓（11 層以上有電梯）、華廈（10 層以下有電梯）、公寓（5 樓以下無電梯），主要用途為住宅。",
    "排除備註有親友／特殊關係、預售屋或分件／分次登記、政府專案配售（區段徵收、合宜住宅）、訴訟或拍賣、瑕疵、急買急賣、債權債務、政府標售、協議價購、公設保留地、都更等字樣的交易。含增建（頂樓加蓋、陽台外推等）的交易保留，因為舊制只有一個合併勾選項，排除會讓前後期標準不一致。",
    "極端值：各行政區 × 型態 × 年，超出四分位距 3 倍的成交剔除。",
    "車位價格取自車位明細，只計車位價格大於 0（有單獨標價）的車位，排除親友／特殊關係交易。單獨標價的比例從 2012 年約 25% 提高到近年約 70%，早期車位序列樣本較少、代表性較弱。",
    "單季成交少於 5 筆的點以空心圓標示，數值波動大，僅供參考。",
    "行政區比較的年度數值是該年全部成交的中位數（不是季中位數再平均）。年化漲幅 =（終點年中位數 ÷ 起點年中位數）^(1 ÷ 年數) − 1，只反映房價變動，不含租金收益，也不代表個別物件的實際報酬。起點或終點年成交少於 5 筆時不計算。最新一季所在年度仍在補登，不列入完整年度。",
]


def _series(stats, item_col, time_col="季"):
    out = {}
    for (city, dist, item), g in stats.groupby(["縣市", "行政區", item_col]):
        g = g.sort_values(time_col)
        out[f"{city}|{dist}|{item}"] = {
            "q": [str(t) for t in g[time_col]],
            "med": g["中位數"].round(2).tolist(),
            "mean": g["平均數"].round(2).tolist(),
            "p25": g["P25"].round(2).tolist(),
            "p75": g["P75"].round(2).tolist(),
            "n": g["筆數"].astype(int).tolist(),
        }
    return out


def _recent(stats, item_col):
    return {f"{r.縣市}|{r.行政區}|{getattr(r, item_col)}": {"med": round(r.中位數, 2), "n": int(r.筆數)}
            for r in stats.itertuples()}


def build_dashboard(houses, parking, house_stats, park_stats, last_season, out_path: Path, plotly_tag):
    rh, label = recent_stats(houses, "型態", "單價萬坪")
    rp, _ = recent_stats(parking, "車位類別", "車位價萬")
    present = set(house_stats["縣市"] + "|" + house_stats["行政區"])
    hy, py = aggregate_yearly(houses, parking)
    quarters = sorted(house_stats["季"].unique())
    # 第一個完整年度：最早有第一季資料的年份（2012 只有 8～12 月）。
    # 最後完整年度：最新一季永遠還在補登，所以最新一季所在年度不算完整。
    first_full = min(int(q[:4]) for q in quarters if q.endswith("Q1"))
    last_full = int(quarters[-1][:4]) - 1
    data = {
        "quarters": quarters,
        "firstFullYear": first_full,
        "lastFullYear": last_full,
        "houseYear": _series(hy, "型態", "年"),
        "parkYear": _series(py[py["車位類別"] == "全部"], "車位類別", "年"),
        "completeQuarter": sorted(house_stats["季"].unique())[-2],
        "districts": {c: [d for d in ds if f"{c}|{d}" in present] for c, ds in DISTRICTS.items()},
        "house": _series(house_stats, "型態"),
        "park": _series(park_stats, "車位類別"),
        "recent": {"house": _recent(rh, "型態"), "park": _recent(rp, "車位類別")},
        "recentLabel": label,
        "lastSeason": last_season,
        "notes": NOTES,
    }
    tpl = (Path(__file__).parent / "dashboard_template.html").read_text(encoding="utf-8")
    html = tpl.replace("__PLOTLY__", plotly_tag).replace(
        "__DATA__", json.dumps(data, ensure_ascii=False, separators=(",", ":")))
    out_path.write_text(html, encoding="utf-8")


def build_excel(houses, parking, house_stats, park_stats, funnel, out_path: Path):
    hw, label = recent_wide(houses, "型態", "單價萬坪")
    pw, _ = recent_wide(parking, "車位類別", "車位價萬")
    order = {f"{c}|{d}": i for i, (c, d) in enumerate(
        [(c, "全市") for c in DISTRICTS] + [(c, d) for c, ds in DISTRICTS.items() for d in ds])}

    def sort(df):
        return df.assign(_o=(df["縣市"] + "|" + df["行政區"]).map(order)).sort_values("_o", kind="stable").drop(columns="_o")

    hw = sort(hw)[["縣市", "行政區", "大樓", "華廈", "公寓", "大樓_筆數", "華廈_筆數", "公寓_筆數"]]
    pcols = [c for c in ["全部", "坡道平面", "坡道機械", "升降平面", "升降機械", "塔式車位", "一樓平面", "其他"] if c in pw]
    pw = sort(pw)[["縣市", "行政區"] + pcols + [c + "_筆數" for c in pcols]]
    # 房屋季中位數寬表：每列一季，每欄一個 行政區-型態，方便直接畫圖
    piv = house_stats.pivot_table(index="季", columns=["縣市", "行政區", "型態"], values="中位數").round(1)
    piv.columns = ["-".join(c) for c in piv.columns]

    with pd.ExcelWriter(out_path, engine="xlsxwriter") as xw:
        sheets = [
            (f"房屋_近4季({label})", hw),
            (f"車位_近4季", pw),
            ("房屋_季統計", sort(house_stats).round(2)),
            ("車位_季統計", sort(park_stats).round(2)),
            ("房屋_季中位數寬表", piv.reset_index()),
            ("清洗漏斗", funnel),
            ("說明", pd.DataFrame({"說明": NOTES})),
        ]
        for name, df in sheets:
            name = name[:31]
            df.to_excel(xw, sheet_name=name, index=False)
            ws = xw.sheets[name]
            ws.freeze_panes(1, 2 if "行政區" in df.columns else 1)
            for i, c in enumerate(df.columns):
                width = max(len(str(c)), *(df[c].astype(str).str.len().head(200).tolist() or [4])) + 2
                ws.set_column(i, i, min(width * 1.6 if i < 2 else width * 1.2, 60))
