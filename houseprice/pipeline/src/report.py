"""產出 Excel 報表與互動式 HTML 儀表板。"""
import hashlib
import json
from pathlib import Path

import pandas as pd

from aggregate import HOUSE_KEYS, aggregate_yearly, recent_stats, recent_wide
from clean import DISTRICTS

NOTES = [
    "時間軸是「交易日期」所在的季，不是資料公告的期別。最新一季的案件還會在下一期陸續登錄，筆數會偏少。",
    "房屋單價 =（總價 − 車位總價）÷（建物移轉面積 − 車位面積），換算成萬元／坪。",
    "車位價格併在總價裡的交易（大樓約 2～4 成），以同行政區、同車位類別、同年度的車位成交中位數估算後扣除（不足 5 筆改用全市，全市也不足 5 筆則不列入）。不論車位是否單獨標價，車位面積沒填（0）的交易無法扣出淨面積、單價會被低估，一律不列入。直接排除這類交易會讓新北市大樓高估約 3～4%。",
    "屋況：中古屋 = 屋齡 3 年以上；新成屋 = 屋齡 1～3 年（即屋齡篩選的 1～3 年級距）；預售屋取自預售屋買賣檔，時間軸為簽約日。成屋屋齡未滿 1 年（含交易日期早於完工）的交易一律排除：這類交易多半是預售屋交屋過戶，價格是數年前簽約時決定，2017 年以前多半沒有備註可辨識，而且同一戶已在預售屋序列以簽約價計入一次。建築完成年月缺漏、無法判斷屋齡的交易也排除（占比從 2012 年約 9% 降到近年約 3%，保留會讓樣本組成隨時間改變）。",
    "預售屋資料在 2021 年 7 月 1 日（實價登錄 2.0）前後不可比：之前只有委託代銷的案件要申報，而且是代銷契約結束後才整批申報，建商自售的案件不必申報；之後不論自售或代銷，都在簽約 30 日內申報。2021Q3 以前的預售屋序列樣本不完整、偏向代銷大案，圖上以虛線標示斷點。預售屋的行政區中位數主要取決於當期在賣哪些建案（地段、等級），單季或單年的大幅跳動多半是建案組成改變，不是同一批房子漲跌。已解約的預售屋交易排除；併入總價的預售車位以預售屋自己的車位成交中位價估算扣除。",
    "細分篩選：屋齡級距（3～10、10～20、20～30、30 年以上，只適用中古屋）與淨坪數級距（扣除車位後未滿 20、20～35、35～50、50 坪以上，三種屋況都適用）。兩種篩選擇一使用、不交叉，分得越細樣本越少，請留意空心點（少於 5 筆）。",
    "只計「房地」或「房地＋車位」的單一建物交易，建物型態為住宅大樓（11 層以上有電梯）、華廈（10 層以下有電梯）、公寓（5 樓以下無電梯），主要用途為住宅。",
    "排除備註有親友／特殊關係、預售屋或分件／分次登記、政府專案配售（區段徵收、合宜住宅）、訴訟或拍賣、瑕疵、急買急賣、債權債務、政府標售、協議價購、公設保留地、都更等字樣的交易。含增建（頂樓加蓋、陽台外推等）的交易保留，因為舊制只有一個合併勾選項，排除會讓前後期標準不一致。",
    "極端值：各行政區 × 型態 × 屋況 × 年，超出四分位距 3 倍的成交剔除。",
    "車位價格取自車位明細，只計車位價格大於 0（有單獨標價）的車位，排除親友／特殊關係交易。單獨標價的比例從 2012 年約 25% 提高到近年約 70%，早期車位序列樣本較少、代表性較弱。",
    "單季成交少於 5 筆的點以空心圓標示，數值波動大，僅供參考。",
    "行政區比較的年度數值是該年全部成交的中位數（不是季中位數再平均）。年化報酬率 =（終點年中位數 ÷ 起點年中位數）^(1 ÷ 年數) − 1，只反映房價變動，不含租金收益，也不代表個別物件的實際報酬。起點或終點年成交少於 5 筆時不計算。最新一季所在年度仍在補登，不列入完整年度。",
]


DEFAULT_SUB = "全部"
PRESALE_BREAK = "2021Q3"  # 實價登錄 2.0：2021-07-01 起預售屋改為簽約 30 日內申報


def _series(stats, item_cols, labels, time_col="季", fields=("med", "mean", "p25", "p75")):
    """每條序列以標籤索引 i 對應 labels（季或年），數值取一位小數以縮小檔案。"""
    pos = {str(t): k for k, t in enumerate(labels)}
    cols = {"med": "中位數", "mean": "平均數", "p25": "P25", "p75": "P75"}
    out = {}
    for keys, g in stats.groupby(["縣市", "行政區"] + item_cols):
        g = g.sort_values(time_col)
        s = {"i": [pos[str(t)] for t in g[time_col]]}
        for f in fields:
            s[f] = g[cols[f]].round(1).tolist()
        s["n"] = g["筆數"].astype(int).tolist()
        out["|".join(map(str, keys))] = s
    return out


def _recent(stats, item_cols):
    return {"|".join(map(str, [r["縣市"], r["行政區"]] + [r[c] for c in item_cols])): {"med": round(r["中位數"], 1), "n": int(r["筆數"])}
            for _, r in stats.iterrows()}


def _split(d):
    """依 key 末段（細分）拆成「全部」與其他細分。"""
    tail = "|" + DEFAULT_SUB
    return ({k: v for k, v in d.items() if k.endswith(tail)}, {k: v for k, v in d.items() if not k.endswith(tail)})


def build_dashboard(segs, parking, house_stats, park_stats, last_season, out_path: Path, plotly_tag):
    rh, label = recent_stats(segs, HOUSE_KEYS, "單價萬坪")
    rp, _ = recent_stats(parking, "車位類別", "車位價萬")
    present = set(house_stats["縣市"] + "|" + house_stats["行政區"])
    hy, py = aggregate_yearly(segs, parking)
    quarters = sorted(house_stats["季"].unique())
    years = list(range(int(quarters[0][:4]), int(quarters[-1][:4]) + 1))
    # 第一個完整年度：最早有第一季資料的年份（2012 只有 8～12 月）。
    # 最後完整年度：最新一季永遠還在補登，所以最新一季所在年度不算完整。
    first_full = min(int(q[:4]) for q in quarters if q.endswith("Q1"))
    last_full = int(quarters[-1][:4]) - 1
    house, house_x = _split(_series(house_stats, HOUSE_KEYS, quarters))
    house_y, house_y_x = _split(_series(hy, HOUSE_KEYS, years, "年", fields=("med",)))
    recent_h, recent_h_x = _split(_recent(rh, HOUSE_KEYS))
    data = {
        "quarters": quarters,
        "years": years,
        "firstFullYear": first_full,
        "lastFullYear": last_full,
        "houseYear": house_y,
        "parkYear": _series(py[py["車位類別"] == "全部"], ["車位類別"], years, "年", fields=("med",)),
        "completeQuarter": quarters[-2],
        "districts": {c: [d for d in ds if f"{c}|{d}" in present] for c, ds in DISTRICTS.items()},
        "house": house,
        "park": _series(park_stats, ["車位類別"], quarters),
        "recent": {"house": recent_h, "park": _recent(rp, ["車位類別"])},
        "recentLabel": label,
        "lastSeason": last_season,
        "presaleBreak": PRESALE_BREAK,
        "notes": NOTES,
    }
    extra = {"house": house_x, "houseYear": house_y_x, "recent": recent_h_x}
    extra_js = json.dumps(extra, ensure_ascii=False, separators=(",", ":"))
    data["segVersion"] = hashlib.sha1(extra_js.encode()).hexdigest()[:10]  # 內容變了才換網址，避免瀏覽器快取舊檔
    tpl = (Path(__file__).parent / "dashboard_template.html").read_text(encoding="utf-8")
    html = tpl.replace("__PLOTLY__", plotly_tag).replace(
        "__DATA__", json.dumps(data, ensure_ascii=False, separators=(",", ":")))
    out_path.write_text(html, encoding="utf-8")
    # 其他屋況／細分的序列較大，另存成 script，網頁選到時才載入（file:// 開啟也可用）
    (out_path.parent / "segments.js").write_text(
        "window.HP_SEGMENTS=" + extra_js + ";\n", encoding="utf-8")


def build_excel(segs, parking, house_stats, park_stats, funnel, out_path: Path):
    base = segs[segs["細分"] == DEFAULT_SUB]
    hw, label = recent_wide(base, "型態", "單價萬坪")
    pw, _ = recent_wide(parking, "車位類別", "車位價萬")
    order = {f"{c}|{d}": i for i, (c, d) in enumerate(
        [(c, "全市") for c in DISTRICTS] + [(c, d) for c, ds in DISTRICTS.items() for d in ds])}

    def sort(df):
        return df.assign(_o=(df["縣市"] + "|" + df["行政區"]).map(order)).sort_values("_o", kind="stable").drop(columns="_o")

    types = [t for t in ["大樓", "華廈", "公寓", "新成屋", "預售屋"] if t in hw]
    hw = sort(hw)[["縣市", "行政區"] + types + [t + "_筆數" for t in types]]
    pcols = [c for c in ["全部", "坡道平面", "坡道機械", "升降平面", "升降機械", "塔式車位", "一樓平面", "其他"] if c in pw]
    pw = sort(pw)[["縣市", "行政區"] + pcols + [c + "_筆數" for c in pcols]]
    # 房屋季中位數寬表：每列一季，每欄一個 行政區-型態，方便直接畫圖
    quarterly = house_stats[house_stats["細分"] == "全部"].drop(columns="細分")
    piv = quarterly.pivot_table(
        index="季", columns=["縣市", "行政區", "型態"], values="中位數").round(1)
    piv.columns = ["-".join(c) for c in piv.columns]

    with pd.ExcelWriter(out_path, engine="xlsxwriter") as xw:
        sheets = [
            (f"房屋_近4季({label})", hw),
            ("車位_近4季", pw),
            ("房屋_季統計", sort(quarterly).round(2)),
            ("細分_年統計", sort(aggregate_yearly(segs, parking)[0]).round(2)),
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
