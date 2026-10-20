"""雙北實價登錄：下載 → 清洗 → 彙總 → Excel 與互動式儀表板。

用法（在 AIFinLab 根目錄）：
    python houseprice/pipeline/run_all.py            下載新一季；有新季別才重算
    python houseprice/pipeline/run_all.py --force    不論有無新季別都重算
    python houseprice/pipeline/run_all.py --no-download --force
產出：houseprice/index.html、houseprice/雙北房價統計.xlsx、houseprice/last_season.txt
原始資料存在 houseprice/data/（不進 git）。
"""
import argparse
import sys
from pathlib import Path

PIPE = Path(__file__).resolve().parent
SITE = PIPE.parent  # houseprice/
sys.path.insert(0, str(PIPE / "src"))

import pandas as pd  # noqa: E402
from aggregate import aggregate_houses, aggregate_parking, expand_segments  # noqa: E402
from clean import clean_houses, clean_parking, clean_presale, load_raw  # noqa: E402
from download import download_all, is_done  # noqa: E402
from report import build_dashboard, build_excel  # noqa: E402

PLOTLY_TAG = '<script src="https://cdn.jsdelivr.net/npm/plotly.js-dist-min@2.35.2/plotly.min.js"></script>'


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--no-download", action="store_true")
    ap.add_argument("--force", action="store_true", help="沒有新季別也重算")
    args = ap.parse_args()

    raw, proc = SITE / "data" / "raw", SITE / "data" / "processed"
    proc.mkdir(parents=True, exist_ok=True)
    marker = SITE / "last_season.txt"

    if not args.no_download:
        print("1/4 下載季檔")
        download_all(raw)
    seasons = sorted((p.name for p in raw.iterdir() if (p / ".done").exists()) if raw.exists() else [],
                     key=lambda s: (int(s[:-2]), int(s[-1])))
    if not seasons:
        sys.exit(f"錯誤：{raw} 沒有任何已完成下載的季檔，請先不加 --no-download 執行一次")
    last_season = seasons[-1]
    published = marker.read_text().strip() if marker.exists() else ""

    # 防呆：下載中途若遇到錯誤頁（維護、限流、封鎖）會被當成「尚未公告」而提早停止，
    # 此時不可用殘缺資料重算並覆蓋已發布的網站
    key = lambda s: (int(s[:-2]), int(s[-1]))  # noqa: E731
    expected = [f"{y}S{q}" for y in range(101, key(last_season)[0] + 1) for q in range(1, 5)]
    expected = expected[:expected.index(last_season) + 1]
    missing = sorted(set(expected) - set(seasons), key=key)
    if missing:
        sys.exit(f"錯誤：季檔不連續，缺少 {', '.join(missing)}，停止更新")
    stale = [s for s in seasons if not is_done(raw / s)]
    if stale:
        sys.exit(f"錯誤：{len(stale)} 季是舊版下載（缺預售屋檔），請不加 --no-download 執行以補抓")
    if published and key(last_season) < key(published):
        sys.exit(f"錯誤：最新季別 {last_season} 早於已發布的 {published}，下載可能中斷，停止更新")

    if last_season == published and not args.force:
        print(f"沒有新的季別（目前 {last_season}），不需更新")
        return

    print("2/4 清洗")
    main_df, park_df = load_raw(raw)
    parking = clean_parking(main_df, park_df)
    houses, funnel = clean_houses(main_df, park_df, parking)
    # 預售屋併入總價的車位，用預售屋自己的車位成交價估算（預售車位行情與成屋不同）
    main_b, park_b = load_raw(raw, "b")
    presale, funnel_b = clean_presale(main_b, park_b, clean_parking(main_b, park_b))
    funnel = pd.concat([funnel.assign(資料="成屋"), funnel_b.assign(資料="預售屋")], ignore_index=True)[["資料", "步驟", "筆數"]]
    print(funnel.to_string(index=False))
    print(f"   車位明細 {len(parking):,} 個")
    homes = pd.concat([houses, presale], ignore_index=True)
    print(homes.groupby(["屋況", "型態"]).size().to_string())
    homes.to_csv(proc / "houses.csv.gz", index=False, encoding="utf-8-sig")
    parking.to_csv(proc / "parking.csv.gz", index=False, encoding="utf-8-sig")

    print("3/4 彙總")
    segs = expand_segments(homes)
    hs, ps = aggregate_houses(segs), aggregate_parking(parking)

    print("4/4 產出")
    build_excel(segs, parking, hs, ps, funnel, SITE / "雙北房價統計.xlsx")
    build_dashboard(segs, parking, hs, ps, last_season, SITE / "index.html", PLOTLY_TAG)
    marker.write_text(last_season + "\n")
    print(f"完成：{SITE}（資料至 {last_season}）")


if __name__ == "__main__":
    main()
