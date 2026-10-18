"""下載內政部實價登錄季檔，只保留雙北成屋買賣與車位明細。"""
import io
import time
import zipfile
from pathlib import Path

import requests

URL = "https://plvr.land.moi.gov.tw/DownloadSeason?season={season}&type=zip&fileName=lvr_landcsv.zip"
KEEP = ["a_lvr_land_a.csv", "a_lvr_land_a_park.csv", "f_lvr_land_a.csv", "f_lvr_land_a_park.csv"]
FIRST_YEAR = 101


def season_list(last_year):
    return [f"{y}S{q}" for y in range(FIRST_YEAR, last_year + 1) for q in range(1, 5)]


def fetch(season, retries=3):
    for i in range(retries):
        try:
            r = requests.get(URL.format(season=season), timeout=120)
            r.raise_for_status()
            return r.content
        except requests.RequestException as e:
            print(f"  {season} 第 {i + 1} 次失敗：{e}")
            time.sleep(5 * (i + 1))
    raise RuntimeError(f"{season} 下載失敗")


def download_all(raw_dir: Path, last_year=None):
    raw_dir.mkdir(parents=True, exist_ok=True)
    last_year = last_year or time.localtime().tm_year - 1911
    got = []
    for season in season_list(last_year):
        out = raw_dir / season
        if (out / ".done").exists():
            got.append(season)
            continue
        content = fetch(season)
        try:
            z = zipfile.ZipFile(io.BytesIO(content))
        except zipfile.BadZipFile:
            print(f"  {season} 尚未公告，停止")
            break
        names = {n.lower(): n for n in z.namelist()}
        # 101S1~101S3 是有效 zip 但幾乎沒有雙北資料，照常標記完成；之後的季別若沒有雙北檔案，
        # 視為尚未公告、不標記完成（否則真正公告時會因已有 .done 而被跳過）
        found = [k for k in KEEP if k in names]
        if not found and season not in ("101S1", "101S2", "101S3"):
            print(f"  {season} 無雙北資料，視為尚未公告，停止")
            break
        out.mkdir(exist_ok=True)
        for k in found:
            (out / k).write_bytes(z.read(names[k]))
        (out / ".done").write_text("ok")
        got.append(season)
        print(f"  {season} 完成（{len(content) / 1e6:.1f} MB，{len(found)} 檔）")
        time.sleep(1.5)
    return got


if __name__ == "__main__":
    download_all(Path(__file__).resolve().parent.parent / "data" / "raw")
