import os
import sys
import json
import datetime
import shutil

CURRENT_DIR = os.path.dirname(os.path.abspath(__file__))
REPORT_SOURCE_DIR = r"L:\hcLee\02_Trading\00_每日晨報"

def sync_latest_reports():
    """
    Syncs HTML reports from 00_每日晨報 to AIFinLab/morning/ and rebuilds reports.json
    """
    files = [f for f in os.listdir(REPORT_SOURCE_DIR) if f.endswith('.html')]
    reports = []
    
    for f in sorted(files, reverse=True):
        date_part = f.replace('.html', '')
        src = os.path.join(REPORT_SOURCE_DIR, f)
        dst = os.path.join(CURRENT_DIR, f)
        shutil.copy2(src, dst)
        reports.append({
            "date": date_part,
            "title": f"全球市場觀盤與台股開盤焦點 ({date_part})",
            "filename": f,
            "summary": "每日美股四大指數、強弱勢類股、總經、VIX與台股連動焦點"
        })
    
    json_path = os.path.join(CURRENT_DIR, "reports.json")
    with open(json_path, 'w', encoding='utf-8') as out:
        json.dump(reports, out, ensure_ascii=False, indent=2)
    print(f"[OK] Synced {len(reports)} reports to AIFinLab/morning/")

if __name__ == "__main__":
    sync_latest_reports()
