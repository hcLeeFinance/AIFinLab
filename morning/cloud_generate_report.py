import os
import sys
import json
import datetime
import urllib.request
import re

CURRENT_DIR = os.path.dirname(os.path.abspath(__file__))
AIFINLAB_DIR = os.path.dirname(CURRENT_DIR)

def fetch_ticker_data(symbol):
    """
    Fetches latest price and change from Yahoo Finance chart API.
    """
    try:
        url = f"https://query1.finance.yahoo.com/v8/finance/chart/{symbol}?interval=1d&range=2d"
        req = urllib.request.Request(url, headers={'User-Agent': 'Mozilla/5.0'})
        with urllib.request.urlopen(req, timeout=10) as resp:
            data = json.loads(resp.read().decode('utf-8'))
            result = data['chart']['result'][0]
            meta = result['meta']
            current_price = meta.get('regularMarketPrice')
            previous_close = meta.get('chartPreviousClose')
            if current_price and previous_close:
                change = current_price - previous_close
                change_pct = (change / previous_close) * 100
                return {
                    "price": f"{current_price:,.2f}",
                    "change": f"{change:+.2f}",
                    "pct": f"{change_pct:+.2f}%",
                    "raw_pct": change_pct
                }
    except Exception as e:
        print(f"[WARN] Failed to fetch {symbol}: {e}")
    return {"price": "--", "change": "--", "pct": "--", "raw_pct": 0.0}

def build_cloud_morning_report():
    today_str = datetime.datetime.now().strftime("%Y-%m-%d")
    
    # Fetch Index Data
    dji = fetch_ticker_data("^DJI")
    spx = fetch_ticker_data("^GSPC")
    ixic = fetch_ticker_data("^IXIC")
    sox = fetch_ticker_data("^SOX")
    tsm = fetch_ticker_data("TSM")
    vix = fetch_ticker_data("^VIX")

    html_content = f"""<!DOCTYPE html>
<html lang="zh-TW">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no">
    <title>每日晨報 - {today_str} 全球市場觀盤與台股開盤焦點</title>
    <style>
        :root {{
            --bg-color: #0f172a;
            --card-bg: #1e293b;
            --card-border: #334155;
            --text-main: #f8fafc;
            --text-muted: #94a3b8;
            --accent-blue: #38bdf8;
            --accent-purple: #c084fc;
            --up-color: #ef4444;
            --down-color: #10b981;
            --warning-gold: #fbbf24;
        }}
        * {{ box-sizing: border-box; margin: 0; padding: 0; }}
        body {{
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Microsoft JhengHei", sans-serif;
            background-color: var(--bg-color);
            color: var(--text-main);
            line-height: 1.6;
            padding-bottom: 50px;
        }}
        .top-header {{
            background: linear-gradient(135deg, #0f172a 0%, #1e1b4b 100%);
            border-bottom: 1px solid var(--card-border);
            padding: 20px 16px;
            position: sticky;
            top: 0;
            z-index: 100;
        }}
        .header-content {{
            max-width: 900px;
            margin: 0 auto;
            display: flex;
            justify-content: space-between;
            align-items: center;
        }}
        .brand-title {{
            font-size: 1.25rem;
            font-weight: 700;
            background: linear-gradient(to right, #38bdf8, #818cf8);
            -webkit-background-clip: text;
            -webkit-text-fill-color: transparent;
            text-decoration: none;
            display: flex;
            align-items: center;
            gap: 8px;
        }}
        .report-date-badge {{
            background: rgba(56, 189, 248, 0.15);
            color: var(--accent-blue);
            padding: 4px 12px;
            border-radius: 20px;
            font-size: 0.85rem;
            font-weight: 600;
            border: 1px solid rgba(56, 189, 248, 0.3);
        }}
        .container {{
            max-width: 900px;
            margin: 20px auto 0;
            padding: 0 16px;
        }}
        .hero-banner {{
            background: linear-gradient(135deg, #1e293b 0%, #0f172a 100%);
            border: 1px solid var(--card-border);
            border-radius: 16px;
            padding: 24px 20px;
            margin-bottom: 24px;
        }}
        .hero-title {{ font-size: 1.4rem; font-weight: 800; margin-bottom: 10px; color: #ffffff; }}
        .hero-subtitle {{ font-size: 0.95rem; color: var(--text-muted); }}
        .quick-stats {{
            display: grid;
            grid-template-columns: repeat(2, 1fr);
            gap: 12px;
            margin-bottom: 24px;
        }}
        @media (min-width: 640px) {{
            .quick-stats {{ grid-template-columns: repeat(4, 1fr); }}
        }}
        .stat-card {{
            background: var(--card-bg);
            border: 1px solid var(--card-border);
            border-radius: 12px;
            padding: 14px;
            text-align: center;
        }}
        .stat-label {{ font-size: 0.8rem; color: var(--text-muted); margin-bottom: 4px; }}
        .stat-value {{ font-size: 1.1rem; font-weight: 700; }}
        .stat-change {{ font-size: 0.85rem; font-weight: 600; margin-top: 2px; }}
        .up {{ color: var(--up-color); }}
        .down {{ color: var(--down-color); }}
        .section-card {{
            background: var(--card-bg);
            border: 1px solid var(--card-border);
            border-radius: 16px;
            padding: 20px;
            margin-bottom: 20px;
        }}
        .section-header {{
            font-size: 1.15rem;
            font-weight: 700;
            color: #ffffff;
            margin-bottom: 16px;
            padding-bottom: 10px;
            border-bottom: 1px solid rgba(255,255,255,0.08);
        }}
        .table-responsive {{ width: 100%; overflow-x: auto; margin-bottom: 12px; }}
        table {{ width: 100%; border-collapse: collapse; font-size: 0.9rem; }}
        th {{ background: rgba(15, 23, 42, 0.6); color: var(--text-muted); padding: 10px 12px; text-align: left; }}
        td {{ padding: 12px; border-bottom: 1px solid rgba(255,255,255,0.05); }}
        .highlight-box {{
            background: rgba(30, 41, 59, 0.8);
            border-left: 4px solid var(--accent-blue);
            padding: 12px 16px;
            border-radius: 0 8px 8px 0;
            margin-top: 12px;
            font-size: 0.9rem;
            color: #cbd5e1;
        }}
        .badge-tag {{ display: inline-block; padding: 2px 8px; border-radius: 6px; font-size: 0.75rem; font-weight: 600; }}
        .badge-up {{ background: rgba(239, 68, 68, 0.15); color: var(--up-color); border: 1px solid rgba(239, 68, 68, 0.3); }}
        .badge-down {{ background: rgba(16, 185, 129, 0.15); color: var(--down-color); border: 1px solid rgba(16, 185, 129, 0.3); }}
        .taiwan-box {{
            background: linear-gradient(135deg, rgba(30, 41, 59, 1) 0%, rgba(15, 23, 42, 1) 100%);
            border: 1px solid rgba(251, 191, 36, 0.4);
            border-radius: 16px;
            padding: 20px;
            margin-bottom: 24px;
        }}
        .taiwan-header {{ color: var(--warning-gold); font-weight: 700; font-size: 1.2rem; margin-bottom: 16px; }}
        footer {{ text-align: center; color: var(--text-muted); font-size: 0.8rem; margin-top: 40px; }}
    </style>
</head>
<body>
    <header class="top-header">
        <div class="header-content">
            <a href="../index.html" class="brand-title"><span>📈</span> FinLab 每日晨報</a>
            <div class="report-date-badge">{today_str}</div>
        </div>
    </header>
    <div class="container">
        <div class="hero-banner">
            <h1 class="hero-title">全球市場觀盤與台股開盤焦點</h1>
            <p class="hero-subtitle">美股四大指數與費半動態摘要、產業強弱勢、歷史季節性時間軸評估與台股觀盤重點。</p>
        </div>

        <div class="quick-stats">
            <div class="stat-card">
                <div class="stat-label">道瓊工業 (DJI)</div>
                <div class="stat-value">{dji['price']}</div>
                <div class="stat-change {'up' if dji['raw_pct']>=0 else 'down'}">{dji['change']} ({dji['pct']})</div>
            </div>
            <div class="stat-card">
                <div class="stat-label">標普 500 (SPX)</div>
                <div class="stat-value">{spx['price']}</div>
                <div class="stat-change {'up' if spx['raw_pct']>=0 else 'down'}">{spx['change']} ({spx['pct']})</div>
            </div>
            <div class="stat-card">
                <div class="stat-label">那斯達克 (IXIC)</div>
                <div class="stat-value">{ixic['price']}</div>
                <div class="stat-change {'up' if ixic['raw_pct']>=0 else 'down'}">{ixic['change']} ({ixic['pct']})</div>
            </div>
            <div class="stat-card">
                <div class="stat-label">費城半導體 (SOX)</div>
                <div class="stat-value">{sox['price']}</div>
                <div class="stat-change {'up' if sox['raw_pct']>=0 else 'down'}">{sox['change']} ({sox['pct']})</div>
            </div>
        </div>

        <div class="section-card">
            <div class="section-header">🇺🇸 美股四大指數與費半表現</div>
            <div class="table-responsive">
                <table>
                    <thead>
                        <tr><th>指數名稱</th><th>收盤點數</th><th>漲跌變動</th></tr>
                    </thead>
                    <tbody>
                        <tr><td><strong>道瓊工業 (DJI)</strong></td><td>{dji['price']}</td><td><span class="badge-tag {'badge-up' if dji['raw_pct']>=0 else 'badge-down'}">{dji['change']} ({dji['pct']})</span></td></tr>
                        <tr><td><strong>標普 500 (SPX)</strong></td><td>{spx['price']}</td><td><span class="badge-tag {'badge-up' if spx['raw_pct']>=0 else 'badge-down'}">{spx['change']} ({spx['pct']})</span></td></tr>
                        <tr><td><strong>那斯達克 (IXIC)</strong></td><td>{ixic['price']}</td><td><span class="badge-tag {'badge-up' if ixic['raw_pct']>=0 else 'badge-down'}">{ixic['change']} ({ixic['pct']})</span></td></tr>
                        <tr><td><strong>費城半導體 (SOX)</strong></td><td>{sox['price']}</td><td><span class="badge-tag {'badge-up' if sox['raw_pct']>=0 else 'badge-down'}">{sox['change']} ({sox['pct']})</span></td></tr>
                    </tbody>
                </table>
            </div>
        </div>

        <!-- Section: Seasonality & Timeline Assessment -->
        <div class="section-card" style="border-left: 4px solid var(--accent-purple);">
            <div class="section-header" style="color: var(--accent-purple);">
                <span>📅</span> 美股與台股歷史季節性與時間軸評估 (Seasonality & Timeline)
            </div>
            <div class="highlight-box" style="margin-bottom: 14px; background: rgba(192, 132, 252, 0.1); border-left-color: var(--accent-purple);">
                <strong>🎯 時間軸評估預測</strong>：預計本波關鍵轉折與築底買點落在 <strong>8 月底至 10 月初</strong> 區間。
            </div>
            <div style="font-size: 0.95rem;">
                <p><strong>1. 短期（7 月底 ～ 8 月中旬）：情緒消化與財報驗證期</strong><br>
                • 關鍵驅動：美股科技巨頭（Microsoft, Google, Meta, Apple, Amazon, Nvidia）重磅財報與展望。<br>
                • 市場現象：8 月中前市場仍處於對 AI 變現能力與 Capex（資本支出）高度挑剔的消化期，震盪波動較大。</p>
                <br>
                <p><strong>2. 中期（8 月下旬 ～ 9 月）：美股歷史淡季與震盪打底期</strong><br>
                • 歷史季節性 (Seasonality)：統計歷史數據，8 月與 9 月通常是美股全年表現最為疲弱、波動最大的月份。<br>
                • 總經與政策面：8 月底 Jackson Hole 全球央行年會、9 月 Fed 利率決策會議，市場持續消化油價與降息/通膨預期，多呈二次打底或橫盤震盪。</p>
                <br>
                <p><strong>3. 轉折/觸底時機（9 月底 ～ 10 月初）：迎接 Q4 旺季行情</strong><br>
                • 旺季效應：歷年美股半導體與科技股通常會在 9 月底至 10 月初見底，並展開第四季 (Q4) 至次年第一季的年底封關與消費旺季行情 (Year-end Rally)。<br>
                • 結論預估：若無極端黑天鵝事件，本波估值修正最可能的觸底拉回時間點落在 8 月底至 9 月底之間，並於 10 月初重新回歸震盪上行軌道。</p>
            </div>
        </div>

        <div class="taiwan-box">
            <div class="taiwan-header">🇹🇼 台灣看盤重點與開盤策略</div>
            <p>• <strong>台積電 ADR (TSM)</strong>：{tsm['price']} ({tsm['pct']})</p>
            <p>• <strong>CBOE VIX 恐慌指數</strong>：{vix['price']} ({vix['pct']})</p>
            <p>• <strong>觀盤戰略</strong>：留意美股科技股財報反應與 AI 供應鏈支撐力道，控制持倉比重，逢拉回觀察支撐承接。」</p>
        </div>

        <footer>FinLab Daily Automated Morning Report System © 2026</footer>
    </div>
</body>
</html>"""

    output_filename = f"{today_str}.html"
    output_path = os.path.join(CURRENT_DIR, output_filename)
    with open(output_path, 'w', encoding='utf-8') as f:
        f.write(html_content)
    print(f"[OK] Generated cloud HTML: {output_path}")

    # Update reports.json
    json_path = os.path.join(CURRENT_DIR, "reports.json")
    reports = []
    if os.path.exists(json_path):
        try:
            with open(json_path, 'r', encoding='utf-8') as f:
                reports = json.load(f)
        except Exception:
            reports = []

    reports = [r for r in reports if r.get('date') != today_str]
    reports.insert(0, {
        "date": today_str,
        "title": "全球市場觀盤與台股開盤焦點",
        "filename": output_filename,
        "summary": "全自動美股四大指數、強弱勢類股、季節性時間軸評估與台股連動焦點"
    })

    with open(json_path, 'w', encoding='utf-8') as f:
        json.dump(reports, f, ensure_ascii=False, indent=2)
    print(f"[OK] Updated {json_path}")

if __name__ == "__main__":
    build_cloud_morning_report()
