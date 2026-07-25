import os
import sys
import json
import datetime
import urllib.request
import concurrent.futures

CURRENT_DIR = os.path.dirname(os.path.abspath(__file__))
AIFINLAB_DIR = os.path.dirname(CURRENT_DIR)

# Symbols fetched for every report. Key -> Yahoo Finance ticker symbol.
TICKER_SYMBOLS = {
    "dji": "^DJI", "spx": "^GSPC", "ixic": "^IXIC", "sox": "^SOX", "tsm": "TSM", "vix": "^VIX",
    "ftse": "^FTSE", "dax": "^GDAXI", "cac": "^FCHI", "stoxx": "^STOXX50E",
    "n225": "^N225", "kospi": "^KS11", "nifty": "^NSEI", "sti": "^STI", "set_idx": "^SET.BK",
    "dxy": "DX-Y.NYB", "usdtwd": "USDTWD=X", "usdjpy": "USDJPY=X", "tnx": "^TNX",
    "gold": "GC=F", "oil": "CL=F",
}
# Subset that also gets an intraday sparkline chart.
INTRADAY_KEYS = ["dji", "spx", "ixic", "sox"]

# Below this success ratio, abort instead of publishing a mostly-empty report.
MIN_QUALITY_RATIO = 0.5


def fetch_ticker_data(symbol):
    """
    Fetches latest price, change, pct, high, low, previous close from Yahoo Finance API.
    Uses interval=5m&range=1d to ensure accurate previous day close comparison.
    """
    try:
        url = f"https://query1.finance.yahoo.com/v8/finance/chart/{symbol}?interval=5m&range=1d"
        req = urllib.request.Request(url, headers={'User-Agent': 'Mozilla/5.0'})
        with urllib.request.urlopen(req, timeout=10) as resp:
            data = json.loads(resp.read().decode('utf-8'))
            result = data['chart']['result'][0]
            meta = result['meta']
            current_price = meta.get('regularMarketPrice')
            previous_close = meta.get('chartPreviousClose') or meta.get('previousClose')
            day_high = meta.get('regularMarketDayHigh')
            day_low = meta.get('regularMarketDayLow')

            # Fallback if 5m range=1d does not return previous close
            if previous_close is None:
                url_fb = f"https://query1.finance.yahoo.com/v8/finance/chart/{symbol}?interval=1d&range=2d"
                req_fb = urllib.request.Request(url_fb, headers={'User-Agent': 'Mozilla/5.0'})
                with urllib.request.urlopen(req_fb, timeout=10) as resp_fb:
                    data_fb = json.loads(resp_fb.read().decode('utf-8'))
                    meta_fb = data_fb['chart']['result'][0]['meta']
                    current_price = current_price or meta_fb.get('regularMarketPrice')
                    previous_close = meta_fb.get('chartPreviousClose') or meta_fb.get('previousClose')
                    day_high = day_high or meta_fb.get('regularMarketDayHigh')
                    day_low = day_low or meta_fb.get('regularMarketDayLow')

            if current_price is not None and previous_close is not None:
                change = current_price - previous_close
                change_pct = (change / previous_close) * 100 if previous_close != 0 else 0
                return {
                    "symbol": symbol,
                    "price": f"{current_price:,.2f}",
                    "raw_price": current_price,
                    "change": f"{change:+.2f}",
                    "pct": f"{change_pct:+.2f}%",
                    "raw_pct": change_pct,
                    "prev_close": previous_close,
                    "high": f"{day_high:,.2f}" if day_high else "--",
                    "low": f"{day_low:,.2f}" if day_low else "--"
                }
    except Exception as e:
        print(f"[WARN] Failed to fetch {symbol}: {e}")
    return {
        "symbol": symbol,
        "price": "--",
        "raw_price": 0.0,
        "change": "--",
        "pct": "--",
        "raw_pct": 0.0,
        "prev_close": 0.0,
        "high": "--",
        "low": "--"
    }


def fetch_intraday_data(symbol):
    """
    Fetches intraday 5-minute price series and previous close for chart plotting.
    """
    try:
        url = f"https://query1.finance.yahoo.com/v8/finance/chart/{symbol}?interval=5m&range=1d"
        req = urllib.request.Request(url, headers={'User-Agent': 'Mozilla/5.0'})
        with urllib.request.urlopen(req, timeout=10) as resp:
            data = json.loads(resp.read().decode('utf-8'))
            result = data['chart']['result'][0]
            meta = result['meta']
            prev_close = meta.get('chartPreviousClose') or meta.get('previousClose')
            quotes = result['indicators']['quote'][0].get('close', [])
            valid_quotes = [q for q in quotes if q is not None]
            return prev_close, valid_quotes
    except Exception as e:
        print(f"[WARN] Failed to fetch intraday for {symbol}: {e}")
        return None, []


def fetch_all_market_data():
    """
    Fetches every ticker + intraday series in parallel (instead of ~25 sequential
    HTTP round-trips) and returns (ticker_data, intraday_data) keyed by TICKER_SYMBOLS keys.
    """
    with concurrent.futures.ThreadPoolExecutor(max_workers=12) as pool:
        ticker_futures = {key: pool.submit(fetch_ticker_data, sym) for key, sym in TICKER_SYMBOLS.items()}
        intraday_futures = {key: pool.submit(fetch_intraday_data, TICKER_SYMBOLS[key]) for key in INTRADAY_KEYS}
        ticker_data = {key: f.result() for key, f in ticker_futures.items()}
        intraday_data = {key: f.result() for key, f in intraday_futures.items()}
    return ticker_data, intraday_data


def assess_data_quality(ticker_data):
    """
    Returns (success_count, total_count, ratio, failed_symbols) so callers can decide
    whether to publish, warn, or abort instead of silently shipping an all-'--' report.
    """
    total = len(ticker_data)
    failed = [d['symbol'] for d in ticker_data.values() if d['price'] == '--']
    success = total - len(failed)
    ratio = success / total if total else 0
    return success, total, ratio, failed


def generate_svg_chart(symbol_id, prev_close, quotes):
    """
    Generates a responsive SVG sparkline chart for intraday trend visual.
    """
    w, h = 320, 90
    if not quotes or not prev_close or prev_close == 0:
        return f'''<svg viewBox="0 0 {w} {h}" width="100%" height="{h}">
            <rect width="{w}" height="{h}" fill="rgba(15,23,42,0.4)" rx="8"/>
            <text x="{w/2}" y="{h/2}" fill="#64748b" font-size="12" text-anchor="middle" dominant-baseline="middle">盤中走勢資料擷取中...</text>
        </svg>'''

    all_vals = quotes + [prev_close]
    min_v, max_v = min(all_vals), max(all_vals)
    span = max_v - min_v
    padding = span * 0.08 if span > 0 else 1.0
    min_v -= padding
    max_v += padding

    def get_y(val):
        return h - 12 - ((val - min_v) / (max_v - min_v)) * (h - 24)

    y_prev = get_y(prev_close)

    n = len(quotes)
    points = []
    for i, q in enumerate(quotes):
        x = 10 + (i / (n - 1 if n > 1 else 1)) * (w - 20)
        y = get_y(q)
        points.append((x, y))

    path_d = "M " + " L ".join([f"{x:.1f},{y:.1f}" for x, y in points])
    fill_d = path_d + f" L {w-10:.1f},{h-5} L 10,{h-5} Z"

    is_up = quotes[-1] >= prev_close
    stroke_color = "#ef4444" if is_up else "#10b981"
    grad_id = f"grad_{symbol_id}_{int(datetime.datetime.now().timestamp()) % 10000}"

    last_x, last_y = points[-1]

    svg = f'''<svg viewBox="0 0 {w} {h}" width="100%" height="{h}" style="overflow:visible; font-family:sans-serif;">
      <defs>
        <linearGradient id="{grad_id}" x1="0" y1="0" x2="0" y2="1">
          <stop offset="0%" stop-color="{stroke_color}" stop-opacity="0.35"/>
          <stop offset="100%" stop-color="{stroke_color}" stop-opacity="0.0"/>
        </linearGradient>
      </defs>
      <rect width="{w}" height="{h}" fill="rgba(15,23,42,0.5)" rx="8" stroke="rgba(255,255,255,0.05)"/>
      <line x1="10" y1="{y_prev:.1f}" x2="{w-10}" y2="{y_prev:.1f}" stroke="#64748b" stroke-dasharray="3,3" stroke-width="1"/>
      <path d="{fill_d}" fill="url(#{grad_id})"/>
      <path d="{path_d}" fill="none" stroke="{stroke_color}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
      <circle cx="{last_x:.1f}" cy="{last_y:.1f}" r="4" fill="{stroke_color}" stroke="#ffffff" stroke-width="1.5"/>
      <text x="{w-12}" y="{y_prev-4:.1f}" fill="#94a3b8" font-size="9" text-anchor="end">平盤 {prev_close:,.1f}</text>
    </svg>'''
    return svg


def format_badge(data):
    """Neutral badge (not red/green) when the fetch failed, so '--' never reads as 'up'."""
    if data['price'] == '--':
        return '<span class="badge-tag badge-na">資料擷取中</span>'
    cls = 'badge-up' if data['raw_pct'] >= 0 else 'badge-down'
    return f'<span class="badge-tag {cls}">{data["change"]} ({data["pct"]})</span>'


def dir_class(data):
    """CSS class for price coloring; empty (neutral) when the fetch failed."""
    if data['price'] == '--':
        return ''
    return 'up' if data['raw_pct'] >= 0 else 'down'


def update_index_embedded_reports(reports):
    """
    Keeps AIFinLab/morning/index.html's EMBEDDED_REPORTS fallback in sync with
    reports.json, so the hub page's date selector still works when opened
    directly (file://) instead of served over http(s).
    """
    index_path = os.path.join(CURRENT_DIR, "index.html")
    if not os.path.exists(index_path):
        print("[WARN] index.html not found; skipping embedded reports update.")
        return
    with open(index_path, 'r', encoding='utf-8') as f:
        content = f.read()
    start_marker = "/* EMBEDDED_REPORTS_START */"
    end_marker = "/* EMBEDDED_REPORTS_END */"
    start_idx = content.find(start_marker)
    end_idx = content.find(end_marker)
    if start_idx == -1 or end_idx == -1:
        print("[WARN] index.html missing EMBEDDED_REPORTS markers; skipping embedded reports update.")
        return
    start_idx += len(start_marker)
    new_block = f"\n        const EMBEDDED_REPORTS = {json.dumps(reports, ensure_ascii=False)};\n        "
    content = content[:start_idx] + new_block + content[end_idx:]
    with open(index_path, 'w', encoding='utf-8') as f:
        f.write(content)
    print(f"[OK] Updated embedded reports list in {index_path}")


def build_cloud_morning_report():
    today_str = datetime.datetime.now().strftime("%Y-%m-%d")

    ticker_data, intraday_data = fetch_all_market_data()

    dji, spx, ixic, sox, tsm, vix = (ticker_data[k] for k in ["dji", "spx", "ixic", "sox", "tsm", "vix"])
    ftse, dax, cac, stoxx = (ticker_data[k] for k in ["ftse", "dax", "cac", "stoxx"])
    n225, kospi, nifty, sti, set_idx = (ticker_data[k] for k in ["n225", "kospi", "nifty", "sti", "set_idx"])
    dxy, usdtwd, usdjpy, tnx, gold, oil = (ticker_data[k] for k in ["dxy", "usdtwd", "usdjpy", "tnx", "gold", "oil"])

    success, total, ratio, failed_symbols = assess_data_quality(ticker_data)
    print(f"[INFO] Data quality: {success}/{total} tickers fetched successfully.")
    if ratio < MIN_QUALITY_RATIO:
        print(f"[ERROR] Only {success}/{total} tickers succeeded ({ratio:.0%}); "
              f"failed: {', '.join(failed_symbols)}. Aborting instead of publishing an empty report.")
        sys.exit(1)
    quality_warning_html = ""
    if failed_symbols:
        quality_warning_html = f'''<div style="background: rgba(251, 191, 36, 0.15); border: 1px solid rgba(251, 191, 36, 0.4); border-radius: 8px; padding: 10px 14px; margin-bottom: 16px; font-size: 0.85rem; color: #fbbf24;">
                ⚠️ 資料擷取異常：{len(failed_symbols)}/{total} 檔行情資料抓取失敗（{', '.join(failed_symbols)}），相關欄位顯示為「資料擷取中」，請人工核對後再參考。
            </div>'''

    svg_dji = generate_svg_chart("dji", dji['prev_close'] or intraday_data['dji'][0], intraday_data['dji'][1])
    svg_spx = generate_svg_chart("spx", spx['prev_close'] or intraday_data['spx'][0], intraday_data['spx'][1])
    svg_ixic = generate_svg_chart("ixic", ixic['prev_close'] or intraday_data['ixic'][0], intraday_data['ixic'][1])
    svg_sox = generate_svg_chart("sox", sox['prev_close'] or intraday_data['sox'][0], intraday_data['sox'][1])

    # Build the updated reports index before the HTML so it can be embedded as a
    # fallback for the history dropdown (fetch('reports.json') is blocked when this
    # file is opened directly via file://).
    output_filename = f"{today_str}.html"
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
        "summary": "包含觀盤速覽重點、美股四大指數與費半日內走勢圖、匯率/債市/黃金大宗商品與區域股市分析"
    })

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
            --accent-green: #34d399;
            --accent-gold: #fbbf24;
            --up-color: #ef4444;
            --down-color: #10b981;
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
            padding: 14px 16px;
            position: sticky;
            top: 0;
            z-index: 100;
        }}
        .header-content {{
            max-width: 960px;
            margin: 0 auto;
            display: flex;
            justify-content: space-between;
            align-items: center;
        }}
        .brand-title {{
            font-size: 1.15rem;
            font-weight: 700;
            background: linear-gradient(to right, #38bdf8, #818cf8);
            -webkit-background-clip: text;
            -webkit-text-fill-color: transparent;
            text-decoration: none;
            display: flex;
            align-items: center;
            gap: 8px;
        }}
        .header-right {{
            display: flex;
            align-items: center;
            gap: 10px;
        }}
        .history-select {{
            background: rgba(15, 23, 42, 0.9);
            color: var(--accent-blue);
            border: 1px solid rgba(56, 189, 248, 0.4);
            padding: 4px 10px;
            border-radius: 8px;
            font-size: 0.85rem;
            font-weight: 600;
            outline: none;
            cursor: pointer;
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
            max-width: 960px;
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
        .hero-title {{ font-size: 1.45rem; font-weight: 800; margin-bottom: 16px; color: #ffffff; display: flex; align-items: center; gap: 8px; }}

        .hero-highlights {{
            display: flex;
            flex-direction: column;
            gap: 12px;
        }}
        .highlight-item {{
            background: rgba(15, 23, 42, 0.7);
            border: 1px solid rgba(255,255,255,0.08);
            border-radius: 12px;
            padding: 12px 14px;
            display: flex;
            align-items: flex-start;
            gap: 12px;
        }}
        @media (max-width: 640px) {{
            .highlight-item {{ flex-direction: column; gap: 6px; }}
        }}
        .highlight-tag {{
            padding: 4px 10px;
            border-radius: 6px;
            font-size: 0.8rem;
            font-weight: 700;
            white-space: nowrap;
            display: inline-block;
        }}
        .tag-us {{ background: rgba(56, 189, 248, 0.2); color: var(--accent-blue); border: 1px solid rgba(56, 189, 248, 0.4); }}
        .tag-global {{ background: rgba(192, 132, 252, 0.2); color: var(--accent-purple); border: 1px solid rgba(192, 132, 252, 0.4); }}
        .tag-tw {{ background: rgba(251, 191, 36, 0.2); color: var(--accent-gold); border: 1px solid rgba(251, 191, 36, 0.4); }}
        .highlight-text {{ font-size: 0.92rem; color: #e2e8f0; line-height: 1.6; }}

        .chart-grid {{
            display: grid;
            grid-template-columns: 1fr;
            gap: 16px;
            margin-bottom: 24px;
        }}
        @media (min-width: 640px) {{
            .chart-grid {{ grid-template-columns: repeat(2, 1fr); }}
        }}
        .index-card {{
            background: var(--card-bg);
            border: 1px solid var(--card-border);
            border-radius: 14px;
            padding: 16px;
            display: flex;
            flex-direction: column;
            justify-content: space-between;
        }}
        .index-header {{
            display: flex;
            justify-content: space-between;
            align-items: flex-start;
            margin-bottom: 8px;
        }}
        .index-name {{ font-size: 1rem; font-weight: 700; color: #ffffff; }}
        .index-subname {{ font-size: 0.75rem; color: var(--text-muted); }}
        .index-price {{ font-size: 1.25rem; font-weight: 800; margin-top: 2px; }}
        .chart-container {{ margin-top: 10px; width: 100%; }}

        .section-card {{
            background: var(--card-bg);
            border: 1px solid var(--card-border);
            border-radius: 16px;
            padding: 20px;
            margin-bottom: 24px;
        }}
        .section-header {{
            font-size: 1.15rem;
            font-weight: 700;
            color: #ffffff;
            margin-bottom: 16px;
            padding-bottom: 10px;
            border-bottom: 1px solid rgba(255,255,255,0.08);
            display: flex;
            align-items: center;
            gap: 8px;
        }}

        .asset-grid {{
            display: grid;
            grid-template-columns: repeat(2, 1fr);
            gap: 12px;
        }}
        @media (min-width: 640px) {{
            .asset-grid {{ grid-template-columns: repeat(3, 1fr); }}
        }}
        .asset-card {{
            background: rgba(15, 23, 42, 0.6);
            border: 1px solid var(--card-border);
            border-radius: 12px;
            padding: 14px;
        }}
        .asset-label {{ font-size: 0.8rem; color: var(--text-muted); margin-bottom: 4px; }}
        .asset-val {{ font-size: 1.1rem; font-weight: 700; color: #ffffff; }}

        .table-responsive {{ width: 100%; overflow-x: auto; margin-bottom: 16px; }}
        table {{ width: 100%; border-collapse: collapse; font-size: 0.9rem; }}
        th {{ background: rgba(15, 23, 42, 0.8); color: var(--text-muted); padding: 10px 12px; text-align: left; font-weight: 600; }}
        td {{ padding: 12px; border-bottom: 1px solid rgba(255,255,255,0.05); }}
        .badge-tag {{ display: inline-block; padding: 3px 8px; border-radius: 6px; font-size: 0.8rem; font-weight: 600; }}
        .badge-up {{ background: rgba(239, 68, 68, 0.15); color: var(--up-color); border: 1px solid rgba(239, 68, 68, 0.3); }}
        .badge-down {{ background: rgba(16, 185, 129, 0.15); color: var(--down-color); border: 1px solid rgba(16, 185, 129, 0.3); }}
        .badge-na {{ background: rgba(148, 163, 184, 0.12); color: var(--text-muted); border: 1px solid rgba(148, 163, 184, 0.3); }}
        .up {{ color: var(--up-color); }}
        .down {{ color: var(--down-color); }}

        .analysis-box {{
            background: linear-gradient(135deg, rgba(30,41,59,0.9) 0%, rgba(15,23,42,0.9) 100%);
            border-left: 4px solid var(--accent-blue);
            border-radius: 0 12px 12px 0;
            padding: 16px;
            margin-bottom: 16px;
            font-size: 0.95rem;
            color: #cbd5e1;
            line-height: 1.7;
        }}
        .analysis-box h4 {{ color: var(--accent-blue); font-size: 1.05rem; margin-bottom: 8px; font-weight: 700; }}

        .taiwan-box {{
            background: linear-gradient(135deg, rgba(30, 41, 59, 1) 0%, rgba(15, 23, 42, 1) 100%);
            border: 1px solid rgba(251, 191, 36, 0.4);
            border-radius: 16px;
            padding: 20px;
            margin-bottom: 24px;
        }}
        .taiwan-header {{ color: var(--accent-gold); font-weight: 700; font-size: 1.2rem; margin-bottom: 14px; display: flex; align-items: center; gap: 8px; }}

        footer {{ text-align: center; color: var(--text-muted); font-size: 0.8rem; margin-top: 40px; }}
    </style>
</head>
<body>
    <header class="top-header">
        <div class="header-content">
            <a href="index.html" class="brand-title"><span>📈</span> FinLab 每日晨報戰情室</a>
            <div class="header-right">
                <select id="quickHistorySelect" class="history-select" onchange="if(this.value) window.location.href=this.value;">
                    <option value="">📅 切換歷史日報...</option>
                </select>
                <div class="report-date-badge">{today_str}</div>
            </div>
        </div>
    </header>
    <div class="container">
        <div style="background: rgba(56, 189, 248, 0.1); border: 1px solid rgba(56, 189, 248, 0.3); border-radius: 10px; padding: 10px 16px; margin-bottom: 16px; font-size: 0.85rem; color: var(--accent-blue); text-align: center;">
            👨‍🏫 如需要深入分析，請洽銘傳大學李修全教授
        </div>

        <!-- Hero Section with Summary Highlights -->
        <div class="hero-banner">
            <h1 class="hero-title"><span>🎯</span> 全球市場觀盤與台股開盤焦點速覽</h1>
            {quality_warning_html}
            <div class="hero-highlights">
                <div class="highlight-item">
                    <span class="highlight-tag tag-us">美股觀盤</span>
                    <div class="highlight-text">
                        道瓊工業 {format_badge(dji)}　標普500 {format_badge(spx)}　那斯達克 {format_badge(ixic)}　費半 {format_badge(sox)}　VIX {vix['price']} ({vix['pct']})　美債10年 {tnx['price']}%
                    </div>
                </div>
                <div class="highlight-item">
                    <span class="highlight-tag tag-global">全球資產</span>
                    <div class="highlight-text">
                        美元指數 {format_badge(dxy)}　紐約黃金 ${gold['price']} {format_badge(gold)}　輕原油 ${oil['price']} {format_badge(oil)}
                    </div>
                </div>
                <div class="highlight-item">
                    <span class="highlight-tag tag-tw">台股開盤</span>
                    <div class="highlight-text">
                        台積電 ADR {format_badge(tsm)}　美元/新台幣 {format_badge(usdtwd)}
                    </div>
                </div>
            </div>
        </div>

        <!-- Section 1: US 4 Major Indices & Intraday Sparklines -->
        <div class="section-card">
            <div class="section-header">
                美股四大指數與費半表現 (含日內走勢)
            </div>
            <div class="chart-grid">
                <div class="index-card">
                    <div class="index-header">
                        <div>
                            <div class="index-name">道瓊工業指數</div>
                            <div class="index-subname">Dow Jones (DJI)</div>
                        </div>
                        {format_badge(dji)}
                    </div>
                    <div class="index-price {dir_class(dji)}">{dji['price']}</div>
                    <div class="chart-container">{svg_dji}</div>
                </div>

                <div class="index-card">
                    <div class="index-header">
                        <div>
                            <div class="index-name">標普 500 指數</div>
                            <div class="index-subname">S&P 500 (SPX)</div>
                        </div>
                        {format_badge(spx)}
                    </div>
                    <div class="index-price {dir_class(spx)}">{spx['price']}</div>
                    <div class="chart-container">{svg_spx}</div>
                </div>

                <div class="index-card">
                    <div class="index-header">
                        <div>
                            <div class="index-name">那斯達克指數</div>
                            <div class="index-subname">Nasdaq Composite (IXIC)</div>
                        </div>
                        {format_badge(ixic)}
                    </div>
                    <div class="index-price {dir_class(ixic)}">{ixic['price']}</div>
                    <div class="chart-container">{svg_ixic}</div>
                </div>

                <div class="index-card">
                    <div class="index-header">
                        <div>
                            <div class="index-name">費城半導體指數</div>
                            <div class="index-subname">PHLX Semiconductor (SOX)</div>
                        </div>
                        {format_badge(sox)}
                    </div>
                    <div class="index-price {dir_class(sox)}">{sox['price']}</div>
                    <div class="chart-container">{svg_sox}</div>
                </div>
            </div>
        </div>

        <!-- Section 2: Forex, Bonds & Gold / Commodities -->
        <div class="section-card">
            <div class="section-header">
                <span>💱</span> 外匯、債市與黃金/大宗商品 (Forex, Bonds & Commodities)
            </div>
            <div class="asset-grid">
                <div class="asset-card">
                    <div class="asset-label">💵 美元指數 (DXY)</div>
                    <div class="asset-val">{dxy['price']}</div>
                    <div style="font-size:0.8rem; margin-top:4px;">{format_badge(dxy)}</div>
                </div>
                <div class="asset-card">
                    <div class="asset-label">🇹🇼 美元 / 新台幣 (USD/TWD)</div>
                    <div class="asset-val">{usdtwd['price']}</div>
                    <div style="font-size:0.8rem; margin-top:4px;">{format_badge(usdtwd)}</div>
                </div>
                <div class="asset-card">
                    <div class="asset-label">🇯🇵 美元 / 日圓 (USD/JPY)</div>
                    <div class="asset-val">{usdjpy['price']}</div>
                    <div style="font-size:0.8rem; margin-top:4px;">{format_badge(usdjpy)}</div>
                </div>
                <div class="asset-card">
                    <div class="asset-label">📉 美國 10 年期國債殖利率</div>
                    <div class="asset-val">{tnx['price']}%</div>
                    <div style="font-size:0.8rem; margin-top:4px;">{format_badge(tnx)}</div>
                </div>
                <div class="asset-card">
                    <div class="asset-label">🪙 紐約黃金期貨 (Gold)</div>
                    <div class="asset-val">${gold['price']}</div>
                    <div style="font-size:0.8rem; margin-top:4px;">{format_badge(gold)}</div>
                </div>
                <div class="asset-card">
                    <div class="asset-label">🛢️ 紐約輕原油期貨 (WTI)</div>
                    <div class="asset-val">${oil['price']}</div>
                    <div style="font-size:0.8rem; margin-top:4px;">{format_badge(oil)}</div>
                </div>
            </div>
        </div>

        <!-- Section 3: Global Regional Stock Markets -->
        <div class="section-card">
            <div class="section-header">
                <span>🌐</span> 全球區域股市表現 (美洲 / 歐洲 / 亞太與東南亞)
            </div>

            <h4 style="color:var(--accent-blue); margin:12px 0 8px;">🇪🇺 歐洲主要指數</h4>
            <div class="table-responsive">
                <table>
                    <thead>
                        <tr><th>市場 / 指數名稱</th><th>最新收盤</th><th>當日漲跌</th></tr>
                    </thead>
                    <tbody>
                        <tr><td><strong>英國富時 100 (FTSE 100)</strong></td><td>{ftse['price']}</td><td>{format_badge(ftse)}</td></tr>
                        <tr><td><strong>德國 DAX 指數</strong></td><td>{dax['price']}</td><td>{format_badge(dax)}</td></tr>
                        <tr><td><strong>法國 CAC 40 指數</strong></td><td>{cac['price']}</td><td>{format_badge(cac)}</td></tr>
                        <tr><td><strong>歐洲斯托克 50 (Euro Stoxx 50)</strong></td><td>{stoxx['price']}</td><td>{format_badge(stoxx)}</td></tr>
                    </tbody>
                </table>
            </div>

            <h4 style="color:var(--accent-purple); margin:16px 0 8px;">🌏 亞太與東南亞主要指數</h4>
            <div class="table-responsive">
                <table>
                    <thead>
                        <tr><th>市場 / 指數名稱</th><th>最新收盤</th><th>當日漲跌</th></tr>
                    </thead>
                    <tbody>
                        <tr><td><strong>日本日經 225 (Nikkei 225)</strong></td><td>{n225['price']}</td><td>{format_badge(n225)}</td></tr>
                        <tr><td><strong>南韓 KOSPI 指數</strong></td><td>{kospi['price']}</td><td>{format_badge(kospi)}</td></tr>
                        <tr><td><strong>印度 Nifty 50 指數</strong></td><td>{nifty['price']}</td><td>{format_badge(nifty)}</td></tr>
                        <tr><td><strong>新加坡海峽時報指數 (STI)</strong></td><td>{sti['price']}</td><td>{format_badge(sti)}</td></tr>
                        <tr><td><strong>泰國 SET 指數</strong></td><td>{set_idx['price']}</td><td>{format_badge(set_idx)}</td></tr>
                    </tbody>
                </table>
            </div>
        </div>

        <!-- Section 4: Deep Analysis & Global Market Commentary -->
        <div class="section-card" style="border-left: 4px solid var(--accent-blue);">
            <div class="section-header" style="color:var(--accent-blue);">
                <span>📊</span> 全球市場觀盤深度解讀 (Detailed Global Analysis)
            </div>

            <div class="analysis-box">
                <p>👨‍🏫 如需深入分析，請洽銘傳大學李修全教授</p>
            </div>
        </div>

        <!-- Section 5: Seasonality & Timeline Assessment -->
        <div class="section-card" style="border-left: 4px solid var(--accent-purple);">
            <div class="section-header" style="color: var(--accent-purple);">
                <span>📅</span> 歷史季節性與時間軸評估 (Seasonality & Timeline)
            </div>
            <div style="background: rgba(192, 132, 252, 0.1); border-left: 4px solid var(--accent-purple); padding: 12px 16px; border-radius: 0 8px 8px 0; font-size:0.95rem; color:#e9d5ff;">
                <p>👨‍🏫 如需深入分析，請洽銘傳大學李修全教授</p>
            </div>
        </div>

        <!-- Section 6: Taiwan Stock Opening Strategy -->
        <div class="taiwan-box">
            <div class="taiwan-header">
                <span>🇹🇼</span> 台灣看盤重點與開盤策略 (Taiwan Opening Strategy)
            </div>
            <div style="font-size: 0.95rem; line-height: 1.8;">
                <p>• <strong>台積電 ADR (TSM)</strong>：{tsm['price']} {format_badge(tsm)}</p>
                <p>• <strong>美元/新台幣 (USD/TWD)</strong>：{usdtwd['price']} {format_badge(usdtwd)}</p>
                <p style="margin-top:10px; color:var(--accent-gold);">👨‍🏫 如需深入分析與操盤策略，請洽銘傳大學李修全教授</p>
            </div>
        </div>

        <footer>FinLab Daily Automated Morning Report System © 2026</footer>
    </div>

    <script>
        // Embedded at generation time so the history dropdown works even when this
        // file is opened directly (file://), where fetch() against reports.json
        // is blocked by the browser. If served over http(s), the fetch below still
        // runs first and wins with the freshest list.
        const EMBEDDED_REPORTS = {json.dumps(reports, ensure_ascii=False)};

        function populateHistorySelect(data) {{
            const selector = document.getElementById('quickHistorySelect');
            if (!selector) return;
            selector.innerHTML = '<option value="">📅 切換歷史日報...</option>';
            data.forEach(item => {{
                const opt = document.createElement('option');
                opt.value = item.filename;
                opt.textContent = `${{item.date}} ${{item.date === '{today_str}' ? '(本日)' : ''}}`;
                if (item.date === '{today_str}') opt.selected = true;
                selector.appendChild(opt);
            }});
        }}

        window.addEventListener('DOMContentLoaded', () => {{
            fetch('reports.json?t=' + new Date().getTime())
                .then(res => res.json())
                .then(data => populateHistorySelect(data))
                .catch(err => {{
                    console.log('reports.json fetch failed (expected when opening this file directly); using embedded list.', err);
                    populateHistorySelect(EMBEDDED_REPORTS);
                }});
        }});
    </script>
</body>
</html>"""

    output_path = os.path.join(CURRENT_DIR, output_filename)
    with open(output_path, 'w', encoding='utf-8') as f:
        f.write(html_content)
    print(f"[OK] Generated cloud HTML: {output_path}")

    with open(json_path, 'w', encoding='utf-8') as f:
        json.dump(reports, f, ensure_ascii=False, indent=2)
    print(f"[OK] Updated {json_path}")

    update_index_embedded_reports(reports)


if __name__ == "__main__":
    build_cloud_morning_report()
