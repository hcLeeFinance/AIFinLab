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
    Fetches latest price, change, pct, high, low, previous close from Yahoo Finance API.
    """
    try:
        url = f"https://query1.finance.yahoo.com/v8/finance/chart/{symbol}?interval=1d&range=2d"
        req = urllib.request.Request(url, headers={'User-Agent': 'Mozilla/5.0'})
        with urllib.request.urlopen(req, timeout=10) as resp:
            data = json.loads(resp.read().decode('utf-8'))
            result = data['chart']['result'][0]
            meta = result['meta']
            current_price = meta.get('regularMarketPrice')
            previous_close = meta.get('chartPreviousClose') or meta.get('previousClose')
            day_high = meta.get('regularMarketDayHigh')
            day_low = meta.get('regularMarketDayLow')
            
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

def build_cloud_morning_report():
    today_str = datetime.datetime.now().strftime("%Y-%m-%d")
    
    # 1. 美洲 (Americas)
    dji = fetch_ticker_data("^DJI")
    spx = fetch_ticker_data("^GSPC")
    ixic = fetch_ticker_data("^IXIC")
    sox = fetch_ticker_data("^SOX")
    tsm = fetch_ticker_data("TSM")
    vix = fetch_ticker_data("^VIX")

    # Fetch Intraday Charts for US 4 Major Indices
    prev_dji, quotes_dji = fetch_intraday_data("^DJI")
    prev_spx, quotes_spx = fetch_intraday_data("^GSPC")
    prev_ixic, quotes_ixic = fetch_intraday_data("^IXIC")
    prev_sox, quotes_sox = fetch_intraday_data("^SOX")

    svg_dji = generate_svg_chart("dji", prev_dji or dji['prev_close'], quotes_dji)
    svg_spx = generate_svg_chart("spx", prev_spx or spx['prev_close'], quotes_spx)
    svg_ixic = generate_svg_chart("ixic", prev_ixic or ixic['prev_close'], quotes_ixic)
    svg_sox = generate_svg_chart("sox", prev_sox or sox['prev_close'], quotes_sox)

    # 2. 歐洲 (Europe)
    ftse = fetch_ticker_data("^FTSE")
    dax = fetch_ticker_data("^GDAXI")
    cac = fetch_ticker_data("^FCHI")
    stoxx = fetch_ticker_data("^STOXX50E")

    # 3. 亞太與東南亞 (Asia-Pacific & SE Asia)
    n225 = fetch_ticker_data("^N225")
    kospi = fetch_ticker_data("^KS11")
    nifty = fetch_ticker_data("^NSEI")
    sti = fetch_ticker_data("^STI")
    set_idx = fetch_ticker_data("^SET.BK")

    # 4. 匯率、債市與黃金大宗商品 (Forex, Bonds & Commodities)
    dxy = fetch_ticker_data("DX-Y.NYB")
    usdtwd = fetch_ticker_data("USDTWD=X")
    usdjpy = fetch_ticker_data("USDJPY=X")
    tnx = fetch_ticker_data("^TNX")      # US 10Y Yield
    fvx = fetch_ticker_data("^FVX")      # US 5Y Yield
    gold = fetch_ticker_data("GC=F")
    oil = fetch_ticker_data("CL=F")

    def format_badge(data):
        raw = data['raw_pct']
        cls = 'badge-up' if raw >= 0 else 'badge-down'
        return f'<span class="badge-tag {cls}">{data["change"]} ({data["pct"]})</span>'

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
            padding: 18px 16px;
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
            padding: 4px 14px;
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
        
        /* Hero Highlights Box */
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

        /* Intraday Chart Grid */
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

        /* Section Cards */
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

        /* Grid for Asset Classes */
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

        /* Tables */
        .table-responsive {{ width: 100%; overflow-x: auto; margin-bottom: 16px; }}
        table {{ width: 100%; border-collapse: collapse; font-size: 0.9rem; }}
        th {{ background: rgba(15, 23, 42, 0.8); color: var(--text-muted); padding: 10px 12px; text-align: left; font-weight: 600; }}
        td {{ padding: 12px; border-bottom: 1px solid rgba(255,255,255,0.05); }}
        .badge-tag {{ display: inline-block; padding: 3px 8px; border-radius: 6px; font-size: 0.8rem; font-weight: 600; }}
        .badge-up {{ background: rgba(239, 68, 68, 0.15); color: var(--up-color); border: 1px solid rgba(239, 68, 68, 0.3); }}
        .badge-down {{ background: rgba(16, 185, 129, 0.15); color: var(--down-color); border: 1px solid rgba(16, 185, 129, 0.3); }}
        .up {{ color: var(--up-color); }}
        .down {{ color: var(--down-color); }}

        /* Analysis Box */
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
            <a href="../index.html" class="brand-title"><span>📈</span> FinLab 每日晨報</a>
            <div class="report-date-badge">{today_str}</div>
        </div>
    </header>
    <div class="container">
        <!-- Hero Section with Summary Highlights -->
        <div class="hero-banner">
            <h1 class="hero-title"><span>🎯</span> 全球市場觀盤與台股開盤焦點速覽</h1>
            <div class="hero-highlights">
                <div class="highlight-item">
                    <span class="highlight-tag tag-us">美股觀盤</span>
                    <div class="highlight-text">
                        科技股受 <strong>CapEx 資本支出疑慮與變現效率</strong> 影響出現評價修正，美股四大指數與費半高位震盪；<strong>VIX 恐慌指數上升至 {vix['price']} ({vix['pct']})</strong>，美債 10 年期殖利率落於 <strong>{tnx['price']}%</strong>。
                    </div>
                </div>
                <div class="highlight-item">
                    <span class="highlight-tag tag-global">全球資產</span>
                    <div class="highlight-text">
                        美元指數報 <strong>{dxy['price']}</strong>，紐約黃金 <strong>${gold['price']}</strong>，輕原油 <strong>${oil['price']}</strong>；歐亞股市隨半導體族群分化，市場避險資金逐漸向低估值、高股息與防禦型板塊轉移。
                    </div>
                </div>
                <div class="highlight-item">
                    <span class="highlight-tag tag-tw">台股開盤</span>
                    <div class="highlight-text">
                        台積電 ADR 報 <strong>{tsm['price']} ({tsm['pct']})</strong>，美元/新台幣 <strong>{usdtwd['price']}</strong>。開盤電子權值股面臨扣抵調整壓力，操盤準則建議<strong>「重質不重量、控制總持倉」</strong>、落實嚴格風控。
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
                    <div class="index-price {'up' if dji['raw_pct']>=0 else 'down'}">{dji['price']}</div>
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
                    <div class="index-price {'up' if spx['raw_pct']>=0 else 'down'}">{spx['price']}</div>
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
                    <div class="index-price {'up' if ixic['raw_pct']>=0 else 'down'}">{ixic['price']}</div>
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
                    <div class="index-price {'up' if sox['raw_pct']>=0 else 'down'}">{sox['price']}</div>
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
                <h4>1. 美股科技股與 CapEx 資本支出疑慮</h4>
                <p>近期美股科技巨頭（CSP 雲端大廠與半導體供應鏈）陸續公佈財報，市場焦點已從單純的「營收成長」轉向檢視「AI CapEx 資本支出之變現效率 (ROI)」。在市場高標準審視下，大廠高額 Capex 引起短期毛利率與自由現金流壓縮之疑慮，誘發美股科技股與費城半導體指數出現階段性獲利回吐與評價修正。</p>
            </div>

            <div class="analysis-box" style="border-left-color: var(--accent-purple);">
                <h4 style="color:var(--accent-purple);">2. VIX 恐慌指數與美債殖利率聯動</h4>
                <p>隨著 CBOE VIX 恐慌指數升至 {vix['price']} ({vix['pct']})，市場避險情緒顯著升溫。美國 10 年期國債殖利率維持在 {tnx['price']}% 震盪，美元指數落於 {dxy['price']}，顯示市場在評估聯轉會 Fed 降息預期與通膨數據彈升風險之間尋求平衡。債券殖利率的高位震盪亦對高本益比科技股形成估值天花板效應。</p>
            </div>

            <div class="analysis-box" style="border-left-color: var(--accent-green);">
                <h4 style="color:var(--accent-green);">3. 歐洲與亞太股市連動效應</h4>
                <p>歐亞股市受美股連帶引導呈現分化格局。日經 225 ({n225['price']}) 與韓股 KOSPI ({kospi['price']}) 受到半導體與出口權值股修正牽連；而印度 Nifty 50 ({nifty['price']}) 與新加坡 STI ({sti['price']}) 則展現內需與金融防禦性支撐力道，凸顯在全球市場波動劇烈下，資金逐漸向低估值、高股息與防禦型板塊轉移。</p>
            </div>
        </div>

        <!-- Section 5: Seasonality & Timeline Assessment -->
        <div class="section-card" style="border-left: 4px solid var(--accent-purple);">
            <div class="section-header" style="color: var(--accent-purple);">
                <span>📅</span> 歷史季節性與時間軸評估 (Seasonality & Timeline)
            </div>
            <div style="background: rgba(192, 132, 252, 0.1); border-left: 4px solid var(--accent-purple); padding: 12px 16px; border-radius: 0 8px 8px 0; margin-bottom: 14px; font-size:0.95rem; color:#e9d5ff;">
                <strong>🎯 時間軸關鍵轉折預估</strong>：預估本波科技股築底與關鍵買點區間落在 <strong>8 月底至 10 月初</strong>。
            </div>
            <div style="font-size: 0.95rem; line-height:1.7;">
                <p><strong>1. 短期（7 月底 ～ 8 月中旬）：財報利空測試與情緒消化期</strong><br>
                美股科技巨頭與半導體供應鏈密集出刊財報，市場對 Capex 與利潤率要求極度苛刻，指數多呈高波動震盪與二度打底走勢。</p>
                <br>
                <p><strong>2. 中期（8 月下旬 ～ 9 月）：美股歷史淡季與橫盤整理</strong><br>
                統計歷史數據，8 月與 9 月為美股與台股全年表現最疲弱且波動最大之季節。搭配 8 月底 Jackson Hole 央行年會與 9 月 Fed 降息決策，市場估值重估完成前多維持橫盤打底。</p>
                <br>
                <p><strong>3. 轉折/觸底時機（9 月底 ～ 10 月初）：迎接 Q4 旺季行情</strong><br>
                歷年半導體與科技股通常會在 9 月底至 10 月初順利築底完成，並迎來第四季 (Q4) 節慶消費旺季與年底封關行情 (Year-end Rally)。預計拉回均為中長期佈局優質績優股之良機。</p>
            </div>
        </div>

        <!-- Section 6: Taiwan Stock Opening Strategy -->
        <div class="taiwan-box">
            <div class="taiwan-header">
                <span>🇹🇼</span> 台灣看盤重點與開盤策略 (Taiwan Opening Strategy)
            </div>
            <div style="font-size: 0.95rem; line-height: 1.8;">
                <p>• <strong>台積電 ADR (TSM) 觀察</strong>：最新報價 {tsm['price']} ({tsm['pct']})。台積電 ADR 走勢為台股開盤指數波動之直接扣抵指標，若 ADR 承壓，大盤開盤指數將面臨點數調整壓力。</p>
                <p>• <strong>匯率動態 (USD/TWD)</strong>：美元兌新台幣報 {usdtwd['price']} ({usdtwd['pct']})，需密密切留意外資淨匯出與期貨空單避險減碼趨勢。</p>
                <p>• <strong>觀盤戰略（重質不重量）</strong>：美股科技股受 CapEx 疑慮修正，連帶壓制台股電子權值股走勢；然中長期 AI 基礎建設需求未變。現階段戰術應秉持<strong>「重質不重量、控制總持倉」</strong>原則，嚴格限制槓桿比率，避開高本益比純題材炒作股，伺機圍繞具備實質獲利保護、高股息防禦屬性與權值實質支撐之績優標的擇優佈局。」</p>
            </div>
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
        "summary": "包含觀盤速覽重點、美股四大指數與費半日內走勢圖、匯率/債市/黃金大宗商品與區域股市分析"
    })

    with open(json_path, 'w', encoding='utf-8') as f:
        json.dump(reports, f, ensure_ascii=False, indent=2)
    print(f"[OK] Updated {json_path}")

if __name__ == "__main__":
    build_cloud_morning_report()
