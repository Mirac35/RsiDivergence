//@version=5
indicator(title="RSI with Divergence and Hidden Divergence + Smoothed MA", format=format.price, timeframe="", timeframe_gaps=true) 

// --- RSI Inputs ---
len = input.int(title="RSI Period", minval=1, defval=14)
src = input.source(title="RSI Source", defval=close)
lbR = input.int(title="Pivot Lookback Right", defval=5, display=display.data_window)
lbL = input.int(title="Pivot Lookback Left", defval=5, display=display.data_window)
rangeUpper = input.int(title="Max of Lookback Range", defval=60, display=display.data_window)
rangeLower = input.int(title="Min of Lookback Range", defval=5, display=display.data_window)
plotBull = input.bool(title="Plot Bullish", defval=true, display=display.data_window)
plotHiddenBull = input.bool(title="Plot Hidden Bullish", defval=false, display=display.data_window)
plotBear = input.bool(title="Plot Bearish", defval=true, display=display.data_window)
plotHiddenBear = input.bool(title="Plot Hidden Bearish", defval=false, display=display.data_window)

// --- Smoothed MA Setitngs ---
smoothingType = input.string(title="Smoothing Type", defval="SMA", options=["SMA", "EMA", "WMA"])
smoothingLen = input.int(title="Smoothing Length", defval=14, minval=1)

// --- Colors ---
bearColor = color.red
bullColor = color.green
hiddenBullColor = color.new(color.green, 80)
hiddenBearColor = color.new(color.red, 80)
textColor = color.white
noneColor = color.new(color.white, 100)

// --- Calculate Formula ---
osc = ta.rsi(src, len)

// --- RSI Smooth Function ---
smoothedRSI = switch smoothingType
    "SMA" => ta.sma(osc, smoothingLen)
    "EMA" => ta.ema(osc, smoothingLen)
    "WMA" => ta.wma(osc, smoothingLen)
    => osc  // default fallback

// --- RSI ve Smoothed RSI Plot ---
plot(osc, title="RSI", linewidth=2, color=#2962FF)
plot(smoothedRSI, title="Smoothed RSI", linewidth=2, color=color.new(#2962FF, 0), style=plot.style_line)


// --- Hline ve Fill ---
hline(50, title="Middle Line", color=#787B86, linestyle=hline.style_dotted)
obLevel = hline(70, title="Overbought", color=#787B86, linestyle=hline.style_dotted)
osLevel = hline(30, title="Oversold", color=#787B86, linestyle=hline.style_dotted)
fill(obLevel, osLevel, title="Background", color=color.rgb(33, 150, 243, 90))

// --- Pivot Detector ---
plFound = na(ta.pivotlow(osc, lbL, lbR)) ? false : true
phFound = na(ta.pivothigh(osc, lbL, lbR)) ? false : true

_inRange(cond) =>
    bars = ta.barssince(cond == true)
    rangeLower <= bars and bars <= rangeUpper

//------------------------------------------------------------------------------
// Regular Bullish
// Osc: Higher Low
oscHL = osc[lbR] > ta.valuewhen(plFound, osc[lbR], 1) and _inRange(plFound[1])
// Price: Lower Low
priceLL = low[lbR] < ta.valuewhen(plFound, low[lbR], 1)
bullCondAlert = priceLL and oscHL and plFound
bullCond = plotBull and bullCondAlert

plot(plFound ? osc[lbR] : na, offset=-lbR, title="Regular Bullish", linewidth=2, color=(bullCond ? bullColor : noneColor), display=display.pane)
plotshape(bullCond ? osc[lbR] : na, offset=-lbR, title="Regular Bullish Label", text=" Boğa ", style=shape.labelup, location=location.absolute, color=bullColor, textcolor=textColor)

//------------------------------------------------------------------------------
// Hidden Bullish
// Osc: Lower Low
oscLL = osc[lbR] < ta.valuewhen(plFound, osc[lbR], 1) and _inRange(plFound[1])
// Price: Higher Low
priceHL = low[lbR] > ta.valuewhen(plFound, low[lbR], 1)
hiddenBullCondAlert = priceHL and oscLL and plFound
hiddenBullCond = plotHiddenBull and hiddenBullCondAlert

plot(plFound ? osc[lbR] : na, offset=-lbR, title="Hidden Bullish", linewidth=2, color=(hiddenBullCond ? hiddenBullColor : noneColor), display=display.pane)
plotshape(hiddenBullCond ? osc[lbR] : na, offset=-lbR, title="Hidden Bullish Label", text=" G. Boğa ", style=shape.labelup, location=location.absolute, color=bullColor, textcolor=textColor)

//------------------------------------------------------------------------------
// Regular Bearish
// Osc: Lower High
oscLH = osc[lbR] < ta.valuewhen(phFound, osc[lbR], 1) and _inRange(phFound[1])
// Price: Higher High
priceHH = high[lbR] > ta.valuewhen(phFound, high[lbR], 1)
bearCondAlert = priceHH and oscLH and phFound
bearCond = plotBear and bearCondAlert

plot(phFound ? osc[lbR] : na, offset=-lbR, title="Regular Bearish", linewidth=2, color=(bearCond ? bearColor : noneColor), display=display.pane)
plotshape(bearCond ? osc[lbR] : na, offset=-lbR, title="Regular Bearish Label", text=" Ayı ", style=shape.labeldown, location=location.absolute, color=bearColor, textcolor=textColor)

//------------------------------------------------------------------------------
// Hidden Bearish
// Osc: Higher High
oscHH = osc[lbR] > ta.valuewhen(phFound, osc[lbR], 1) and _inRange(phFound[1])
// Price: Lower High
priceLH = high[lbR] < ta.valuewhen(phFound, high[lbR], 1)
hiddenBearCondAlert = priceLH and oscHH and phFound
hiddenBearCond = plotHiddenBear and hiddenBearCondAlert

plot(phFound ? osc[lbR] : na, offset=-lbR, title="Hidden Bearish", linewidth=2, color=(hiddenBearCond ? hiddenBearColor : noneColor), display=display.pane)
plotshape(hiddenBearCond ? osc[lbR] : na, offset=-lbR, title="Hidden Bearish Label", text=" G. Ayı ", style=shape.labeldown, location=location.absolute, color=bearColor, textcolor=textColor)

// --- Alertler ---
alertcondition(bullCondAlert, title='Regular Bullish Divergence', message="Found a new Regular Bullish Divergence, `Pivot Lookback Right` number of bars to the left of the current bar")
alertcondition(hiddenBullCondAlert, title='Hidden Bullish Divergence', message='Found a new Hidden Bullish Divergence, `Pivot Lookback Right` number of bars to the left of the current bar')
alertcondition(bearCondAlert, title='Regular Bearish Divergence', message='Found a new Regular Bearish Divergence, `Pivot Lookback Right` number of bars to the left of the current bar')
alertcondition(hiddenBearCondAlert, title='Hidden Bearish Divergence', message='Found a new Hidden Bearish Divergence, `Pivot Lookback Right` number of bars to the left of the current bar')
