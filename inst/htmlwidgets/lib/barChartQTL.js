function barChartQTL(
  el,
  results,
  config,
  thresholds,
  intervals,
  groupMetadata
) {

    // Build the chart as usual - a time Series wrapper
  const chart = gsmViz.default.barChart(
    el, results, config, thresholds, intervals, groupMetadata
  );

  return chart;
}

main_default.barChartQTL = barChartQTL;
