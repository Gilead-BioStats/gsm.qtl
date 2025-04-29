function timeSeriesQTL(
  _element_,
  _results_,
  _config_,
  _thresholds_,
  _intervals_,
  _groupMetadata_
) {
  // 1) Build chart exactly as usual
  const chart = gsmViz.default.timeSeries(
    _element_,
    _results_,
    _config_,
    _thresholds_,
    _intervals_,
    _groupMetadata_
  );

  // 2) Helper to strip out distribution (boxplot/violin) datasets
  function stripDistributions() {
    chart.data.datasets = chart.data.datasets.filter(
      ds => ds.purpose !== "distribution"
    );
  }

  // 3) Strip them out immediately
  stripDistributions();
  chart.update();

  // 4) Monkey‐patch the two helpers that re-run the pipeline
  if (chart.helpers.updateSelectedGroupIDs) {
    const orig = chart.helpers.updateSelectedGroupIDs.bind(chart);
    chart.helpers.updateSelectedGroupIDs = (ids) => {
      orig(ids);
      stripDistributions();
      chart.update();
    };
  }
  if (chart.helpers.updateData) {
    const orig2 = chart.helpers.updateData.bind(chart);
    chart.helpers.updateData = (...args) => {
      orig2(...args);
      stripDistributions();
      chart.update();
    };
  }

  return chart;
}

// expose it
main_default.timeSeriesQTL = timeSeriesQTL;
