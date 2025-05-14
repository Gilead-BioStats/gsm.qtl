function timeSeriesQTL(
  el,
  results,
  config,
  thresholds,
  intervals,
  groupMetadata
) {
  const ALWAYS = "Upper_funnel";

  // Build the chart as usual - a time Series wrapper
  const chart = gsmViz.default.timeSeries(
    el, results, config, thresholds, intervals, groupMetadata
  );

  // Strip out boxplots/violins
  function stripDistributions() {
    chart.data.datasets = chart.data.datasets.filter(
      ds => ds.purpose !== "distribution"
    );
  }

  // ALWAYS plot the funnel
  function styleAlways() {
    chart.data.datasets.forEach(ds => {
      // ds.type==="line" && ds.purpose==="highlight" is your selected-group line
      if (
        ds.type === "line" &&
        ds.purpose === "highlight" &&
        // either by label:
        ds.label === `Study ${ALWAYS}` ||
        // or by checking the raw data:
        (ds.data[0] && ds.data[0].GroupID === ALWAYS)
      ) {
        ds.borderColor   = "red";
        ds.backgroundColor = "red";
        ds.borderDash    = [5,5];
        ds.pointStyle    = "circle";
        ds.pointRadius   = 4;
      }
    });
  }

  // First pass: remove distributions, style ALWAYS, redraw
  stripDistributions();
  styleAlways();
  chart.update();

  // Remove additional selection boxes on the plot
  if (chart.helpers.updateSelectedGroupIDs) {
    const orig = chart.helpers.updateSelectedGroupIDs.bind(chart);
    chart.helpers.updateSelectedGroupIDs = ids => {
      const picked = Array.isArray(ids) ? ids : [ids];
      const union  = [...new Set([ALWAYS, ...picked])];
      orig(union);
      stripDistributions();
      styleAlways();
      chart.update();
    };
  }

  if (chart.helpers.updateData) {
    const orig2 = chart.helpers.updateData.bind(chart);
    chart.helpers.updateData = (...args) => {
      orig2(...args);
      stripDistributions();
      styleAlways();
      chart.update();
    };
  }

  // Ensure a first selection is made
  const initial = Array.isArray(config.selectedGroupIDs)
    ? config.selectedGroupIDs
    : config.selectedGroupIDs ? [config.selectedGroupIDs] : [];
  chart.helpers.updateSelectedGroupIDs(initial);
  return chart;
}

main_default.timeSeriesQTL = timeSeriesQTL;


