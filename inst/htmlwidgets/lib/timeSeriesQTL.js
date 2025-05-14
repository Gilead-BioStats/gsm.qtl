function timeSeriesQTL(
  el,
  results,
  config,
  thresholds,
  intervals,
  groupMetadata
) {
  // 1) Now ALWAYS is an array!
  const ALWAYS = ["Upper_funnel", "Flatline"];

  // 2) Build the chart as usual
  const chart = gsmViz.default.timeSeries(
    el, results, config, thresholds, intervals, groupMetadata
  );

  // 3) Helper to strip out distributions
  function stripDistributions() {
    chart.data.datasets = chart.data.datasets.filter(
      ds => ds.purpose !== "distribution"
    );
  }

  // 4) Style *all* ALWAYS lines
  function styleAlways() {
    chart.data.datasets.forEach(ds => {
      if (
        ds.type === "line" &&
        ds.purpose === "highlight" &&
        // check if this dataset’s GroupID is in our ALWAYS list
        ds.data[0] &&
        ALWAYS.includes(ds.data[0].GroupID)
      ) {
        ds.borderColor     = "red";
        ds.backgroundColor = "red";
        ds.borderDash      = [5,5];
        ds.pointStyle      = "circle";
        ds.pointRadius     = 4;
      }
    });
  }

  // 5) First pass: strip, style, redraw
  stripDistributions();
  styleAlways();
  chart.update();

  // 6) Wrap updateSelectedGroupIDs so we always union in ALL the ALWAYS IDs
  if (chart.helpers.updateSelectedGroupIDs) {
    const orig = chart.helpers.updateSelectedGroupIDs.bind(chart);
    chart.helpers.updateSelectedGroupIDs = ids => {
      const picked = Array.isArray(ids) ? ids : [ids];
      const forced = [...new Set([...ALWAYS, ...picked])];
      orig(forced);
      stripDistributions();
      styleAlways();
      chart.update();
    };
  }

  // 7) Wrap updateData similarly
  if (chart.helpers.updateData) {
    const orig2 = chart.helpers.updateData.bind(chart);
    chart.helpers.updateData = (...args) => {
      orig2(...args);
      stripDistributions();
      styleAlways();
      chart.update();
    };
  }

  // 8) Kick off the very first pass—this will now include *all* ALWAYS IDs
  const initial = Array.isArray(config.selectedGroupIDs)
    ? config.selectedGroupIDs
    : config.selectedGroupIDs ? [config.selectedGroupIDs] : [];
  chart.helpers.updateSelectedGroupIDs(initial);

  return chart;
}

main_default.timeSeriesQTL = timeSeriesQTL;
