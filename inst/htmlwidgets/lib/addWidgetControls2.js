/**
 * Adds a dropdown to highlight the selected group ID or set of group IDs in the chart.
 *
 * @param {Node} el - widget container, an element in the DOM
 * @param {Array} dfResults - analysis results for a single metric
 * @param {Object} lMetric - metric metadata
 * @param {Array} dfGroups - group metadata
 * @param {boolean} bAddGroupSelect - whether to add a group select dropdown
 *
 * @returns {Node} HTML select element
 */
const addWidgetControls2 = function(el, dfResults, lMetric, dfGroups, bAddGroupSelect) {
    if (!bAddGroupSelect)
        return {
            widgetControls: null,
            groupSelect: null
        };

    const instance = el.getElementsByTagName('canvas')[0].chart;

    // add container in which to place dropdown
    const widgetControls = document.createElement('div');
    widgetControls.classList.add('gsm-widget-controls');
    el.prepend(widgetControls);

    // add group select
    const groups = getGroups(dfResults);
    const groupSelect = addSelectControl2(
        widgetControls,
        `Highlighted ${lMetric?.GroupLevel || 'Group'}`,
        groups,
        false,
        groups[0]    // <-- pick the very first group as default
    );
    groupSelect.classList.add('gsm-widget-control--group');

    // add event listener to group select
    groupSelect.addEventListener('change', event => {
        instance.data.config.selectedGroupIDs = event.target.value;

        // scatterPlot and barChart
        if (Object.keys(instance.helpers).includes('updateConfig')) {
            instance.helpers.updateConfig(instance, instance.data.config, instance.data._thresholds_);
        }
        // timeSeries
        else if (Object.keys(instance.helpers).includes('updateSelectedGroupIDs')) {
            instance.helpers.updateSelectedGroupIDs(instance.data.config.selectedGroupIDs);
        }

        // Dispatch [ riskSignalSelected ] event.
        instance.canvas.dispatchEvent(instance.canvas.riskSignalSelected);
    });

    groupSelect.dispatchEvent(new Event('change'));

    return {
        widgetControls,
        groupSelect
    };
}
