const createPlotly = function(message) {
  // message is an array where first element is the data and second is the metadata
  let data = [{
    z: message[0],
    type: 'heatmap',
    colorscale: 'Viridis',
    showscale: true,
    colorbar: {
      title: 'Colorbar Title'
    },
    xgap: 1,
    ygap: 1,
    hovertemplate: 'Value: %{z}<br>Row: %{y}<br>Column: %{x}<extra></extra>'
  }];
  Plotly.newPlot('myDiv', data);
};

// custom message handler from shiny
Shiny.addCustomMessageHandler('first_correlation_matrix', function(message) {
  createPlotly(message);
});