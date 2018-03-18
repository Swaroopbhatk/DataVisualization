Highcharts.chart('spider', {

    chart: {
        polar: true,
        type: 'line'
    },

    title: {
        text: 'County vs Tweets Count',
        x: 82,
         style: {
                              fontWeight: 'bold',
                              font: 'serif'
                             }
    },
    subtitle: {
        text: 'Number of Tweets Breakdown for every county',
        x: 82,
         style: {
                              fontWeight: 'bold',
                              font: 'serif'
                             }
        },

    pane: {
        size: '80%'
    },

    xAxis: {
        categories: ['<b>Carlow</b>', '<b>Clare</b>', '<b>Cork</b>', '<b>Donegal</b>', '<b>Fingal</b>',
                '<b>Galway</b>', '<b>Kerry</b>', '<b>Limerick</b>'],
        tickmarkPlacement: 'on',
        lineWidth: 0
    },

    yAxis: {
        gridLineInterpolation: 'polygon',
        lineWidth: 0,
        min: 0
    },

    tooltip: {
        shared: true,
        pointFormat: '<span style="color:{series.color}">{series.name}: <b>{point.y:,.0f}</b><br/>'
    },

    legend: {
    title: {
            text: 'Date: <br/><span style="font-size: 9px; color: #666; font-weight: normal"></span>',
            style: {
                fontStyle: 'italic'
            }
            },
        align: 'left',
        verticalAlign: 'top',
        y: 70,
        layout: 'vertical'
    },


    series: [
  {
    name: "07-03-2018",
    data: [5, 36, 5, 0, 0, 0, 0, 0],
    pointPlacement: 'on'
  },
  {
     name: "08-03-2018",
     data: [0, 0, 11, 0, 0, 36, 0, 83],
     pointPlacement: 'on'
  },
  {
     name: "09-03-2018",
     data: [0, 97, 108, 0, 0, 0, 0, 56],
	 pointPlacement: 'on'
  },
  {
     name: "10-03-2018",
     data: [0, 0, 177, 0, 0, 180, 73, 0],
	 pointPlacement: 'on'
  },
  {
     name: "11-03-2018",
     data: [0, 0, 87, 0, 0, 0, 226, 0],
	 pointPlacement: 'on'
  },
  {
     name: "12-03-2018",
     data: [0, 38, 0, 134, 0, 0, 0, 39],
	 pointPlacement: 'on'
  },
  {
     name: "13-03-2018",
     data: [0, 0, 2, 0, 0, 5, 93, 339],
	 pointPlacement: 'on'
  }
]
});
