
Highcharts.chart('pie', {
    chart: {
        type: 'pie',
        options3d: {
            enabled: true,
            alpha: 45,
            beta: 0
        }
    },
    title: {
        text: 'Tweets Share Breakdown',
        style: {
                              fontWeight: 'bold',
                              font: 'serif'
                             }
    },
    subtitle: {
        text: 'Number of Tweets Breakdown for every county'
    },
    tooltip: {
        pointFormat: '{series.name}: <b>{point.percentage:.1f}%</b>'
    },
    plotOptions: {
        pie: {
            allowPointSelect: true,
            cursor: 'pointer',
            depth: 35,
            dataLabels: {
                enabled: true,
                format: '{point.name}'
            }
        }
    },
    series: [{
        type: 'pie',
        name: 'Tweets share',
        data: [
  [
    "Carlow",
    0.495049505
  ],
  [
    "Clare",
    1.485148515
  ],
  [
   "Cork",
    5.445544554
  ],
  [
    "Donegal",
    0.495049505
  ],
  {
    name: "Dublin",
    y: 34.15841584,
	sliced: true,
	selected: true
  },
  [
    "Fingal",
    0.495049505
  ],
  [
    "Galway",
    1.98019802
  ],
  [
    "Kerry",
    1.98019802
  ],
  [
    "Limerick",
    2.97029703
  ],
  [
    "Others",
    50
  ],
  [
    "Sligo",
    0.495049505
  ]
]
    }]
});