$.getJSON('data/poptweets.json', function(data){

var processed_json = new Array();
$.map(data, function (obj, i) {
    processed_json.push({text: obj.text,y: obj.Popularity});
});

Highcharts.chart('bar-chart',
        {
            chart: {
                type: 'column',
                margin: 75,
                options3d: {
				enabled: true,
                alpha: 15,
                beta: 15,
                depth: 50
            }
            },
            title: {
                text: 'Top 20 Tweets',
                style: {
                              fontWeight: 'bold',
                              font: 'serif'
                             }
            },

            subtitle: {
                text: 'Top 20 Tweets for #IrishWater from 2018-03-07 to 2018-03-13'
            },

            yAxis: {
                title: {
                    text: 'No of Shares',
                    style: {
                              fontWeight: 'bold'
                             }
                }
            },

            xAxis: {
                title: {
                    text: 'Ranks',
                    style: {
                              fontWeight: 'bold'
                             }
                },
                type: "category",

            },
            legend: {
                // layout: 'vertical',
                // align: 'right',
                // verticalAlign: 'middle',
                enabled: false
            },
            tooltip:{
            formatter: function(){
            return 'Tweet: '+this.point.text;
            }
            },

            plotOptions: {
                series: {
                        color: '#80699B'
                 }
            },
            series: [{
            name: "Tweets",
            data: processed_json
            }]

        });
    });
