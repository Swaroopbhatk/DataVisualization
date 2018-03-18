
$.getJSON('data/tweetsPerDay.json', function(data){

var cnt = {
    name: 'ReTweetCount',
    data: []
    };
var op = new Array(data.length);

for (i = 0; i < data.length; i++) {

    op[i] = new Array(2);
    op[i][0] = data[i].dt;
    op[i][1] = data[i].TotCount;
    cnt.data.push(data[i].dt);

};
cnt.data.push(op);
Highcharts.chart('series',
{
    title: {
        text: 'Twitter Trend',
        style: {
                              fontWeight: 'bold',
                              font: 'serif'
                             }
    },

    subtitle: {
        text: 'Twitter Trend between 2018-03-07 to 2018-03-13 for #IrishWater'
    },

    yAxis: {
        title: {
            text: 'ReTweet Counts',
            style: {
                      fontWeight: 'bold'
                    }
        }
    },

    xAxis: {
        title: {
            text: 'Days',
            style: {
                              fontWeight: 'bold'
                             }
        },
        type: 'date',
        categories: cnt.data,
     },

    legend: {
        layout: 'vertical',
        align: 'right',
        verticalAlign: 'middle'

    },
    plotOptions: {
                series: {
                        color: '#f28f43'
                 }
          },
    series:[{
     name: 'ReTweetCount',
     data: op
     }]
});
});
