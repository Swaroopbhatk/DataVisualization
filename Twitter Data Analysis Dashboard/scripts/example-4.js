$.getJSON('data/follCount.json', function(data){

Highcharts.chart('follower', {
    series: [{
        type: 'wordcloud',
        data: data,
        name: 'Followers'
    }],
    title: {
        text: '<strong>Most Followed Users</strong>'
    },
     subtitle: {
        text: 'Users with highest followers among the displayed users.'
    },
    style: {
             fontWeight: 'bold',
             font: 'serif'
            }
});
});