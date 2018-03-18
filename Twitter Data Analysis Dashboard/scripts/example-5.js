$.getJSON('data/tweetCnt.json', function(dt){
$.getJSON('data/Top50Tweets.json', function(tp){

var geojson,
            activeLayer;
        $(document).ready(function(){


            var map = new L.Map('map-holder', {
                center: new L.LatLng(53.3498, -7.2603),
                zoom: 6.6
            });

            L.tileLayer('https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png', {
                minZoom: 5,
                maxZoom: 15,
                noWrap: true,
                attribution: 'Data: CSO.ie | Tiles courtesy of <a href="http://openstreetmap.se/" target="_blank">OpenStreetMap Sweden</a> &mdash; Map data &copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors, <a href="http://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>'
            }).addTo(map);

            function getColor(d) {
                return d == 'carlow' ? '#800026' :
                      d == 'clare' ? '#3182bd' :
                      d == 'cork' ? '#E31A1C' :
                      d == 'donegal' ? '#fc8d59' :
                      d == 'dublin' ? '#dd1c77' :
                      d == 'fingal' ? '#FEB24C' :
                      d == 'galway' ? '#8856a7' :
                      d == 'kerry' ? '#31a354':
                      d == 'limerick' ? '#d95f0e':
                      d == 'sligo' ? '#FF0000':
                      '#636363';
  }

            $.getJSON('data/County.geojson', function(data){

                geojson = L.geoJSON(data, {
                     ////////////////////////////////
                     ////  Step 1 - look of the layer
                     ////////////////////////////////
                     style: function(feature)
                     {
                         return{
                          color: '#fff',
                          weight: 1,
                         // fillColor: '#FED976',
                          fillOpacity: 0.9,
                          fillColor: getColor(feature.properties.COUNTY.toLowerCase())
                      }},
                     ////////////////////////////////
                     ////  Step 2 - interaction
                     ////////////////////////////////
                     onEachFeature: onEachFeature
                 }).addTo(map);

            var legend = L.control({position: 'bottomright'});

            legend.onAdd = function (map) {

            var div = L.DomUtil.create('div', 'info legend'),
                grades = ['Carlow', 'Clare', 'Cork', 'Donegal', 'Dublin', 'Fingal', 'Galway', 'Kerry', 'Limerick', 'Sligo', 'Others'];
                labels = [];



            for (var i = 0; i < grades.length; i++) {
                div.innerHTML +=
                '<i style="background:' + getColor(grades[i].toLowerCase()) + '"></i> '
                 + (grades[i] ? ' ' + '<strong>'+grades[i] +'</strong>' +' '+'-'+' '+dt[i].Counts.toFixed(2)+'%' +'<br>' : '+');
                }

                return div;
            };

            legend.addTo(map);


            for (var i = 0; i < tp.length; i++) {
			    marker = new L.marker([parseFloat(tp[i].latitude),parseFloat(tp[i].longitude)])
				.bindPopup("<strong>Tweet: </strong>"+ tp[i].text+'<br>'+"<strong>ReTweet Counts: </strong>"+tp[i].tweetCount+'<br>'+"<strong>Location: </strong>"+"("+tp[i].latitude.toFixed(2)+", "+tp[i].longitude.toFixed(2)+")")
				.addTo(map);
		        };

            });
        });

         function highlightLayer(e)
         {
             var layer = e.target;
             activeLayer = layer;

             layer.setStyle({
                 color: '#fff',
                 fillColor: '#9ecae1'
             });

             if (!L.Browser.ie && !L.Browser.opera && !L.Browser.edge) {
                 layer.bringToFront();
             }

         }

         function resetHighlight(e) {
             geojson.resetStyle(e.target);
         }


         function clickedMap(e)
         {
             console.log(e);
         }

        function onEachFeature(feature, layer) {


             layer.on({
                 mouseover: highlightLayer,
                 mouseout: resetHighlight,
                 click: clickedMap
             });

            ////////////////////////////////
            ////  Step 3 - add a popup
            ////////////////////////////////
                 function tweet(x){
                 return x == 'carlow' ? '<strong>'+x.toUpperCase()+ '</strong> '+'<br><strong>Tweets Share: </strong>'+'0.495%' :
                      x == 'clare' ? '<strong>'+x.toUpperCase()+ '</strong> '+'<br><strong>Tweets Share: </strong>'+'1.485%' :
                      x == 'cork' ? '<strong>'+x.toUpperCase()+ '</strong> '+'<br><strong>Tweets Share: </strong>'+'5.445%' :
                      x == 'donegal' ? '<strong>'+x.toUpperCase()+ '</strong> '+'<br><strong>Tweets Share: </strong>'+'0.4950%' :
                      x == 'dublin' ? '<strong>'+x.toUpperCase()+ '</strong> '+'<br><strong>Tweets Share: </strong>'+'34.158%' :
                      x == 'fingal' ? '<strong>'+x.toUpperCase()+ '</strong> '+'<br><strong>Tweets Share: </strong>'+'0.495%' :
                      x == 'galway' ? '<strong>'+x.toUpperCase()+ '</strong> '+'<br><strong>Tweets Share: </strong>'+'1.980%' :
                      x == 'kerry' ? '<strong>'+x.toUpperCase()+ '</strong> '+'<br><strong>Tweets Share: </strong>'+'1.980%':
                      x == 'limerick' ? '<strong>'+x.toUpperCase()+ '</strong> '+'<br><strong>Tweets Share: </strong>'+'2.970%':
                      x == 'sligo' ? '<strong>'+x.toUpperCase()+ '</strong> '+'<br><strong>Tweets Share: </strong>'+'0.495%':
                      '<strong>'+x.toUpperCase()+ '</strong> '+'<br><strong>Tweets Share (others): </strong>'+'50%';
                 };

                 var popUpContent = tweet(feature.properties.COUNTY.toLowerCase())
                 layer.bindPopup(popUpContent);


        };

});
});