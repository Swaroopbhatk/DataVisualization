"""This map contains 3 layers
 *Base Map
 *Polygon layer
 *Dot layer
 Required library folium and ginger"""

import folium
import pandas

location = pandas.read_csv("Volcanoes_USA.csv")
n_row = location.shape[0]
Map = folium.Map(location=[38.58, -99.09], zoom_start=6, tiles="Mapbox Bright")


def col_producer(col):
    if col < 1000:
        return "green"
    elif 1000 <= col < 3000:
        return "orange"
    else:
        return "red"


fgv = folium.FeatureGroup(name="Volcanoes Marker")#feature group 1 to separate Marker

# Adding coordinates or feature groups to maps
for i in range(n_row):
    fgv.add_child(folium.Marker(location=[location.LAT[i], location.LON[i]],
                                popup="Elevation = " + str(location.ELEV[i])+"m",
                                icon=folium.Icon(color=col_producer(float(location.ELEV[i])))))


fg = folium.FeatureGroup(name="population")#feature group 1 to separate Marker
fg.add_child(folium.GeoJson(data=open("world.json",
                                      mode='r', encoding="utf-8-sig").read(),
                            style_function= lambda x: {"fillColor": "yellow" if x["properties"]["POP2005"] < 10000000
                                                       else "orange"
                            if 100000000 <= x["properties"]["POP2005"] < 200000000
                                                       else "red"}))

Map.add_child(fg)
Map.add_child(fgv)
Map.add_child(folium.LayerControl())  # used to control the layer
Map.save("Map1.html")
