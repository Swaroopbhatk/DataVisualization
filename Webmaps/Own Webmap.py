""" Ths plots the previous accidents locations to the map and
can be analysed to add the caution board for the drivers"""
import pandas
import folium
from sklearn.model_selection import train_test_split

RANDOM_SEED = 5
location = pandas.read_csv("Accidents.csv", sep=',', error_bad_lines=False, index_col=False, dtype='unicode')
train, test = train_test_split(location, test_size=0.008)
location = test
location.Longitude = location.Longitude.astype(float)
location.Latitude = location.Latitude.astype(float)
location.Accident_Severity = location.Accident_Severity.astype(int)
location = location[location.Accident_Severity > 2]
location.dropna()
location = location.reset_index(drop=True)
location.Accident_Severity.astype(int)
n_row = location.shape[0]
Map = folium.Map(location=[55.3781, -3.4360], zoom_start=6, tiles="cartodbpositron")


def col_producer(col):
    if col <= 2:
        return "orange"
    else:
        return "red"


fgv = folium.FeatureGroup(name="Accident")

# Adding coordinates or feature groups to maps
for i in range(n_row):
    #try:
        if location.Latitude[i] != "NULL" or location.Longitude[i] != "NULL":
            print(i)
            fgv.add_child(folium.Marker(location=[location.Latitude[i], location.Longitude[i]],
                                        popup="Severity = " + str(location.Accident_Severity[i]),
                                        icon=folium.Icon(color=col_producer(int(location.Accident_Severity[i])))))
    #except:
        #pass

fg = folium.FeatureGroup(name="population")
fg.add_child(folium.GeoJson(data=open("world.json",
                                      mode='r', encoding="utf-8-sig").read(),
                            style_function=lambda x: {"fillColor": "yellow" if x["properties"]["POP2005"] < 10000000
                            else "orange"
                            if 100000000 <= x["properties"]["POP2005"] < 200000000
                                                       else "red"}))


Map.add_child(fgv)
Map.add_child(fg)
Map.add_child(folium.LayerControl())  # used to control the layer
Map.save("Accident_Prone_Area.html")
