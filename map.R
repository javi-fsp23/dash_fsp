graf_mapa<-function(as){ 
  jsscript<-   paste0(
    "function(cluster) {
const groups= [",paste("'", as$Incidente ,"'",sep="",collapse=","),"];
const colors= {
groups: [",paste("'", as$Color ,"'",sep="",collapse=","),"],
center:'#ddd',
text:'black'
};
const markers= cluster.getAllChildMarkers();

const proportions= groups.map(group => markers.filter(marker => marker.options.group === group).length / markers.length);
function sum(arr, first= 0, last) {
return arr.slice(first, last).reduce((total, curr) => total+curr, 0);
}
const cumulativeProportions= proportions.map((val, i, arr) => sum(arr, 0, i+1));
cumulativeProportions.unshift(0);

const width = 2*Math.sqrt(markers.length);
const radius= 15+width/2;

const arcs= cumulativeProportions.map((prop, i) => { return {
x   :  radius*Math.sin(2*Math.PI*prop),
y   : -radius*Math.cos(2*Math.PI*prop),
long: proportions[i-1] >.5 ? 1 : 0
}});
const paths= proportions.map((prop, i) => {
if (prop === 0) return '';
else if (prop === 1) return `<circle cx='0' cy='0' r='${radius}' fill='none' stroke='${colors.groups[i]}' stroke-width='${width}' stroke-alignment='center' stroke-linecap='butt' />`;
else return `<path d='M ${arcs[i].x} ${arcs[i].y} A ${radius} ${radius} 0 ${arcs[i+1].long} 1 ${arcs[i+1].x} ${arcs[i+1].y}' fill='none' stroke='${colors.groups[i]}' stroke-width='${width}' stroke-alignment='center' stroke-linecap='butt' />`
});

return new L.DivIcon({
html: `
<svg width='60' height='60' viewBox='-30 -30 60 60' style='width: 60px; height: 60px; position: relative; top: -24px; left: -24px;' >
<circle cx='0' cy='0' r='15' stroke='none' fill='${colors.center}' />
<text x='0' y='0' dominant-baseline='central' text-anchor='middle' fill='${colors.text}' font-size='15'>${markers.length}</text>
${paths.join('')}
</svg>
`,
className: 'marker-cluster'
});
}")
return(jsscript)}


icons<-function(icono, color_m ,color){
  makeAwesomeIcon(icon= icono, 
                  markerColor=color_m,
                  library = "fa",
                  iconColor  = color)}


library(sf)
library(leaflet)
library(leaflet.extras)
library(dplyr)
load("Z:/apps/shapes/edos.RData")

as<-ad %>% filter(Color!="green")

leaflet() %>%setView( -99.1, 19.37, 10.5 ) %>% addTiles() %>%
  #addPolygons(data = mx, weight = 2, fillOpacity = 0, color = 'blue',  label = ~nom_ent ) %>%
  addAwesomeMarkers(data = as, lat = ~lat, lng = ~long, icon = awesomeIconList(icons(as$icon, as$Color, as$Color)), 
                    group = ~Incidente, clusterOptions = markerClusterOptions(iconCreateFunction =JS(graf_mapa(as))),
                    popup = paste(paste0("<b>","Unidad ", as$unidad, "(", as$nombre_fcia,")", "</b>:", paste0(as$Incidente," (", as$Folio, ")<br>")),
                                   "<b>", "Sub-incidente: ","</b>", as$Subincidente, "<br>",
                                   "<b>", "Fecha: ","</b>", as$Recepcion, "<br>",
                                   "<b>", "Gerente de farmacia: ","</b>", as$ger_fcia, "<br>",
                                   "<b>", "Tel. Gerente de farmacia: ","</b>", paste0("<a href=tel:+", as$tel_ger_fcia,">", as$tel_ger_fcia, "</a>"), "<br>",
                                   "<b>", "Gerente de zona: ","</b>", as$ger_zona, "<br>",
                                   "<b>", "Tel. Gerente de zona: ","</b>", paste0("<a href=tel:+", as$tel_ger_zona,">", as$tel_ger_zona, "</a>"), "<br>",
                                  "<b>", "Tel. consultorio: ","</b>", paste0("<a href=tel:+", as$cel_consul,">", as$cel_consul, "</a>"), "<br>",
                                  "<b>", "Ger. siniestro consultorio: ","</b>", as$ger_siniestro_consul, "<br>",
                                  "<b>", "Tel. Ger. siniestro consultorio: ","</b>", paste0("<a href=tel:+", as$tel_ger_consul,">", as$tel_ger_consul, "</a>"), "<br>")) %>%
  addLayersControl( baseGroups = c("Street Map", "Carto", "World Imagen", "Dark Matter"),
                    overlayGroups = c("Heatmap") ,
                    options = layersControlOptions(collapsed = T),
                    position = 'bottomleft') %>% hideGroup( c("Heatmap"))  %>%
  addLegend(title = "Prioridad: ", position = c("topright"), colors =  c("green", "orange", "red"),
            labels = c("Baja", "Media", "Alta")) %>%
  addProviderTiles("OpenStreetMap", group = "Street Map") %>%
  addProviderTiles("CartoDB.DarkMatter", group = "Dark Matter") %>%
  addProviderTiles("CartoDB.Positron", group = "Carto") %>%
  addProviderTiles("Esri.WorldImagery",  group = "World Imagen") %>%
  addHeatmap(lng = as$long, lat = as$lat, max = .05,
             radius = 20, group="Heatmap", blur = 20)