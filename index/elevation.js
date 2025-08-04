// -----------------------------------------------------------------
// // 1. Study area and visualization setup
// -----------------------------------------------------------------

// Region of interest (rectangle around Amazonia)
var roi = ee.Geometry.BBox(-79.49160, -18.16247, -43.69472, 10.05915);
var muni_amazonia = ee.FeatureCollection('amazonia_municipalities_50p_all_encoding');
// print(muni_amazonia.limit(10));

// Paramenters for visualization
var precipitationVis = {
  min: 0,
  max: 50,
  palette: ['#ffffff', '#00FFFF', '#0080FF', '#0000FF', '#8000FF', '#FF00FF', '#FF0080', '#FF0000', '#FFFF00']
};
Map.centerObject(roi, 4);


// -----------------------------------------------------------------
// // 2. Dataset setup
// -----------------------------------------------------------------

var elevation = ee.Image("CGIAR/SRTM90_V4").select('elevation');
// print(elevation);

var slope = ee.Terrain.slope(elevation);
// print(slope);

// -----------------------------------------------------------------
// // 4. Aggregation by munipality
// -----------------------------------------------------------------

// Define the combined reducer ------------------
var mean_max_reducer = ee.Reducer.mean()
  .combine({
    reducer2: ee.Reducer.max(),
    sharedInputs: true 
  });

  // 3. Calculate zonal mean elevation
var zonalElevation = elevation.reduceRegions({
  collection: muni_amazonia,
  reducer: mean_max_reducer, 
  scale: 90, // SRTM resolution is ~90m
});
// print(zonalElevation);

// Drop geometry for tabular export ------------------
var features_no_geometry = zonalElevation.map(function(feature) {
  return feature.setGeometry(null);
});


var zonal_slope = slope.reduceRegions({
  collection: muni_amazonia,
  reducer: mean_max_reducer,
  scale: 90, // SRTM resolution is ~90m
});

var features_no_geometry_slope = zonal_slope.map(function(feature) {
  return feature.setGeometry(null);
});


// // -----------------------------------------------------------------
// // // 5. Visualization and Export
// // -----------------------------------------------------------------

Map.addLayer(elevation, precipitationVis);
Map.addLayer(slope, precipitationVis);

// Export table to the drive  ------------------

Export.table.toDrive({
  collection: features_no_geometry,
  description: 'elevation_municipality',
  folder: 'GEE_Exports',
  fileNamePrefix: 'elevation_mean_max_per_municipality',
  fileFormat: 'CSV'
});

Export.table.toDrive({
  collection: features_no_geometry_slope,
  description: 'slope_municipality',
  folder: 'GEE_Exports',
  fileNamePrefix: 'slope_mean_max_per_municipality',
  fileFormat: 'CSV'
});

// Export.image.toDrive({
//   image: elevation,
//   description: 'elevation',
//   folder: 'GEE_Exports',
//   scale: 90,
//   region: roi,
//   maxPixels: 2000000000,
//   fileFormat: 'GeoTIFF'
// });