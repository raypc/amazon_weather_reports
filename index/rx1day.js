// -----------------------------------------------------------------
// // 1. Study area and visualization setup
// -----------------------------------------------------------------

// Region of interest (rectangle around Amazonia)
var roi = ee.Geometry.BBox(-79.49160, -18.16247, -43.69472, 10.05915);
var muni_amazonia = ee.FeatureCollection('amazonia_municipalities_50p_all_encoding');

// Paramenters for visualization
var precipitationVis = {
  min: 0,
  max: 150,
  palette: ['#ffffff', '#00FFFF', '#0080FF', '#0000FF', '#8000FF', '#FF00FF', '#FF0080', '#FF0000', '#FFFF00']
};
Map.centerObject(roi, 4);


// -----------------------------------------------------------------
// // 2. Dataset and period setup
// -----------------------------------------------------------------

// Load the CHIRPS daily precipitation data  -----------------------
var chirps = ee.ImageCollection('UCSB-CHG/CHIRPS/DAILY');
var start_year = 2013;
var end_year = 2023;


// -----------------------------------------------------------------
// // 3. Maximum 1-day precipitation
// -----------------------------------------------------------------

// Function to calculate annual Rx1day -----------------------
var calculate_annual_rx1day = function(year) {
  var start_date = ee.Date.fromYMD(year, 1, 1);
  var end_date = start_date.advance(1, 'year');

  // Filter CHIRPS data for one year
  var yearly_chirps = chirps.filterDate(start_date, end_date);

  // Find the maximum daily precipitation for the year
  var rx1day = yearly_chirps.select('precipitation').reduce(ee.Reducer.max());

  // Set properties
  return rx1day.set('year', year).set('system:time_start', start_date.millis());
};

// Map the function to get the collection of Rx1day -----------------------
var years = ee.List.sequence(start_year, end_year);
var annual_rx1day_collection = ee.ImageCollection(years.map(calculate_annual_rx1day));
// print(annual_rx1day_collection);


// -----------------------------------------------------------------
// // 4. Aggregation by munipality
// -----------------------------------------------------------------

// Define the combined reducer ------------------
var mean_max_reducer = ee.Reducer.mean()
  .combine({
    reducer2: ee.Reducer.max(),
    sharedInputs: true
  });

// Define the function to apply reduceRegions to a single image ------------------
var apply_reduce_regions = function(image) {
  var reduced_image = image.reduceRegions({
    collection: muni_amazonia,
    reducer: mean_max_reducer,
    scale: 5566
  });
  // Add a property to the FeatureCollection to identify the image it came from
  var image_date = image.get('system:time_start');
  // Map over the reduced features to add the image_time property
  return reduced_image.map(function(feature) {
    return feature.set('image_time', image_date);
  });
};

// Map the function over ImageCollection ------------------
var annual_rx1day_reduced_collection = annual_rx1day_collection.map(apply_reduce_regions);
var all_reduced_features = annual_rx1day_reduced_collection.flatten(); // Flatten the collection of FeatureCollections for easier handling/export

// Drop geometry for tabular export ------------------
var features_no_geometry = all_reduced_features.map(function(feature) {
  return feature.setGeometry(null);
});


// -----------------------------------------------------------------
// // 5. Visualization and Export
// -----------------------------------------------------------------

// Add each annual CDD image to the map  ------------------
// years.evaluate(function(years_list) {
//   years_list.forEach(function(year) {
//     var rx1day_image = annual_rx1day_collection.filter(ee.Filter.eq('year', year)).first();
//     Map.addLayer(rx1day_image.clip(roi), precipitationVis, 'Rx1day_' + year);
//   });
// });


// Export table to the drive  ------------------

Export.table.toDrive({
  collection: features_no_geometry,
  description: 'rx1day_municipality',
  folder: 'GEE_Exports',
  fileNamePrefix: 'rx1day_annual_mean_max_per_municipality',
  fileFormat: 'CSV'
});

// Export a separate GeoTIFF for each year
// for (var i = start_year; i <= end_year; i++) {
//   var year = i;
//   // Corrected line: Use ee.Filter.eq() instead of 'eq' string
//   var image_year = annual_rx1day_collection.filter(ee.Filter.eq('year', year)).first(); 
//   Export.image.toDrive({
//     image: image_year,
//     description: 'rx1day_' + year,
//     folder: 'GEE_Exports',
//     fileNamePrefix: 'rx1day_' + year,
//     scale: 5566, 
//     region: roi,
//     maxPixels: 2000000000,
//     fileFormat: 'GeoTIFF'
//   });
// }

