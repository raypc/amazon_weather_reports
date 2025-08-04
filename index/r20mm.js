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

// Define the CHIRPS dataset and parameters
var chirps = ee.ImageCollection('UCSB-CHG/CHIRPS/DAILY');
var start_year = 2013;
var end_year = 2023;
var prcp_threshold = 20; // mm


// -----------------------------------------------------------------
// // 3. Annual count of days when PRCP ≥ 20mm
// -----------------------------------------------------------------

// Filter the CHIRPS data for the desired time period ------------------
var chirps_filtered = chirps.filterDate(
  ee.Date.fromYMD(start_year, 1, 1),
  ee.Date.fromYMD(end_year + 1, 1, 1) // Filter until the beginning of the year after end_year
);

// Create a function to process each year ------------------
var calculate_annual_heavy_prcp = function(year) {
  var start_date = ee.Date.fromYMD(year, 1, 1);
  var end_date = start_date.advance(1, 'year');

  // Filter the daily images for the current year
  var yearly_collection = chirps_filtered.filterDate(start_date, end_date);

  // Apply the threshold to each image and count days meeting the criteria
  // Map a function over the collection: for each daily image, create a binary image (1 where prcp >= threshold, 0 otherwise)
  var heavy_prcp_days = yearly_collection.map(function(image) {
    return image.select('precipitation').gte(prcp_threshold).rename('heavy_prcp_flag');
  });

  // Sum the binary images to get the total count of heavy precipitation days for the year
  // The 'sum()' reducer will add up all the '1' values for each pixel
  var annual_count = heavy_prcp_days.sum().rename('annual_heavy_prcp_count');

  // Set properties for the resulting image
  return annual_count.set('year', year);
};

// Iterate over the years and apply the function ------------------
var years = ee.List.sequence(start_year, end_year);
var annual_counts_collection = ee.ImageCollection.fromImages(
  years.map(calculate_annual_heavy_prcp)
);


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

// Map the function over your ImageCollection ------------------
var annual_counts_reduced_collection = annual_counts_collection.map(apply_reduce_regions);
var all_reduced_features = annual_counts_reduced_collection.flatten(); // Flatten the collection of FeatureCollections for easier handling/export

// Drop geometry for tabular export ------------------
var features_no_geometry = all_reduced_features.map(function(feature) {
  return feature.setGeometry(null);
});


// -----------------------------------------------------------------
// // 5. Visualization and Export
// -----------------------------------------------------------------

// // Add each annual count of days when PRCP ≥ 20mm image to the map
// years.evaluate(function(years_list) {
//   years_list.forEach(function(year) {
//     var r20mm_image = annual_counts_collection.filter(ee.Filter.eq('year', year)).first();
//     Map.addLayer(r20mm_image.clip(roi), precipitationVis, 'r20mm_' + year);
//   });
// });

// Export table to the drive ------------------

Export.table.toDrive({
  collection: features_no_geometry,
  description: 'r20mm_municipality',
  folder: 'GEE_Exports',
  fileNamePrefix: 'r20mm_annual_mean_max_per_municipality',
  fileFormat: 'CSV'
});

// // Export a separate GeoTIFF for each year
// for (var i = start_year; i <= end_year; i++) {
//   var year = i;
  
//   var image_year = annual_counts_collection.filter(ee.Filter.eq('year', year)).first();
//   image_year = image_year.toDouble(); 
  
//   Export.image.toDrive({
//     image: image_year,
//     description: 'r20mm_' + year,
//     folder: 'GEE_Exports',
//     fileNamePrefix: 'r20mm_' + year,
//     scale: 5566, 
//     region: roi,
//     maxPixels: 2000000000,
//     fileFormat: 'GeoTIFF'
//   });
// }
