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

// Define the dataset: CHIRPS daily 
var chirps = ee.ImageCollection('UCSB-CHG/CHIRPS/DAILY');
var start_year = 2013;
var end_year = 2023;
var years = ee.List.sequence(start_year, end_year);


// -----------------------------------------------------------------
// // 3. Maximum consecutive 5-day precipitation
// -----------------------------------------------------------------

// Function to calculate Rx5day for a single year ------------------
var calculate_rx5day = function(year) {
  year = ee.Number(year);
  var start_date = ee.Date.fromYMD(year, 1, 1);
  var end_date = start_date.advance(1, 'year');

  var chirps_filter = chirps.filterDate(start_date, end_date); // Filter CHIRPS data for the current year
  var offsets = ee.List.sequence(0, 4); // List of offsets for a 5-day window

  // Map over the annual CHIRPS collection to create the recipitation amount for the 5-day interval
  var rolling_sums = chirps_filter.map(function(image) {
    var date = image.date();
    
    // Create the 5-day window
    var window_images = offsets.map(function(offset) {
      var window_start = date.advance(ee.Number(offset).multiply(-1), 'day');
      var window_end = window_start.advance(1, 'day');
      return chirps_filter.filterDate(window_start, window_end).first();
    });

    var sum_image = ee.ImageCollection(window_images).sum(); // Convert the list of images to an ImageCollection and sum them
    return sum_image.set('system:time_start', date.millis()); // Set the time_start property to the current day for proper indexing
  });

  // Find the maximum value of the 5-day rolling sums for the year
  var rx5day = ee.ImageCollection(rolling_sums).max()
    .rename('Rx5day_mm')
    .set('year', year);
  return rx5day;
};

// Map the function over the list of years to get a collection of Rx5day images ------------------
var rx5day_collection = ee.ImageCollection(years.map(calculate_rx5day));


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
var annual_rx5day_reduced_collection = rx5day_collection.map(apply_reduce_regions);
var all_reduced_features = annual_rx5day_reduced_collection.flatten(); // Flatten the collection of FeatureCollections for easier handling/export

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
//     var rx5day_image = rx5day_collection.filter(ee.Filter.eq('year', year)).first();
//     Map.addLayer(rx5day_image.clip(roi), precipitationVis, 'Rx5day_' + year);
//   });
// });


// Export table to the drive  ------------------
Export.table.toDrive({
  collection: features_no_geometry,
  description: 'rx5day_municipality',
  folder: 'GEE_Exports',
  fileNamePrefix: 'rx5day_annual_mean_max_per_municipality',
  fileFormat: 'CSV'
});

