

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

// Define the time period for analysis (2013-2023)
var start_date = '2013-01-01';
var end_date = '2024-01-01';

// Define the baseline period for percentile calculation (1961-1990)
var baseline_start_date = '1961-01-01';
var baseline_end_date = '1991-01-01';

// Load the CHIRPS Daily dataset
var chirps_daily = ee.ImageCollection('UCSB-CHG/CHIRPS/DAILY');
var chirps_period = chirps_daily.filterDate(start_date, end_date);
var chirps_baseline = chirps_daily.filterDate(baseline_start_date, baseline_end_date);

// A very wet day is defined as being greater than the 95th percentile of “wet days” (R ≥1mm) during the 1961-1990 reference period
var wet_day_threshold = 1.0; // mm


// -----------------------------------------------------------------
// // 3. Rainfall on very wet days (R95pTOT)
// -----------------------------------------------------------------

// Function to calculate the 95th percentile for a given period ------------------
var calculate_95th_percentile = function(collection) {
  // Filter for wet days (precipitation >= wet_day_threshold)
  var wet_days = collection.map(function(image) {
    return image.updateMask(image.select('precipitation').gte(wet_day_threshold));
  });

  // Compute the 95th percentile of precipitation values over the entire period
  var percentile_95 = wet_days.select('precipitation')
                           .reduce(ee.Reducer.percentile([95]));
  return percentile_95.rename('percentile_95');
};

// Calculate the 95th percentile using the 1961-1990 baseline period ------------------
var p95_image = calculate_95th_percentile(chirps_baseline);

// Function to calculate R95pTOT for a single year ------------------
var calculate_r95p_tot = function(year) {
  year = ee.Number(year);

  var start_of_year = ee.Date.fromYMD(year, 1, 1);
  var end_of_year = start_of_year.advance(1, 'year');

  var annual_chirps = chirps_daily.filterDate(start_of_year, end_of_year);

  // For each daily image in the year, mask out values below the 95th percentile
  var very_wet_days_in_year = annual_chirps.map(function(image) {
    var precipitation = image.select('precipitation');
    // Use the 95th percentile image calculated from the baseline period
    var p95 = p95_image.select('percentile_95');

    // Mask out pixels where precipitation is less than the 95th percentile or below wet day threshold
    return precipitation.updateMask(precipitation.gte(p95).and(precipitation.gte(wet_day_threshold)));
  });

  // Sum the precipitation from the very wet days for the year
  var band_name = ee.String('R95pTOT_').cat(year.format('%d'));
  var r95p_tot = very_wet_days_in_year.sum().rename(band_name);

  // Set the 'year' property on the image
  return r95p_tot.set('year', year);
};

// Generate a list of years from 2013 to 2023 ------------------
var years = ee.List.sequence(2013, 2023);

// Map the R95pTOT calculation function over the years ------------------
var r95p_tot_collection = ee.ImageCollection(years.map(calculate_r95p_tot));
// print(r95p_tot_collection);

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

  // Add a property to the FeatureCollection to identify the image it came from (date)
  var image_date = image.get('system:time_start');
  // Map over the reduced features to add the image_time property
  return reduced_image.map(function(feature) {
    return feature.set('image_time', image_date);
  });
};

// Map the function over your ImageCollection ------------------
var r95p_muni = r95p_tot_collection.map(apply_reduce_regions);
var all_reduced_features = r95p_muni.flatten(); // Flatten the collection of FeatureCollections for easier handling/export

// Drop geometry for tabular export ------------------
var features_no_geometry = all_reduced_features.map(function(feature) {
  return feature.setGeometry(null);
});


// -----------------------------------------------------------------
// // 6. Visualization and Export
// -----------------------------------------------------------------

// // Add each annual count of days when PRCP ≥ 20mm image to the map ------------------
// years.evaluate(function(years_list) {
//   years_list.forEach(function(year) {
//     var r95p_image = r95p_tot_collection.filter(ee.Filter.eq('year', year)).first();
//     Map.addLayer(r95p_image.clip(roi), precipitationVis, 'r95p_tot_' + year);
//   });
// });


// Export table to the drive ------------------
Export.table.toDrive({
  collection: features_no_geometry,
  description: 'r95p_tot_municipality',
  folder: 'GEE_Exports',
  fileNamePrefix: 'r95p_tot_annual_mean_max_per_municipality',
  fileFormat: 'CSV'
});
  