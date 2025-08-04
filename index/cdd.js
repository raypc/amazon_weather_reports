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

// Load the CHIRPS Daily precipitation dataset
var chirps = ee.ImageCollection('UCSB-CHG/CHIRPS/DAILY');
var start_year = 2013;
var end_year = 2023;
var years = ee.List.sequence(start_year, end_year);

// -----------------------------------------------------------------
// // 3. Annual maximum number of consecutive days with RR < 1mm
// -----------------------------------------------------------------

// Function to calculate the annual maximum number of consecutive dry days  ------------------
var calculate_annual_cdd = function(year) {
  var start_date = ee.Date.fromYMD(year, 1, 1);
  var end_date = start_date.advance(1, 'year');

  // Filter the CHIRPS collection for the current year
  var yearly_precip = chirps.filterDate(start_date, end_date);

  // Create a binary image collection where 1 represents a dry day (RR < 1mm)
  var dry_days = yearly_precip.map(function(image) {
    var dry = image.lt(1);
    return dry.set('system:time_start', image.get('system:time_start'));
  });

  // Function to count consecutive dry days using iterate
  var count_consecutive = function(image, list) {
    list = ee.List(list);
    var previous_image = ee.Image(list.get(-1));
    var current_consecutive = image.add(previous_image).multiply(image);
    
    // This ensures all images in the list are homogeneous
    return list.add(current_consecutive.uint16().copyProperties(image, ['system:time_start']));
  };

  // Create an initial image with a matching data type and band name
  var first_image = ee.Image(0).uint16()
    .rename('precipitation')
    .set('system:time_start', start_date.millis());

  // Iterate through the dry day collection to count consecutive days
  var consecutive_days_list = ee.List(dry_days.iterate(count_consecutive, ee.List([first_image])));
  
  var consecutive_days = ee.ImageCollection.fromImages(
    consecutive_days_list.slice(1) // Remove the initial 'first_image'
  );

  // Get the maximum number of consecutive dry days for the year
  var max_cdd = consecutive_days.max();

  // Return the result with the year as a property
  return max_cdd.set('year', year);
};

// Map the function over the list of years  ------------------
var annual_cdd = ee.ImageCollection.fromImages(years.map(calculate_annual_cdd));
print('Annual Maximum Consecutive Dry Days:', annual_cdd);


// -----------------------------------------------------------------
// // 4. Aggregation by munipality
// -----------------------------------------------------------------

// Define the combined reducer ------------------
var mean_max_reducer = ee.Reducer.mean()
  .combine({
    reducer2: ee.Reducer.max(),
    sharedInputs: true // Important: tells the reducers to operate on the same input
  });

// Define the function to apply reduceRegions to a single image ------------------
var apply_reduce_regions = function(image) {
  var reduced_image = image.reduceRegions({
    collection: muni_amazonia,
    reducer: mean_max_reducer, // Use the combined reducer here
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
var annual_cdd_reduced_collection = annual_cdd.map(apply_reduce_regions);
var all_reduced_features = annual_cdd_reduced_collection.flatten(); // Flatten the collection of FeatureCollections for easier handling/export

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
//     var cdd_image = annual_cdd.filter(ee.Filter.eq('year', year)).first();
//     Map.addLayer(cdd_image.clip(roi), precipitationVis, 'CDD_' + year);
//   });
// });


// Export table to the drive  ------------------
Export.table.toDrive({
  collection: features_no_geometry,
  description: 'cdd_municipality',
  folder: 'GEE_Exports',
  fileNamePrefix: 'cdd_annual_mean_max_per_municipality',
  fileFormat: 'CSV'
});


// Export a separate GeoTIFF for each year
// for (var i = start_year; i <= end_year; i++) {
//   var year = i;
//   // Corrected line: Use ee.Filter.eq() instead of 'eq' string
//   var image_year = annual_cdd.filter(ee.Filter.eq('year', year)).first(); 
//   Export.image.toDrive({
//     image: image_year,
//     description: 'cdd_' + year,
//     folder: 'GEE_Exports',
//     fileNamePrefix: 'cdd_' + year,
//     scale: 5566, 
//     region: roi,
//     maxPixels: 2000000000,
//     fileFormat: 'GeoTIFF'
//   });
// }
