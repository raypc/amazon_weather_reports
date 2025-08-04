var municipalities = ee.FeatureCollection('amazonia_municipalities_50p_all_encoding');

// Define the date range
var startYear = 2013;
var endYear = 2023;

// Function to filter FIRMS data for a given year and process it
var processYear = function(year) {
  var startDate = ee.Date.fromYMD(year, 1, 1);
  var endDate = ee.Date.fromYMD(year, 12, 31);
  
  // Filter FIRMS collection for the year
  var yearlyFirms = ee.ImageCollection('FIRMS')
    .filterDate(startDate, endDate)
    .select('T21'); // T21 is the brightness temperature band
  
  // Create a binary image where 1=hot pixel, 0=no hot pixel
  // Here we assume any pixel with T21 > 0 is a hot pixel
  var hotPixels = yearlyFirms.map(function(image) {
    return image.gt(0).rename('hot_pixel');
  });
  
  // Sum all hot pixels through the year (creates a single image with count per pixel)
  var yearlyHotPixelCount = hotPixels.sum().rename('count');
  
  // Reduce regions to get counts per municipality
  var countsPerMunicipality = yearlyHotPixelCount.reduceRegions({
    collection: municipalities,
    reducer: ee.Reducer.sum(),
    scale: 1000, // FIRMS resolution is ~1km
  });
  
  // Add year property to each feature
  countsPerMunicipality = countsPerMunicipality.map(function(feature) {
    return feature.set('year', year);
  });
  
  return countsPerMunicipality;
};

// Process all years
var years = ee.List.sequence(startYear, endYear);
var allYearsData = ee.FeatureCollection(years.map(processYear)).flatten();

// Drop geometry for tabular export ------------------
var features_no_geometry = allYearsData.map(function(feature) {
  return feature.setGeometry(null);
});

// Export the results to Google Drive
Export.table.toDrive({
  collection: features_no_geometry,
  description: 'hotpixel_annual_sum_per_municipality',
  fileFormat: 'CSV'
});

