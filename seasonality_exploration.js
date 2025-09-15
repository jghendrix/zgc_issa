// Google earth engine code to get dates for seasonal definitions?

// Region ---------------------------------------------------------------------
  var Fogo =  ee.Geometry.Polygon(
    [[[-54.359, 49.514],
      [-53.974, 49.514],
      [-53.974, 49.768],
      [-54.359, 49.768]]], null, false);
  
  Map.addLayer(Fogo);
  
  
  // Images ---------------------------------------------------------------------
    var dataset = ee.ImageCollection('NOAA/VIIRS/001/VNP22Q2')
    .filter(ee.Filter.date('2013-01-01', '2021-12-31'));
    var dates = dataset.select([
      'Date_Mid_Greenup_Phase_1',
      'Date_Mid_Senescence_Phase_1',
      'EVI2_Onset_Greenness_Increase_1']);
    
    // Export ---------------------------------------------------------------------
    var folder = 'TN Caribou';
    
    Export.image.toDrive({
      image: dates,
      description: '_phenology_500_m',
      region: Fogo,
      scale: 500,
      folder: folder
    });
    
 //   Export.image.toDrive({
//      image: hermosilla_2019,
  //    description: today + '_Hermosilla_2019_land_cover',
    //  region: region,
    //  scale: 30,
    //  folder: folder
    //s});
    