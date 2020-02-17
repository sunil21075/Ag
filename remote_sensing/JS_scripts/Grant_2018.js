////////////////////////////////////////////////////////////////////////
/////
/////     We can read a shapefile with many polygons, and using map()
/////     read all images corresponding to a polygon. However, the result of
/////     that is a featureCollection for which we do not know how to
/////     access to the info. inside. So, we want to do a for-loop for each polygon 
/////     now.
/////

var grant_2018_regions = ee.FeatureCollection(grant_2018_SF);
print(grant_2018_regions);

////////////////////////////////////////////////////////////////////////////////////////
///
///                           functions definitions start
///
/// Function to mask clouds using the Sentinel-2 QA band.
function maskS2clouds(image) {
    var qa = image.select('QA60');

    // Bits 10 and 11 are clouds and cirrus, respectively.
    var cloudBitMask = 1 << 10;
    var cirrusBitMask = 1 << 11;

    // Both flags should be set to zero, indicating clear conditions.
    var mask = qa.bitwiseAnd(cloudBitMask).eq(0).and(
                        qa.bitwiseAnd(cirrusBitMask).eq(0));

    // Return the masked and scaled data, without the QA bands.
    return image.updateMask(mask).divide(10000)
                          .select("B.*")
                          .copyProperties(image, ["system:time_start"]);
}

////////////////////////////////////////////////
///
/// add Day of Year to an image
///

function addDate_to_image(image){
  var doy = image.date().getRelative('day', 'year');
  var doyBand = ee.Image.constant(doy).uint16().rename('doy');
  doyBand = doyBand.updateMask(image.select('B8').mask());

  return image.addBands(doyBand);
}

////////////////////////
///
/// add Day of Year to an imageCollection
///

function addDate_to_collection(collec){
  var C = collec.map(addDate_to_image);
  return C;
}

////////////////////////////////////////////////
///
///
/// add NDVI to an image

function addNDVI_to_image(image) {
  var ndvi = image.normalizedDifference(['B8', 'B4']).rename('NDVI');
  return image.addBands(ndvi);
}

////////////////////////////////////////////////
///
///
/// add NDVI to an imageCollection

function add_NDVI_collection(image_IC){
  var NDVI_IC = image_IC.map(addNDVI_to_image);
  return NDVI_IC;
}

////////////////////////////////////////////////
///
///
/// Extract ImageCollection from Sentinel 2- level 1C 
/// dates etc are hard-coded to enable us to use map() function

function extract_sentinel_IC(a_feature){
    // var start_date = 
    // var end_date = 
    // var cloud_percentage = 
    // var geom = ee.Feature(feature_col).geometry();
    var geom = a_feature.geometry();
    var imageC = ee.ImageCollection('COPERNICUS/S2')
                .filterDate('2012-01-01', '2019-12-31')
                .filterBounds(geom)
                //.filterMetadata('CLOUDY_PIXEL_PERCENTAGE', "less_than", 10)
                .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', 10))
                .sort('system:time_start', true);
    
    imageC = imageC.map(maskS2clouds);
    imageC = imageC.select(['B8', 'B4', 'B3', 'B2']);
                
    imageC = addDate_to_collection(imageC);
    imageC = add_NDVI_collection(imageC);
    // imageC = imageC.sort('system:time_start', true);

    //imageC = imageC.map(add_NDVI_collection)
  return imageC;
}

///
///                         functions definitions end
///
////////////////////////////////////////////////////////////////////////////////////////

var Grant_2018_IC = grant_2018_regions.map(extract_sentinel_IC);
print(Grant_2018_IC);

var Grant_2018_IC_first = ee.ImageCollection(Grant_2018_IC.first());
print(Grant_2018_IC_first);
print(typeof(Grant_2018_IC_first));

var first_region = grant_2018_regions.first();

// print(Grant_2018_IC_first.get(1).geometries())

//#############################################################
//
//                        Plot Charts on right (Console)
//
//#############################################################


var first_NDVI_TS = ui.Chart.image.doySeriesByYear({
                            imageCollection: Grant_2018_IC_first, 
                            bandName: 'NDVI', 
                            region: first_region.geometry(), 
                            regionReducer: ee.Reducer.mean() 
                            // scale: plot_scale
                              });

print(first_NDVI_TS);

//#############################################################
//
//                        Plot the Map below
//
//#############################################################

Map.setCenter(-119.525, 47.525995, 16);
// Map.addLayer(first_region.geometry())
Map.addLayer(first_region.geometry(), 
             {color: 'FF0000'}, 
             'The farm is plotted in red');


// var vizParams = {
//   bands: ['B4', 'B3', 'B2'],
//   min: 0,
//   max: 0.5,
//   gamma: [0.95, 1.1, 1]
// };
// Map.addLayer(Grant_2018_IC_first.get(1), vizParams, 'false color composite');



// print(first_region.geometry().centroid())
// Look at here: 
// https://www.google.com/maps/place/47%C2%B031'33.2%22N+119%C2%B031
// '30.0%22W/@47.5259,-119.525,813m/data=!3m1!1e3!4m5!3m4!1s0x0:0x0!
// 8m2!3d47.5259!4d-119.525

//#############################################################
//
//                        Export data
//
//#############################################################

//#############################################################
//
//      Parameters (maybe) needed for exporting files
//

// var gg = first_region.geometry()
// var gg = first_region.geometry().bounds().getInfo();

// print ("gg");
// print (gg);

var k = Grant_2018_IC_first.size()
var Grant_2018_IC_first_list = Grant_2018_IC_first.toList(k)

// print (Grant_2018_IC_first)
// print (typeof(Grant_2018_IC_first))

// var Grant_2018_IC_first_flatten = Grant_2018_IC_first.flatten()
// print (Grant_2018_IC_first_flatten)

//#############################################################

// Export.table.toDrive({
//   collection: Grant_2018_IC_first_flatten,
//   //scale: 10,
//   description:'Grant_2018_IC_first_flatten',
//   folder:"Grant_2018_IC_first",
//   fileFormat: 'CSV'
// });

// var batch = require('users/fitoprincipe/geetools:batch');
// (batch.Download.ImageCollection.toDrive(Grant_2018_IC_first, 
//                                         'LLL', 
//                                         {scale:10}));

// batch.Download.ImageCollection.toDrive(Grant_2018_IC_first, 
//                                       'LLL', 
//                                       {scale: 10 ,
//                                         region: gg// or geometry.getInfo()
//                                     });

//#############################################################

// Export.table.toDrive({collection: Grant_2018_IC_first, 
//                       fileFormat: 'GeoJSON',
//                       description:'Grant_2018_IC_first',
//                       folder:"Grant_2018_IC_first",
//                       });

//   #############################################################

// ## We can NOT use the following, since we have imageCollection. 
// ## Not an image
// Export.image.toDrive({
//   image: Grant_2018_IC_first,
//   description: 'Grant_2018_IC_first',
//   maxPixels: 1e9,
//   scale:10,
//   folder:"Grant_2018_IC_first"
// });