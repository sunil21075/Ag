////////////////////////////////////////////////////////////////////////
/////
/////     We can read a shapefile with many polygons, and using map()
/////     read all images corresponding to a polygon. However, the result of
/////     that is a featureCollection for which we do not know how to
/////     access to the info. inside. So, we want to do a for-loop for each polygon 
/////     now.
/////

var grant_2018_regions = ee.FeatureCollection(grant_2018_SF);
// print(grant_2018_regions);

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
    var start_dt = '2018-01-01'
    var end_dt = '2018-12-31'
    // var cloud_percentage = 
    // var geom = ee.Feature(feature_col).geometry();
    var geom = a_feature.geometry();
    var newDict = {'original_polygon_1': geom};
    var imageC = ee.ImageCollection('COPERNICUS/S2')
                .filterDate(start_dt, end_dt)
                .filterBounds(geom)
                //.filterMetadata('CLOUDY_PIXEL_PERCENTAGE', "less_than", 10)
                .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', 10))
                .sort('system:time_start', true);
    
    // toss out cloudy pixels
    imageC = imageC.map(maskS2clouds);
    
    // pick up some bands
    imageC = imageC.select(['B8', 'B4', 'B3', 'B2']);
    
    // add DoY as a band
    imageC = addDate_to_collection(imageC);
    
    // add NDVI as a band
    imageC = add_NDVI_collection(imageC);
    
    // add original geometry to each image
    // we do not need to do this really:
    imageC = imageC.map(function(im){return(im.set(newDict))});
    
    // add original geometry and WSDA data as a feature to the collection
    imageC = imageC.set({ 'original_polygon': geom,
                          'WSDA':a_feature
                        });
    // imageC = imageC.sort('system:time_start', true);

    //imageC = imageC.map(add_NDVI_collection)
  return imageC;
}

function reduce_IC_mean(an_IC){
  var reduction_geometry = ee.Feature(ee.Geometry(an_IC.get('original_polygon')));
  var WSDA = an_IC.get('WSDA');
  
  var reduced = an_IC.map(function(image){
                            return image.reduceRegions({
                                                          collection:reduction_geometry,
                                                          reducer:ee.Reducer.mean(), 
                                                          scale: 10
                                                      });
                                          }
                          ).flatten();
                          
  reduced = reduced.set({ 'original_polygon': reduction_geometry,
                          'WSDA':WSDA
                        });
  print("ssss");
  WSDA = ee.Feature(WSDA);
  WSDA = WSDA.toDictionary();
  print(WSDA);
  // var newDict = {'WSDA':WSDA};
  reduced = reduced.map(function(im){return(im.set(WSDA))}); 
  return(reduced);
}

///
///                         functions definitions end
///
////////////////////////////////////////////////////////////////////////////////////////
var Grant_2018_IC = grant_2018_regions.map(extract_sentinel_IC);

// var Grant_2018_IC_reduced_mean = reduce_collection_of_collections_mean(Grant_2018_IC);
// print(Grant_2018_IC_reduced_mean);

var Grant_2018_IC_first = ee.ImageCollection(Grant_2018_IC.first());
print("1");
print(Grant_2018_IC_first);

var an_imggg = Grant_2018_IC_first.first();
print("2");
print(an_imggg);

var aa = reduce_IC_mean(Grant_2018_IC_first);
print("3");
print (aa);

Export.table.toDrive({
  collection: aa,
  description:'flatten_finallyyy',
  folder:"Grant_2018_IC_first_IC",
  fileFormat: 'SHP'
});

Export.table.toDrive({
  collection: aa,
  description:'flatten_finallyyy',
  folder:"Grant_2018_IC_first_IC",
  fileFormat: 'CSV'
});


// var Grant_2018_IC_first_geom = Grant_2018_IC_first.get('original_polygon');

// var an_im = Grant_2018_IC_first.first();
// var red = an_im.reduceRegion({reducer: ee.Reducer.mean(),
//                   geometry: Grant_2018_IC_first_geom,
//                   scale: 10
//                   });
// print (red);

// var reduced = Grant_2018_IC_first.map(function(image){
//   return image.reduceRegions({
//           collection:ee.Feature(ee.Geometry(Grant_2018_IC_first_geom)),
//           reducer:ee.Reducer.mean(), 
//           scale: 10
//   });
// });
// print(reduced.flatten())


// var aaa = Grant_2018_IC_first.map(function(ima){
//                                     var vv = ima.reduceRegion({reducer: ee.Reducer.mean(),
//                                                       geometry: Grant_2018_IC_first_geom,
//                                                       scale: 10
//                                                       });
//                                     vv = ee.Image(vv)     
//                                       return (vv)
//                                                 }
//                                   );
// print (aaa);

// red = red.set("a", Grant_2018_IC_first_geom);
// print (red);

// red = red.set('original_polygon', Grant_2018_IC_first.get('original_polygon'));
// print(red);

// var IC_reduced = Grant_2018_IC_first.map(function(im1){
//                           var reduced_image = im1.reduceRegion({
//                                                           reducer: ee.Reducer.mean(),
//                                                           geometry: Grant_2018_IC_first_geom,
//                                                           scale: 10
//                                                                   });
//                           reduced_image = ee.feature(reduced_image)
//                           return reduced_image;
    
//                                         }
//                           );
// print(IC_reduced)

// print(typeof(Grant_2018_IC_first));

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

// print(first_NDVI_TS);

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

