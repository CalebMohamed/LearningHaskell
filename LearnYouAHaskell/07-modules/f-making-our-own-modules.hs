-- first of all you need a .hs file

-- 'modules export functions' meaning that we can define internal helper functions to be referenced in the exported functions
-- only the exported functions can be references in other files

-- at the top of the module you specify the name of the module and its exported functions followed by a where:

module Geometry
( sphereVolume
, sphereArea
, cubeVolume
, cubeArea
, cuboidArea
, cuboidVolume
) where

sphereVolume :: Float -> Float
sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)

sphereArea :: Float -> Float
sphereArea radius = 4 * pi * (radius ^2)

cubeVolume :: Float -> Float  
cubeVolume side = cuboidVolume side side side  
  
cubeArea :: Float -> Float  
cubeArea side = cuboidArea side side side  
  
cuboidVolume :: Float -> Float -> Float -> Float  
cuboidVolume a b c = rectangleArea a b * c  
  
cuboidArea :: Float -> Float -> Float -> Float  
cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2  
  
rectangleArea :: Float -> Float -> Float  
rectangleArea a b = a * b

-- to import a custom module, the src has to be in the same folder as the program its being referenced in
-- then you use an import {modulename} matching the defined module name

-- modules can have heirarchical structure - consider the Data.Map and Data.List modules
-- to make a custom version of this, make a folder with the module name
-- then make scripts with the {submoduleName}.hs
-- then in the scripts define with:
-- module {moduleName}.{submoduleName}
-- (specific-func-1
-- , specific-func-2
-- ) where