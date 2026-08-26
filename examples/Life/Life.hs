module Life
  ( page
  , framePage
  , frameSrcFor
  , assetBaseFor
  , mainJS
  , initialPop
  , initialCatalogCells
  , soupSeedPop
  , catalogJs
  , canonicalShapeHash
  , shapeHash
  )
where

import Catalog (canonicalShapeHash, catalogJs, shapeHash)
import Client (mainJS)
import Page (assetBaseFor, framePage, frameSrcFor, page)
import Patterns (initialCatalogCells, initialPop, soupSeedPop)
