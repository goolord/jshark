module Life
  ( page
  , framePage
  , frameSrcFor
  , assetBaseFor
  , sourceSrcFor
  , mainJS
  , initialPop
  , initialCatalogCells
  , soupSeedPop
  , catalogJs
  , engineWorkerJs
  , lifeLutWorkerBootJs
  , canonicalShapeHash
  , shapeHash
  )
where

import Catalog (canonicalShapeHash, catalogJs, shapeHash)
import Client (mainJS)
import EngineWorker (engineWorkerJs)
import LutBoot (lifeLutWorkerBootJs)
import Page (assetBaseFor, framePage, frameSrcFor, page, sourceSrcFor)
import Patterns (initialCatalogCells, initialPop, soupSeedPop)
