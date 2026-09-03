module JShark.Example.Life
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

import JShark.Example.Life.Catalog (canonicalShapeHash, catalogJs, shapeHash)
import JShark.Example.Life.Client (mainJS)
import JShark.Example.Life.EngineWorker (engineWorkerJs)
import JShark.Example.Life.LutBoot (lifeLutWorkerBootJs)
import JShark.Example.Life.Page
  ( assetBaseFor
  , framePage
  , frameSrcFor
  , page
  , sourceSrcFor
  )
import JShark.Example.Life.Patterns
  ( initialCatalogCells
  , initialPop
  , soupSeedPop
  )
