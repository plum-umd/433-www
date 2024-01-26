{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Site.Pandoc
Description : Pandoc related functions specific for the site.
Copyright   : (c) 2019-2021 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Module providing Pandoc related functions and data types for the site.
-}

module PandocToc where


import           Data.Functor.Identity          ( runIdentity )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Text.Pandoc.Options            ( WriterOptions
                                                , writerNumberSections
                                                , writerSyntaxMap
                                                , writerTOCDepth
                                                , writerTableOfContents
                                                , writerTemplate
                                                )
import           Text.Pandoc.Templates          ( Template
                                                , compileTemplate
                                                )

-- | Adds writer options for Table of Content rendering.
withTOC :: WriterOptions -> WriterOptions
withTOC options = options { writerNumberSections  = True
                          , writerTableOfContents = True
                          , writerTOCDepth        = 2
                          , writerTemplate        = Just tocTemplate
                          }


tocTemplate :: Template Text
tocTemplate = either error id . runIdentity . compileTemplate "" $ T.unlines
  [ "<div class=\"toc\">" -- <div class=\"header\">Table of Contents</div>
  , "$toc$"
  , "</div>"
  , "$body$"
  ]
