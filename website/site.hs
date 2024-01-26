--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

import PandocToc

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "code/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Enable automatic TOC for syllabus
    match "syllabus.md" $ do
        route   $ setExtension "html"
        compile $ pandocCompilerWith defaultHakyllReaderOptions (withTOC defaultHakyllWriterOptions)
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "about.rst" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "schedule.md" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "style.md" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "resources.md" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "index.md" $ do
      route $ setExtension "html"
      compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "assignments/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
--            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "assignments.md" $ do
      route $ setExtension "html"
      compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
            

--    match "posts/*" $ do
--        route $ setExtension "html"
--        compile $ pandocCompiler
--            >>= loadAndApplyTemplate "templates/post.html"    postCtx
--            >>= loadAndApplyTemplate "templates/default.html" postCtx
--            >>= relativizeUrls
-- 
--    create ["archive.html"] $ do
--        route idRoute
--        compile $ do
--            posts <- recentFirst =<< loadAll "posts/*"
--            let archiveCtx =
--                    listField "posts" postCtx (return posts) `mappend`
--                    constField "title" "Archives"            `mappend`
--                    defaultContext
-- 
--            makeItem ""
--                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
--                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
--                >>= relativizeUrls
-- 
--    create ["assignments.html"] $ do
--        route idRoute
--        compile $ do
--            asgns <- recentFirst =<< loadAll "assignments/*"
--            let archiveCtx =
--                    listField "assignments" defaultContext (return asgns) `mappend`
--                    constField "title" "Assignments"            `mappend`
--                    defaultContext
-- 
--            makeItem ""
--                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
--                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
--                >>= relativizeUrls


--    match "index.html" $ do
--        route idRoute
--        compile $ do
--            posts <- recentFirst =<< loadAll "posts/*"
--            let indexCtx =
--                    listField "posts" postCtx (return posts) `mappend`
--                    defaultContext
-- 
--            getResourceBody
--                >>= applyAsTemplate indexCtx
--                >>= loadAndApplyTemplate "templates/default.html" indexCtx
--                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

