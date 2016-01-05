module Main where

--------------------------------------------------------------------------------
-- GHC Stuff                                                                  --
--------------------------------------------------------------------------------

{-#    LANGUAGE          OverloadedStrings #-}
{-#    OPTIONS_GHC       -Wall             #-}

--------------------------------------------------------------------------------
-- Imports                                                                    --
--------------------------------------------------------------------------------

import Data.List.Split ( chunksOf            )
import Data.Monoid     ( (<>)                )
import Hakyll
import System.FilePath ( takeBaseName        )

--------------------------------------------------------------------------------
-- Hakyll build rules                                                         --
--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do
    match (    fromGlob "assets/ico/*"
          .||. fromGlob "assets/img/*"
          .||. fromGlob "posts/*/**" ) $ do
        route   idRoute
        compile copyFileCompiler

    match (fromGlob "assets/css/*") $ do
        route   idRoute
        compile compressCssCompiler

    match (fromGlob "resources/**") $ do
        route $ gsubRoute "resources/" (const "")
        compile copyFileCompiler

    match (fromGlob "static/*.md") $ do
        route $ gsubRoute "static/" (const "") `composeRoutes`
                  setExtension "html"
        compile $ do
            path <- getResourceFilePath
            let baseName  = takeBaseName path
                staticCtx = constField (baseName ++ "Active") "true" <>
                            defaultContext
            pandocCompiler
                >>= loadAndApplyTemplate (fromFilePath "templates/structure.html")
                                         staticCtx
                >>= relativizeUrls

    match (fromGlob "posts/*") $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate (fromFilePath "templates/post.html")
                                     postCtx
            >>= loadAndApplyTemplate (fromFilePath "templates/signature.html")
                                     postCtx
            >>= loadAndApplyTemplate (fromFilePath "templates/structure.html")
                                     postCtx
            >>= relativizeUrls
    
    postsNumber <- ((`div` postsPerPage) . length)
      <$> getMatches (fromGlob "posts/*")
    let posts  = (recentFirst =<< loadAll (fromGlob "posts/*")
                  :: Compiler [Item String])
        chunks = chunksOf postsPerPage <$> posts
        indeces = map (fromFilePath . (++ ".html") . ("index" ++))
                      ("" : map show [2 .. postsNumber])
    
    create indeces $ do
        route idRoute
        compile $ indexCompiler chunks
    
    create [fromFilePath "atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx <> bodyField "description"
            items <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots (fromGlob "posts/*") "content"
            renderAtom feedConfiguration feedCtx items
  
    create [fromFilePath "rss.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx <> bodyField "description"
            items <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots (fromGlob "posts/*") "content"
            renderRss feedConfiguration feedCtx items

    match (fromGlob "templates/*") $ compile templateCompiler

--------------------------------------------------------------------------------
-- Hakyll dynamic helper stuff                                                --
--------------------------------------------------------------------------------

indexCompiler :: Compiler [[Item String]] -> Compiler (Item String)
indexCompiler chunks = do
    index <- extractIndex <$> getUnderlying
    size <- length <$> chunks
    let postsCtx = listField "posts"
                             (teaserField "teaser" "content" <> postCtx)
                             ((!! index) <$> chunks)                     <>
                   constField "postsActive" "true"                       <>
                   constField "title"       "All posts"                  <>
                   defaultContext
    makeItem ""
        >>= loadAndApplyTemplate (fromFilePath "templates/posts.html")
                                 postsCtx
        >>= loadAndApplyTemplate (fromFilePath "templates/nav.html")
                                 (postsCtx <> navCtx index size)
        >>= loadAndApplyTemplate (fromFilePath "templates/structure.html")
                                 postsCtx
        >>= relativizeUrls

extractIndex :: Identifier -> Int
extractIndex i = if null s then 0 else (read s) - 1
  where s = takeWhile (/= '.') . drop 5 . toFilePath $ i

navCtx :: Int -> Int -> Context String
navCtx index size = (if index == 0
                     then missingField
                     else constField "next" (indexString (index - 1)))
                    <> (if index == size - 1
                       then missingField
                       else constField "prev" (indexString (index + 1)))
                    <> defaultContext
  where indexString i = if i == 0 then "" else show (i + 1)

--------------------------------------------------------------------------------
-- Hakyll static helper stuff                                                 --
--------------------------------------------------------------------------------

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext

postsPerPage :: Int
postsPerPage = 10

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "m09's blog"
    , feedDescription = "The place where I share whatever I want to share."
    , feedAuthorName  = "Hugo “m09” Mougard"
    , feedAuthorEmail = "mog@crydee.eu"
    , feedRoot        = "http://blog.crydee.eu"
    }
