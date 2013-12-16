--------------------------------------------------------------------------------
-- GHC Stuff                                                                  --
--------------------------------------------------------------------------------

{-#    LANGUAGE          OverloadedStrings #-}
{-#    OPTIONS_GHC       -Wall             #-}

--------------------------------------------------------------------------------
-- Imports                                                                    --
--------------------------------------------------------------------------------

import Data.Functor    ( (<$>)               )
import Data.List.Split ( chunksOf            )
import Data.Monoid     ( (<>)                )
import Hakyll
import System.FilePath ( takeBaseName        )

--------------------------------------------------------------------------------
-- Hakyll build rules                                                         --
--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do
    match (    "assets/ico/*"
          .||. "assets/img/*"
          .||. "posts/*/**" ) $ do
        route   idRoute
        compile copyFileCompiler

    match "assets/css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "resources/**" $ do
        route $ gsubRoute "resources/" (const "")
        compile copyFileCompiler

    match "static/*.md" $ do
        route $ gsubRoute "static/" (const "") `composeRoutes`
                  setExtension "html"
        compile $ do
            path <- getResourceFilePath
            let baseName  = takeBaseName path
                staticCtx = constField (baseName ++ "Active") "true" <>
                            defaultContext
            pandocCompiler
                >>= loadAndApplyTemplate "templates/structure.html" staticCtx
                >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"      postCtx
            >>= loadAndApplyTemplate "templates/signature.html" postCtx
            >>= loadAndApplyTemplate "templates/structure.html" postCtx
            >>= relativizeUrls
    
    postsNumber <- ((`div` postsPerPage) . length) <$> getMatches "posts/*"
    let posts  = (recentFirst =<< loadAll "posts/*" :: Compiler [Item String])
        chunks = chunksOf postsPerPage <$> posts
        indeces = map (fromFilePath . (++ ".html") . ("index" ++))
                      ("" : map show [2 .. postsNumber])
    
    create indeces $ do
        route idRoute
        compile $ indexCompiler chunks
    
    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx <> bodyField "description"
            items <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/*" "content"
            renderAtom feedConfiguration feedCtx items
  
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx <> bodyField "description"
            items <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/*" "content"
            renderRss feedConfiguration feedCtx items

    match "templates/*" $ compile templateCompiler

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
        >>= loadAndApplyTemplate "templates/posts.html" postsCtx
        >>= loadAndApplyTemplate "templates/nav.html"
                                 (postsCtx <> navCtx index size)
        >>= loadAndApplyTemplate "templates/structure.html" postsCtx
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
    { feedTitle       = "Mog's Bλog"
    , feedDescription = "The place where I share whatever I want to share."
    , feedAuthorName  = "Hugo 'Mog' Mougard"
    , feedAuthorEmail = "mog@crydee.eu"
    , feedRoot        = "http://blog.creedy.eu"
    }
