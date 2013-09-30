--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
import           Data.Monoid     ( (<>)         )
import           Hakyll
import           System.FilePath ( takeBaseName )
--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match (    "assets/ico/*"
          .||. "assets/img/*" ) $ do
        route   idRoute
        compile copyFileCompiler

    match "assets/css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "files/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "static/*.md" $ do
        route $ gsubRoute "static/" (const "") `composeRoutes`
                  setExtension "html"
        compile $ do
            path <- getResourceFilePath
            let baseName = takeBaseName path
                staticCtx = constField (baseName ++ "Active") "true" <>
                            defaultContext
            pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" staticCtx
                >>= relativizeUrls

    match "about.md" $ do
        route   $ setExtension "html"
        let aboutCtx =
                constField "aboutActive" "true"  <>
                constField "title"       "About" <>
                defaultContext
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" aboutCtx
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/post.html"    postCtx
                >>= loadAndApplyTemplate "templates/signature.html"    postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

    create ["index.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let postsCtx =
                    listField "posts"
                              (teaserField "teaser" "content" <> postCtx)
                              (return posts)                              <>
                    constField "postsActive" "true"                       <>
                    constField "title"       "All posts"                  <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" postsCtx
                >>= loadAndApplyTemplate "templates/default.html"   postsCtx
                >>= relativizeUrls

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx <> bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/*" "content"
            renderAtom feedConfiguration feedCtx posts

    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx <> bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/*" "content"
            renderRss feedConfiguration feedCtx posts

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Mog's Bλog"
    , feedDescription = "The place where I share whatever I want to share."
    , feedAuthorName  = "Hugo 'Mog' Mougard"
    , feedAuthorEmail = "hugo.mougard@gmail.com"
    , feedRoot        = "http://blog.creedy.eu"
    }
