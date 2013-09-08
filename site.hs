--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "assets/ico/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "assets/img/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "files/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "assets/css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "feed.md" $ do
        route   $ setExtension "html"
        let feedCtx =
                constField "feedActive" "true"  `mappend`
                constField "title"      "Feed" `mappend`
                defaultContext
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" feedCtx
            >>= relativizeUrls

    match "about.md" $ do
        route   $ setExtension "html"
        let aboutCtx =
                constField "aboutActive" "true"  `mappend`
                constField "title"       "About" `mappend`
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
                    listField "posts" postCtx (return posts) `mappend`
                    constField "postsActive" "true"       `mappend`
                    constField "title"          "All posts"  `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" postsCtx
                >>= loadAndApplyTemplate "templates/default.html"   postsCtx
                >>= relativizeUrls

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/*" "content"
            renderAtom feedConfiguration feedCtx posts

    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/*" "content"
            renderRss feedConfiguration feedCtx posts

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Mog's Bλog"
    , feedDescription = "The place where I share whatever I want to share."
    , feedAuthorName  = "Hugo 'Mog' Mougard"
    , feedAuthorEmail = "hugo.mougard@gmail.com"
    , feedRoot        = "http://www.creedy.eu"
    }
