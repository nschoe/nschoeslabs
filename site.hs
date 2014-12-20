--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend, (<>))
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.md", "contact.markdown", "haskell.md", "ai.md", "webrtc.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "articles/*" $ do
            route $ setExtension "html"
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/article.html" articleCtx
                >>= loadAndApplyTemplate "templates/default.html" articleCtx
                >>= relativizeUrls

    match "cv.pdf" $ do
            route idRoute
            compile copyFileCompiler

    create ["articles.html"] $ do
        route idRoute
        compile $ do
            articles <- recentFirst =<< loadAll "articles/*"
            let articlesCtx =
                    listField "articles" articleCtx (return articles) <>
                    constField "title" "Articles"            <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/articles.html" articlesCtx
                >>= loadAndApplyTemplate "templates/default.html" articlesCtx
                >>= relativizeUrls

    match "messages/*" $ do
        route $ setExtension "html"
        compile $ getResourceBody
                >>= relativizeUrls

    -- create ["archive.html"] $ do
    --     route idRoute
    --     compile $ do
    --         articles <- recentFirst =<< loadAll "articles/*"
    --         let archiveCtx =
    --                 listField "articles" articlesCtx (return articles) `mappend`
    --                 constField "title" "Archives"            `mappend`
    --                 defaultContext

    --         makeItem ""
    --             >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
    --             >>= loadAndApplyTemplate "templates/default.html" archiveCtx
    --             >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            lastTwoArticles <- return . take 2 =<< recentFirst =<< loadAll "articles/*"
            lastTwoMessages <- return . take 2 =<< recentFirst =<< loadAll "messages/*"
            let indexCtx =
                    listField "lastTwoArticles" articleCtx (return lastTwoArticles) <>
                    listField "lastTwoMessages" articleCtx (return lastTwoMessages) <>
                    defaultContext
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
articleCtx :: Context String
articleCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

-- lastTwoArticlesCtx :: Context String
-- lastTwoArticlesCtx =
--     dateField "date" "%b %e, %Y" `mappend`
--     defaultContext

-- lastMessagesCtx :: Context String
-- lastMessagesCtx =
--     dateField "date" "%b %e, %Y" `mappend`
--     defaultContext