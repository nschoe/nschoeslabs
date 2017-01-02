--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid (mappend, (<>))
import           Hakyll
import Data.Char (toLower)
import Text.Pandoc
import Text.Pandoc.Options
import qualified Data.Set as S

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "javascript/*" $ do
        route idRoute
        compile copyFileCompiler

    match "favicon.gif" $ do
        route idRoute
        compile copyFileCompiler

    match "fonts/*" $ do
        route idRoute
        compile copyFileCompiler

    match (fromList ["about.md", "haskell.md", "ai.md", "webrtc.md"]) $ do
        route   $ setExtension "html"
        compile $ myPandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "articles/*" $ do
            route $ setExtension "html"
            compile $ myPandocCompiler
                >>= loadAndApplyTemplate "templates/article.html" articleCtx
                >>= saveSnapshot "renderedArticles"
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

    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = articleCtx `mappend` bodyField "description"
            articles <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "articles/*" "renderedArticles"
            renderRss nschoeslabsRSSConfig feedCtx articles

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
articleCtx :: Context String
articleCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

--------------------------------------------------------------------------------

nschoeslabsRSSConfig :: FeedConfiguration
nschoeslabsRSSConfig = FeedConfiguration
    { feedTitle       = "nschoe's labs"
    , feedDescription = "Haskell, Artificial Intelligence and WebRTC."
    , feedAuthorName  = "Nicolas SCHOEMAEKER"
    , feedAuthorEmail = "ns.schoe@gmail.com"
    , feedRoot        = "http://www.nschoe.com"
    }

--------------------------------------------------------------------------------

config :: Configuration
config = defaultConfiguration
    {
        -- deployCommand = "rsync -avz -e ssh ./_site/ nschoe@nschoe.com:public_html/"
        -- deployCommand = "rsync -avz -e ssh ./_site/ nschoe@178.32.220.77:/www/"
        deployCommand = "rsync -avz -e ssh ./_site/ nschoe@nschoe.com:/www/"
      , previewHost = "0.0.0.0"
    }

--------------------------------------------------------------------------------
{-
    Many thanks to Alp Mestan (alpmestan.com) for this neat function.
-}
myPandocCompiler :: Compiler (Item String)
myPandocCompiler = do
    ident <- getUnderlying
    myPandocCompiler' =<< getMetadataField ident "toc"

myPandocCompiler' :: Maybe String -> Compiler (Item String)
myPandocCompiler' withToc = pandocCompilerWith defaultHakyllReaderOptions $
    case withToc of
        Just x | map toLower x `elem` ["true", "yes"] -> writerWithToc
               | otherwise                            -> writerOpts
        Nothing                                       -> writerOpts

    where mathExtensions = [Ext_tex_math_dollars, Ext_tex_math_double_backslash, Ext_latex_macros]
          defaultExtensions = writerExtensions defaultHakyllWriterOptions
          newExtensions = foldr S.insert defaultExtensions mathExtensions
          writerOpts    = defaultHakyllWriterOptions
                            { writerReferenceLinks = True
                            , writerSectionDivs = True
                            , writerExtensions = newExtensions
                            , writerHTMLMathMethod = MathJax ""
                            , writerHtml5 = True
                            }
          writerWithToc = writerOpts
                            { writerTableOfContents = True
                            , writerTemplate = "$if(toc)$<div id=\"toc\"><h3>Table of contents</h3>$toc$</div>$endif$\n$body$"
                            , writerStandalone = True
                            }
