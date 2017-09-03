--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad          (liftM)
import           Data.Monoid            (mappend)
import           Hakyll

import qualified Data.ByteString.Lazy.Char8 as C
import           Text.Jasmine

import           Data.List              (isSuffixOf)
import           System.FilePath.Posix  (takeBaseName,takeDirectory,(</>))

--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do

    ----------
    --  github
    ----------

    match "CNAME" $ do
        route   idRoute
        compile copyFileCompiler

    match "README.md" $ do
        route   idRoute
        compile copyFileCompiler

    ----------
    --  assets
    ----------

    match "favicon.ico" $ do
        route   idRoute
        compile copyFileCompiler

    match "fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "blog/include/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "js/*" $ do
        route   idRoute
        compile compressJsCompiler

    --------
    --  blog
    --------

    --  tagging

    tags <- buildTags "blog/*" (fromCapture "tags/*/index.html")

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route $ niceRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title
                      `mappend` listField "posts" postCtx (failNull posts)
                      `mappend` defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
                >>= cleanIndexUrls
        
    --  archive

    create ["blog_archive.html"] $ do
        route $ constRoute "blog/index.html"
        compile $ do
            posts <- recentFirst =<< loadAll "blog/*"
            let ctx =
                    constField "title" "Blog"              `mappend`
                    listField "posts" postCtx (failNull posts) `mappend`
                    (postCtxWithTags tags)

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
                >>= cleanIndexUrls

    --  routing

    match "blog/*" $ do
        route $ niceRoute
        compile $ do
            let ctx = postCtxWithTags tags

            pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html"    ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
                >>= cleanIndexUrls

    -- pagination

    pag <- buildPaginateWith grouper "blog/*" makeId

    paginateRules pag $ \pageNum pattern -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let paginateCtx = paginateContext pag pageNum
                ctx =
                    constField "title" ("Blog - Page " ++ (show pageNum)) `mappend`
                    listField "posts" postCtx (failNull posts) `mappend`
                    paginateCtx `mappend`
                    (postCtxWithTags tags)

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
                >>= cleanIndexUrls

    -----------------
    --  special pages
    -----------------

    create ["courses.markdown"] $ do
        route $ constRoute "courses/index.html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
            >>= cleanIndexUrls

    -----------
    -- homepage
    -----------

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "blog/*"
            let indexCtx =
                    listField "posts" postCtx (failNull (take 5 posts)) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls
                >>= cleanIndexUrls

    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------

-- contexts

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = 
    tagsField "tags" tags `mappend` 
    tagCloudField "tagCloud" 100 250 tags `mappend`
    postCtx

-- routing

niceRoute :: Routes
niceRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p
                                </> drop 11 (takeBaseName p)
                                </> "index.html"
                            where p = toFilePath ident

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls clean)
    where
        idx = "index.html"
        clean url
            | idx `isSuffixOf` url = take (length url - length idx) url
            | otherwise            = url

-- pagination

grouper :: MonadMetadata m => [Identifier] -> m [[Identifier]]
grouper ids = (liftM (paginateEvery 10) . sortRecentFirst) ids

makeId :: PageNumber -> Identifier
makeId pageNum = fromFilePath $ "blog/page/" ++ (show pageNum) ++ "/index.html"

-- other

-- https://codetalk.io/posts/2016-05-10-compiling-scss-and-js-in-hakyll.html
compressJsCompiler :: Compiler (Item String)
compressJsCompiler = do
  let minifyJS = C.unpack . minify . C.pack . itemBody
  s <- getResourceString
  return $ itemSetBody (minifyJS s) s

-- on fail, template $if()$ will fall to $else$
failNull :: [Item a] -> Compiler [Item a]
failNull items = if null items then fail "failNull" else return items