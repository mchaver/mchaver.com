--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Default (def)
import           Hakyll

import           Text.Blaze.Html                 (toHtml, toValue, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Control.Monad (forM, filterM)
import Data.Char (toLower)
import Data.Function (on)
import Data.List (intersect, sort, sortBy)
import Data.Maybe (catMaybes)
import Data.Ord (Down (..), comparing)
import qualified Data.Text as T

import Text.Pandoc.Options (WriterOptions (..))
import Text.Pandoc.Templates (compileTemplate)

postCtxWithTags :: Tags -> Context String
-- postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx
postCtxWithTags tags = tt "tags" tags `mappend` postCtx

tt = tagsFieldWith getTags simpleTagRenderLink (mconcat)

simpleTagRenderLink :: String -> (Maybe FilePath) -> Maybe H.Html
simpleTagRenderLink _   Nothing         = Nothing
simpleTagRenderLink tag (Just filePath) =
  Just $ H.a ! A.class_ "tag" ! A.href (toValue $ toUrl filePath) $ toHtml tag

tagsFieldWith' :: (Identifier -> Compiler [String])
              -- ^ Get the tags
              -> (String -> (Maybe FilePath) -> Maybe H.Html)
              -- ^ Render link for one tag
              -> ([H.Html] -> H.Html)
              -- ^ Concatenate tag links
              -> String
              -- ^ Destination field
              -> Tags
              -- ^ Tags structure
              -> Context a
              -- ^ Resulting context
tagsFieldWith' getTags' renderLink cat key tags = field key $ \item -> do
    tags' <- getTags' $ itemIdentifier item
    links <- forM tags' $ \tag -> do
        route' <- getRoute $ tagsMakeId tags tag
        return $ renderLink tag route'

    return $ renderHtml $ cat $ catMaybes $ links

--------------------------------------------------------------------------------
-- Related posts: other posts ranked by how many tags they share with this one,
-- most overlap first, ties broken by recency. Used for the "Related posts"
-- block at the foot of each post.
relatedIdentsOf :: Item a -> Compiler [Identifier]
relatedIdentsOf item = do
    let ident = itemIdentifier item
    myTags <- getTags ident
    others <- filter (/= ident) <$> getMatches "posts/*"
    scored <- forM others $ \i -> do
        ts <- getTags i
        pure (length (myTags `intersect` ts), i)
    pure [ i
         | (n, i) <- sortBy (comparing (Down . fst) <> comparing (Down . snd)) scored
         , n > 0 ]

relatedField :: Context String
relatedField =
    listFieldWith "related" linkedPostCtx
        (\item -> take 3 <$> relatedIdentsOf item >>= mapM makeItem)
    `mappend`
    boolFieldM "hasRelated" (fmap (not . null) . relatedIdentsOf)

-- A minimal context for a post referenced only by its 'Identifier' (title + url).
linkedPostCtx :: Context Identifier
linkedPostCtx =
    field "url"   (\i -> maybe "#" toUrl <$> getRoute (itemBody i)) `mappend`
    field "title" (\i -> getMetadataField' (itemBody i) "title")

--------------------------------------------------------------------------------
-- Series: every post sharing this post's @series:@ metadata, in chronological
-- order (filenames are date-prefixed, so a plain sort is chronological). The
-- current post is flagged so the template can render it un-linked.
seriesField :: Context String
seriesField = listFieldWith "seriesPosts" seriesItemCtx $ \item -> do
    let ident = itemIdentifier item
    mSeries <- getMetadataField ident "series"
    case mSeries of
        Nothing -> pure []
        Just s  -> do
            ids  <- getMatches "posts/*"
            same <- filterM (\i -> (== Just s) <$> getMetadataField i "series") ids
            mapM (\i -> makeItem (i, i == ident)) (sort same)

seriesItemCtx :: Context (Identifier, Bool)
seriesItemCtx =
    field "url"     (\i -> maybe "#" toUrl <$> getRoute (fst (itemBody i))) `mappend`
    field "title"   (\i -> getMetadataField' (fst (itemBody i)) "title")   `mappend`
    boolField "current" (snd . itemBody)

--------------------------------------------------------------------------------
-- Topics page: one section per tag, each listing its posts (newest first).
topicCtx :: Tags -> Context (String, [Identifier])
topicCtx tags =
    field "topic"    (pure . fst . itemBody)                                  `mappend`
    field "count"    (pure . show . length . snd . itemBody)                  `mappend`
    field "topicUrl" (\i -> maybe "#" toUrl <$> getRoute (tagsMakeId tags (fst (itemBody i)))) `mappend`
    listFieldWith "posts" linkedPostCtx
        (\i -> mapM makeItem (sortBy (comparing Down) (snd (itemBody i))))

--------------------------------------------------------------------------------
-- | Pandoc writer template: prepend an auto-generated table of contents (only
-- when the document actually has headings) to the rendered body.
tocTemplateSource :: T.Text
tocTemplateSource = T.unlines
    [ "$if(toc)$"
    , "<nav class=\"toc\"><div class=\"toc-title\">Contents</div>$table-of-contents$</nav>"
    , "$endif$"
    , "$body$"
    ]

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    -- Pandoc writer template that prepends an auto-generated table of contents
    -- (when the document has headings) to the rendered body.
    tocTemplate <- preprocess (either error id <$> compileTemplate "" tocTemplateSource)

    let tocWriterOptions = def
            { writerTableOfContents = True
            , writerTOCDepth        = 3
            , writerTemplate        = Just tocTemplate
            }

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "projects.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
    
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")
    
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title
                      `mappend` listField "posts" postCtx (return posts)
                      `mappend` defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
    
    create ["topics.html"] $ do
        route idRoute
        compile $ do
            let topics = sortBy (comparing (map toLower . fst)) (tagsMap tags)
                topicsCtx =
                    listField "topics" (topicCtx tags) (mapM makeItem topics) `mappend`
                    constField "title" "Topics"                               `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/topics.html"  topicsCtx
                >>= loadAndApplyTemplate "templates/default.html" topicsCtx
                >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        let postCtx' = postCtxWithTags tags `mappend` relatedField `mappend` seriesField
        compile $ do
            -- Clean article body for the RSS feed: no table of contents and no
            -- page chrome (series nav, related posts, date, tags).
            _ <- pandocCompilerWith defaultHakyllReaderOptions defaultHakyllWriterOptions
                >>= saveSnapshot "content"
            -- Full page: body carries a table of contents, then the post chrome.
            pandocCompilerWith defaultHakyllReaderOptions tocWriterOptions
                >>= loadAndApplyTemplate "templates/post.html"    postCtx'
                >>= loadAndApplyTemplate "templates/default.html" postCtx'
                >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" (postCtxWithTags tags) (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" (postCtxWithTags tags) (return posts) `mappend`
                    constField "title" ""                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "content"
            let feedCtx = postCtx `mappend` bodyField "description"
            renderRss feedConfig feedCtx posts

    match "templates/*" $ compile templateBodyCompiler

    -- loads the react project directly without changing anything
    match "projects/**" $ do
      route idRoute
      compile $ getResourceLBS


--------------------------------------------------------------------------------
feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
    { feedTitle       = "mchaver.com"
    , feedDescription = "Posts from mchaver.com"
    , feedAuthorName  = "James M.C. Haver II"
    , feedAuthorEmail = "mchaver@gmail.com"
    , feedRoot        = "https://mchaver.com"
    }

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%0Y-%m-%d" `mappend`
    defaultContext
