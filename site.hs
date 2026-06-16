--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Default (def)
import           Hakyll

import           Text.Blaze.Html                 (toHtml, toValue, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Control.Monad (forM, filterM)
import Data.Aeson (Value, encode, object, (.=))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum, isAscii, toUpper)
import Data.Function (on)
import Data.List (intersect, sort, sortBy)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (Down (..), comparing)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Numeric (showHex)
import System.FilePath (takeFileName)

import Text.Pandoc.Options (WriterOptions (..))
import Text.Pandoc.Templates (compileTemplate)

postCtxWithTags :: Tags -> Context String
-- postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx
postCtxWithTags tags = tt "tags" tags `mappend` postCtx

tt = tagsFieldWith getTags simpleTagRenderLink (mconcat)

-- Tags link into the archive with their topic filter pre-applied (the archive's
-- JS reads ?topic=…). The route argument is ignored: there are no per-tag pages.
simpleTagRenderLink :: String -> (Maybe FilePath) -> Maybe H.Html
simpleTagRenderLink tag _ =
  Just $ H.a ! A.class_ "tag" ! A.href (toValue (archiveTopicUrl tag)) $ toHtml tag

-- A link to the archive filtered to one topic, e.g. "/archive.html?topic=monad%20transformer".
archiveTopicUrl :: String -> String
archiveTopicUrl tag = "/archive.html?topic=" ++ urlEncodeQuery tag

-- Percent-encode a string for use as a URL query value (UTF-8 bytes; unreserved
-- chars pass through). The archive's URLSearchParams reads it back decoded.
urlEncodeQuery :: String -> String
urlEncodeQuery = concatMap enc . B.unpack . TE.encodeUtf8 . T.pack
  where
    enc w
      | unreserved c = [c]
      | otherwise    = '%' : pad (map toUpper (showHex w ""))
      where c = toEnum (fromIntegral w)
    unreserved c = (isAscii c && isAlphaNum c) || c `elem` ("-_.~" :: String)
    pad [d] = ['0', d]
    pad ds  = ds

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
-- Faceted homepage helpers.

-- Keep only the loaded posts whose `kind` metadata matches.
filterByKind :: String -> [Item String] -> Compiler [Item String]
filterByKind k = filterM (\i -> (== Just k) <$> getMetadataField (itemIdentifier i) "kind")

-- Posts carrying an `updated` field, most-recently-updated first.
recentlyUpdated :: [Item String] -> Compiler [Item String]
recentlyUpdated posts = do
    keyed <- forM posts $ \i -> do
        mu <- getMetadataField (itemIdentifier i) "updated"
        pure (mu, i)
    pure [ i | (Just _, i) <- sortBy (comparing (Down . fst)) keyed ]

--------------------------------------------------------------------------------
-- One entry in the client-side search index (search.json).
searchDoc :: Item String -> Compiler Value
searchDoc post = do
    let ident = itemIdentifier post
        date  = take 10 (takeFileName (toFilePath ident))
    route' <- getRoute ident
    title  <- fromMaybe "" <$> getMetadataField ident "title"
    kind   <- fromMaybe "" <$> getMetadataField ident "kind"
    state  <- fromMaybe "complete" <$> getMetadataField ident "state"
    tags'  <- getTags ident
    let url  = maybe "#" toUrl route'
        body = take 5000 (stripTags (itemBody post))
    pure $ object
        [ "title" .= title
        , "url"   .= url
        , "kind"  .= kind
        , "state" .= state
        , "date"  .= date
        , "tags"  .= tags'
        , "text"  .= body
        ]

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

    -- Treat each post's `kind` as a one-element tag set so we get per-kind index
    -- pages (kinds/tutorial.html, ...) that the content-type badges link to.
    kinds <- buildTagsWith
        (\ident -> maybe [] (: []) <$> getMetadataField ident "kind")
        "posts/*"
        (fromCapture "kinds/*.html")

    tagsRules kinds $ \kind pattern -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" ("Posts: " ++ kind)
                      `mappend` listField "posts" (postCtxWithTags tags) (return posts)
                      `mappend` defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html"     ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    -- No per-tag pages: tag links route to /archive.html?topic=… (see
    -- simpleTagRenderLink). The `tags` structure is still used to render the
    -- tag pills and to compute related posts.

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

    -- Archive is rendered client-side from search.json (search + topic/kind
    -- filters + date sort), so it just needs the page chrome here.
    create ["archive.html"] $ do
        route idRoute
        compile $ do
            let ctx = constField "title" "Archive" `mappend` defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            allPosts <- recentFirst =<< loadAll "posts/*"
            tut      <- take 5 <$> filterByKind "tutorial"  allPosts
            ref      <- take 5 <$> filterByKind "reference" allPosts
            notes    <- take 5 <$> filterByKind "note"      allPosts
            updated  <- take 5 <$> recentlyUpdated allPosts
            let pc = postCtxWithTags tags
                indexCtx =
                    listField "recent"     pc (return (take 6 allPosts))   `mappend`
                    listField "tutorials"  pc (return tut)                 `mappend`
                    listField "references" pc (return ref)                 `mappend`
                    listField "notes"      pc (return notes)               `mappend`
                    listField "updated"    pc (return updated)             `mappend`
                    boolField "hasUpdated" (const (not (null updated)))    `mappend`
                    constField "title" ""                                  `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    create ["search.json"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
            docs  <- mapM searchDoc posts
            makeItem (BL.toStrict (encode docs))

    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "content"
            let feedCtx = postCtx `mappend` bodyField "description"
            renderRss feedConfig feedCtx posts

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "content"
            let feedCtx = postCtx `mappend` bodyField "description"
            renderAtom feedConfig feedCtx posts

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
