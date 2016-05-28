--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
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

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls
    
    create ["tutorials.html"] $ do
        route idRoute
        compile $ do
            -- tutorialCategories <- recentFirst =<< loadAll "tutorials/*"
            -- tutorialCategories <- recentFirst =<<  loadAll "tutorials/*"
            let tutorialCategories = mapM makeItem ["Attoparsec", "Good Stuff"]
            -- print tutorialCategories
            -- listField "tutorialCategories" postCtx (return tutorialCategories) `mappend`
            -- constField "title" "Tutorials"            `mappend`
            -- defaultContext
            let tutorialsCtx = listField "tutorialCategories" defaultContext tutorialCategories `mappend` 
                               constField "title" "Tutorials"                                   `mappend`
                               defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tutorials.html" tutorialsCtx
                >>= loadAndApplyTemplate "templates/default.html" tutorialsCtx
                >>= relativizeUrls    

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls


    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext


{-
-- metaKeywordContext will return a Context containing a String
metaKeywordContext :: Context String
-- can be reached using $metaKeywords$ in the templates
-- Use the current item (markdown file)
metaKeywordContext = field "metaKeywords" $ \item -> do
  -- tags contains the content of the "tags" metadata
  -- inside the item (understand the source)
  tags <- getMetadataField (itemIdentifier item) "tags"
  -- if tags is empty return an empty string
  -- in the other case return
  --   <meta name="keywords" content="$tags$">
  return $ maybe "" showMetaTags tags
    where
      showMetaTags t = "<meta name=\"keywords\" content=\""
                       ++ t ++ "\">\n"


import Control.Monad
import System.Directory
import System.FilePath
listDirectory "tutorials/" >>= fmap ("tutorials/") >>= filterM doesDirectoryExist

let dir = "tutorials/"
canonicalizePath
mapM (canonicalizePath . (dir </>))
dirs <- listDirectory dir
dirPs <- ((dir </>) &&& id)
<- filterM (doesDirectoryExist . fst) dirPs


getDirectoriesInDirectory :: FilePath -> IO [FilePath]
getDirectoriesInDirectory dir = do
  dirs <- listDirectory dir
  let dirps = ((dir </>) &&& id) <$> dirs
  r <-  filterM (doesDirectoryExist . fst) dirps
  return $ snd <$> r

getFilesInDirectory :: FilePath -> IO [FilePath]
getFilesInDirectory dir = do
  dirs <- listDirectory dir
  let dirps = ((dir </>) &&& id) <$> dirs
  r <-  filterM (doesFileExist . fst) dirps
  return $ snd <$> r


doesFileExist

canonical, relative, fileName
-}