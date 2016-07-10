{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Monad        (liftM)

import           Data.Monoid          (mappend)
import           Data.List            (sortBy)
import           Data.Ord             (comparing)
import qualified Data.Set as S

import           Hakyll
import           Hakyll.Core.Metadata (MonadMetadata)
import           Text.Pandoc.Options

import           Safe

import           System.FilePath      (splitDirectories, takeBaseName)

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


    match "tutorials/posts/haskell/attoparsec/*" $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/post.html"    numberedCtx
            >>= loadAndApplyTemplate "templates/default.html" numberedCtx
            >>= relativizeUrls

    match "notes/**" $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/note.html"    defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
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
            attos <- orderFirst =<< loadAll "tutorials/posts/haskell/attoparsec/*"

            let tutorialsCtx =
                    listField "attos" defaultContext (return attos) `mappend`
                    constField "title" "Tutorials"           `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tutorials.html" tutorialsCtx
                >>= loadAndApplyTemplate "templates/default.html" tutorialsCtx
                >>= relativizeUrls

    create ["notes.html"] $ do
      route idRoute
      compile $ do
        taplNotes <- loadAll "notes/books/TAPL/*"

        let notesCtx =
              listField "taplNotes" defaultContext (return taplNotes) `mappend`
              constField "title" "Notes" `mappend`
              defaultContext

        makeItem ""
          >>= loadAndApplyTemplate "templates/notes.html" notesCtx
          >>= loadAndApplyTemplate "templates/default.html" notesCtx
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

numberedCtx :: Context String
numberedCtx = orderField "order" `mappend` defaultContext

orderField :: String    -- ^ key in which to the order number should appear
           -> Context a -- ^ Resulting context
orderField key = field key $ \i -> getOrderField $ itemIdentifier i


getOrderField :: MonadMetadata m => Identifier -> m String
getOrderField id' = maybe empty' (return . show) mOrderInt
  where
    paths    = splitDirectories $ toFilePath id'
    mTitle   = headMay . reverse $ paths
    mmOrder  = headMay . take 1 . splitAll "-" <$> mTitle
    mOrderInt =
      case mmOrder of
        Nothing -> Nothing
        Just mOrder ->
         case mOrder of
           Nothing -> Nothing
           Just o  -> readMay o :: Maybe Int

    empty' = fail $ "Could not parse order for " ++ show id'

orderFirst :: MonadMetadata m => [Item a] -> m [Item a]
orderFirst =
    sortByM $ getOrderField . itemIdentifier
  where
    sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
    sortByM f xs = (map fst . sortBy (comparing snd)) <$>
                   mapM (\x -> fmap (x,) (f x)) xs


--------------------------------------------------------------------------------
-- | The reverse of 'chronological'
orderLast :: MonadMetadata m => [Item a] -> m [Item a]
orderLast = fmap reverse . chronological


pandocMathCompiler = pandocCompilerWith defaultHakyllReaderOptions writerOptions
  where
    mathExtensions = [Ext_tex_math_dollars, Ext_tex_math_double_backslash,
                      Ext_latex_macros]
    defaultExtensions = writerExtensions defaultHakyllWriterOptions
    newExtensions = foldr S.insert defaultExtensions mathExtensions
    writerOptions = defaultHakyllWriterOptions {
                      writerExtensions = newExtensions
                    , writerHTMLMathMethod = MathJax ""
                    }

{-
stack clean
stack build
stack exec site rebuild
stack exec site watch
-}
