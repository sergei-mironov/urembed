{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Main where

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Writer
import Language.JavaScript.Parser
import System.Environment
import System.Process
import System.Exit
import System.IO
import System.FilePath
import Text.Printf
import Data.Either
import Data.Generics
import Data.Char
import Data.List
import Data.Data
import Data.Typeable
import Data.Maybe

import Options.Applicative
import Paths_urembed

io :: (MonadIO m) => IO a -> m a
io = liftIO

hio :: (MonadIO m) => Handle -> String -> m ()
hio h = io . hPutStrLn h

err,out :: (MonadIO m) => String -> m ()
err = hio stderr
out = hio stdout

-- | Parse the JavaScript file, extract top-level functions, convert their
-- signatures into Ur/Web format, return them as the list of strings
parse_js :: FilePath -> IO [String]
parse_js file = do
  s <- readFile file
  em <-  runErrorT $ do
    c <- either fail return (parse s file)
    forM (findTopLevelFunctions c) $ \f@(fn:_) -> (do
      urs_line <$> mapM extractEmbeddedType f
      ) `catchError` (\(e::String) -> do
        err $ printf "ignoring function %s, reason:\n\t%s" fn e
        return [])
  case em of
    Right s -> return s
    Left e -> fail $ printf "error parsing %s. error:\n\t%s" file e
  where
    urs_line :: [(String,String)] -> String
    urs_line ((n,nt):args) = printf "val %s : %s" (n++('_':nt)) (fmtargs args nt) where
      fmtargs :: [(String,String)] -> String -> String
      fmtargs ((an,at):as) ret = printf "%s -> %s" at (fmtargs as ret)
      fmtargs ([]) ret = printf "transaction %s" ret

    extractEmbeddedType :: (Monad m) => String -> m (String,String)
    extractEmbeddedType [] = error "BUG: empty identifier"
    extractEmbeddedType name = check . span (/= '_') $ name where
      check (n,'_':t) = return (n,t)
      check _ = fail $ printf "Can't extract the type from the identifier '%s'" name

    findTopLevelFunctions top = map decls $ listify is_func top where
      is_func n@(JSFunction a b c d e f) = True
      is_func _ = False
      decls (JSFunction a b c d e f) = (identifiers b) ++ (identifiers d)
      identifiers x = map name $ listify ids x where
        ids i@(JSIdentifier s) = True
        ids _ = False
        name (JSIdentifier n) = n
      

data Args = A
  { tgtdir :: FilePath
  , urinclude :: FilePath
  , files :: [FilePath]
  }

pargs :: Parser Args
pargs = A
  <$> strOption
      (  long "output"
      <> short 'o'
      <> metavar "FILE.urp"
      <> help "Target Ur project file" )
  <*> strOption
      (  long "urinclude"
      <> short 'I'
      <> metavar "DIR"
      <> help "Custom location of the UrWeb's includes"
      <> value "/usr/local/include/urweb" )
  <*> arguments str ( metavar "FILE" <> help "File to embed" )

main :: IO ()
main = execParser opts >>= main_
  where
    opts = info (helper <*> pargs)
      (  fullDesc
      <> progDesc (unlines [
            "Converts a FILE to the Ur/Web's module. The Module will contain a 'binary' "
          , "  function returning the FILE as a blob. "
          , " "
          , "  Example: urembed -o static/Static.urp Style.css Script.js"
          , " "
          , "  Urembed honores CC and LD env vars used to call the C compiler and linker"
          , "  respectively. gcc and ld are used by default" ])
      <> header "UrEmebed is the Ur/Web module generator" )

main_ (A tgt ui ins) = do
  let mkname = upper1 . map under . takeFileName where
        under c | c`elem`"_-. /" = '_'
                | otherwise = c
        upper1 [] = []
        upper1 (x:xs) = (toUpper x) : xs

  when (null ins) $ do
    fail "At least one file should be specified"

  when (not $ (map toLower $ takeExtension tgt) == ".urp") $ do
    fail "Target file should have .urp extention"

  -- FIXME: main part is implemented inside the shell script. It would be better
  -- to do the whole job in Haskell, the fastets way is ..
  forM_ ins $ \inf -> do
    env <- do
      let def = [ ("TGT", takeDirectory tgt)
                , ("FILE",inf)
                , ("URE_MODULE_NAME", mkname inf)
                , ("UR_INCLUDE", ui) ]
      case (takeExtension inf) == ".js" of
        True -> do
          decls <- parse_js inf
          return $ def ++ [ ("URE_JS_DECLS", unlines decls) ]
        False -> return def
    exec_embed_sh env

  let line s = tell (s++"\n")
  writeFile tgt $ execWriter $ do
    forM_ ins $ \inf -> do
      line $ printf "library %s" (mkname inf)
    line []
    line "Static"

  let datatype = execWriter $ do
        tell "datatype content = "
        tell (mkname (head ins))
        forM_ (tail ins) (\f -> tell $ printf " | %s" (mkname f))

  writeFile (replaceExtension tgt "urs") $ execWriter $ do
    line datatype
    line "val binary : content -> transaction blob"
    line "val blobpage : content -> transaction page"

  writeFile (replaceExtension tgt "ur") $ execWriter $ do
    line datatype
    line "fun binary c = case c of"
    line $ printf "      %s => %s.binary ()" (mkname (head ins)) (mkname (head ins))
    forM_ (tail ins) (\f -> line $ printf "    | %s => %s.binary ()" (mkname f) (mkname f))
    line "fun blobpage c = case c of"
    line $ printf "      %s => %s.blobpage ()" (mkname (head ins)) (mkname (head ins))
    forM_ (tail ins) (\f -> line $ printf "    | %s => %s.blobpage ()" (mkname f) (mkname f))

    where

      exec_embed_sh env' = do
        script <- getDataFileName "embed.sh"
        env <- do
          add <- forM ["PATH","CC","LD"] $ \n -> do
            maybe Nothing (\x -> (Just (n,x))) <$> lookupEnv n
          return $ (mapMaybe id add) ++ env'
        (ho,_,herr,ph) <- runInteractiveProcess "sh" [script] Nothing (Just env)
        code <- waitForProcess ph
        when (code /= ExitSuccess) $ do
          hGetContents herr >>= err
          fail $ printf "%s failed to complete" script
        return ()


