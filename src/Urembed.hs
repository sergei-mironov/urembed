{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Main where

import Control.Monad.Error
import Control.Monad.State
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

parse_js file modname = do
  let jmodname = (modname++"_js")
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
      <> help "Path to the location of UrWeb's includes"
      <> value "/usr/local/include/urweb" )
  <*> arguments str ( metavar "FILE" <> help "Source file name" )

main :: IO ()
main = execParser opts >>= main_
  where
    opts = info (helper <*> pargs)
      (  fullDesc
      <> progDesc "Embeds a file as blob"
      <> header "Uremebed is the Ur/Web module generator" )

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

main_ (A tgt ui [inf]) = do
  let modname = (takeBaseName tgt)
  let tgtdir = (takeDirectory tgt)
  env <- do
    let def = [ ("TGT",tgtdir)
              , ("FILE",inf)
              , ("URE_MODULE_NAME", modname)
              , ("UR_INCLUDE", ui) ]
    case (takeExtension inf) == ".js" of
      True -> do
        decls <- parse_js inf modname
        return $ def ++ [ ("URE_JS_DECLS", unlines decls) ]
      False -> return def
  exec_embed_sh env

