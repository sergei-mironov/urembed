{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}
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
import System.Directory
import System.Info
import Network.Mime
import Text.Printf
import Data.Either
import Data.Generics
import Data.Char
import Data.List
import Data.Data
import Data.Typeable
import Data.Maybe
import Data.String
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS

import qualified Development.Cake3 as C3
import Development.Cake3(runMake,makevar,cmd,rule,extvar,File(..),phony,depend)
import qualified Development.Cake3.Rules.UrWeb as C3
import Development.Cake3.Rules.UrWeb (Config(..), urdeps,defaultConfig)

import Options.Applicative

import Paths_urembed

io :: (MonadIO m) => IO a -> m a
io = liftIO

hio :: (MonadIO m) => Handle -> String -> m ()
hio h = io . hPutStrLn h

err,out :: (MonadIO m) => String -> m ()
err = hio stderr
out = hio stdout


span2 :: String -> String -> Maybe (String,String)
span2 inf s = span' [] s where
  span' _ [] = Nothing
  span' acc (c:cs)
    | isPrefixOf inf (c:cs) = Just (acc, drop (length inf) (c:cs))
    | otherwise = span' (acc++[c]) cs

data JSFunc = JSFunc {
    urdecl :: String
  , urname :: String
  , jsname :: String
  } deriving(Show)

data JSType = JSType {
    urtdecl :: String
  } deriving(Show)

-- | Parse the JavaScript file, extract top-level functions, convert their
-- signatures into Ur/Web format, return them as the list of strings
parse_js :: FilePath -> IO (Either String ([JSType],[JSFunc]))
parse_js file = do
  s <- readFile file
  runErrorT $ do
    c <- either fail return (parse s file)
    f <- concat <$> (forM (findTopLevelFunctions c) $ \f@(fn:_) -> (do
      ts <- mapM extractEmbeddedType (f`zip`(False:repeat True))
      let urdecl_ = urs_line ts
      let urname_ = (fst (head ts))
      let jsname_ = fn
      return [JSFunc urdecl_ urname_ jsname_]
      ) `catchError` (\(e::String) -> do
        err $ printf "ignoring function %s, reason:\n\t%s" fn e
        return []))
    t <- concat <$> (forM (findTopLevelVars c) $ \vn -> (do
      (n,t) <- extractEmbeddedType (vn,False)
      return [JSType $ printf "type %s" t]
      )`catchError`  (\(e::String) -> do
        err $ printf "ignoring variable %s, reason:\n\t%s" vn e
        return []))

    return (t,f)

  where
    urs_line :: [(String,String)] -> String
    urs_line [] = error "wrong function signature"
    urs_line ((n,nt):args) = printf "val %s : %s" n (fmtargs args nt) where
      fmtargs :: [(String,String)] -> String -> String
      fmtargs ((an,at):as) ret = printf "%s -> %s" at (fmtargs as ret)
      fmtargs ([]) ret = printf "transaction %s" ret

    extractEmbeddedType :: (Monad m) => (String,Bool) -> m (String,String)
    extractEmbeddedType ([],_) = error "BUG: empty identifier"
    extractEmbeddedType (name,fallback) = check (msum [span2  "__" name , span2 "_as_" name]) where
      check (Just (n,t)) = return (n,t)
      check _ | fallback == True = return (name,name)
              | fallback == False = fail $ printf "Can't extract the type from the identifier '%s'" name

    findTopLevelFunctions :: JSNode -> [[String]]
    findTopLevelFunctions top = map decls $ listify is_func top where
      is_func n@(JSFunction a b c d e f) = True
      is_func _ = False
      decls (JSFunction a b c d e f) = (identifiers b) ++ (identifiers d)

    findTopLevelVars :: JSNode -> [String]
    findTopLevelVars top = map decls $ listify is_var top where
      is_var n@(JSVarDecl a []) = True
      is_var _ = False
      decls (JSVarDecl a _) = (head $ identifiers a);
      
    identifiers x = map name $ listify ids x where
      ids i@(JSIdentifier s) = True
      ids _ = False
      name (JSIdentifier n) = n

data Args = A
  { tgtdir :: FilePath
  , version :: Bool
  , files :: [FilePath]
  }

pargs :: Parser Args
pargs = A
  <$> strOption
      (  long "output"
      <> short 'o'
      <> metavar "FILE.urp"
      <> help "Name of the Ur/Web project being generated"
      <> value "")
  <*> flag False True ( long "version" <> help "Show version information" )
  <*> arguments str ( metavar "FILE" <> help "File to embed" )
  where
    osdefgcc | isInfixOf "linux" os = "/usr/bin/gcc"
             | isInfixOf "windows" os = "c:\\cygwin\\usr\\bin\\gcc"
             | otherwise = "/usr/local/bin/gcc"


replaceExtensions f x = addExtension (dropExtensions f) x
f .= x = replaceExtensions f x

main :: IO ()
main = do
  h <- (getDataFileName >=> readFile) "Help.txt" 
  main_ =<< execParser (
    info (helper <*> pargs)
      (  fullDesc
      <> progDesc h
      <> header "UrEmebed is the Ur/Web module generator" ))

main_ (A tgturp True ins) = do
  hPutStrLn stderr "urembed version 0.5.0.0"

main_ (A tgturp False ins) = do

  let tgtdir = takeDirectory tgturp

  when (null tgtdir) $ do
    fail "An output directory should be specified, use -o"

  when (null ins) $ do
    fail "At least one file should be specified, see --help"

  exists <- doesDirectoryExist tgtdir
  when (not exists) $ do
    fail "Output is not a directory"

  let indest n = tgtdir </> n
  let write n wr = writeFile (indest n) $ execWriter $ wr

  forM_ ins $ \inf -> do
    hPutStrLn stderr (printf "Processing %s" inf)

    let modname = (mkname inf)
    let modname_c = modname ++ "_c"
    let blobname = modname ++ "_c_blob"
    let modname_js = modname ++ "_js"
    let mime = BS.unpack (defaultMimeLookup (fromString inf))

    -- Module_c.urp
    let binfunc = printf "uw_%s_binary" modname_c
    let textfunc = printf "uw_%s_text" modname_c

    write (replaceExtension modname_c ".urs") $ do
      line $ "val binary : unit -> transaction blob"
      line $ "val text : unit -> transaction string"

    let csrc = replaceExtension modname_c ".c"
    write csrc $ do
      line $ "// Thanks, http://stupefydeveloper.blogspot.ru/2008/08/cc-embed-binary-data-into-elf.html"
      line $ "#include <urweb.h>"
      line $ "#include <stdio.h>"
      let start = printf "_binary___%s_start" blobname
      let size = printf "_binary___%s_size" blobname
      line $ "extern int " ++ size  ++ ";"
      line $ "extern int " ++ start ++ ";"
      line $ "uw_Basis_blob " ++ binfunc ++ " (uw_context ctx, uw_unit unit)"
      line $ "{"
      line $ "  uw_Basis_blob blob;"
      line $ "  blob.data = (char*)&" ++ start ++ ";"
      line $ "  blob.size = (size_t)&" ++ size ++ ";"
      line $ "  return blob;"
      line $ "}"
      line $ ""
      line $ "uw_Basis_string " ++ textfunc ++ " (uw_context ctx, uw_unit unit) {"
      line $ "  char* data = (char*)&" ++ start ++ ";"
      line $ "  size_t size = (size_t)&" ++ size ++ ";"
      line $ "  char * c = uw_malloc(ctx, size+1);"
      line $ "  char * write = c;"
      line $ "  int i;"
      line $ "  for (i = 0; i < size; i++) {"
      line $ "    *write =  data[i];"
      line $ "    if (*write == '\\0')"
      line $ "    *write = '\\n';"
      line $ "    *write++;"
      line $ "  }"
      line $ "  *write=0;"
      line $ "  return c;"
      line $ "  }"

    let header = (replaceExtension modname_c ".h")
    write header $ do
      line $ "#include <urweb.h>"
      line $ "uw_Basis_blob " ++ binfunc ++ " (uw_context ctx, uw_unit unit);"
      line $ "uw_Basis_string " ++ textfunc ++ " (uw_context ctx, uw_unit unit);"

    let binobj = replaceExtension modname_c ".o"
    let dataobj = replaceExtension modname_c ".data.o"

    write (replaceExtension modname_c ".urp") $ do
      line $ "ffi " ++ modname_c
      line $ "include " ++ header
      line $ "link " ++ binobj
      line $ "link " ++ dataobj

    -- Copy the file to the target dir and run linker from there. Thus the names
    -- it places will be correct (see start,size in _c)
    copyFile inf (indest blobname)

    -- Module_js.urp
    (jstypes,jsdecls) <- if ((takeExtension inf) == ".js") then do
                            e <- parse_js inf
                            case e of
                              Left e -> do
                                err e
                                return ([],[])
                              Right decls -> do
                                -- err (show decls)
                                return decls
                         else
                            return ([],[])

    write (replaceExtension modname_js ".urs") $ do
      forM_ jstypes $ \decl -> line (urtdecl decl)
      forM_ jsdecls $ \decl -> line (urdecl decl)

    write (replaceExtension modname_js ".urp") $ do
      line $ "ffi " ++ modname_js
      forM_ jsdecls $ \decl ->
        line $ printf "jsFunc %s.%s = %s" modname_js (urname decl) (jsname decl)
    
    -- Module.urp
    write (replaceExtension modname ".urs") $ do
      line $ "val binary : unit -> transaction blob"
      line $ "val text : unit -> transaction string"
      line $ "val blobpage : unit -> transaction page"
      forM_ jstypes $ \decl -> line (urtdecl decl)
      forM_ jsdecls $ \d -> line (urdecl d)

    write (replaceExtension modname ".ur") $ do
      line $ "val binary = " ++ modname_c ++ ".binary"
      line $ "val text = " ++ modname_c ++ ".text"
      forM_ jsdecls $ \d ->
        line $ printf "val %s = %s.%s" (urname d) modname_js (urname d)
      line $ printf "fun blobpage {} = b <- binary () ; returnBlob b (blessMime \"%s\")" mime

    write (replaceExtension modname ".urp") $ do
      line $ "library " ++ modname_c
      line $ "library " ++ modname_js
      line $ ""
      line $ modname

  -- Static.urp
  let tgt_in = replaceExtensions tgturp ".urp.in"

  writeFile tgt_in $ execWriter $ do
    forM_ ins $ \inf -> do
      line $ printf "library %s" (mkname inf)
    line []
    line (takeBaseName tgturp)

  let datatype = execWriter $ do
        tell "datatype content = "
        tell (mkname (head ins))
        forM_ (tail ins) (\f -> tell $ printf " | %s" (mkname f))

  writeFile (replaceExtensions tgt_in "urs") $ execWriter $ do
    line datatype
    line "val binary : content -> transaction blob"
    line "val text : content -> transaction string"
    line "val blobpage : content -> transaction page"

  writeFile (replaceExtensions tgt_in "ur") $ execWriter $ do
    line datatype
    line "fun binary c = case c of"
    line $ printf "      %s => %s.binary ()" (mkname (head ins)) (mkname (head ins))
    forM_ (tail ins) (\f -> line $
          printf "    | %s => %s.binary ()" (mkname f) (mkname f))
    line "fun blobpage c = case c of"
    line $ printf "      %s => %s.blobpage ()" (mkname (head ins)) (mkname (head ins))
    forM_ (tail ins) (\f -> line $
           printf "    | %s => %s.blobpage ()" (mkname f) (mkname f))
    line "fun text c = case c of"
    line $ printf "      %s => %s.text ()" (mkname (head ins)) (mkname (head ins))
    forM_ (tail ins) (\f -> line $
           printf "    | %s => %s.text ()" (mkname f) (mkname f))

  -- Build the Makefile
  setCurrentDirectory tgtdir
  writeFile ((takeBaseName tgturp) .= ".mk") =<< (mdo
    let file x = C3.file' tgtdir tgtdir x
    let cc = extvar "CC"
    let ld = extvar "LD"
    let incl = extvar "UR_INCLUDE_DIR"

    let tgt_in = file (takeBaseName tgturp .= ".urp.in") 
    let tgt = file (takeBaseName tgturp .= ".urp") 

    urp_in <- C3.ruleM tgt_in $ do
      flip urdeps tgt_in (
        defaultConfig {
          urObjRule = \f -> rule f $ do
            case isInfixOf "data" (C3.takeExtensions f) of
              True -> do
                let src = C3.fromFilePath . (++"_blob") . dropExtensions . C3.toFilePath $ f
                C3.shell [cmd| $(ld) -r -b binary -o $f $(src :: File) |]
              False -> do
                let src = C3.fromFilePath . flip replaceExtensions "c" . C3.toFilePath $ f
                C3.shell [cmd| $(cc) -c -I $incl -o $f $(src :: File) |]
          })

    urp <- C3.ruleM tgt $ do
      C3.shell [cmd|cp $(urp_in) $(urp) |]
      C3.shell [cmd|echo $urp|]

    runMake $ do
      C3.place (phony "urp" (depend urp))
    )

  hPutStrLn stderr "Done"

    where

      line s = tell (s++"\n")
      
      process = process' Nothing
      process' wd args = do
        (_,hout,herr,ph) <- runInteractiveProcess (head args) (tail args) wd Nothing
        code <- waitForProcess ph
        when (code /= ExitSuccess) $ do
          hGetContents hout >>= hPutStrLn stderr
          hGetContents herr >>= hPutStrLn stderr 
          fail $ printf "process %s failed to complete with %s" (show args) (show code)
        return ()

      mkname = upper1 . map under . takeFileName where
        under c | c`elem`"_-. /" = '_'
                | otherwise = c
        upper1 [] = []
        upper1 (x:xs) = (toUpper x) : xs

