--------------------------------------------------------------------------------
module Main where


--------------------------------------------------------------------------------
import           Control.Applicative ((<$>), (<|>))
import           Control.Monad       (forM_)
import           Data.List           (isPrefixOf)
import           System.Exit         (ExitCode (..))
import           System.IO           (hGetContents)
import qualified System.Process      as Process
import qualified Text.Parsec         as P


--------------------------------------------------------------------------------
main :: IO ()
main = do
    -- Quick and dirty main for now.
    Screen monitors <- xrandrQuery
    let primary = head [m | m <- monitors, "LVDS" `isPrefixOf` monitorName m]
    xrandrRun ["--output", monitorName primary, "--auto", "--primary"]

    let secondary =
            [ m
            | m <- monitors
            , "VGA" `isPrefixOf` monitorName m, monitorConnected m
            ]

    forM_ secondary $ \s -> xrandrRun
        [ "--output", monitorName s
        , "--auto", "--right-of", monitorName primary
        ]


--------------------------------------------------------------------------------
xrandr :: [String] -> IO String
xrandr args = do
    putStrLn $ unwords ("xrandr" : args)
    (_, Just xrandrOut, _, xrandrHandle) <- Process.createProcess $
        (Process.proc "xrandr" args) {Process.std_out = Process.CreatePipe}
    output   <- hGetContents $ xrandrOut
    exitCode <- Process.waitForProcess xrandrHandle
    case exitCode of
        ExitSuccess -> return output
        _           -> error $ "xrandr failed with " ++ show exitCode


--------------------------------------------------------------------------------
xrandrQuery :: IO Screen
xrandrQuery = do
    output <- xrandr []
    case P.parse parseScreen "xrandr" output of
        Left  err    -> error $ show err
        Right screen -> return screen


--------------------------------------------------------------------------------
xrandrRun :: [String] -> IO ()
xrandrRun args = xrandr args >>= \_ -> return ()


--------------------------------------------------------------------------------
data Screen = Screen
    { screenMonitors :: [Monitor]
    } deriving (Show)


--------------------------------------------------------------------------------
data Monitor = Monitor
    { monitorName      :: String
    , monitorConnected :: Bool
    , monitorModes     :: [Mode]
    } deriving (Show)


--------------------------------------------------------------------------------
data Mode = Mode
    { modeWidth   :: Int
    , modeHeight  :: Int
    , modeRefresh :: Double
    , modeActive  :: Bool
    , modeNative  :: Bool
    } deriving (Show)


--------------------------------------------------------------------------------
type Parser a = P.Parsec String () a


--------------------------------------------------------------------------------
parseScreen :: Parser Screen
parseScreen = do
    _        <- P.manyTill P.anyChar P.newline
    monitors <- P.many parseMonitor
    return $ Screen monitors


--------------------------------------------------------------------------------
parseMonitor :: Parser Monitor
parseMonitor = do
    name      <- P.many P.alphaNum
    _         <- P.space
    connected <- (== "connected") <$> P.many P.letter
    _         <- P.manyTill P.anyChar P.newline
    modes     <- concat <$> P.many parseModes
    return $ Monitor name connected modes


--------------------------------------------------------------------------------
parseModes :: Parser [Mode]
parseModes = P.try $ do
    _      <- P.many1 P.space
    (w, h) <- parseSize
    modes  <- P.many $ do
        _       <- P.many1 P.space
        refresh <- read <$> P.many1 (P.digit <|> P.char '.')
        active  <- (== '*') <$> P.anyChar
        native  <- (== '+') <$> P.anyChar
        return (refresh, active, native)
    _      <- P.manyTill P.anyChar P.newline
    return [Mode w h r a n | (r, a, n) <- modes]


--------------------------------------------------------------------------------
parseSize :: Parser (Int, Int)
parseSize = do
    width  <- P.many P.digit
    _      <- P.char 'x'
    height <- P.many P.digit
    return (read width, read height)
