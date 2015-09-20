{-# LANGUAGE ScopedTypeVariables #-}

import System.IO (stderr, hPrint, hPutStrLn)
import System.Environment (getArgs)
import qualified Vision.Image as I
import Vision.Image.Storage.DevIL (Autodetect (..), load, save)
import Options.Applicative

description = progDesc "Removes all but one colors"
header' = header "KeepOneColor"

yellow = I.RGBPixel 255 255 0
black = I.RGBPixel 0 0 0
white = I.RGBPixel 255 255 255


data Param = Param {
    fileIn :: String,
    fileOut :: String,
    colorToKeep :: I.RGBPixel,
    bgColor :: I.RGBPixel,
    replaceColor :: I.RGBPixel
}

readRGB :: String -> Either String I.RGBPixel
readRGB s = case tripple of
    [((a, b, c), "")] | valid (a, b, c) -> Right (I.RGBPixel (f a) (f b) (f c))
                      | otherwise -> Left "all numbers between 0 and 255"
    _ -> Left "e.g. (2, 4, 5)"
    where
        valid (a, b, c) = all (\a -> 0 <= a && a <= 255) [a, b, c]
        tripple = reads s :: [((Int, Int, Int), String)]
        f = fromIntegral

showRGB :: I.RGBPixel -> String
showRGB a = "(" ++ show (I.rgbRed a) ++ ", "
    ++ show (I.rgbGreen a) ++ ", " ++ show (I.rgbBlue a) ++ ")"

argparser :: Parser Param
argparser = Param
    <$> argument str (metavar "INPUT")
    <*> argument str (metavar "OUTPUT")
    <*> option (eitherReader readRGB)
               (long "keep" <> short 'k'
               <> help "rgb of a color to keep"
               <> metavar "(R, G, B)"
               <> value yellow
               <> showDefaultWith showRGB)
    <*> option (eitherReader readRGB)
               (long "background" <> short 'b'
               <> help "rgb of the background color"
               <> metavar "(R, G, B)"
               <> value white
               <> showDefaultWith showRGB)
    <*> option (eitherReader readRGB)
               (long "replace" <> short 'r'
               <> help "rgb of a replacement color"
               <> metavar "(R, G, B)"
               <> value black
               <> showDefaultWith showRGB)

main :: IO ()
main = execParser opts >>= runWithOptions
    where opts = info (helper <*> argparser) (fullDesc <> description <> header')

runWithOptions :: Param -> IO ()
runWithOptions opts = do
    io <- load Autodetect (fileIn opts)
    case io of
        Left err -> do
            hPutStrLn stderr "Unable to load image:"
            hPrint stderr err
        Right (rgb :: I.RGB) -> do
            mErr <- save Autodetect (fileOut opts)
                (I.map (processPixels opts) rgb :: I.RGB)
            case mErr of
                Nothing -> return ()
                Just err -> do
                    hPutStrLn stderr "Unable to save the image:"
                    hPrint stderr err

processPixels :: Param -> I.RGBPixel -> I.RGBPixel
processPixels opts p = if p == colorToKeep opts then replaceColor opts else bgColor opts
