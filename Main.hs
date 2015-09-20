{-# LANGUAGE ScopedTypeVariables #-}

import System.Environment (getArgs)
import qualified Vision.Image as I
import Vision.Image.Storage.DevIL (Autodetect (..), load, save)

data KeepPixel = KeepPixel {
    fileIn :: String,
    fileOut :: String
}

main :: IO ()
main = do
    [input, output] <- getArgs
    io <- load Autodetect input
    case io of
        Left err -> do
            putStrLn "Unable to load image:"
            print err
        Right (rgb :: I.RGB) -> do
            mErr <- save Autodetect output (I.map processPixels rgb :: I.RGB)
            case mErr of
                Nothing  ->
                    putStrLn "Success."
                Just err -> do
                    putStrLn "Unable to save the image:"
                    print err


processPixels :: I.ImagePixel I.RGB -> I.ImagePixel I.RGB
processPixels p = if p == I.RGBPixel 255 255 0 then black else I.RGBPixel 255 255 255

black :: I.ImagePixel I.RGB
black = I.RGBPixel 0 0 0
