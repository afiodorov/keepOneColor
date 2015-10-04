{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Vector as V
import Foreign.Storable (Storable)
import Data.Maybe (fromMaybe, isNothing, isJust, catMaybes, fromJust)
import Debug.Trace (trace, traceIO, traceStack)
import System.IO (stderr, hPrint, hPutStrLn)
import qualified Vision.Image as I
import qualified Vision.Primitive as P
import Vision.Image.Storage.DevIL (Autodetect (..), load, save)
import Options.Applicative (header, progDesc, Parser, argument, option, str,
    metavar, long, eitherReader, value, short, help, showDefaultWith, (<>),
    execParser, info, helper, fullDesc)
import Control.Monad (msum, when)
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed (bounds, (!), UArray, elems)

type Pixels = [P.Point]
data Blob a = Blob {
    color :: a,
    pixels :: Pixels
}

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
    [((a, b, c), "")] | valid [a, b, c] -> Right (I.RGBPixel (f a) (f b) (f c))
                      | otherwise -> Left "all numbers between 0 and 255"
    _ -> Left "e.g. (2, 4, 5)"
    where
        valid = all (\a -> 0 <= a && a <= 255)
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
            saveBlobs (processImage rgb)
            mErr <- save Autodetect (fileOut opts)
                (fromMasked black (processImage rgb))
            case mErr of
                Nothing -> return ()
                Just err -> do
                    hPutStrLn stderr "Unable to save the image:"
                    hPrint stderr err

saveBlobs :: I.DelayedMask I.RGBPixel -> IO ()
saveBlobs img = sequence_ actions
    where
    names = map (("tmp/" ++) . show) [1..]
    namesWithExt = map (++ ".png") names
    blobImages = map (fromMasked black) (allBlobs img) :: [I.RGB]
    actions = zipWith saveImage namesWithExt blobImages
    saveImage :: String -> I.RGB -> IO ()
    saveImage path img = do
            mErr <- save Autodetect path img
            case mErr of
                Nothing -> return ()
                Just err -> do
                    hPutStrLn stderr "Unable to save the image:"
                    hPrint stderr err

processPixels :: Param -> I.RGBPixel -> I.RGBPixel
processPixels opts p = if p == colorToKeep opts then replaceColor opts else bgColor opts

fromMasked :: I.RGBPixel ->  I.DelayedMask I.RGBPixel -> I.RGB
fromMasked color masked = I.fromFunction (I.shape masked) unMaskedPixel
    where unMaskedPixel pixel = fromMaybe color (masked `I.maskedIndex` pixel)


processImage :: I.RGB -> I.DelayedMask I.RGBPixel
processImage img = I.fromFunction (I.shape img) $ \pt ->
        if filterImg pt img then Just (img `I.index` pt) else Nothing

allBlobs :: I.DelayedMask I.RGBPixel -> [I.DelayedMask I.RGBPixel]
allBlobs img = allBlobs' (createBlobImg img)
    where
        allBlobs' Nothing = []
        allBlobs' (Just (blob, imgOut)) = blob : allBlobs' (createBlobImg imgOut)


type BlobState s = STUArray s (Int, Int) Bool

extractBlob :: (Storable a) => I.DelayedMask a -> P.Point -> UArray (Int, Int) Bool
extractBlob img pt = runSTUArray $ let (w, h) = toTuple (I.shape img) in do
            blobArr <- newArray ((0, 0), (w, h)) False
            visitArr <- newArray ((0, 0), (w, h)) False
            extractBlob' img visitArr blobArr pt
            return blobArr


extractBlob' :: (Storable a) => I.DelayedMask a -> BlobState s -> BlobState s -> P.Point  -> ST s ()
extractBlob' img visitArr blobArr pt = let
    validNeigboors' = map (uncurry P.ix2) (validNeigboors (I.shape img) pt)
    walk = map (extractBlob' img visitArr blobArr) validNeigboors'
    in do
        isNotVisited <- visitSite img pt visitArr
        when (isNotVisited && isJust (img I.!? pt)) $ do
            writeArray blobArr (toTuple pt) True
            sequence_ walk


visitSite :: (Storable a) => I.DelayedMask a -> P.Point -> BlobState s -> ST s Bool
visitSite img p isVisitedArr = do
    visited <- readArray isVisitedArr (toTuple p)
    if visited then return False
    else do
        writeArray isVisitedArr (toTuple p) True
        return True

findNonEmpty :: Foreign.Storable.Storable a => I.DelayedMask a -> Maybe P.Point
findNonEmpty img = let
    P.Z P.:. w P.:. h = I.shape img
    indices = map (uncurry P.ix2) [(x, y) | x <- [0..(w - 1)], y <- [0..(h - 1)]]
    values = map (I.maskedIndex img) indices
    nonEmptyIndices = zipWith (\a b -> if isNothing b then Nothing else Just a) indices values
    in
    msum nonEmptyIndices

createBlobImg :: I.DelayedMask I.RGBPixel -> Maybe (I.DelayedMask I.RGBPixel, I.DelayedMask I.RGBPixel)
createBlobImg img = let startingPixel = findNonEmpty img in
    case startingPixel of
        Nothing -> Nothing
        Just pt -> Just (blobImg, outImg)
            where
                blobPoints = extractBlob img pt
                blobImg = I.fromFunction (I.shape img) (\pt -> if blobPoints ! toTuple pt then img I.!? pt else Nothing)
                outImg = I.fromFunction (I.shape img) (\pt -> if blobPoints ! toTuple pt then Nothing else img I.!? pt)


filterImg :: P.Point -> I.RGB -> Bool
filterImg pt img =
    length sameColored >= 6
    where
    sameColored = filter (I.index img pt ==) $ getNeighboors pt img

getNeighboors :: P.Point -> I.RGB -> [I.RGBPixel]
getNeighboors pt img = map (I.index img . uncurry P.ix2) (validNeigboors (I.shape img) pt)

toTuple :: P.Point -> (Int, Int)
toTuple p = let P.Z P.:. x P.:. y = p in (x, y)

validNeigboors :: P.Point -> P.Point -> [(Int, Int)]
validNeigboors shape pt = let
        P.Z P.:. w P.:. h = shape
        P.Z P.:. x P.:. y = pt
        add (a, b) (c, d) = (a + c, b + d)
        l = [(-1,-1), (-1,0), (-1,1), (0,-1), (0,1), (1,-1), (1,0), (1,1)]
        allNeigboors = zipWith add (replicate (length l) (x, y)) l
        in
        filter (\(a, b) -> 0 <= a && a < w && 0 <= b && b < h) allNeigboors
