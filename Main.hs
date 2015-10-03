{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Vector as V
import Control.Monad.State
import Foreign.Storable (Storable)
import Data.Maybe (fromMaybe, isNothing, catMaybes, fromJust)
import Debug.Trace (trace, traceIO, traceStack)
import System.IO (stderr, hPrint, hPutStrLn)
import qualified Vision.Image as I
import qualified Vision.Primitive as P
import qualified Data.Vector.Unboxed.Mutable as M
import Vision.Image.Storage.DevIL (Autodetect (..), load, save)
import Options.Applicative (header, progDesc, Parser, argument, option, str,
    metavar, long, eitherReader, value, short, help, showDefaultWith, (<>),
    execParser, info, helper, fullDesc)
import Control.Monad (msum)
import Control.Monad.ST

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
            {-saveBlobs (processImage rgb)-}
            mErr <- save Autodetect (fileOut opts)
                (fromMasked black (fst . fromJust $ createBlobImg (processImage rgb)))
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


type BlobState a = (I.DelayedMask a, I.DelayedMask a, V.Vector P.Point)


extractBlob :: (Storable a) => I.DelayedMask a -> P.Point -> State (BlobState a) [P.Point]
extractBlob img pt = let
    P.Z P.:. w P.:. h = I.shape img
    P.Z P.:. x P.:. y = pt
    l = [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)]
    add (a, b) (c, d) = (a + c, b + d)
    allNeigboors = zipWith add (replicate (length l) (x, y)) l
    validNeigboors = filter (\(a, b) -> 0 <= a && a < w && 0 <= b && b < h) allNeigboors
    validNeigboors' = map (uncurry P.ix2) validNeigboors
    walk = map (extractBlob img) validNeigboors'
    in do
        attempt <- visitSite img pt
        case attempt of
            Just site -> (liftM concat . sequence) $ return [site] : walk
            _         -> return []


visitSite :: (Storable a) => I.DelayedMask a -> P.Point -> State (BlobState a) (Maybe P.Point)
visitSite img p = do
    (blobImg, oldImg, visitedPoints) <- get
    if p `V.elem` visitedPoints then
        return Nothing
    else let visitedPoints' = V.snoc visitedPoints p in
        if isNothing (img `I.maskedIndex` p)
            then do
                put (blobImg, oldImg, visitedPoints')
                return Nothing
            else let
                    blobImg' = I.fromFunction (I.shape blobImg) (\pt -> if pt == p then oldImg I.!? pt else blobImg I.!? pt)
                    oldImg' = I.fromFunction (I.shape oldImg) (\pt -> if pt == p then Nothing else oldImg I.!? pt)
                in do
                put (blobImg', oldImg', visitedPoints')
                return (Just p)

findNonEmpty :: Foreign.Storable.Storable a => I.DelayedMask a -> Maybe P.Point
findNonEmpty img = let
    P.Z P.:. w P.:. h = I.shape img
    indices = map (uncurry P.ix2) [(x, y) | x <- [0..(w - 1)], y <- [0..(h-1)]]
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
                empty = I.fromFunction (I.shape img) (const Nothing)
                initState = (empty, img, V.fromList [])
                (blobPoints, (blobImg, outImg, _)) = runState (extractBlob img pt) initState


filterImg :: P.Point -> I.RGB -> Bool
filterImg pt img =
    length sameColored >= 6
    where
    sameColored = filter (I.index img pt ==) $ getNeighboors pt img

getNeighboors :: P.Point -> I.RGB -> [I.RGBPixel]
getNeighboors pt img = let
        P.Z P.:. w P.:. h = I.shape img
        P.Z P.:. x P.:. y = pt
        add (a, b) (c, d) = (a + c, b + d)
        l = [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)]
        allNeigboors = zipWith add (replicate (length l) (x, y)) l
        validNeigboors = filter (\(a, b) -> 0 <= a && a < w && 0 <= b && b < h) allNeigboors
    in
    map (I.index img . uncurry P.ix2) validNeigboors
