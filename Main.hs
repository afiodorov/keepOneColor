{-# LANGUAGE ScopedTypeVariables #-}

import Data.Array.Base  (unsafeFreezeSTUArray)
import qualified Data.Vector as V
import Foreign.Storable (Storable)
import Data.Maybe (fromMaybe, isNothing, isJust, catMaybes, fromJust, listToMaybe)
import Debug.Trace (trace, traceIO, traceStack)
import System.IO (stderr, hPrint, hPutStrLn)
import qualified Vision.Image as I
import qualified Vision.Primitive as P
import Vision.Image.Storage.DevIL (Autodetect (..), load, save)
import Options.Applicative (header, progDesc, Parser, argument, option, str,
    metavar, long, eitherReader, value, short, help, showDefaultWith, (<>),
    execParser, info, helper, fullDesc)
import Control.Monad (msum, when, guard)
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed (bounds, (!), UArray, elems, assocs)
import Data.STRef

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
allBlobs img = runST $ do
    mask <- newArray ((0, 0), toTuple (I.shape img)) False
    blob <- createBlob img mask
    if isJust blob && validBlob (fromJust blob)
        then return [blobToImg (fromJust blob)]
        else return []
    where
        validBlob :: Blob a -> Bool
        validBlob b = number b >= 10
        blobToImg :: Blob I.RGBPixel -> I.DelayedMask I.RGBPixel
        blobToImg blob = I.fromFunction (I.shape img) $ \px ->
            if pixArr blob ! toTuple px then img I.!? px else Nothing




type BlobState s = STUArray s (Int, Int) Bool

data Blob a = Blob {
    color :: a,
    number :: Int,
    pixArr :: UArray (Int, Int) Bool
}

extractBlob :: (Storable a) => I.DelayedMask a -> P.Point -> Maybe (Blob a)
extractBlob img pt = let color = img I.!? pt in
    if isNothing color then Nothing else
        Just Blob {color=fromJust color, number=counter, pixArr=pixArr}
    where
        (pixArr, counter) = runST $ let (w, h) = toTuple (I.shape img) in do
                blobArr <- newArray ((0, 0), (w, h)) False
                visitArr <- newArray ((0, 0), (w, h)) False
                counter <- newSTRef 0
                extractBlob' img visitArr blobArr counter pt
                count <- readSTRef counter
                pixArr <- unsafeFreezeSTUArray blobArr
                return (pixArr, count)


extractBlob' :: (Storable a) => I.DelayedMask a -> BlobState s -> BlobState s -> STRef s Int -> P.Point -> ST s ()
extractBlob' img visitArr blobArr counter pt = let
    validNeigboors' = map (uncurry P.ix2) (validNeigboors (I.shape img) pt)
    walk = map (extractBlob' img visitArr blobArr counter) validNeigboors'
    in do
        isNotVisited <- visitSite img pt visitArr
        when (isNotVisited && isJust (img I.!? pt)) $ do
            writeArray blobArr (toTuple pt) True
            modifySTRef counter (+ 1)
            sequence_ walk


visitSite :: (Storable a) => I.DelayedMask a -> P.Point -> BlobState s -> ST s Bool
visitSite img p isVisitedArr = do
    visited <- readArray isVisitedArr (toTuple p)
    if visited then return False
    else do
        writeArray isVisitedArr (toTuple p) True
        return True

findNonEmpty :: BlobState s -> ST s (Maybe (Int, Int))
findNonEmpty state = do
    assocs <- getAssocs state
    return (listToMaybe (keepIndices assocs))
    where
        keepIndices l = map fst (filter (not . snd) l)



createBlob :: (Storable a) => I.DelayedMask a -> BlobState s -> ST s (Maybe (Blob a))
createBlob img mask = do
    startingPixel <- findNonEmpty mask
    case startingPixel of
        Nothing -> return Nothing
        Just pt -> if isJust blob then
            let blobIndices = map fst (filter snd (assocs (pixArr (fromJust blob))))
                in do
                mapM_ (\x -> writeArray mask x True) blobIndices
                return Nothing
        else return Nothing
            where
                blob = extractBlob img (uncurry P.ix2 pt)


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
