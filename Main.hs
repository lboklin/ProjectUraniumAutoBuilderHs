#! /usr/bin/env nix-shell
#! nix-shell -i runghc -p "haskellPackages.ghcWithPackages (p: [p.turtle p.text])"
{-# LANGUAGE OverloadedStrings, LambdaCase, RecordWildCards, BangPatterns #-}

-- | Translation of https://github.com/acedogblast/ProjectUraniumAutoBuilder/blob/master/Controller.java
module Main where

import Prelude hiding (FilePath)
import Turtle hiding (Size)
import Turtle.Format
import Control.Arrow ((>>>))
import Control.Exception (throw, toException, catch)
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import qualified Data.List as List
import qualified Data.Text as Text
import System.IO.Unsafe (unsafePerformIO)


data Config = Config
    { pokemonUraniumDir :: FilePath
    , godotProjectDir :: FilePath
    , ffmpegBin :: FilePath
    , dryRun :: Bool
    }

data AssetBaseName = PNG Text | Rename AssetBaseName Text
data ResizeAnd = Rearrange Column Size Padding | BeDone
type Column = Int
type Padding = Maybe Int
type Size = Int


main :: IO ()
main = do
    ([assetsDir, targetDir], isDryRun) <- do
        args <- arguments
        let args_ = List.delete "--dry-run" args
            isDryRun = length args_ /= length args
        args_ & mapM (fromText >>> realpath) >>= \case
            paths@[_, _] ->
                return (paths, isDryRun)
            _ -> do
                err "Usage: <path/to/pokemon-uranium> <path/to/godot-project>"
                return ([], False)
    Just ffmpegBin <- which "ffmpeg"
    let cfg = Config assetsDir targetDir ffmpegBin isDryRun
    when isDryRun $
        putStrLn "This is a dry run. Nothing will be changed."
    putStrLn "Working... please wait."

    copyAndFixAssets cfg
    putStrLn "Done!"


copyAndFixAssets :: Config -> IO ()
copyAndFixAssets cfg@Config{..} = do
    forM_ ["Fonts/", "Audio/", "Graphics/"] $ \subDir -> do
        let srcsub = pokemonUraniumDir </> subDir
            tgtsub = godotProjectDir </> subDir
        printf ("Recursively copying: "%fp%" => "%fp%"\n") srcsub tgtsub
        unless dryRun $
            cptree srcsub tgtsub
    deleteBrokenAndUnused cfg
    fixAudioFiles cfg
    resizeTilesets cfg


resizeTilesets :: Config -> IO ()
resizeTilesets cfg@Config{..} = do
    let location = godotProjectDir </> "Graphics/Tilesets/"
        mkRealPath baseName = location </> fromText baseName <.> "png"
           
    forM_ pngs $ \(baseName, ops) -> do
        basenameIn <- case baseName of
            Rename (PNG bn) newbn -> do
                printf ("Renaming: "%fp%" => "%fp%"\n")
                    (relpath godotProjectDir $ mkRealPath bn) (relpath godotProjectDir $ mkRealPath newbn)
                unless dryRun $
                    mv (mkRealPath bn) (mkRealPath newbn)
                return newbn
            PNG bn -> return bn
        let basenameOut = basenameIn <> "-Resized"
            output = mkRealPath basenameOut
        resizeTileset cfg (mkRealPath basenameIn) ops output
        when (elem basenameOut uselessPngs) $ do
            printf ("Removing useless file: "%fp%"\n") output
            rm output
  where
    pngs =
        [ (PNG "Cavetiles", Rearrange 4 640 Nothing)
        , (PNG "foresttiles", Rearrange 4 640 Nothing)
        , (Rename (PNG "Indoor(1)") "Indoor", Rearrange 5 576 Nothing)
        , (Rename (PNG "Inside Gyms") "InsideGyms", Rearrange 4 640 Nothing)
        , (Rename (PNG "PU-Gym 5") "PU-Gym-5", BeDone)
        , (PNG "NPU-Angelure", Rearrange 4 400 Nothing)
        , (PNG "PU-AngelureFinal", Rearrange 4 400 Nothing)
        , (PNG "NPU-Bealbeach", Rearrange 3 736 Nothing)
        , (PNG "PU-Bealbeach", Rearrange 3 736 Nothing)
        , (Rename (PNG "NPU-Legen Town") "NPU-Legen-Town", Rearrange 5 464 Nothing)
        , (Rename (PNG "PU-Legen Town") "NPU-Legen-Town", Rearrange 5 464 Nothing)
        , (PNG "NPU-Nowtoch", Rearrange 5 352 Nothing)
        , (PNG "PU-Nowtoch", Rearrange 5 352 Nothing)
        , (PNG "NPU-Rainforest", Rearrange 3 720 Nothing)
        , (PNG "PU-Rainforest", Rearrange 3 720 Nothing)
        , (PNG "NPU-Route01-02-Moki-Kevlar", Rearrange 4 400 Nothing)
        , (PNG "PU-Route01-02-Moki-Kevlar", Rearrange 4 400 Nothing)
        , (PNG "PU-Route03-Comet", Rearrange 4 560 (Just 338))
        , (PNG "NPU-Route03-Comet", Rearrange 4 560 (Just 338))
        , (PNG "NPU-Route05-06", Rearrange 4 400 Nothing)
        , (PNG "PU-Route05-06", Rearrange 4 400 Nothing)
        , (PNG "NPU-Route08", Rearrange 5 496 Nothing)
        , (PNG "PU-Route08", Rearrange 5 496 Nothing)
        , (PNG "NPU-Route09", Rearrange 4 400 Nothing)
        , (PNG "PU-Route09", Rearrange 4 400 Nothing)
        , (PNG "NPU-Route16", Rearrange 4 400 Nothing)
        , (PNG "PU-Route16", Rearrange 4 400 Nothing)
        , (PNG "NPU-Silverport", Rearrange 4 400 Nothing)
        , (PNG "PU-Silverport", Rearrange 4 400 Nothing)
        , (PNG "NPU-Tsukinami", Rearrange 3 800 Nothing)
        , (PNG "PU-Tsukinami", Rearrange 3 800 Nothing)
        , (PNG "NPU-Veneza", Rearrange 4 464 Nothing)
        , (PNG "PU-Veneza", Rearrange 4 464 Nothing)
        , (PNG "PU-CaveSet", Rearrange 8 736 Nothing)
        , (PNG "PU-Championship", Rearrange 4 720 Nothing)
        , (PNG "PU-NuclearPlantInside", BeDone)
        , (PNG "PU-Nuclear08", Rearrange 7 352 Nothing)
        , (PNG "PU-NuclearPlant", Rearrange 3 720 Nothing)
        , (PNG "PU-PowerPlant_2", Rearrange 3 800 Nothing)
        , (PNG "PU-Tsukinami-Indoors", Rearrange 4 720 Nothing)
        , (PNG "PU-Underwater", Rearrange 2 544 Nothing)
        , (PNG "PU-VictoryRoad", Rearrange 3 720 Nothing)
        ]

    uselessPngs =
        [ "4"
        , "5"
        , "6"
        , "7"
        , "8"
        , "NPU-Legen_Town"
        , "OLDPU-Victory Road"
        , "Outside"
        , "Outside(new)"
        , "PU-Angelure"
        , "teste"
        , "tileset blank"
        ]


deleteBrokenAndUnused :: Config -> IO ()
deleteBrokenAndUnused Config{..} = do
    forM_ brokenOrUnused $ \rp -> do
        printf ("Deleting "%fp%"\n") rp
        unless dryRun $ rm rp
  where
    brokenOrUnused :: [FilePath]
    brokenOrUnused = fmap (godotProjectDir </> "Graphics/" </>)
        [ "Icons/icon000 - Cópia.png"
        , "Pictures/dialup.png"
        , "Pictures/Map_icon - Cópia.png"
        , "Transitions/RotatingPieces.png"
        ]


fixAudioFiles :: Config -> IO ()
fixAudioFiles Config{..} =
    let location = godotProjectDir </> "Audio/SE/"
     in forM_
        [ ("pcm_s16le", "computerclose.WAV", "computerclosePCM.wav")
        , ("pcm_s16le", "computeropen.WAV", "computeropenPCM.wav")
        , ("copy", "PU-Grasswalk.ogg", "PU-GrasswalkFixed.ogg")
        ]
        $ \(arg, fileIn, fileOut) -> do
            let input = location </> fileIn
                output = location </> fileOut

            printf ("Fixing audio file: "%fp%" => "%fp%"\n")
                (relpath godotProjectDir input) (relpath godotProjectDir output)
            if dryRun then
                void $ ffmpegDry [input] ["-c:a", arg] output
            else do
                !_ <- ffmpeg [input] ["-c:a", arg] output
                rm input


resizeTileset :: Config -> FilePath -> ResizeAnd -> FilePath -> IO ExitCode
resizeTileset Config{..} input resizeAnd output = do
    let location = godotProjectDir </> "Graphics/Tilesets/"
        scaled = location </> "scaled.png"

    printf ("Resizing and renaming "%fp%" => "%fp%"\n")
        (relpath godotProjectDir input) (relpath godotProjectDir output)
    unless dryRun $ do
        -- rescale
        ffmpeg [input] ["-vf", "scale=iw/2:ih/2:flags=neighbor"] scaled

        case resizeAnd of
            BeDone -> return ()
            Rearrange columns size mbPadding -> do
                let parts =
                        [ location </> fromText (format d n) <.> "png"
                        | n <- [1 .. columns]
                        ]

                -- rearrange columns
                forM_ [0 .. columns - (maybe 1 (const 2) mbPadding)] $ \columni -> do
                    let args = ["-vf", format ("crop=128:"%d%":0:"%d) size (size * columni)]
                    ffmpeg [scaled] args (parts !! columni) & void

                whenJust mbPadding $ \padding ->
                    let args =
                            [ "-vf"
                            , format
                                ("crop=128:"%d%":0:"%d%",pad=128:"%d%":0:0:0x000000@0x00")
                                padding (size * (columns - 1)) size
                            ]
                     in void $ ffmpeg [scaled] args (parts !! (columns - 1))

                -- put columns together
                let args = ["-filter_complex", format ("hstack=inputs="%d) columns]
                 in ffmpeg parts args output

                -- Cleanup
                forM_ (input : scaled : parts)
                    rm

    return ExitSuccess


--- Commands

ffmpegDry :: [FilePath] -> [Text] -> FilePath -> IO ExitCode
ffmpegDry inputs args output = do
    Just ffmpegBin <- which "ffmpeg"
    let inputFormatted :: [Text]
        inputFormatted = case inputs of
            [] -> []
            _ -> "-i" : List.intersperse "-i" (toText' <$> inputs)
        cmdArgs = "-y" : inputFormatted ++ args ++ [toText' output]
        fullCmd = Text.unwords $ toText' ffmpegBin : cmdArgs
    printf ("Dry-executing: "%s%"\n") fullCmd
    return ExitSuccess


ffmpeg :: [FilePath] -> [Text] -> FilePath -> IO ExitCode
ffmpeg inputs args output = do
    Just ffmpegBin <- which "ffmpeg"
    let cmd = toText' ffmpegBin
        inputFormatted :: [Text]
        inputFormatted = case inputs of
            [] -> []
            _ -> "-i" : List.intersperse "-i" (toText' <$> inputs)
        cmdArgs = "-y" : inputFormatted ++ args ++ [toText' output]
    procStrictWithErr cmd cmdArgs empty >>= \case
        (ec@(ExitFailure _), stdOut, stdErr) -> do
            catch
                (throw (toException $ ProcFailed cmd cmdArgs ec))
                (\e -> eprintf ("Caught "%w%"\nstderr:\n"%s%"\n") (e :: ProcFailed) stdErr)
            return ec
        _ -> return ExitSuccess


--- UTIL


relpath :: FilePath -> FilePath -> FilePath
relpath fromDir fullPath =
    fullPath
        & stripPrefix' fromDir
        & fromMaybe fullPath

debugPrint :: MonadIO io => Format (io ()) (a -> IO b) -> a -> a
debugPrint txt s = do
    let !_ = unsafePerformIO (printf (txt%"\n") s)
    s


toText' :: FilePath -> Text
toText' =
    let err = format ("Unable to decode filename: "%s) >>> Text.unpack >>> error
    in toText >>> either err id

whenJust = flip (maybe (return ()))


-- Fixes: stripPrefix "/path/to/dir"  "/path/to/dir/subdir" == Nothing
--        stripPrefix "/path/to/dir/" "/path/to/dir/subdir" == Just "subdir"
stripPrefix' :: FilePath -> FilePath -> Maybe FilePath
stripPrefix' p1 p2 =
    toText p1
        & id `either` id (<> "/")
        & fromText
        & stripPrefix `flip` p2
