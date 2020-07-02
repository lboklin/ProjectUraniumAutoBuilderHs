#! /usr/bin/env nix-shell
#! nix-shell -i runghc -p ffmpeg "haskellPackages.ghcWithPackages (p: [p.turtle p.text])"
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

    copyAndFixAssets cfg
    putStrLn "\nDone!"


copyAndFixAssets :: Config -> IO ()
copyAndFixAssets cfg@Config{..} = do
    forM_ ["Fonts", "Audio", "Graphics"] $ \subDir -> do
        let srcsub = pokemonUraniumDir </> subDir
            tgtsub = godotProjectDir </> subDir
        printf ("Recursively copying assets in: "%fp%" => "%fp%"\n") srcsub tgtsub
        unless dryRun $
            cptree srcsub tgtsub
    fixAudioFiles cfg
    resizeTilesets cfg

    deleteBrokenAndUnused cfg


resizeTilesets :: Config -> IO ()
resizeTilesets cfg@Config{..} = do
    let location = godotProjectDir </> "Graphics/Tilesets"
        toFP baseName = fromText baseName <.> "png"
        mkRealPath baseName = location </> toFP baseName

    printf ("\nWorking in "%fp%"\n") location
    forM_ pngs $ \(baseName, ops) -> do
        basenameIn <- case baseName of
            Rename (PNG bn) newbn -> do
                printf ("Renaming: "%fp%" => "%fp%"\n")
                    (toFP bn) (toFP newbn)
                unless dryRun $
                    mv (mkRealPath bn) (mkRealPath newbn)
                return newbn
            PNG bn -> return bn
        let basenameOut = basenameIn <> "-Resized"
            output = mkRealPath basenameOut
        resizeTileset cfg (mkRealPath basenameIn) ops output
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


deleteBrokenAndUnused :: Config -> IO ()
deleteBrokenAndUnused Config{..} = do
    putStrLn "\nLooking for unused or broken files..."
    forM_ brokenOrUnused $ \absPath ->
        if dryRun then
            printf ("Removing: "%fp%"\n") absPath
        else whenM (testfile absPath) $ do
            printf ("Removing: "%fp%"\n") absPath
            rm absPath
  where
    brokenOrUnused :: [FilePath]
    brokenOrUnused = fmap
        (godotProjectDir </> "Graphics" </>)
        [ "Icons/icon000 - Cópia.png"
        , "Pictures/dialup.png"
        , "Pictures/Map_icon - Cópia.png"
        , "Transitions/RotatingPieces.png"
        , "Tilesets/4.png"
        , "Tilesets/5.png"
        , "Tilesets/6.png"
        , "Tilesets/7.png"
        , "Tilesets/8.png"
        , "Tilesets/NPU-Legen_Town.png"
        , "Tilesets/OLDPU-Victory Road.png"
        , "Tilesets/Outside.png"
        , "Tilesets/Outside(new).png"
        , "Tilesets/PU-Angelure.png"
        , "Tilesets/teste.png"
        , "Tilesets/tileset blank.png"
        ]


fixAudioFiles :: Config -> IO ()
fixAudioFiles Config{..} = do
    let location = godotProjectDir </> "Audio/SE"
    printf ("\nWorking in "%fp%"\n") location
    forM_
        [ ("pcm_s16le", "computerclose.WAV", "computerclosePCM.wav")
        , ("pcm_s16le", "computeropen.WAV", "computeropenPCM.wav")
        , ("copy", "PU-Grasswalk.ogg", "PU-GrasswalkFixed.ogg")
        ]
        $ \(arg, fileIn, fileOut) -> do
            let input = location </> fileIn
                output = location </> fileOut
                cmnPfx = commonPrefix [input, output]

            printf ("Fixing audio file: "%fp%" => "%fp%"\n")
                (relpath cmnPfx input) (relpath cmnPfx output)
            if dryRun then
                void $ ffmpegDry [input] ["-c:a", arg] output
            else do
                !_ <- ffmpeg [input] ["-c:a", arg] output
                rm input


resizeTileset :: Config -> FilePath -> ResizeAnd -> FilePath -> IO ExitCode
resizeTileset Config{..} input resizeAnd output = do
    let location = godotProjectDir </> "Graphics/Tilesets"
        scaled = location </> "scaled.png"
        cmnPfx = commonPrefix [input, output]

    printf ("Resizing and renaming: "%fp%" => "%fp%"\n")
        (relpath cmnPfx input) (relpath cmnPfx output)
    unless dryRun $ do
        -- rescale
        !ExitSuccess <- ffmpeg [input] ["-vf", "scale=iw/2:ih/2:flags=neighbor"] scaled

        case resizeAnd of
            BeDone -> mv scaled output
            Rearrange columns size mbPadding -> do
                let parts =
                        [ location </> fromText (format d n) <.> "png"
                        | n <- [1 .. columns]
                        ]

                -- rearrange columns
                forM_ [0 .. columns - (maybe 1 (const 2) mbPadding)] $ \columni -> do
                    let args = ["-vf", format ("crop=128:"%d%":0:"%d) size (size * columni)]
                    !ExitSuccess <- ffmpeg [scaled] args (parts !! columni)
                    return ()

                whenJust mbPadding $ \padding ->
                    let args =
                            [ "-vf"
                            , format
                                ("crop=128:"%d%":0:"%d%",pad=128:"%d%":0:0:0x000000@0x00")
                                padding (size * (columns - 1)) size
                            ]
                     in do
                         !ExitSuccess <- ffmpeg [scaled] args (parts !! (columns - 1))
                         return ()

                -- put columns together

                let args = ["-filter_complex", format ("hstack=inputs="%d) columns]
                 in do
                     !ExitSuccess <- ffmpeg parts args output
                     -- printf ("Composed "%fp%" from "%w%"\n") output parts
                     return ()

                -- Cleanup
                forM_ (input : scaled : parts)
                    rm

    testfile output <&> (|| dryRun) >>= \case
        True -> return ExitSuccess
        False -> die $ format ("resizeTileset: failed to produce output: "%fp%"\n") output


--- Commands


ffmpegDry :: [FilePath] -> [Text] -> FilePath -> IO ExitCode
ffmpegDry inputs args output = do
    ffmpegBin <- getffmpeg
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
    ffmpegBin <- getffmpeg
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
        !_ -> testfile output >>= \case
            True -> do
                -- printf ("ffmpeg: output success: "%fp%"\n") output
                return ExitSuccess
            False -> do
                die (format ("ffmpeg: output failure: "%fp%"\n") output)
                return (ExitFailure 1)

--- UTIL


debugPrint :: MonadIO io => Format (io ()) (a -> IO b) -> a -> a
debugPrint txt s = do
    let !_ = unsafePerformIO (printf (txt%"\n") s)
    s

getffmpeg :: IO FilePath
getffmpeg = (which "ffmpeg") <&> fromMaybe (error "ffmpeg is not installed")


relpath :: FilePath -> FilePath -> FilePath
relpath fromDir fullPath =
    fullPath
        & stripPrefix (directory fromDir)
        & fromMaybe fullPath


toText' :: FilePath -> Text
toText' =
    let err = format ("Unable to decode filename: "%s) >>> Text.unpack >>> error
    in toText >>> either err id


whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust = flip (maybe (return ()))


whenM :: Monad m => m Bool -> m () -> m ()
whenM mb mf = do
    b <- mb
    when b
        mf
