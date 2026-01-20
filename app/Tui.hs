{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}

module Tui
  ( runTui
  ) where

import Brick
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.Center (hCenter)
import Control.Concurrent.Async (async, cancel, Async)
import Control.Monad (forM)
import Data.List (intersperse)
import qualified Data.Text as T
import Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import Lens.Micro ((^.))
import Lens.Micro.Mtl (use, (%=))
import Lens.Micro.TH (makeLenses)
import System.IO (Handle, hGetLine, hIsEOF, hSetBuffering, BufferMode(..))
import System.Process (createProcess, shell, CreateProcess(..), StdStream(..), ProcessHandle, getPid)
import System.Posix.Signals (signalProcessGroup, sigKILL)
import System.Posix.Types (CPid(..))
import Control.Exception (catch, SomeException)
import Control.Concurrent (threadDelay)

import Types (Project(..))

-- | アプリケーションの状態
data AppState = AppState
    { _projects :: [Project]
    , _projectLogs :: [[T.Text]]  -- 各プロジェクトのログ
    , _selectedPane :: Int        -- 選択中のペイン
    , _processHandles :: [ProcessHandle]
    , _asyncReaders :: [Async ()]
    }

makeLenses ''AppState

-- | カスタムイベント
data CustomEvent = LogUpdate Int T.Text  -- プロジェクトインデックスとログ行

-- | リソース名
data Name = ProjectPane Int
    deriving stock (Eq, Ord, Show)

-- | TUIを起動
runTui :: [Project] -> IO ()
runTui projs = do
    -- イベントチャンネルを作成
    chan <- newBChan 10

    -- 各プロジェクトを起動してログを収集
    (handles, readers) <- startAllProjects projs chan

    let initialState = AppState
            { _projects = projs
            , _projectLogs = replicate (length projs) []
            , _selectedPane = 0
            , _processHandles = handles
            , _asyncReaders = readers
            }

    -- Vtyを初期化
    let buildVty = mkVty V.defaultConfig
    initialVty <- buildVty

    -- アプリを実行
    finalState <- customMain initialVty buildVty (Just chan) app initialState

    -- クリーンアップ（プロセスグループごとSIGKILL）
    mapM_ killProcessGroup (finalState ^. processHandles)
    mapM_ cancel (finalState ^. asyncReaders)
    threadDelay 100000  -- 100ms待機
    putStrLn "All processes stopped."

-- | プロセスグループ全体をSIGKILLで強制終了
killProcessGroup :: ProcessHandle -> IO ()
killProcessGroup ph = do
    maybePid <- getPid ph
    case maybePid of
        Just pid -> do
            let pgid = CPid (fromIntegral pid)
            signalProcessGroup sigKILL pgid `catch` ignoreError
        Nothing -> return ()
  where
    ignoreError :: SomeException -> IO ()
    ignoreError _ = return ()

-- | プロジェクトを全て起動
startAllProjects :: [Project] -> BChan CustomEvent -> IO ([ProcessHandle], [Async ()])
startAllProjects projs chan = do
    results <- forM (zip [0..] projs) $ \(idx, proj) -> do
        let processConfig = (shell (projectCommand proj))
                { cwd = Just (projectPath proj)
                , std_out = CreatePipe
                , std_err = CreatePipe
                , create_group = True
                }
        (_, Just hout, Just herr, ph) <- createProcess processConfig

        -- バッファリングをLineBufferingに設定
        hSetBuffering hout LineBuffering
        hSetBuffering herr LineBuffering

        -- stdout読み取りスレッド
        reader1 <- async $ readOutput chan idx hout
        -- stderr読み取りスレッド
        reader2 <- async $ readOutput chan idx herr

        return (ph, [reader1, reader2])

    let handles = map fst results
    let readers = concatMap snd results
    return (handles, readers)

-- | 出力を読み取ってイベントを送信
readOutput :: BChan CustomEvent -> Int -> Handle -> IO ()
readOutput chan idx h = go `catch` handleError
  where
    go = do
        eof <- hIsEOF h
        if eof
            then return ()
            else do
                line <- hGetLine h
                writeBChan chan (LogUpdate idx (T.pack line))
                go
    handleError :: SomeException -> IO ()
    handleError _ = return ()  -- ハンドルが閉じられた場合などは静かに終了

-- | Brickアプリケーション定義
app :: App AppState CustomEvent Name
app = App
    { appDraw = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return ()
    , appAttrMap = const theMap
    }

-- | UIを描画
drawUI :: AppState -> [Widget Name]
drawUI s = [ui]
  where
    ui = vBox
        [ hCenter $ str "dev-launcher TUI (q: quit, Tab: switch pane, ↑↓: scroll)"
        , hBorder
        , projectPanes
        ]

    projectPanes = case length (s ^. projects) of
        0 -> str "No projects"
        1 -> renderPane s 0
        _ -> hBox $ intersperse vBorder' $ map (renderPane s) [0 .. length (s ^. projects) - 1]

    vBorder' = hLimit 1 $ fill '│'

-- | 単一のペインを描画
renderPane :: AppState -> Int -> Widget Name
renderPane s idx =
    let proj = (s ^. projects) !! idx
        logs = (s ^. projectLogs) !! idx
        isSelected = s ^. selectedPane == idx
        borderStyle = if isSelected then withAttr selectedAttr else id
        title = T.pack $ projectName proj ++ " :" ++ show (maybe 0 id (projectPort proj))
        logLines = map (str . T.unpack) (takeLast 100 logs)
        logWidget = if null logLines then str "Waiting for output..." else vBox logLines
    in borderStyle $ borderWithLabel (txt title) $
        viewport (ProjectPane idx) Vertical logWidget

-- | 最後のN行を取得
takeLast :: Int -> [a] -> [a]
takeLast n xs = drop (max 0 (length xs - n)) xs

-- | イベントハンドラ
handleEvent :: BrickEvent Name CustomEvent -> EventM Name AppState ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar '\t') [])) = do
    numProjects <- length <$> use projects
    selectedPane %= \p -> (p + 1) `mod` numProjects
handleEvent (VtyEvent (V.EvKey V.KUp [])) = do
    idx <- use selectedPane
    vScrollBy (viewportScroll (ProjectPane idx)) (-1)
handleEvent (VtyEvent (V.EvKey V.KDown [])) = do
    idx <- use selectedPane
    vScrollBy (viewportScroll (ProjectPane idx)) 1
handleEvent (AppEvent (LogUpdate idx line)) = do
    projectLogs %= updateAt idx (++ [line])
    -- 自動スクロール
    vScrollToEnd (viewportScroll (ProjectPane idx))
handleEvent _ = return ()

-- | リストの特定インデックスを更新
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt idx f xs = take idx xs ++ [f (xs !! idx)] ++ drop (idx + 1) xs

-- | 属性マップ
theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (selectedAttr, V.white `on` V.blue)
    ]

selectedAttr :: AttrName
selectedAttr = attrName "selected"
