module AI.UASI.Utils.StringTools where

import AI.UASI.CommonTypes

import Data.List.Split

--buggy - convert to use real parser.

readPlotFile :: FilePath -> IO [PlotData]
readPlotFile fn = do
    lns <- fmap lines $ readFile fn
    return $ map stringToPlot lns

stringToPlot :: String -> PlotData
stringToPlot l = 
    let tokens = splitOn " " l
        v      = read . head $ tokens
        vals   = map (read . (!!1) . splitOn ":") . tail $ tokens
    in (v, vals)

plotToString :: PlotData -> String
plotToString (bm, vec) = show bm ++ " " ++ unwords vals
  where
    vals = map (\(i,v) -> show i ++ ":" ++ show v) $ zip [1..] vec

