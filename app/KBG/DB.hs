{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use catMaybes" #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# HLINT ignore "Use bimap" #-}
{-# HLINT ignore "Use void" #-}

module KBG.DB (withDb, initDb, saveSubmission, Submission, getAllSubmissions) where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Data.String (fromString)
import Data.Time (UTCTime, getCurrentTime)

data Submission = Submission
    {   subId       :: Int
    ,   subUserId   :: Int
    ,   subVoo      :: String
    ,   subSec      :: String
    ,   subPen      :: String
    ,   subInt      :: String
    ,   subExt      :: String
    ,   subOnd      :: String
    ,   subTime     :: UTCTime
    } deriving (Show)

instance FromRow Submission where

    fromRow = Submission <$> field <*> field <*> field <*> field
                         <*> field <*> field <*> field <*> field <*> field

withDb :: String -> (Connection -> IO a) -> IO a
withDb connStr action = do
    conn <- connectPostgreSQL (fromString connStr)
    result <- action conn
    close conn
    return result

initDb :: Connection -> IO ()
initDb conn = execute_ conn
    "CREATE TABLE IF NOT EXISTS submissions (\
    \  id           SERIAL PRIMARY KEY,\
    \  user_id      INTEGER NOT NULL,\
    \  voo          TEXT,\
    \  sec          TEXT,\
    \  pen          TEXT,\
    \  int          TEXT,\
    \  ext          TEXT,\
    \  ond          TEXT,\
    \  submitted_at TIMESTAMPTZ NOT NULL\
    \)" >> return ()

saveSubmission :: Connection -> Int -> [(String, String)] -> IO ()
saveSubmission conn userId fields = do
    now <- getCurrentTime
    let get k = maybe "" id (lookup k fields)
    _ <- execute conn
        "INSERT INTO submissions (user_id, voo, sec, pen, int, ext, ond, submitted_at)\
        \ VALUES (?,?,?,?,?,?,?,?)\
        \ ON CONFLICT (user_id) DO UPDATE SET\
        \   voo = EXCLUDED.voo, sec = EXCLUDED.sec, pen = EXCLUDED.pen,\
        \   int = EXCLUDED.int, ext = EXCLUDED.ext, ond = EXCLUDED.ond,\
        \   submitted_at = EXCLUDED.submitted_at"
        ( userId
        , get "voo", get "sec", get "pen"
        , get "int", get "ext", get "ond"
        , now
        )
    return ()

getAllSubmissions :: Connection -> IO [Submission]
getAllSubmissions conn = query_ conn "SELECT * FROM submissions ORDER BY submitted_at DESC"