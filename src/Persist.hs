{-# LANGUAGE CPP                  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Persist where


import           Control.Monad (join, void)
import           Database.MongoDB
import qualified Data.Map as M
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.Text hiding (foldl, null, find)

import           Types


instance ZettelEditor (Action IO) where

  saveNewCategory i t = void $ insert "category"
    [ "id" =: String (unCategoryId i)
    , "title" =: String t
    , "threads" =: Array [] ]

  -- TODO: update thread list of each category
  saveNewThread i t ls cs = void $ insert "thread"
    [ "id" =: String (unThreadId i)
    , "title" =: String t
    , "comments" =: Array []
    , "links" =: Array
      ((\l -> Doc
              [ "to" =: String (unThreadId (linkTo l))
              , "description" =: String (linkDescription l) ])
        <$> ls)
    , "categorization" =: Array (String . unCategoryId <$> cs) ]

  saveNewComment tid t = modify (select [ "id" =: String (unThreadId tid) ] "thread")
                         [ "$push" =: Doc [ "comments" =: String t ] ]


  getDatabase = do
    catCur  <- find (select [] "category")
    cats    <- collect CategoryId parseCategory mempty catCur
    thCur   <- find (select [] "thread")
    threads <- collect ThreadId parseThread mempty thCur
    return (Zettel cats threads)


collect :: Ord id => (Text -> id) -> (Document -> Maybe a) -> M.Map id a -> Cursor -> Action IO (M.Map id a)
collect toId parse m cur = do
  docs <- nextBatch cur
  let addDoc m d = fromMaybe m $ do
        i <- toId <$> (d !? "id" >>= cast')
        x <- parse d
        return (M.insert i x m)
  let m' = foldl addDoc m docs
  if null docs then return m' else collect toId parse m' cur


parseCategory :: Document -> Maybe Category
parseCategory d = do
  t  <- d !? "title" >>= cast'
  i  <- CategoryId <$> (d !? "id" >>= cast')
  ts <- d !? "threads" >>= cast'
  return (Category t i (ThreadId <$> ts))


parseLink :: Document -> Maybe Link
parseLink d = do
  t <- ThreadId <$> (d !? "to" >>= cast')
  e <- d !? "description" >>= cast'
  return (Link t e)


parseThread :: Document -> Maybe Thread
parseThread d = do
  i  <- ThreadId <$> (d !? "id" >>= cast')
  t  <- d !? "title" >>= cast'
  ls <- d !? "links" >>= cast'
  ts <- d !? "comments" >>= cast'
  cs <- d !? "categorization" >>= cast'
  return (Thread i t ts (catMaybes $ parseLink <$> ls) (CategoryId <$> cs))
