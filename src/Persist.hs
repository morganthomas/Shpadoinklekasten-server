{-# LANGUAGE CPP                  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Persist where


import           Control.Monad (join, void)
import           Control.Monad.IO.Class
import           Database.MongoDB
import qualified Data.Map as M
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.Text hiding (foldl, null, find)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Time.Calendar
import           Data.Time.LocalTime
import           Data.UUID as U
import           System.Random (randomIO)

import           Types


instance ZettelEditor (Action IO) where

  saveNewCategory i t sid = do
    _ <- validateSession sid
    void $ insert "category"
      [ "id" =: String (unCategoryId i)
      , "title" =: String t
      , "threads" =: Array [] ]


  saveNewThread i t ls cs sid = do
    s <- validateSession sid
    c <- liftIO $ localDay . zonedTimeToLocalTime <$> getZonedTime
    insert "thread"
      [ "id" =: String (unThreadId i)
      , "title" =: String t
      , "author" =: String (unUserId (sessionUser s))
      , "created" =: dayToDoc c
      , "comments" =: Array []
      , "links" =: Array
        ((\l -> Doc
                [ "to" =: String (unThreadId (linkTo l))
                , "description" =: String (linkDescription l) ])
         <$> ls)
      , "categorization" =: Array (String . unCategoryId <$> cs) ]

    modify (select [ "id" =: Doc [ "$in" =: (String . unCategoryId <$> cs) ] ] "category")
      [ "$push" =: Doc [ "threads" =: String (unThreadId i) ] ]


  saveNewComment tid t sid = do
    _ <- validateSession sid
    modify (select [ "id" =: String (unThreadId tid) ] "thread")
      [ "$push" =: Doc [ "comments" =: String t ] ]


  getDatabase sid = do
    s       <- validateSession sid
    catCur  <- find (select [] "category")
    cats    <- collect CategoryId parseCategory mempty catCur
    thCur   <- find (select [] "thread")
    threads <- collect ThreadId parseThread mempty thCur
    usCur   <- find (select [] "user")
    users   <- collect UserId parseUser mempty usCur
    return (Zettel cats threads users (Just s))


  login uid p = do
    mu  <- findOne (select ["id" =: String (unUserId uid)] "user")
    sid <- SessionId . U.toText <$> liftIO randomIO
    c   <- liftIO $ localDay . zonedTimeToLocalTime <$> getZonedTime
    case mu of
      Nothing -> return Nothing
      Just u -> do
        case PasswordHash <$> (u !? "pwhash" >>= cast') of
          Just h | h == p -> do
                     insert "session"
                       [ "id" =: String (unSessionId sid)
                       , "user" =: String (unUserId uid)
                       , "created" =: dayToDoc c ]
                     return . Just $ Session sid uid c
          _ -> return Nothing


collect :: Ord id => (Text -> id) -> (Document -> Maybe a) -> M.Map id a -> Cursor -> Action IO (M.Map id a)
collect toId parse m cur = do
  docs <- nextBatch cur
  let addDoc m d = fromMaybe m $ do
        i <- toId <$> (d !? "id" >>= cast')
        x <- parse d
        return (M.insert i x m)
  let m' = foldl addDoc m docs
  if Prelude.null docs then return m' else collect toId parse m' cur


parseCategory :: Document -> Maybe Category
parseCategory d = do
  t  <- d !? "title" >>= cast'
  i  <- CategoryId <$> (d !? "id" >>= cast')
  ts <- d !? "threads" >>= cast'
  return (Category t i (ThreadId <$> ts))


parseUser :: Document -> Maybe UserProfile
parseUser d = do
  i <- UserId <$> (d !? "id" >>= cast')
  n <- d !? "fullName" >>= cast'
  e <- d !? "email" >>= cast'
  c <- d !? "created" >>= cast' >>= parseDay
  return (UserProfile i n e c)


parseLink :: Document -> Maybe Link
parseLink d = do
  t <- ThreadId <$> (d !? "to" >>= cast')
  e <- d !? "description" >>= cast'
  return (Link t e)


parseComment :: Document -> Maybe Comment
parseComment d = do
  a <- UserId <$> (d !? "author" >>= cast')
  c <- d !? "created" >>= cast' >>= parseDay
  t <- d !? "text" >>= cast'
  return (Comment a c t)


parseThread :: Document -> Maybe Thread
parseThread d = do
  i  <- ThreadId <$> (d !? "id" >>= cast')
  t  <- d !? "title" >>= cast'
  a  <- UserId <$> (d !? "author" >>= cast')
  c  <- d !? "created" >>= cast' >>= parseDay
  ls <- d !? "links" >>= cast'
  ts <- d !? "comments" >>= cast'
  cs <- d !? "categorization" >>= cast'
  return (Thread i t a c (catMaybes $ parseComment <$> ts) (catMaybes $ parseLink <$> ls) (CategoryId <$> cs))


parseDay :: Document -> Maybe Day
parseDay doc = do
  d <- doc !? "day" >>= cast'
  m <- doc !? "month" >>= cast'
  y <- doc !? "year" >>= cast'
  return (fromGregorian y m d)


dayToDoc :: Day -> Document
dayToDoc day = let (y, m, d) = toGregorian day in [ "year" =: y, "month" =: m, "day" =: d ]


parseSession :: Document -> Maybe Session
parseSession d = do
  sid <- SessionId <$> (d !? "id" >>= cast')
  uid <- UserId <$> (d !? "user" >>= cast')
  c   <- d !? "created" >>= cast' >>= parseDay
  return (Session sid uid c)


validateSession :: SessionId -> Action IO Session
validateSession sid = do
  -- TODO: make sessions expire
  ms <- join . fmap parseSession <$> findOne (select ["id" =: String (unSessionId sid)] "session")
  case ms of
    Just s -> return s
    Nothing -> fail "invalid session"
