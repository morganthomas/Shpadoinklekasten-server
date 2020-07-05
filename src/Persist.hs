{-# LANGUAGE CPP                  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns         #-}


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

  saveChange (NewCategory i t) sid = do
    _ <- validateSession sid
    void $ insert "category"
      [ "id" =: String (U.toText (unCategoryId i))
      , "title" =: String t
      , "threads" =: Array [] ]


  saveChange (NewThread ic it t) sid = do
    s <- validateSession sid
    c <- liftIO $ localDay . zonedTimeToLocalTime <$> getZonedTime
    insert "thread"
      [ "id" =: String (U.toText (unThreadId it))
      , "title" =: String t
      , "author" =: String (unUserId (sessionUser s))
      , "created" =: dayToDoc c
      , "comments" =: Array []
      , "links" =: Array []
      , "categorization" =: Array [String (U.toText (unCategoryId ic))] ]

    modify (select [ "id" =: String (U.toText (unCategoryId ic)) ] "category")
      [ "$push" =: Doc [ "threads" =: String (U.toText (unThreadId it)) ] ]


  saveChange (NewComment it ic t) sid = do
    s <- validateSession sid
    c <- liftIO $ localDay . zonedTimeToLocalTime <$> getZonedTime
    modify (select [ "id" =: String (U.toText (unThreadId it)) ] "thread")
      [ "$push" =: Doc [ "comments" =: String (U.toText (unCommentId ic)) ] ]
    insert "comment"
      [ "author" =: String (unUserId (sessionUser s))
      , "created" =: dayToDoc c
      , "text" =: String t ]
    return ()


  getDatabase sid = do
    s       <- validateSession sid
    catCur  <- find (select [] "category")
    cats    <- collect (fmap CategoryId . U.fromText) parseCategory mempty catCur
    thCur   <- find (select [] "thread")
    threads <- collect (fmap ThreadId . U.fromText) parseThread mempty thCur
    usCur   <- find (select [] "user")
    users   <- collect (Just . UserId) parseUser mempty usCur
    return (Zettel cats threads mempty mempty mempty mempty users (Just s)) -- TODO


  login uid p = do
    mu  <- findOne (select ["id" =: String (unUserId uid)] "user")
    sid <- SessionId <$> liftIO randomIO
    c   <- liftIO $ localDay . zonedTimeToLocalTime <$> getZonedTime
    case mu of
      Nothing -> return Nothing
      Just u -> do
        case PasswordHash <$> (u !? "pwhash" >>= cast') of
          Just h | h == p -> do
                     insert "session"
                       [ "id" =: String (U.toText (unSessionId sid))
                       , "user" =: String (unUserId uid)
                       , "created" =: dayToDoc c ]
                     return . Just $ Session sid uid c []
          _ -> return Nothing


collect :: Ord id => (Text -> Maybe id) -> (Document -> Maybe a) -> M.Map id a -> Cursor -> Action IO (M.Map id a)
collect toId parse m cur = do
  docs <- nextBatch cur
  let addDoc m d = fromMaybe m $ do
        i <- d !? "id" >>= cast' >>= toId
        x <- parse d
        return (M.insert i x m)
  let m' = foldl addDoc m docs
  if Prelude.null docs then return m' else collect toId parse m' cur


parseCategory :: Document -> Maybe Category
parseCategory d = do
  t  <- d !? "title" >>= cast'
  i  <- CategoryId <$> (d !? "id" >>= cast' >>= U.fromText)
  ts <- fmap ThreadId <$> (d !? "threads" >>= cast' >>= sequence . fmap U.fromText)
  let f = CategoryId <$> (d !? "from" >>= cast' >>= U.fromText)
  tr <- d !? "isTrash" >>= cast'
  return (Category t i ts f tr)


parseUser :: Document -> Maybe UserProfile
parseUser d = do
  i <- UserId <$> (d !? "id" >>= cast')
  n <- d !? "fullName" >>= cast'
  e <- d !? "email" >>= cast'
  c <- d !? "created" >>= cast' >>= parseDay
  return (UserProfile i n e c)


parseComment :: Document -> Maybe Comment
parseComment d = do
  i <- CommentId <$> (d !? "id" >>= cast' >>= U.fromText)
  a <- UserId <$> (d !? "author" >>= cast')
  c <- d !? "created" >>= cast' >>= parseDay
  e <- d !? "edits" >>= cast' >>= sequence . fmap (parseEdit)
  return (Comment i a c e)


parseEdit :: Document -> Maybe Edit
parseEdit d = do
  c <- d !? "created" >>= cast' >>= parseDay
  t <- d !? "text" >>= cast'
  return (Edit c t)


parseThread :: Document -> Maybe Thread
parseThread d = do
  i  <- ThreadId <$> (d !? "id" >>= cast' >>= U.fromText)
  t  <- d !? "title" >>= cast'
  a  <- UserId <$> (d !? "author" >>= cast')
  c  <- d !? "created" >>= cast' >>= parseDay
  ts <- fmap CommentId <$> (d !? "comments" >>= cast' >>= sequence . fmap U.fromText)
  cs <- fmap CategoryId <$> (d !? "categorization" >>= cast' >>= sequence . fmap U.fromText)
  let f = ThreadId <$> (d !? "createdFrom" >>= cast' >>= U.fromText)
  return (Thread i t a c ts cs f)


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
  sid <- SessionId <$> (d !? "id" >>= cast' >>= U.fromText)
  uid <- UserId <$> (d !? "user" >>= cast')
  c   <- d !? "created" >>= cast' >>= parseDay
  return (Session sid uid c []) -- TODO changes


validateSession :: SessionId -> Action IO Session
validateSession sid = do
  -- TODO: make sessions expire
  ms <- join . fmap parseSession <$> findOne (select ["id" =: String (U.toText (unSessionId sid))] "session")
  case ms of
    Just s -> return s
    Nothing -> fail "invalid session"
