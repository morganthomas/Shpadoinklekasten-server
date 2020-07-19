{-# LANGUAGE CPP                  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns         #-}


module Persist where


import           Control.Applicative ((<|>))
import           Control.Monad (join, void)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Database.MongoDB
import           Data.List (uncons)
import qualified Data.Map as M
import           Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as S
import           Data.Text hiding (foldl, null, find, uncons, takeWhile, dropWhile)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Time.Calendar
import           Data.Time.LocalTime
import           Data.UUID as U
import           System.Random (randomIO)

import           Types


now :: Action IO Day
now = liftIO $ localDay . zonedTimeToLocalTime <$> getZonedTime


doc :: Value -> Document
doc (Doc d) = d


instance ZettelEditor (Action IO) where

  saveChange (NewCategory i t) sid = do
    _ <- validateSession sid
    void . insert "category" . doc . val $ Category t i [] Nothing False


  saveChange (NewThread ic it t) sid = do
    s <- validateSession sid
    n <- now
    _ <- insert "thread" . doc . val $ Thread it t (sessionUser s) n [] [ic] Nothing
    modify (select [ "id" =: ic ] "category") [ "$push" =: Doc [ "threads" =: it ] ]


  saveChange (NewComment it ic t) sid = do
    s <- validateSession sid
    n <- now
    modify (select [ "id" =: it ] "thread") [ "$push" =: Doc [ "comments" =: ic ] ]
    void . insert "comment" . doc . val $ Comment ic (sessionUser s) n [Edit n t]


  saveChange (AddThreadToCategory cid tid) sid = do
    s <- validateSession sid
    modify (select [ "id" =: cid ] "category") [ "$push" =: Doc [ "threads" =: tid ] ]
    modify (select [ "id" =: tid ] "thread") [ "$push" =: Doc [ "categorization" =: cid ] ]


  saveChange (RemoveThreadFromCategory cid tid) sid = do
    s <- validateSession sid
    modify (select [ "id" =: cid ] "category") [ "$pull" =: Doc [ "threads" =: tid ] ]
    modify (select [ "id" =: tid ] "thread") [ "$pull" =: Doc [ "categorization" =: cid ] ]


  saveChange (RetitleCategory cid0 cid1 txt) sid = do
    s <- validateSession sid
    void . runMaybeT $ do
      d <- MaybeT . findOne $ select [ "id" =: cid0 ] "category"
      c <- MaybeT . return $ cast' (Doc d)
      lift . insert "category" . doc . val $ c { categoryId = cid1, categoryTitle = txt }
      lift $ modify (select [ "id" =: cid0 ] "category") [ "isTrash" =: True ]


  saveChange (RetitleThread tid0 tid1 txt) sid = do
    s <- validateSession sid
    void . runMaybeT $ do
      d <- MaybeT . findOne $ select [ "id" =: tid0 ] "thread"
      t <- MaybeT . return $ cast' (Doc d)
      lift . insert "thread" . doc. val $ t { threadId = tid1, threadTitle = txt }
      lift $ modify (select [ "id" =: Doc [ "$in" =: val <$> categorization t ] ] "category")
        [ "$pull" =: tid0, "$push" =: tid1 ]
      lift $ modify (select [] "trashcan") [ "$push" =: Doc [ "threadIds" =: tid0 ] ]


  saveChange (RemoveComment tid0 tid1 cid) sid = do
    s <- validateSession sid
    void . runMaybeT $ do
      d <- MaybeT . findOne $ select [ "id" =: tid0 ] "thread"
      t <- MaybeT . return $ cast' (Doc d)
      let t' = t { threadId = tid0
                 , threadCommentIds = Prelude.filter (/= cid) (threadCommentIds t) }
      lift . insert "thread" . doc . val $ t'
      lift $ modify (select [ "id" =: Doc [ "$in" =: categorization t ] ] "category")
        [ "$pull" =: Doc [ "threadIds" =: tid0 ], "$push" =: Doc [ "threadIds" =: tid1 ] ]
      lift $ modify (select [] "trashcan") [ "$push" =: Doc [ "threadIds" =: tid0 ] ]


  saveChange (TrashCategory cid) sid = do
    s <- validateSession sid
    modify (select [ "id" =: cid ] "category") [ "isTrash" =: True ]


  saveChange (UntrashCategory cid) sid = do
    s <- validateSession sid
    modify (select [ "id" =: cid ] "category") [ "isTrash" =: False ]


  saveChange (SplitThread tid0 tid1 tid2 cid) sid = do
    s <- validateSession sid
    n <- now
    void . runMaybeT $ do
      dt <- MaybeT . findOne $ select [ "id" =: tid0 ] "thread"
      t0 <- MaybeT . return $ cast' (Doc dt)
      dc <- MaybeT . findOne $ select [ "id" =: cid ] "comment"
      c  <- MaybeT . return $ cast' (Doc dc)
      let cs0 = threadCommentIds t0
      let cs1 = takeWhile (/= cid) cs0
      cs2 <- MaybeT . return . fmap snd . uncons $ dropWhile (/= cid) cs0
      let t1 = Thread tid1 (threadTitle t0) (sessionUser s) n cs1 (categorization t0) (Just tid0)
      let t2 = Thread tid2 (commentText c) (sessionUser s) n cs2 (categorization t0) (Just tid0)
      lift . insert "thread" . doc . val $ t1
      lift . insert "thread" . doc . val $ t2
      lift $ modify (select [ "id" =: Doc [ "$in" =: val <$> categorization t0 ] ] "category")
        [ "$pull" =: Doc [ "threadIds" =: tid0 ]
        , "$push" =: Doc [ "threadIds" =: Array [ val tid1, val tid2 ] ] ]
      lift $ modify (select [] "trashcan") [ "$push" =: Doc [ "threadIds" =: tid0 ] ]


  saveChange (AddCommentToThread tid cid) sid = do
    s <- validateSession sid
    modify (select [ "id" =:tid ] "thread") [ "$push" =: Doc [ "commentIds" =: cid ] ]


  saveChange (AddCommentRangeToThread tid0 cid0 cidN tid1) sid = do
    s <- validateSession sid
    void . runMaybeT $ do
      d  <- MaybeT . findOne $ select [ "id" =: tid0 ] "thread"
      t0 <- MaybeT . return $ cast' (Doc d)
      let cs = (++ [cidN]) . takeWhile (/= cidN) . dropWhile (/= cid0) $ threadCommentIds t0
      lift $ modify (select [ "id" =: tid1 ] "thread") [ "$push" =: Doc [ "commentIds" =: cs ] ]


  -- TODO remaining saveChange cases


  getDatabase sid = do
    s         <- validateSession sid
    catCur    <- find (select [] "category")
    cats      <- collectMap (fmap CategoryId . U.fromText) parseCategory mempty catCur
    catOrd    <- fromMaybe [] . fmap (fmap (CategoryId)) . join . fmap sequence . fmap (fmap (U.fromText))
                   . join . fmap cast' . join . fmap (!? "categoryOrdering")
                 <$> findOne (select [] "categoryOrdering")
    thCur     <- find (select [] "thread")
    threads   <- collectMap (fmap ThreadId . U.fromText) parseThread mempty thCur
    trashCur  <- find (select [] "trashcan")
    trash     <- collectSet (fmap ThreadId . join . fmap U.fromText . join . fmap cast' . (!? "threadId"))
                 mempty trashCur
    comCur    <- find (select [] "comment")
    comments  <- collectMap (fmap CommentId . U.fromText) parseComment mempty comCur
    lblCur    <- find (select [] "relationLabel")
    labels    <- collectSet parseRelationLabel mempty lblCur
    relCur    <- find (select [] "relation")
    relations <- collectSet parseRelation mempty relCur
    usCur     <- find (select [] "user")
    users     <- collectMap (Just . UserId) parseUser mempty usCur
    return (Zettel cats catOrd threads trash comments labels relations users (Just s))


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
                     return . Just $ Session sid uid c
          _ -> return Nothing


collectMap :: Ord id => (Text -> Maybe id) -> (Document -> Maybe a) -> M.Map id a -> Cursor -> Action IO (M.Map id a)
collectMap toId parse m cur = do
  docs <- nextBatch cur
  let addDoc m d = fromMaybe m $ do
        i <- d !? "id" >>= cast' >>= toId
        x <- parse d
        return (M.insert i x m)
  let m' = foldl addDoc m docs
  if Prelude.null docs then return m' else collectMap toId parse m' cur


collectSet :: Ord a => (Document -> Maybe a) -> S.Set a -> Cursor -> Action IO (S.Set a)
collectSet parse s cur = do
  docs <- nextBatch cur
  let addDoc s d = fromMaybe s $ flip S.insert s <$> parse d
  let s' = foldl addDoc s docs
  if Prelude.null docs then return s' else collectSet parse s' cur


parseRelationLabel :: Document -> Maybe RelationLabel
parseRelationLabel d = parseSymmetric <|> parseAsymmetric
  where parseSymmetric = SymmL . SymmL' <$> d !? "symmetric"
        parseAsymmetric = do
          l <- d !? "left"
          r <- d !? "right"
          return . AsymL $ AsymL' l r


parseRelation :: Document -> Maybe Relation
parseRelation d = parseSymmetric <|> parseAsymmetric
  where parseSymmetric = Symm <$>
          do l <- d !? "label" >>= cast'
             m <- l !? "symmetric"
             f <- d !? "from" >>= cast' >>= U.fromText
             t <- d !? "to" >>= cast' >>= U.fromText
             return (Rel (SymmL' m) (ThreadId f, ThreadId t))

        parseAsymmetric = Asym <$>
          do l <- d !? "label" >>= cast'
             m <- l !? "left"
             n <- l !? "right"
             f <- d !? "from" >>= cast' >>= U.fromText
             t <- d !? "to" >>= cast' >>= U.fromText
             return (Rel (AsymL' m n) (ThreadId f, ThreadId t))


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
  return (Session sid uid c)


validateSession :: SessionId -> Action IO Session
validateSession sid = do
  -- TODO: make sessions expire
  ms <- join . fmap parseSession <$> findOne (select ["id" =: String (U.toText (unSessionId sid))] "session")
  case ms of
    Just s -> return s
    Nothing -> fail "invalid session"
