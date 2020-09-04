{-# LANGUAGE CPP                  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns         #-}


module Persist where


import           Control.Applicative ((<|>))
import           Control.Monad (join, void, forM_)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Database.MongoDB
import           Data.List (uncons)
import qualified Data.Map as M
import           Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as S
import           Data.Text hiding (foldl, null, find, uncons, takeWhile, dropWhile, filter)
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
    _ <- insert "category" . doc . val $ Category t i [] Nothing False
    modify (select [] "categoryOrdering") [ "$push" =: Doc [ "categoryIds" =: i ] ]


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

  saveChange (NewEdit ic t) sid = do
    s <- validateSession sid
    n <- now
    modify (select [ "id" =: ic ] "comment") [ "$push" =: Doc [ "edits" =: (Edit n t) ] ]

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
      cd <- MaybeT . findOne $ select [ "id" =: cid0 ] "category"
      c  <- MaybeT . return $ cast' (Doc cd)
      lift . insert "category" . doc . val $ c { categoryId = cid1, categoryTitle = txt }
      lift $ modify (select [ "id" =: cid0 ] "category") [ "isTrash" =: True ]
      od <- MaybeT . findOne $ select [] "categoryOrdering"
      o  <- MaybeT . return $ od !? "categoryIds" >>= cast'
      let o' :: [CategoryId] = (\cid -> if cid == cid0 then cid1 else cid) <$> o
      lift $ modify (select [] "categoryOrdering") [ "categoryIds" =: o' ]


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
    void . runMaybeT $ do
      d <- MaybeT . findOne $ select [] "categoryOrdering"
      o <- MaybeT . return $ d !? "categoryIds" >>= cast'
      let o' = filter (/= cid) o
      lift $ modify (select [] "categoryOrdering") [ "categoryIds" =: o' ]


  saveChange (UntrashCategory cid) sid = do
    s <- validateSession sid
    modify (select [ "id" =: cid ] "category") [ "isTrash" =: False ]
    void . runMaybeT $ do
      d <- MaybeT . findOne $ select [] "categoryOrdering"
      o <- MaybeT . return $ d !? "categoryIds" >>= cast'
      let o' = o ++ [cid]
      lift $ modify (select [] "categoryOrdering") [ "categoryIds" =: o' ]


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


  saveChange (MoveCategory cid i) sid = do
    s <- validateSession sid
    void . runMaybeT $ do
      d <- MaybeT . findOne $ select [] "categoryOrdering"
      o <- MaybeT . return $ d !? "categoryIds" >>= cast'
      let o' :: [CategoryId] = insertAt i cid $ filter (/= cid) o
      lift $ modify (select [] "categoryOrdering") [ "categoryIds" =: o' ]


  saveChange (MoveComment tid cid i) sid = do
    s <- validateSession sid
    void . runMaybeT $ do
      d <- MaybeT . findOne $ select [ "id" =: tid ] "thread"
      t <- MaybeT . return $ cast' (Doc d)
      let cs = insertAt i cid $ filter (/= cid) (threadCommentIds t)
      lift $ modify (select [ "id" =: tid ] "thread") [ "commentIds" =: cs ]


  saveChange (MoveThread cid tid i) sid = do
    s <- validateSession sid
    void . runMaybeT $ do
      d <- MaybeT . findOne $ select [ "id" =: cid ] "category"
      c <- MaybeT . return $ cast' (Doc d)
      let ts = insertAt i tid $ filter (/= tid) (categoryThreadIds c)
      lift $ modify (select [ "id" =: cid ] "category") [ "threadIds" =: ts ]


  saveChange (NewRelationLabel l) sid = do
    s <- validateSession sid
    void . insert "relationLabel" . doc $ val l


  saveChange (DeleteRelationLabel l) sid = do
    s <- validateSession sid
    delete $ select (doc (val l)) "relationLabel"


  saveChange (NewRelation r) sid = do
    s <- validateSession sid
    void . insert "relation" . doc $ val r


  saveChange (DeleteRelation r) sid = do
    s <- validateSession sid
    delete $ select (doc (val r)) "relation"


  saveChange (ComposedChanges cs) sid = forM_ cs (\c -> saveChange c sid)


  getDatabase sid = do
    s         <- validateSession sid
    catCur    <- find (select [] "category")
    cats      <- collectMap (fmap CategoryId . U.fromText) parseCategory mempty catCur
    catOrd    <- fromMaybe [] . fmap (fmap (CategoryId)) . join . fmap sequence . fmap (fmap (U.fromText))
                   . join . fmap cast' . join . fmap (!? "categoryIds")
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
    n   <- now
    case mu of
      Nothing -> return Nothing
      Just u -> do
        case PasswordHash <$> (u !? "pwhash" >>= cast') of
          Just h | h == p -> do
                     insert "session"
                       [ "id" =: sid
                       , "user" =: uid
                       , "created" =: dayToGreg n ]
                     return . Just $ Session sid uid n
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
parseCategory = cast' . Doc

parseUser :: Document -> Maybe UserProfile
parseUser = cast' . Doc

parseComment :: Document -> Maybe Comment
parseComment = cast' . Doc

parseEdit :: Document -> Maybe Edit
parseEdit = cast' . Doc

parseThread :: Document -> Maybe Thread
parseThread = cast' . Doc

parseSession :: Document -> Maybe Session
parseSession = cast' . Doc

validateSession :: SessionId -> Action IO Session
validateSession sid = do
  -- TODO: make sessions expire
  ms <- join . fmap parseSession <$> findOne (select ["id" =: String (U.toText (unSessionId sid))] "session")
  case ms of
    Just s -> return s
    Nothing -> fail "invalid session"
