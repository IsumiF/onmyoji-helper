{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module OnmyojiHelper.Core.Reward.Internal where

import           Control.Lens
import           Control.Monad                  (join)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Data.Foldable                  (find)
import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict                as Map
import           Data.Maybe                     (isJust)
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import qualified Data.Text.Encoding             as Text
import           OnmyojiHelper.Base.Monad       (liftMaybe)
import           OnmyojiHelper.IO.ResourceFiles
import qualified Text.HTML.TagSoup              as TagSoup
import qualified Text.HTML.TagSoup.Tree         as TagSoup
import           Text.Read                      (readMaybe)

data Dungeon = Dungeon
  { _dungeon_name  :: Text -- ^关卡名
  , _dungeon_cost  :: Map Text Int -- ^开销名称->开销数值
  , _dungeon_elves :: Map Text Int -- ^怪物名称->怪物数量
  }

makeLenses ''Dungeon

readRewardData :: MonadResourceFiles m => m Text
readRewardData = do
    content <- readResourceFile "yys_xsfy.html"
    pure (Text.decodeUtf8 content)

parseDungeons :: Text -> Except Text [Dungeon]
parseDungeons src = do
    body <- maybeToExceptT "fail to find body" . liftMaybe $
      findBody (TagSoup.parseTree src)
    contents <- maybeToExceptT "fail to find content in body" . liftMaybe $
      fmap subTrees (findDivClass "content" body)
    case contents of
      _:explore:_ -> maybeToExceptT "can't read explore dungeons" . liftMaybe $
        getExploreDungeons explore
      _           -> except (Left "incorrect content format")

getExploreDungeons :: TagSoup.TagTree Text -> Maybe [Dungeon]
getExploreDungeons root = do
    panelContent <- safeIndex 1 rootSubs
    let subs = subTrees panelContent
    concat <$> traverse getFromExploreChapter subs
  where
    rootSubs = subTrees root

-- |read @<div class="cp">@
getFromExploreChapter :: TagSoup.TagTree Text -> Maybe [Dungeon]
getFromExploreChapter root =
    case rootSubs of
      [] -> Nothing
      headingTag:dungeonTags -> do
        heading <- getInnerText headingTag
        dungeons <- traverse getOneDungeonFromChapter dungeonTags
        let heading' = Text.append heading " "
        Just $ fmap (over dungeon_name (Text.append heading')) dungeons
  where
    rootSubs = subTrees root

-- |read @<tbw clx>@ in explore chapter
getOneDungeonFromChapter :: TagSoup.TagTree Text -> Maybe Dungeon
getOneDungeonFromChapter root =
    case subTrees root of
      [key, value] -> join $ chapterKvToDungeon <$> getInnerText key <*> getInnerText value
      _ -> Nothing

chapterKvToDungeon :: Text -> Text -> Maybe Dungeon
chapterKvToDungeon keyText valueText =
    Dungeon keyText' cost <$> elves
  where
    (valueText', difficulty) = Text.span (/= '（') valueText
    keyText' = keyText <> difficulty
    cost = Map.fromList [("体力", 3), ("时间", 1)]
    elvesStr = Text.splitOn " " valueText'
    parseElf elfStr =
      case Text.split (== '×') elfStr of
        [elfName, elfCountStr] -> do
          elfCount <- readMaybe . Text.unpack $ elfCountStr
          pure (elfName, elfCount :: Int)
        _                      -> Nothing
    elves = Map.fromList <$> traverse parseElf elvesStr

findDivClass :: Text -> TagSoup.TagTree Text -> Maybe (TagSoup.TagTree Text)
findDivClass className tree@(TagSoup.TagBranch name attrs subs) =
    if name == "div" && hasClass className attrs
    then Just tree
    else join . find isJust . fmap (findDivClass className) $ subs
findDivClass _ _ = Nothing

hasClass :: Text -> [TagSoup.Attribute Text] -> Bool
hasClass className attrs =
  case lookup "class" attrs of
    Nothing         -> False
    Just allClasses -> className `elem` Text.words allClasses

findBody :: [TagSoup.TagTree Text] -> Maybe (TagSoup.TagTree Text)
findBody = find (\t -> treeName t == "body")

treeName :: TagSoup.TagTree Text -> Text
treeName (TagSoup.TagBranch name _ _) = name
treeName _                            = ""

subTrees :: TagSoup.TagTree Text -> [TagSoup.TagTree Text]
subTrees (TagSoup.TagBranch _ _ subs) = subs
subTrees _                            = []

safeIndex :: Int -> [a] -> Maybe a
safeIndex _ []     = Nothing
safeIndex n (x:xs) = if n == 0 then Just x else safeIndex (n - 1) xs

getInnerText :: TagSoup.TagTree Text -> Maybe Text
getInnerText node =
    safeIndex 0 subs >>= getText
  where
    subs = subTrees node
    getText :: TagSoup.TagTree Text -> Maybe Text
    getText (TagSoup.TagLeaf tag) =
      case tag of
        TagSoup.TagText txt -> Just txt
        _                   -> Nothing
    getText _ = Nothing
