import Data.List (find, nub)
import Data.Maybe (catMaybes)
-- Define data types for publications
data PublicationType = Book | Article | ConferencePaper deriving (Eq, Show)
data Publication = Publication {
  pubType :: PublicationType,
  pubAuthors :: [String],
  pubTitle :: String,
  pubCity :: Maybe String,
  pubPublisher :: Maybe String,
  pubJournal :: Maybe String,
  pubYear :: Maybe Int,
  pubJournalIssue :: Maybe Int,
  pubPages :: Maybe String,
  pubConference :: Maybe String
} deriving (Eq, Show)

-- Define a sample database of publications
publications :: [Publication]
publications = [
  Publication Book ["John Doe"] "My Book" (Just "New York") (Just "ABC Publishing") (Nothing) (Just 2021) Nothing Nothing Nothing,
  Publication Article ["John Doe", "Jane Smith"] "My Article" (Nothing) (Nothing) (Just "Journal of Science") (Just 2020) (Just 1) (Just "10-20") Nothing,
  Publication ConferencePaper ["Jane Smith"] "My Conference Paper" (Just "Paris") (Nothing) (Nothing) (Just 2019) Nothing (Just "100-110") (Just "Conference on Research")
 ]

-- Define functions to perform operations on the database

-- 2.1 Determine the type of a publication given its title
getTypeByTitle :: String -> [Publication] -> Maybe PublicationType
getTypeByTitle title pubs = pubType <$> find (\p -> pubTitle p == title) pubs

-- 2.2 Find all publications of a given author
getPublicationsByAuthor :: String -> [Publication] -> [Publication]
getPublicationsByAuthor author pubs = filter (\p -> elem author (pubAuthors p)) pubs

-- 2.3 Find all publications of a given author where they are the only author
getSoloPublicationsByAuthor :: String -> [Publication] -> [Publication]
getSoloPublicationsByAuthor author pubs = filter (\p -> length (pubAuthors p) == 1 && head (pubAuthors p) == author) pubs

-- 2.4 Find all unique publishers, journals, and conference names in the database
getPublishers :: [Publication] -> [String]
getPublishers pubs = nub $ catMaybes (pubPublisher <$> pubs)

getJournals :: [Publication] -> [String]
getJournals pubs = nub $ catMaybes (pubJournal <$> pubs)

getConferences :: [Publication] -> [String]
getConferences pubs = nub $ catMaybes (pubConference <$> pubs)

-- 2.5 Get statistics on the number of publications of each type in the database
getPublicationTypeCounts :: [Publication] -> [(PublicationType, Int)]
getPublicationTypeCounts pubs = [(t, length $ filter (\p -> pubType p == t) pubs) | t <- [Book, Article, ConferencePaper]]

main = do 
    -- Determine the type of a publication given its title
    print(getTypeByTitle "My Article" publications) 
    -- Find all publications of a given author
    print(getPublicationsByAuthor "John Doe" publications)
    print(getPublicationTypeCounts publications)
