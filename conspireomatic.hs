import System.Random (randomRIO)
import System.Time
import Text.Atom.Feed
import Text.Atom.Feed.Export
import qualified Text.Feed.Types as T 
import Text.Feed.Util
import Text.XML.Light.Output

main :: IO ()
main = feed >>= putStr

-- Generate and pretty print our feed. Not sure if using Text.XML.Light.Output functions
-- is the right way to do this. But it works OK.
feed = do 
    f <- makeFeed
    return . ppTopElement $ xmlFeed f

-- Pick a random element from a list
pickFrom :: [a] -> IO a
pickFrom xs = randomRIO (0, length xs - 1) >>= return . (xs !!)

-- Date feed published (i.e. now-ish)
feedDate :: IO String
feedDate = do
    now <- getClockTime
    return $ toFeedDateString T.AtomKind now 

-- The Person who wrote our feed
feedAuthor :: IO Person
feedAuthor = do 
    return (Person { personName = "Conspiracy Jen"
                   , personURI = Nothing
									 , personEmail = Nothing 
									 , personOther = []
									 })

-- Make feed with a single conspiracy in it
makeFeed :: IO Feed
makeFeed = do
    p <- feedAuthor
    d <- feedDate
    e <- conspiracyEntry
    return ( Feed   { feedId = "Conspire-o-matic"
                    , feedTitle = TextString "Definitely true conspiracies: The latest"
								    , feedUpdated = d
								    , feedAuthors = [p]
								    , feedCategories = []
								    , feedContributors = []
								    , feedGenerator = Nothing
								    , feedIcon     = Nothing
								    , feedLinks    = []
								    , feedLogo     = Nothing
								    , feedRights   = Nothing
								    , feedSubtitle = Just $ HTMLString "The truth <em>they</em> don't want you to know"
								    , feedEntries  = [e]
								    , feedAttrs    = []
								    , feedOther    = []
                    })

-- Generate a conspiracy entry
conspiracyEntry :: IO Entry
conspiracyEntry = do 
      p <- feedAuthor
      d <- feedDate
      c <- conspiracy
      return (Entry   { entryId = c
										  , entryTitle = TextString c
										  , entryUpdated = d
										  , entryAuthors = [p]
										  , entryCategories = []
										  , entryContent = Just $ TextContent c
										  , entryContributor = []
										  , entryLinks = []
										  , entryPublished = Just d
										  , entryRights = Nothing
										  , entrySource = Nothing
										  , entrySummary = Just $ TextString c
										  , entryInReplyTo = Nothing
										  , entryInReplyTotal = Nothing
										  , entryAttrs = []
										  , entryOther = []
										  })

-- Return a single conspiracy
conspiracy :: IO String
conspiracy = do  
			u <- pickFrom authorities
			c <- pickFrom actors
			t <- pickFrom actions 
			return $ u ++ " says that " ++ c ++ " " ++ t ++ "\n"  

-- This is unused but generates all possibe conspiracies (or does it?)
conspiracies :: [String]
conspiracies = [ u ++ " says that " ++ c ++ " " ++ t ++ "\n" | u<-authorities
																										         , c<-actors
																										         , t<-actions ]

-- Totally reliable sources of conspiracies AKA the goodies
authorities = [ "A scientist in an unrelated field"
							, "A website I read"
							, "Jim Marrs"
							, "John Lear"
							, "L. Ron Hubbard"
							, "Steven M. Greer"
							, "The internet"
							, "This book I've been reading"
							, "This program on YouTube"
						  , "Overwhelming evidence"
              , "David Icke"
              , "This guy I met in the pub"
							]

-- The baddies
actors      = [ "'they'"
							, "Aliens"
							, "BP"
							, "Bill Gates"
							, "Communists"
							, "Fascists"
							, "Ipswich Town Council"
							, "Le Cercle"
							, "Microsoft"
							, "Monsanto"
							, "Opus Dei"
							, "Tavistock Institute of Human Relations"
							, "World Bankers"
							, "Zionists"
							, "atheists"
							, "climate scientists"
							, "muslims"
							, "oil companies"
							, "our secret overlords"
							, "shapeshifting lizards"
							, "the Bildeberg Group"
							, "the Black Dragon Society"
							, "the CIA"
							, "the Jews"
							, "the Lizard People"
							, "the Mafia"
							, "the New World Order"
							, "the World Bank"
							, "the World Government"
							, "the drug companies"
							, "the government"
							, "the reptilian elite"
              , "Scientologists"
              , "the Freemasons"
              , "the Illuminati"
							]

-- Naughty things done by the baddies
actions     = [  "already landed on Mars"
							 , "assassinated Kennedy"
							 , "control our minds with television"
							 , "created AIDS"
							 , "faked Saddam Hussein's death to cover up the effects of flouride in the water"
							 , "faked the Holocaust"
							 , "faked the Middle Ages in a huge phantom time conspiracy"
							 , "faked the Moon landings"
							 , "have Elvis working with them in a secret base under a mountain"
							 , "introduced HIV"
							 , "invaded the Bay of Pigs to cover up something bigger"
							 , "invented global warming to stop us asking questions about 9/11"
							 , "invented heroin to stop us asking questions"
							 , "keep telling us the Paul McCartney is still alive when we know he died in 1966"
							 , "killed Princess Diana"
							 , "knew about Roswell all along"
							 , "know about an everlasting lightbulb, but won't tell us about it because they are afraid the economy would collapse"
							 , "made marijuana illegal to help the paper industry"
							 , "planned Pearl Harbour to justify a land war in Europe"
							 , "planned the Lockerbie bombing so Thatcher could introduce the Poll Tax"
							 , "put flouride in the water"
							 , "spray chemtrails across the sky to keep us docile"
							 , "spray us with Chemtrails to hide the fact that Tupac is still alive"
							 , "use subliminal advertising to bend us to their will"
							 , "used technology from Area 51 to invent computers"
							 , "wrote all of Shakespeare's plays"
               , "started the second world war"
							 , "caused 9/11"
							 ]
