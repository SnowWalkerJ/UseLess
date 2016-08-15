module Data.Crawler where
    import Data.OCTable
    import Control.Monad.Trans
    data Crawler a b = Crawler{
        entryPoint::a,
        webHandler::OCTableT a IO b,
        dataHandler::b->IO ()
    }
    runCrawler::Crawler a b->IO ()
    runCrawler crawler = untilExhaustedT [(entryPoint crawler)] $ do
        x <- webHandler crawler
        lift $ dataHandler crawler x