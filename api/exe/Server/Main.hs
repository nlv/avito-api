
module Main where

import Options.Applicative
import Data.Semigroup ((<>))

import App

optParser :: Parser Options
optParser = Options 
        <$> option auto ( long "port"       <> short 'p' <> help "Service port")
        <*> strOption   ( long "dbname"     <> short 'b' <> help "Database name")
        <*> strOption   ( long "dbuser"     <> short 'u' <> help "Database user")
        <*> strOption   ( long "dbpassword" <> short 'w' <> help "Database password")
        <*> strOption   ( long "s3url"      <> short 'l' <> help "S3 url")
        <*> strOption   ( long "s3user"     <> short 'a' <> help "S3 user")
        <*> strOption   ( long "s3skey"     <> short 'k' <> help "S3 secret key")
        <*> strOption   ( long "s3region"   <> short 'r' <> help "S3 region")

main :: IO ()
main = run =<< execParser opts
    where opts = info (optParser <**> helper)
                    ( fullDesc
                    <> progDesc "Avitobot service"
                    <> header "avitobot - avitobot service")
