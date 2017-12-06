
{-# LANGUAGE GADTs, ScopedTypeVariables, BangPatterns #-}

module Main where

import System.Environment (getArgs)
import Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec (parse)
import Text.Megaparsec.Char.Lexer as L hiding (space, lexeme)
import qualified Text.Megaparsec.Char.Lexer as L (space, lexeme)
import Text.Megaparsec.Char (spaceChar, string, alphaNumChar)
import Control.Applicative (empty)
import Control.Monad (when)
import Data.Void


-- Data types:

type ParseErr = Void
type Parser = Parsec ParseErr String

type X = Float
type Y = Float
type Z = Float
data Vertex = Vertex !X !Y !Z deriving (Eq, Show)
type Normal = Vertex
data Facet = Facet !Normal !(Vertex, Vertex, Vertex) deriving (Eq, Show)
type SolidName = String
data Solid = Solid SolidName [Facet] deriving (Eq, Show)


-- Functions:

parse :: Parsec e s a -> s -> Either (ParseError (Token s) e) a
parse parser stream = Text.Megaparsec.parse parser "" stream

parseSolid :: Parser Solid
parseSolid = do
    keyword "solid"
    maybeName <- lexeme . optional $ many alphaNumChar
    facets <- many parseFacet
    keyword "endsolid"
    case maybeName of
      Just name | name /= "" -> do
        keyword name
        return $ Solid name facets
      _ -> return $ Solid "" facets

parseFacet :: Parser Facet
parseFacet = lexeme $ do
  keyword "facet normal"
  (x, y, z) <- parseXYZ
  let normal = Vertex x y z
  keyword "outer loop"
  vertex1 <- parseVertex
  vertex2 <- parseVertex
  vertex3 <- parseVertex
  keyword "endloop"
  keyword "endfacet"
  return $ Facet normal (vertex1, vertex2, vertex3)

parseVertex :: Parser Vertex
parseVertex = lexeme $ do
  keyword "vertex"
  (x, y, z) <- parseXYZ
  return $ Vertex x y z

parseXYZ :: Parser (X, Y, Z)
parseXYZ = lexeme $ do
  x <- lexeme signedFloat
  y <- lexeme signedFloat
  z <- lexeme signedFloat
  return (x, y, z)


-- Helper functions:

lexeme :: (Token s ~ Char, MonadParsec e s m) => m a -> m a
lexeme = L.lexeme whitespace

whitespace :: (Token s ~ Char, MonadParsec e s m) => m ()
whitespace = L.space spaceParser commentParser blockCommentParser where
  spaceParser = spaceConsumer
  commentParser = empty
  blockCommentParser = empty

spaceConsumer :: (Token s ~ Char, MonadParsec e s m) => m ()
spaceConsumer = spaceChar >> return ()

signedFloat :: (Token s ~ Char, MonadParsec e s m) => m Float
signedFloat = signed (maybeSpaceConsumer >> return ()) $ lexeme float
  where maybeSpaceConsumer = optional spaceConsumer

keyword :: (Token s ~ Char, Tokens s ~ String, MonadParsec e s m) => String -> m ()
keyword s = lexeme $ string s >> return ()


-- Main function:

main :: IO ()
main = do
  args <- getArgs
  when (length args >= 1) $ do
    let (stlFile:_) = args
    contents <- readFile stlFile
    case parse parseSolid contents of
        Left err -> do
          putStrLn "Failed to parse STL file:"
          putStrLn $ parseErrorPretty err
        Right solid -> do
          putStrLn "Parsed STL file:"
          putStrLn $ show solid
