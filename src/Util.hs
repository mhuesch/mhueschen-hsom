module Util where

answer :: Bool -> String
answer True = "Yes"
answer False = "No"

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs
