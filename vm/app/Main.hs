module Main where

import Data.List
import Data.Map
import Data.String.Utils
import Text.Read
import Data.Maybe
import Data.Char
-- import Debug.Trace

type Stack = [Int]
type Registers = Map Int Int
newtype Register = Register Int
newtype InsCode = InsCode Int

(|>) x f = f x

main :: IO ()
main =
	run
		"9,32768,32769,4,19,32768"
		(Data.Map.fromList [])


run :: String -> Registers -> IO ()
run input registers =
	input
		|> inputToStack
		|> next registers
		-- |> sequence


inputToStack :: String -> Stack
inputToStack input =
	input
		|> Data.String.Utils.split ","
		|> Data.List.map parseInt
		|> Data.Maybe.catMaybes


parseInt :: String -> Maybe Int
parseInt c =
	Text.Read.readMaybe c :: Maybe Int


next :: Registers -> Stack -> IO ()
next registers stack =
	case stack of
		[] ->
			return ()
		(x:xs) ->
			runInstruction registers (InsCode x) xs


runInstruction :: Registers -> InsCode -> Stack -> IO ()
runInstruction registers code stack =
	case code of
		InsCode 4 ->
			doEq registers stack
		InsCode 9 ->
			doAdd registers stack
		InsCode 19 ->
			doWriteAscii registers stack
		_ ->
			print registers
			-- return ()


getValue :: Registers -> Int -> Int
getValue reg n =
	(numToRegister n
		>>= (\r -> Data.Map.lookup r reg)
		)
		|> fromMaybe n


numToRegister :: Int -> Maybe Int
numToRegister n =
	if n >= 32768 && n <= 32775 then
		Just (n - 32768)
	else
		Nothing


withRegister :: Int -> Registers -> Maybe (Register, Int)
withRegister n reg =
	case numToRegister n of
		Just regNum ->
			Just (Register regNum, getValue reg regNum)

		Nothing ->
			Nothing

-- eq: 4 a b c
-- set <a> to 1 if <b> is equal to <c>; set it to 0 otherwise
doEq :: Registers -> Stack -> IO ()
doEq reg stack =
	case stack of
		(a:b:c:rest) ->
			case withRegister a reg of
				Just (Register regNum, value) ->
					let
						value =
							if getValue reg b == getValue reg c then
								1
							else
								0
						newReg =
							Data.Map.insert
								regNum
								value
								reg
					in
						next newReg rest

				Nothing ->
					return ()
		_ ->
			return ()

-- add: 9 a b c
-- assign into <a> the sum of <b> and <c> (modulo 32768)
doAdd :: Registers -> Stack -> IO ()
doAdd reg stack =
	case stack of
		(a:b:c:rest) ->
			case withRegister a reg of
				Just (Register regNum, value) ->
					let
						newReg =
							Data.Map.insert
								regNum
								(mod (b + c) 32768)
								reg
					in
						next
							newReg
							rest
				Nothing ->
					return ()

		_ ->
			return ()

-- out: 19 a
-- write the character represented by ascii code <a> to the terminal
doWriteAscii :: Registers -> Stack -> IO ()
doWriteAscii reg stack =
	case stack of
		(a:rest) ->
			getValue reg a
				|> Data.Char.chr
				|> print
				>>= (\_ -> next reg rest)
		_ ->
			return ()