module Main where

import Data.List
import Data.Map
import Data.String.Utils
import Text.Read
import Data.Maybe
import Data.Char

newtype Register = Register Int
newtype Code = Code Int

newtype Value = Value Int deriving (Eq, Show)
newtype Raw = Raw Int deriving (Eq, Show)

unwrapValue (Value v) =
	v

type Stack = [Raw]
type Registers = Map Int Value

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
		|> Data.List.map Raw


parseInt :: String -> Maybe Int
parseInt c =
	Text.Read.readMaybe c :: Maybe Int


next :: Registers -> Stack -> IO ()
next registers stack =
	case stack of
		[] ->
			return ()
		(Raw x:xs) ->
			runInstruction registers (Code x) xs


runInstruction :: Registers -> Code -> Stack -> IO ()
runInstruction registers code stack =
	case code of
		Code 0 ->
			return ()
		Code 1 ->
			doSet registers stack
		Code 4 ->
			doEq registers stack
		Code 9 ->
			doAdd registers stack
		Code 19 ->
			doWriteAscii registers stack
		_ ->
			print registers


rawToValue :: Registers -> Raw -> Value
rawToValue registers (Raw n) =
	(rawToRegister (Raw n)
		>>= (registerValue registers)
		)
		|> fromMaybe (Value n)


rawToRegister :: Raw -> Maybe Register
rawToRegister (Raw n) =
	if n >= 32768 && n <= 32775 then
		Just (Register (n - 32768))
	else
		Nothing


registerValue :: Registers -> Register -> Maybe Value
registerValue registers (Register regNum) =
	Data.Map.lookup regNum registers


withRegister :: Raw -> Registers -> Maybe (Register, Value)
withRegister n registers =
	case rawToRegister n of
		Just reg ->
			Just (reg, registerValue registers reg |> fromMaybe (Value 0))

		Nothing ->
			Nothing


-- set: 1 a b
-- set register <a> to the value of <b>
doSet :: Registers -> Stack -> IO ()
doSet reg stack =
	return ()

-- eq: 4 a b c
-- set <a> to 1 if <b> is equal to <c>; set it to 0 otherwise
doEq :: Registers -> Stack -> IO ()
doEq reg stack =
	case stack of
		(a:b:c:rest) ->
			case withRegister a reg of
				Just (Register regNum, _) ->
					let
						newValue =
							if rawToValue reg b == rawToValue reg c then
								Value 1
							else
								Value 0
						newReg =
							Data.Map.insert
								regNum
								newValue
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
		(a:Raw b:Raw c:rest) ->
			case withRegister a reg of
				Just (Register regNum, _) ->
					let
						newReg =
							Data.Map.insert
								regNum
								(Value (mod (b + c) 32768))
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
			rawToValue reg a
				|> unwrapValue
				|> Data.Char.chr
				|> print
				>>= (\_ -> next reg rest)
		_ ->
			return ()