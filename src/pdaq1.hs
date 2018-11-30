-- <https://ca320.computing.dcu.ie/pda.html> Link to Assignment.

type Configuration = (Int, String, String)
type Transition = ((Int, String, String), (Int, String))
type PDA = (Int, [Int], [Transition])
data Result = Accept | Reject deriving Show

run :: PDA -> String -> Result
run (ss, fs, tr) "" = Accept
run (ss, fs, tr) xs = runp (ss, fs, tr) [(ss, xs, "")]


runp :: PDA -> [Configuration] -> Result
runp (ss, fs, tr) [] = Reject
runp (ss, fs, tr) (x:xs)
	| endState x fs == True = Accept
	| otherwise = runp (ss, fs, tr) (xs ++ (getTransitions x tr []))

getTransitions :: Configuration -> [Transition] -> [Configuration] -> [Configuration]
getTransitions (ss, xs, st) [] cf = cf
getTransitions (ss, "", st) tr cf = cf
getTransitions (ss, xs, st) (t:tr) cf = getTransitions (ss, xs, st) tr (cf ++ (step (ss, xs, st) t))

step :: Configuration -> Transition -> [Configuration]

step (a, (b:bs), "") ((d, "", ""), (g, ""))
	| a == d = [(g, (b:bs), "")]
	| otherwise = []

step (a, (b:bs), "") ((d, "", ""), (g, [h]))
	| a == d = [(g, (b:bs), [h])]
	| otherwise = []

step (a, (b:bs), "") ((d, [e], ""), (g, ""))
	| a == d && b == e = [(g, bs, "")]
	| otherwise = []

step (a, (b:bs), "") ((d, [e], ""), (g, [h]))
	| a == d && b == e = [(g, bs, [h])]
	| otherwise = []

step (a, (b:bs), (c:cs)) ((d, "", ""), (g, ""))
	| a == d = [(g, (b:bs), (c:cs))]
	| otherwise = []

step (a, (b:bs), (c:cs)) ((d, "", ""), (g, [h]))
	| a == d= [(g, (b:bs), (h:c:cs))]
	| otherwise = []

step (a, (b:bs), (c:cs)) ((d, "", [f]), (g, ""))
	| a == d && c == f = [(g, (b:bs), cs)]
	| otherwise = []

step (a, (b:bs), (c:cs)) ((d, "", [f]), (g, [h]))
	| a == d && c == f = [(g, (b:bs), (h:cs))]
	| otherwise = []

step (a, (b:bs), (c:cs)) ((d, [e], ""), (g, ""))
	| a == d && b == e = [(g, bs, (c:cs))]
	| otherwise = []

step (a, (b:bs), (c:cs)) ((d, [e], ""), (g, [h]))
	| a == d && b == e = [(g, bs, (h:c:cs))]
	| otherwise = []

step (a, (b:bs), (c:cs)) ((d, [e], [f]), (g, ""))
	| a == d && b == e && c == f = [(g, bs, cs)]
	| otherwise = []

step (a, (b:bs), (c:cs)) ((d, [e], [f]), (g, [h]))
	| a == d && b == e && c == f = [(g, bs, (h:cs))]
	| otherwise = []



step (a, (b:bs), "") ((d, "", [f]), (g, "")) = []
step (a, (b:bs), "") ((d, "", [f]), (g, [h])) = []
step (a, (b:bs), "") ((d, [e], [f]), (g, "")) = []
step (a, (b:bs), "") ((d, [e], [f]), (g, [h])) = []

endState :: Configuration -> [Int] -> Bool
endState (ss, xs, st) [] = False
endState (ss, xs, st) (f:fs)
	| ss == f && xs == "" && st == "" = True
	| otherwise = endState (ss, xs, st) fs
