input ← ⊃⎕NGET 'input.txt' 1
i ← 'A X' 'A Y' 'A Z' 'B X' 'B Y' 'B Z' 'C X' 'C Y' 'C Z'
p ← 3 4 8 1 5 9 2 6 7
⎕ ← +/p[i⍸input]
