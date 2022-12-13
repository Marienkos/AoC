input ← ⊃⎕NGET 'input.txt' 1
data ← ⍎¨¨((×≢¨)⊆⊢)input
⎕ ← ⌈/+/¨data
