#let proof( thm: none , body ) = block(width: 100%)[
  #if thm != none [
    #box(fill :purple.mix(white), inset: 0.2em, baseline: 0.2em, radius: 0.2em)[*Theorem*] #thm
  ] \
    #box(fill :eastern.mix(yellow), inset: 0.2em, baseline: 0.2em, radius: 0.2em)[*Proof*] 
    #body
    #place(bottom + right)[#h(1fr) $qed$]

]

#proof[
  I am writing a proof over here.
]

#proof( thm: [This is a theorem statement.] )[
  Here is its proof.
]
