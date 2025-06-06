// Based on(and mostly copied from) Evan.sty

#import "@preview/gentle-clues:1.2.0": *
// Provides the cute boxes
#import "theorems.typ": *
// Provides the Theorems Environment
#import "@preview/codly:1.3.0": *
#import "@preview/codly-languages:0.1.1": *

#let fonts = (
  text: ("Libertinus Serif", "Noto Serif CJK TC", "Noto Color Emoji"),
  sans: ("Noto Sans", "Noto Sans CJK TC", "Noto Color Emoji"),
  mono: ("Fira Code"),
)

#let haskell(codeText) = [

  #show raw: it => [
    #let colour = color.linear-rgb(30%,50%,10%)
    #let fillColour = color.linear-rgb(30%,50%,10%,15%)
    #box[
      #circle(fill: fillColour,inset : 1pt)[
        #set align(center + horizon) 
        #text(fill : colour, "\u{03BB}")
      ]
    ]
    #text(fill : colour, baseline : -2pt, "code")
    #box(
      fill: fillColour,
      width : 100%,
      inset: 8pt,
      radius: 5pt,
      text(font:fonts.mono,stylistic-set: (3,4,5,6,9), it)
    )
  ]
  
  #raw(
    lang : "haskell",
    theme: "theme.tmTheme",
    codeText.text.trim(">", at : start).replace("\n>","\n")
  )

]


// Current: Evan Chen Defaults
// Todo: Choose a "better" Serif(body), Sans(Headings) and Mono font

#let colors = (
  title: eastern,
  headers: maroon,
  partfill: rgb("#002299"),
  label: red,
  hyperlink: blue,
  strong: rgb("#000055")
)
// Current: Evan Defaults
// Todo: Choose a "better" color list



#let toc = {
  show outline.entry.where(level: 1): it => {
    v(1.2em, weak:true)
    text(weight:"bold", font:fonts.sans, it)
  }
  text(fill:colors.title, size:1.4em, font:fonts.sans, [*Table of contents*])
  v(0.6em)
  outline(
    title: none,
    indent: 2em,
  )
}

#let epigraph(quote, authors) = {
  if type(authors) != array {
    authors = (authors,)
  }
  align(right)[
    #block(
      width: 40%,
      [
        #align(left)[#quote]
        #line(length: 100%)
        #emph[#authors.join("\n")]
      ]
    )
  ]
}


#let eqn(s) = {
  set math.equation(numbering: "(I)")
  s
}
// Yay, Roman Numerals!

#let pageref(label) = context {
  let loc = locate(label)
  let nums = counter(page).at(loc)
  link(loc, "page " + numbering(loc.page-numbering(), ..nums))
}

#let definition(..args) = clue(
  accent-color: _get-accent-color-for("abstract"),
  icon: _get-icon-for("abstract"),
  title: "Definition",
  ..args
)

#let problem(..args) = clue(
  accent-color: _get-accent-color-for("question"),
  icon: _get-icon-for("question"),
  title: "Problem",
  ..args
)
#let exercise(..args) = clue(
  accent-color: _get-accent-color-for("task"),
  icon: _get-icon-for("task"),
  title: "Exercise",
  ..args
)
#let example(..args) = clue(
  accent-color: _get-accent-color-for("example"),
  icon: _get-icon-for("example"),
  title: "Example",
  ..args
)
#let solution(..args) = clue(
  accent-color: _get-accent-color-for("conclusion"),
  icon: _get-icon-for("conclusion"),
  title: "Solution",
  ..args
)
#let remark(..args) = clue(
  accent-color: _get-accent-color-for("info"),
  icon: _get-icon-for("info"),
  title: "Remark",
  ..args
)
#let idea(..args) = clue(
  accent-color: _get-accent-color-for("idea"),
  icon: _get-icon-for("idea"),
  title: "Idea",
  ..args
)


#let code(..args) = clue(
  accent-color: _get-accent-color-for("code"),
  icon: _get-icon-for("code"),
  title: "Code",
  ..args
)


#let side(..args) = clue(
  accent-color: rgb("#bbbbbb"),
  icon: _get-icon-for("quote"),
  title: "Side Note",
  ..args
)

// Theorem environments
#let thm-args = (padding: (x: 0.5em, y: 0.6em), outset: 0.9em, counter: "thm", base-level: 1)
#let thm = thm-plain("Theorem",  fill: rgb("#eeeeff"), ..thm-args)
#let lem = thm-plain("Lemma", fill: rgb("#eeeeff"), ..thm-args)
#let prop = thm-plain("Proposition", fill: rgb("#eeeeff"), ..thm-args)
#let cor = thm-plain("Corollary", fill: rgb("#eeeeff"), ..thm-args)
#let conj = thm-plain("Conjecture", fill: rgb("#eeeeff"), ..thm-args)
#let ex = thm-def("Example", fill: rgb("#ffeeee"), ..thm-args)
#let algo = thm-def("Algorithm", fill: rgb("#ddffdd"), ..thm-args)
#let claim = thm-def("Claim", fill: rgb("#ddffdd"), ..thm-args)
#let rmk = thm-def("Remark", fill: rgb("#eeeeee"), ..thm-args)
#let defn = thm-def("Definition", fill: rgb("#ffffdd"), ..thm-args)
#let prob = thm-def("Problem", fill: rgb("#eeeeee"), ..thm-args)
#let exer = thm-def("Exercise", fill: rgb("#eeeeee"), ..thm-args)
#let exerstar = thm-def("Exercise", fill: rgb("#eeeeee"),
  title-fmt: (x) => { strong(x + " (*)") },
  ..thm-args)
#let ques = thm-def("Question", fill: rgb("#eeeeee"), ..thm-args)
#let fact = thm-def("Fact", fill: rgb("#eeeeee"), ..thm-args)

#let todo = thm-plain("TODO", fill: rgb("#ddaa77"), padding: (x: 0.2em, y: 0.2em), outset: 0.4em).with(numbering: none)
#let proof = thm-proof("Proof")
#let soln = thm-proof("Solution")

// i have no idea how this works but it seems to work ¯\_(ツ)_/¯
// Was a note left by Evan. I will not be making any changes here.
#let recall-thm(target-label) = {
  context {
    let el = query(target-label).first()
    let loc = el.location()
    let thms = query(selector(<meta:thm-env-counter>).after(loc))
    let thmloc = thms.first().location()
    let thm = thm-stored.at(thmloc).last()
    (thm.fmt)(
      thm.name, link(target-label, str(thm.number)), thm.body, ..thm.args.named(),
    )
  }
}

// Some shorthands(add as needed)
#let int = sym.integral
#let pmod(x) = $space (mod #x)$
#let bf(x) = $bold(upright(#x))$
#let boxed(x) = rect(stroke: rgb("#003300") + 1.5pt,
  fill: rgb("#eeffee"),
  inset: 5pt, text(fill: rgb("#000000"), x))

#let url(s) = {
  link(s, text(font:fonts.mono, s))
}

// Ersatz part command (similar to Koma-Script part in scrartcl)
#let part(s) = {
  heading(numbering: none, text(size: 1.4em, fill: colors.partfill, s))
}

// Unnumbered heading commands
#let h1(..args) = heading(level: 1, outlined: false, numbering: none, ..args)
#let h2(..args) = heading(level: 2, outlined: false, numbering: none, ..args)
#let h3(..args) = heading(level: 3, outlined: false, numbering: none, ..args)
#let h4(..args) = heading(level: 4, outlined: false, numbering: none, ..args)
#let h5(..args) = heading(level: 5, outlined: false, numbering: none, ..args)
#let h6(..args) = heading(level: 6, outlined: false, numbering: none, ..args)

// Main entry point to use in a global show rule
#let main(
  title: none,
  author: none,
  subtitle: none,
  date: none,
  maketitle: true,
  report-style: false,
  body
) = {
  // Set document parameters
  if (title != none) {
    set document(title: title)
  }
  if (author != none) {
    set document(author: author)
  }

  // Figures formatting
  show figure.caption: cap => context {
    set text(0.95em)
    block(inset: (x: 5em), [
      #set align(left)
      #text(weight: "bold")[#cap.supplement #cap.counter.display(cap.numbering)]#cap.separator#cap.body
    ])
  }

  // Table formatting
  show figure.where(kind: table): fig => {
    // Auto emphasize the table headers
    show table.cell.where(y: 0): set text(weight: "bold")
    let tableframe(stroke) = (x, y) => (
      left: 0pt,
      right: 0pt,
      top: if y <= 1 { stroke } else { 0pt },
      bottom: stroke,
    )
    set table(
      stroke: tableframe(rgb("#21222c")),
      fill: (_, y) => if (y==0) { rgb("#ffeeff") } else if calc.even(y) { rgb("#eaf2f5") },
    )
    fig
  }

  // Report parameters
  show ref: it => {
    let el = it.element
    if el != none and el.func() == heading and el.level == 1 and it.supplement == auto and report-style {
      ref(it.target, supplement: "Chapter")
    } else {
      it
    }
  }

  // General settings
  set page(
    paper: "a4",
    margin: auto,
    header: context {
      set align(right)
      set text(size:0.8em)
      if (not maketitle or counter(page).get().first() > 1) {
        text(weight:"bold", title)
        if (author != none) {
          h(0.2em)
          sym.dash.em
          h(0.2em)
          text(style:"italic", author)
        }
      }
    },
    numbering: "1",
  )
  set par(
    justify: true
  )
  set text(
    font: fonts.text,
    size: 11pt,
    fallback: false,
  )

  // For bold elements, use sans font
  show strong: set text(font:fonts.sans, size: 0.9em)

  // Theorem environments
  show: thm-rules.with(qed-symbol: $square$)

  // Change quote display
  set quote(block: true)
  show quote: set pad(x:2em, y:0em)
  show quote: it => {
    set text(style:"italic")
    v(-1em)
    it
    v(-0.5em)
  }

  // Indent lists
  set enum(indent: 1em)
  set list(indent: 1em)

  // Section headers
  set heading(numbering: "1.1")
  show heading: it => {
    block([
      #if (it.numbering != none) [
        #text(fill:colors.headers,
          (if (report-style and it.level == 1) {"Chapter " } else { "§" })
          + counter(heading).display()
          + (if (report-style and it.level == 1) { "." } else { "" })
        )
        #h(0.2em)
      ]
      #it.body
      #v(0.4em)
    ])
  }
  show heading: set text(font:fonts.sans, size: 15pt)
  show heading.where(level: 1): set text(size: 20pt)
  show heading.where(level: 2): set text(size: 12pt)

  // Hyperlinks should be pretty
  show link: it => {
    set text(fill:
      if (type(it.dest) == label) { colors.label } else { colors.hyperlink }
    )
    it
  }
  show ref: it => {
    link(it.target, it)
  }

  // Gentle clues default font should be sans
  show: gentle-clues.with(
    title-font: "Noto Sans"
  )

  // Title page, if maketitle is true
  if maketitle {
    v(2.5em)
    set align(center)
    set block(spacing: 2em)
    block(text(fill:colors.title, size:2em, font:fonts.sans, weight:"bold", title))
    if (subtitle != none) {
      block(text(size:1.5em, font:fonts.sans, weight:"bold", subtitle))
    }
    if (author != none) {
      block(smallcaps(text(size:1.7em, author)))
    }
    if (type(date) == datetime) {
      block(text(size:1.2em, date.display("[day] [month repr:long] [year]")))
    }
    else if (date != none) {
      block(text(size:1.2em, date))
    }
    v(1.5em)
  }
  body
}
