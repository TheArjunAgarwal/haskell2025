#import "@preview/cetz:0.3.4" 

//length of projection of a vector to a rectangle centred at origin
#let len_of_proj_to_rect((x,y),rect) = {
  let w = rect.width/2cm
  let h = rect.height/2cm
  let t = (w/x,h/y).sorted(key:it=>calc.abs(it)).at(0)
  return calc.sqrt(((x*x)+(y*y))*t*t)
}

#let tree(
  data,
  spread : 0.8,
  grow : 1,
  pad : .1
) = context{$#cetz.canvas({ 
  import cetz : *
  import draw: *
  tree.tree(
    data, 
    spread: spread,     
    grow: grow , 
    draw-node: (node, ..) => {
      content((),node.content)
    }, 
    draw-edge: (.., parent, child) => {
      let coords = it => (it.x, -it.y)
      let p = coords(parent)
      let c = coords(child)
      let diff = p.zip(c).map(it=>(it.at(0)-it.at(1)))
      let p_pad = pad+len_of_proj_to_rect(diff, measure(parent.content))
      let c_pad = pad+len_of_proj_to_rect(diff, measure(child.content))
      if ( 
        type(child.content) != type("") 
        and child.content.has("kind") 
      ) {
        if child.content.kind == "tree_dots" {
          let line_(e) = line(
            (p,0.15+p_pad,(c.at(0) + e, c.at(1))),
            ((c.at(0) + e, c.at(1)),0.05+c_pad,p),
            stroke: (cap: "round", thickness: 0.1pt, )
          )
          return {
            line_(-0.28)
            line_(-0.1)
            line_(0.08)
          }
        }
        if child.content.kind == "tree_far_away" {
          return line(
            (p,p_pad,c),
            (c,c_pad,p),
            stroke: (
              thickness: 2pt, 
              cap : "round",  
              dash: ("dot", 4pt)
            )
          )
        }
      }
      return line(
        (p,p_pad,c),
        (c,c_pad,p),
        stroke: (
          thickness: 0.3pt,
          cap: "round"
        )
      )
    }
  )
})$}

#let far_away = it => {
  figure(
    it,
    kind : "tree_far_away",
    supplement: none
  )
}

#let dots = {
  figure(
    $dot dot dot" "$,
    kind : "tree_dots",
    supplement: none
  )
}

Trees can be spread horizontally . . .

#tree(
  ($f $,
    $x_1$,
    $x_2$,
    $x_3$,
    dots,
    $x_(n-1)$,
    $x_n$
  )
) 

. . . or vertically -

#tree( pad : 2,
  (`(:)`,
    `x1`,
    (`(:)`,
      `x2`,
      (`(:)`,
        `x3`,
        (far_away(`(:)`),
          `xn-1`,
          (`(:)`,
            `xn`,
            `[]`
          )
        )
      )
    )
  )
)
