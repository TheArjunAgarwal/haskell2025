//--------------------- functions ----------------------

#let targets( reference , supposed_target ) = if reference.has("element") {
  
  if reference.element.has("kind") {
  
    reference.element.kind == supposed_target
  
  } else { false }

} else { false }

#let current( sel , loc ) = context{

  let selections_before_loc = query(sel.before(loc))

  let empty( arr ) = arr.len() == 0

  if empty( selections_before_loc ) {
    none
  } else {
    selections_before_loc.last()
  }
  
}

//--------------------- settings ----------------------

#let settings( user_end_body ) = {
  
  // show figure : it => { align( start , it ) }

  user_end_body

}

