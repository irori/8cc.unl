require './bfs'

# Church numbers
CNTBL = [
  "`ki",
  "i",
  "``s``s`kski",
  "```ss``ss`ki``s`ksk",
  "``ci``s``s`kski",
  "``s``s`ksk``ci``s``s`kski",
  "```ss``ss`k``ci``s``s`kski``s`ksk",
  "```ss``ss``ss`k``ci``s``s`kski``s`ksk",
  "```s``s`ksk`ci``s``s`kski",
  "````ss`ss``ss`ki``s`ksk",
  "```ss```ss`ss``ss`ki``s`ksk", # 10
  "```ss``ss```ss`ss``ss`ki``s`ksk",
  "```ss``ss``ss```ss`ss``ss`ki``s`ksk",
  "```ss``ss``ss``ss```ss`ss``ss`ki``s`ksk",
  "```ss``ss``ss``ss``ss```ss`ss``ss`ki``s`ksk",
  "```ss``ss``ss``ss``ss``ss```ss`ss``ss`ki``s`ksk",
  "```s`cii``s``s`kski",
  "``s``s`ksk```s`cii``s``s`kski",
  "```ss``ss`k```s`cii``s``s`kski``s`ksk",
  "```ss``ss``ss`k```s`cii``s``s`kski``s`ksk",
  "````sss``s`ksk``ci``s``s`kski", # 20
  "```ss``s``sss`k``ci``s``s`kski``s`ksk",
  "```ss``ss``s``sss`k``ci``s``s`kski``s`ksk",
  "```ss``ss``ss``s``sss`k``ci``s``s`kski``s`ksk",
  "```s``si``s`ci`k`s``s`ksk``s`cii``s``s`kski",
]
CNTBL[256] = "``ci``ci``s``s`kski"

class UnlAsmBase < BFAsm
  S2 = "``s"
  K1 = "`k"
  CONS = "``s``s`ks``s`kk``s`ks``s`k`sik`kk"
  CAR = "``si`kk"
  CDR = "``si`k`ki"
  DOTCAR = "k"   # (x DOTCAR) = (CAR x)
  DOTCDR = "`ki" # (x DOTCDR) = (CDR x)

  # (CONS_KI x) = (cons (K I) x)
  CONS_KI = "``s`k`s``si`k`kik"

  # (CONS_K x) = (cons K x)
  CONS_K = "``s`k`s``si`kkk"

  # (APPLY_CDR f lst) = (cons (car lst) (f (cdr lst)))
  APPLY_CDR = "``s`k`si``s`kk``s`k`s``s`ks``s`kk``s`ks``s`k`sik``s`kk`s`kk"

  # (REPLACE_CAR x lst) = (cons x (cdr lst))
  REPLACE_CAR = "``s`k`si``s`kk``s`kk``s`k`s``s`k`s``s`ks``s`k`sik``s`kkkk"

  # ([REPLACE_CAR1 + <expr>] lst) = (cons <expr> (cdr lst))
  REPLACE_CAR1 = "``si`k`k``s``s`k`s``s`ks``s`k`sik``s`kkk`k"

  # (COMPOSE f g x) = (f (g x))
  COMPOSE = "``s`ksk"

  # (n POST_INC) = n+1
  POST_INC = "```sii``s``s`ks``s`k`si``s`kk``s`k`s`k``s`k`s``si`k`kik``s`k`si``s``s`kskk`k`k``s`k`s``si`kkk"

  # (n POST_DEC) = n-1
  POST_DEC = "```sii``s`k`s``si`k``s`k`s``si`k`kik``s`kk``s`k`s`k``s`k`s``si`kkk``s`k`si``s``s`kskk"

  def regpos(r)
    {:pc => 0, :a => 1, :b => 2, :c => 3, :d => 4, :bp => 5, :sp => 6}[r] or
      raise "unknown reg: #{r}"
  end
end
