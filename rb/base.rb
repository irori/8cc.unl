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
  CONS_KI = "``s`k`s``si`k`kik"
  CONS_K = "``s`k`s``si`kkk"
  CAR = "``si`kk"
  CDR = "``si`k`ki"
  DOTCAR = "k"
  DOTCDR = "`ki"
  APPLY_CDR = "``s`k`si``s`kk``s`k`s``s`ks``s`kk``s`ks``s`k`sik``s`kk`s`kk"
  REPLACE_CAR_FLIP = "``s`k`si``s`kk``s`kk``s`k`s``s`k`s``s`ks``s`k`sik``s`kkkk"
  REPLACE_CAR = "``s`k`s``s``s`ks``s`kk``s`ks``s`k`sik`kk``s``s`ksk`k`k`ki"
  CONS = "``s``s`ks``s`kk``s`ks``s`k`sik`kk"
  COMPOSE = "``s`ksk"

  def regpos(r)
    {:pc => 0, :a => 1, :b => 2, :c => 3, :d => 4, :bp => 5, :sp => 6}[r] or
      raise "unknown reg: #{r}"
  end
end
