#!/usr/bin/env ruby

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

class UnlAsm < BFAsm
  def regpos(r)
    {:pc => 0, :a => 1, :b => 2, :c => 3, :d => 4, :bp => 5, :sp => 6}[r] or
      raise "unknown reg: #{r}"
  end

  def generate_list(elems)
    code = []
    elems.each {|elem| code.push("``s``si`k", elem, "`k")}
    code.pop
    code.push("v")
  end

  def le_number(n)
    generate_list((0...BITS).map{|b| (n & 1 << b).zero? ? "`ki" : "k"})
  end

  CONS_KI = "``s`k`s``si`k`kik"
  CONS_K = "``s`k`s``si`kkk"
  def le_number2(n)
    code = []
    (0..BITS).each do |b|
      if b + 1 < BITS && (1 << b) > n
        code.push("``", churchnum(BITS - b), CONS_KI)
        break
      elsif b + 1 < BITS && (1 << BITS) - 1 == ((1 << b - 1) | n)
        code.push("``", churchnum(BITS - b), CONS_K)
        break
      else
        code.push("``s``si`k", (n & 1 << b) == 0 ? "`ki" : "k")
        code.push("`k") if b + 1 < BITS
      end
    end
    code.push("v")
  end

  def le_0
    cap(churchnum(BITS), cap(CONS, "`ki"), "v")
  end

  def churchnum(n)
    CNTBL[n] or raise "churchnum(#{n}) is not in the table"
  end

  def cap(*args)
    ["`" * (args.size - 1), *args]
  end

  def ap(f, x)
    [:lambda, f, x]
  end

  def ap2(f, x, y)
    [:lambda, [:lambda, f, x], y]
  end

  def ap3(f, x, y, z)
    [:lambda, [:lambda, [:lambda, f, x], y], z]
  end

  def bit_not(x)
    ap2(x, "`ki", "k")
  end

  def lambda?(e)
    e == :var || e[0] == :lambda
  end

  def unlambda(e)
    if e == :var
      "i"
    elsif e[0] == :lambda
      if e[2] == :var && !lambda?(e[1])
        e[1]
      else
        ["``s", unlambda(e[1]), unlambda(e[2])]
      end
    else
      ["`k", e]
    end
  end

  CAR = "``si`kk"
  CDR = "``si`k`ki"
  DOTCAR = "k"
  DOTCDR = "`ki"

  def nth(n, lst)
    # TODO: optimize for small |n|
    ap(ap(cap(churchnum(n), CDR), lst), DOTCAR)
  end

  APPLY_CDR = "``s`k`si``s`kk``s`k`s``s`ks``s`kk``s`ks``s`k`sik``s`kk`s`kk"
  REPLACE_CAR_FLIP = "``s`k`si``s`kk``s`kk``s`k`s``s`k`s``s`ks``s`k`sik``s`kkkk"
  def replace_nth(val, n)
    if lambda?(val)
      ap(cap(churchnum(n), APPLY_CDR), ap(REPLACE_CAR_FLIP, val))
    else
      # This returns constant expr, not lambda!
      cap(churchnum(n), APPLY_CDR,
          ["``si`k`k``s``s`k`s``s`ks``s`k`sik``s`kkk`k", val])
      #cap(cap(churchnum(n), APPLY_CDR), cap(REPLACE_CAR_FLIP, val))
    end
  end

  def vm_regs
    ap(CAR, :var)
  end

  def lib_inc(e)
    ap(nth(2, :var), e)
  end

  def lib_dec(e)
    ap(nth(3, :var), e)
  end

  def lib_add(e1, e2)
    ap2(nth(4, :var), e1, e2)
  end

  def lib_sub(e1, e2)
    ap2(nth(5, :var), e1, e2)
  end

  def lib_eq(e1, e2)
    ap2(nth(6, :var), e1, e2)
  end

  def lib_lt(e1, e2)
    ap2(nth(7, :var), e1, e2)
  end

  def lib_load(addr)
    ap2(nth(8, :var), :var, addr)
  end

  def lib_store(addr, val)
    ap3(nth(9, :var), :var, addr, val)
  end

  def lib_putc(e)
    ap(nth(10, :var), e)
  end

  def lib_getc
    ap("c", nth(11, :var))
  end

  def reg_or_simm(x)
    if sym?(x)
      nth(regpos(x), vm_regs)
    else
      le_number(x)
    end
  end

  REPLACE_CAR = "``s`k`s``s``s`ks``s`kk``s`ks``s`k`sik`kk``s``s`ksk`k`k`ki"
  CONS = "``s``s`ks``s`kk``s`ks``s`k`sik`kk"

  def set_reg(reg, val)
    ap2(CONS, ap(replace_nth(val, regpos(reg)), vm_regs),
              ap(CDR, :var))
  end

  def generate_op(op, args, lineno)
    case op
    when :mov
      set_reg(args[0], reg_or_simm(args[1]))
    when :add
      if args[1] == 1
        set_reg(args[0], lib_inc(reg_or_simm(args[0])))
      elsif args[1] == ((1 << BITS) - 1)
        set_reg(args[0], lib_dec(reg_or_simm(args[0])))
      else
        set_reg(args[0], lib_add(reg_or_simm(args[0]), reg_or_simm(args[1])))
      end
    when :sub
      if args[1] == 1
        set_reg(args[0], lib_dec(reg_or_simm(args[0])))
      else
        set_reg(args[0], lib_sub(reg_or_simm(args[0]), reg_or_simm(args[1])))
      end
    when :jmp
      set_reg(:pc, reg_or_simm(args[0]))
    when :jeq
      jumped = set_reg(:pc, reg_or_simm(args[0]))
      ap2(lib_eq(reg_or_simm(args[1]), reg_or_simm(args[2])), jumped, :var)
    when :jne
      jumped = set_reg(:pc, reg_or_simm(args[0]))
      ap2(lib_eq(reg_or_simm(args[1]), reg_or_simm(args[2])), :var, jumped)
    when :jlt
      jumped = set_reg(:pc, reg_or_simm(args[0]))
      ap2(lib_lt(reg_or_simm(args[1]), reg_or_simm(args[2])), jumped, :var)
    when :jgt
      jumped = set_reg(:pc, reg_or_simm(args[0]))
      ap2(lib_lt(reg_or_simm(args[2]), reg_or_simm(args[1])), jumped, :var)
    when :jle
      jumped = set_reg(:pc, reg_or_simm(args[0]))
      ap2(lib_lt(reg_or_simm(args[2]), reg_or_simm(args[1])), :var, jumped)
    when :jge
      jumped = set_reg(:pc, reg_or_simm(args[0]))
      ap2(lib_lt(reg_or_simm(args[1]), reg_or_simm(args[2])), :var, jumped)
    when :eq
      cmp = lib_eq(reg_or_simm(args[0]), reg_or_simm(args[1]))
      set_reg(args[0], ap2(CONS, cmp, cap(churchnum(BITS - 1), cap(CONS, "`ki"), "v")))
    when :ne
      cmp = bit_not(lib_eq(reg_or_simm(args[0]), reg_or_simm(args[1])))
      set_reg(args[0], ap2(CONS, cmp, cap(churchnum(BITS - 1), cap(CONS, "`ki"), "v")))
    when :lt
      cmp = lib_lt(reg_or_simm(args[0]), reg_or_simm(args[1]))
      set_reg(args[0], ap2(CONS, cmp, cap(churchnum(BITS - 1), cap(CONS, "`ki"), "v")))
    when :gt
      cmp = lib_lt(reg_or_simm(args[1]), reg_or_simm(args[0]))
      set_reg(args[0], ap2(CONS, cmp, cap(churchnum(BITS - 1), cap(CONS, "`ki"), "v")))
    when :le
      cmp = bit_not(lib_lt(reg_or_simm(args[1]), reg_or_simm(args[0])))
      set_reg(args[0], ap2(CONS, cmp, cap(churchnum(BITS - 1), cap(CONS, "`ki"), "v")))
    when :ge
      cmp = bit_not(lib_lt(reg_or_simm(args[0]), reg_or_simm(args[1])))
      set_reg(args[0], ap2(CONS, cmp, cap(churchnum(BITS - 1), cap(CONS, "`ki"), "v")))
    when :load
      set_reg(args[0], lib_load(reg_or_simm(args[1])))
    when :store
      val = reg_or_simm(args[0])
      addr = reg_or_simm(args[1])
      lib_store(addr, val)
    when :putc
      ap2("k", :var, lib_putc(reg_or_simm(args[0])))
    when :getc
      set_reg(args[0], lib_getc)
    when :exit
      ap("e", "i")
    when :dump
      ap("i", :var)  # TODO: implement
    else
      raise "unknown operator #{op}"
    end
  end

  COMPOSE = "``s`ksk"

  def generate_chunk(chunk)
    code = nil
    chunk.each do |op, *args, lineno|
      op = unlambda(generate_op(op, args, lineno))
      code = code ? cap(COMPOSE, op, code) : op
    end
    code
  end

  def emit
    generate_list(@code.map{|chunk| generate_chunk(chunk)}).flatten.join('')
  end

  def emit_data
    cap(churchnum(256),
        cap(CONS, le_0),
        generate_list(@data.map{|x, addr| le_number2(x)})).flatten.join('')
  end
end

if __FILE__ == $0
  unla = UnlAsm.new
  code, data = unla.parse(File.read(ARGV[0]))
  puts "``"
  puts "# VM core"
  puts IO.read('vmcore.unl')
  puts "# instructions"
  puts unla.emit
  puts "# data"
  puts unla.emit_data
end
