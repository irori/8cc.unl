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
  def emit(s)
    print(s)
  end

  def emit_tick(n)
    emit("`" * n)
  end

  def emit_list(src)
    sep = ''
    src.each do |x|
      emit(sep)
      emit("``s``si`k")
      yield(x)
      sep = "`k"
    end
    emit("v")
  end

  def regpos(r)
    {:pc => 0, :a => 1, :b => 2, :c => 3, :d => 4, :bp => 5, :sp => 6}[r] or
      raise "unknown reg: #{r}"
  end

  def emit_number(n)
    emit_list(0...BITS) do |b|
      if (n & 1 << b).zero?
        emit("`ki")
      else
        emit("k")
      end
    end
  end

  CONS_KI = "``s`k`s``si`k`kik"
  CONS_K = "``s`k`s``si`kkk"
  def emit_number2(n)
    (0..BITS).each do |b|
      if b + 1 < BITS && (1 << b) > n
        emit("``")
        emit_churchnum(BITS - b)
        emit(CONS_KI)
        break
      elsif b + 1 < BITS && (1 << BITS) - 1 == ((1 << b - 1) | n)
        emit("``")
        emit_churchnum(BITS - b)
        emit(CONS_K)
        break
      else
        emit("``s``si`k")
        emit((n & 1 << b) == 0 ? "`ki" : "k")
        emit("`k") if b + 1 < BITS
      end
    end
    emit("v")
  end

  def emit_le_0
    emit_tick(2)
    emit_churchnum(BITS)
    emit_tick(1)
    emit(CONS)
    emit("`ki")
    emit("v")
  end

  def emit_churchnum(n)
    s = CNTBL[n] or raise "churchnum(#{n}) is not in the table"
    emit(s)
  end

  CAR = "``si`kk"
  CDR = "``si`k`ki"
  DOTCAR = "k"
  DOTCDR = "`ki"

  def emit_nth(n)
    # TODO: optimize for small |n|
    emit("``s")
    emit("``s")
    emit("`k")
    emit("`")
    emit_churchnum(n)
    emit(CDR)
    yield
    emit("`k")
    emit(DOTCAR)
  end

  def emit_lib(n)
    emit("``s")
    emit("`")
    emit_churchnum(n)
    emit(CDR)
    emit("`k")
    emit(DOTCAR)
  end

  def emit_getreg(reg)
    emit_nth(regpos(reg)) {
      emit(CAR) # vm_regs
    }
  end

  APPLY_CDR = "``s`k`si``s`kk``s`k`s``s`ks``s`kk``s`ks``s`k`sik``s`kk`s`kk"
  REPLACE_CAR_FLIP = "``s`k`si``s`kk``s`kk``s`k`s``s`k`s``s`ks``s`k`sik``s`kkkk"

  def emit_lib_inc
    emit("``s")
    emit_lib(2)
  end

  def emit_lib_dec
    emit("``s")
    emit_lib(3)
  end

  def emit_lib_add
    emit("``s")
    emit("``s")
    emit_lib(4)
  end

  def emit_lib_sub
    emit("``s")
    emit("``s")
    emit_lib(5)
  end

  def emit_lib_eq
    emit("``s")
    emit("``s")
    emit_lib(6)
  end

  def emit_lib_lt
    emit("``s")
    emit("``s")
    emit_lib(7)
  end

  def emit_lib_load
    emit("``s")

    emit("``s")
    emit_lib(8)
    emit("i")
  end

  def emit_lib_store
    emit("``s")
    emit("``s")

    emit("``s")
    emit_lib(9)
    emit("i")
  end

  def emit_lib_putc
    emit("``s")
    emit_lib(10)
  end

  def emit_lib_getc
    emit("``s")
    emit("`kc")
    emit_lib(11)
  end

  def emit_value(reg_or_imm)
    if sym?(reg_or_imm)
      emit_getreg(reg_or_imm)
    else
      emit("`k")
      emit_number(reg_or_imm)
    end
  end

  REPLACE_CAR = "``s`k`s``s``s`ks``s`kk``s`ks``s`k`sik`kk``s``s`ksk`k`k`ki"
  CONS = "``s``s`ks``s`kk``s`ks``s`k`sik`kk"

  def emit_setreg(reg)
    emit("``s")
    emit("``s")
    emit("`k")
    emit(CONS)
    emit("``s")

    emit("``s")
    emit("`k")
    emit_tick(1)
    emit_churchnum(regpos(reg))
    emit(APPLY_CDR)
    emit("``s")
    emit("`k")
    emit(REPLACE_CAR_FLIP)
    yield

    emit(CAR)
    emit(CDR)
  end

  def emit_setreg_imm(reg, n)
    emit("``s")
    emit("``s")
    emit("`k")
    emit(CONS)
    emit("``s")

    emit("`k")
    emit_tick(2)
    emit_churchnum(regpos(reg))
    emit(APPLY_CDR)
    emit("``si`k`k``s``s`k`s``s`ks``s`k`sik``s`kkk`k")
    emit_number(n)

    emit(CAR)
    emit(CDR)
  end

  def emit_jmp(reg_or_imm)
    if sym?(reg_or_imm)
      emit_setreg(:pc) { emit_getreg(reg_or_imm) }
    else
      emit_setreg_imm(:pc, reg_or_imm)
    end
  end

  def emit_bool_to_number
    emit("``s")
    emit("``s")
    emit("`k")
    emit(CONS)
    yield
    emit("`k")
    emit_tick(2)
    emit_churchnum(BITS - 1)
    emit_tick(1)
    emit(CONS)
    emit("`ki")
    emit("v")
  end

  def emit_op(op, args, lineno)
    case op

    when :mov
      if sym?(args[1])
        emit_setreg(args[0]) { emit_getreg(args[1]) }
      else
        emit_setreg_imm(args[0], args[1])
      end

    when :add
      emit_setreg(args[0]) {
        if args[1] == 1
          emit_lib_inc
          emit_getreg(args[0])
        elsif args[1] == ((1 << BITS) - 1)
          emit_lib_dec
          emit_getreg(args[0])
        else
          emit_lib_add
          emit_getreg(args[0])
          emit_value(args[1])
        end
      }

    when :sub
      emit_setreg(args[0]) {
        if args[1] == 1
          emit_lib_dec
          emit_getreg(args[0])
        else
          emit_lib_sub
          emit_getreg(args[0])
          emit_value(args[1])
        end
      }

    when :jmp
      emit_jmp(args[0])

    when :jeq, :jne, :jlt, :jgt, :jle, :jge
      emit("``s")
      emit("``s")

      if op == :jeq || op == :jne
        emit_lib_eq
      else
        emit_lib_lt
      end

      if op == :jgt || op == :jle
        emit_value(args[2])
        emit_value(args[1])
      else
        emit_value(args[1])
        emit_value(args[2])
      end

      if [:jne, :jle, :jge].include?(op)
        emit("i")
        emit_jmp(args[0])
      else
        emit_jmp(args[0])
        emit("i")
      end

    when :eq, :ne, :lt, :gt, :le, :ge
      emit_setreg(args[0]) {
        emit_bool_to_number {
          if [:ne, :le, :ge].include?(op)
            emit("``s")
            emit("``s")
          end

          if op == :eq || op == :ne
            emit_lib_eq
          else
            emit_lib_lt
          end

          if op == :gt || op == :le
            emit_value(args[1])
            emit_value(args[0])
          else
            emit_value(args[0])
            emit_value(args[1])
          end

          if [:ne, :le, :ge].include?(op)
            emit("`k`ki")
            emit("`kk")
          end
        }
      }

    when :load
      emit_setreg(args[0]) {
        emit_lib_load
        emit_value(args[1])
      }

    when :store
      emit_lib_store
      emit_value(args[1])
      emit_value(args[0])

    when :putc
      emit("``s")
      emit("k")
      emit_lib_putc
      emit_value(args[0])

    when :getc
      emit_setreg(args[0]) { emit_lib_getc }

    when :exit
      emit("``s`ke`ki")

    when :dump
      emit("i")  # TODO: implement

    else
      raise "unknown operator #{op}"
    end
  end

  COMPOSE = "``s`ksk"

  def emit_chunk(chunk)
    (chunk.size-1).downto(0) do |i|
      op, *args, lineno = chunk[i]
      if i > 0
        emit_tick(2)
        emit(COMPOSE)
      end
      emit_op(op, args, lineno)
    end
  end

  def emit_code
    emit_list(@code) {|chunk| emit_chunk(chunk)}
  end

  def emit_data
    emit_tick(2)
    emit_churchnum(256)
    emit_tick(1)
    emit(CONS)
    emit_le_0  # TODO: emit_number2(0) is smaller
    emit_list(@data) {|x, addr| emit_number2(x)}
  end
end

if __FILE__ == $0
  unla = UnlAsm.new
  code, data = unla.parse(File.read(ARGV[0]))
  puts "``"
  puts "# VM core"
  puts IO.read('vmcore.unl')
  puts "# instructions"
  unla.emit_code
  puts
  puts "# data"
  unla.emit_data
  puts
end
