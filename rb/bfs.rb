#!/usr/bin/env ruby
# .bfs parser, based on https://github.com/shinh/bflisp/blob/master/bfasm.rb

if ENV['BFS24']
  $bfs24 = true
  BITS = 24
else
  BITS = 16
end
UINT_MAX = (1 << BITS) - 1
INT_MIN = -(1 << (BITS - 1))

while true
  if ARGV[0] == '-v'
    $verbose = true
    ARGV.shift
  elsif ARGV[0] == '-q'
    $quiet = true
    ARGV.shift
  elsif ARGV[0] == '-m'
    $bfs24 = true
    ARGV.shift
  else
    break
  end
end

def sym?(o)
  o.class == Symbol
end

# 16bit CPU.
# Registers: A, B, C, D, BP, and SP
#
# mov   <reg>, <reg|simm|label>
# add   <reg>, <reg|simm>
# sub   <reg>, <reg|simm>
# load  <reg>, <reg|simm>
# store <reg>, <reg|simm>
# eq    <reg>, <reg|simm>
# ne    <reg>, <reg|simm>
# lt    <reg>, <reg|simm>
# gt    <reg>, <reg|simm>
# le    <reg>, <reg|simm>
# ge    <reg>, <reg|simm>
# jmp   <reg|label>
# jeq   <reg|label>, <reg>, <reg|simm>
# jne   <reg|label>, <reg>, <reg|simm>
# jlt   <reg|label>, <reg>, <reg|simm>
# jgt   <reg|label>, <reg>, <reg|simm>
# jle   <reg|label>, <reg>, <reg|simm>
# jge   <reg|label>, <reg>, <reg|simm>
# putc  <reg|simm>
# getc  <reg>
# exit

class BFAsm
  def initialize
    @jmp_ops = {
      :jmp => 1,
      :jeq => 3,
      :jne => 3,
      :jlt => 3,
      :jgt => 3,
      :jle => 3,
      :jge => 3,
    }

    @ops = {
      :mov => 2,
      :add => 2,
      :sub => 2,
      :load => 2,
      :store => 2,
      :eq => 2,
      :ne => 2,
      :lt => 2,
      :gt => 2,
      :le => 2,
      :ge => 2,
      :putc => 1,
      :getc => 1,
      :exit => 0,
      :dump => 0,
    }
    @jmp_ops.each do |k, v|
      @ops[k] = v
    end

    @code = []
    @data = []
    @labels = {}

    @code << [[:jmp, 'main', -1]]
    @labels['main'] = 1
    @labeled_pcs = { 1 => true }

    @labels['_edata'] = cur_data_addr
    add_data(0)
  end

  def cur_data_addr
    @data.size + 256
  end

  def add_data(v)
    @data << [v, cur_data_addr]
    if @data.size > 60000
      raise "Too much data!"
    end
  end

  def parse(code)
    in_data = false
    prev_op = nil
    code.split("\n").each_with_index do |line, lineno|
      lineno += 1

      # TODO: Weird!
      line.sub!(/(^#|\s# ).*/, '')
      line.strip!
      next if line.empty?

      if line =~ /^\.set\s+(\w+)\s*,\s*(.+)/
        @labels[$1] = @labels[$2] || Integer($2)
        next
      end

      if line =~ /^\.(text|data)/
        in_data = $1 == 'data'
        if in_data
          if $' =~ / (-?\d+)/
            add_data(cur_data_addr + $1.to_i)
          end
        end
        next
      end

      if line =~ /^(\.?\w+):$/
        if @labels[$1] && $1 != 'main'
          raise "multiple label definition (#$1) at line #{lineno}"
        end
        if in_data
          @labels[$1] = cur_data_addr
        else
          @labeled_pcs[@code.size] = true
          @labels[$1] = @code.size
        end
        next
      end

      if in_data
        if line =~ /^\.long (-?\d+)/
          add_data($1.to_i & UINT_MAX)
        elsif line =~ /^\.long (\.?\w+)/
          if !@labels[$1]
            raise 'TODO'
          end
          add_data(@labels[$1])
        elsif line =~ /^\.lcomm (\w+), (\d+)/
          name = $1
          size = $2.to_i
          @labels[name] = cur_data_addr
          size.times{
            add_data(0)
          }
        elsif line =~ /^\.(ascii|string) (".*")/
          str = eval($2)
          str.each_byte{|b|
            add_data(b)
          }
          add_data(0) if $1 == 'string'
        else
          raise "unknown data at line #{lineno}"
        end
        next
      end

      op = line[/^\S+/].to_sym
      args = $'.split(',')
      if !@ops[op]
        raise "invalid op (#{op}) at line #{lineno}"
      end

      if @ops[op] != args.size
        raise "invalid number of args for #{op} at line #{lineno}"
      end

      args = args.each_with_index.map{|a, i|
        a.strip!
        if a =~ /^-?0x(\d+)$/
          a.hex
        elsif a =~ /^-?\d+$/
          a.to_i
        elsif a =~ /^(a|b|c|d|pc|bp|sp)$/i
          $1.downcase.to_sym
        elsif a =~ /^\.?\w+$/ && ((@jmp_ops[op] && i == 0) ||
                                  (op == :mov && i == 1))
          a
        else
          raise "invalid use of #{op} at line #{lineno}"
        end
      }

      if args.size == 2
        if !sym?(args[0])
          raise "invalid first arg for #{op} at line #{lineno}"
        end
      end

      if op == :getc && !sym?(args[0])
        raise "invalid arg for #{op} at line #{lineno}"
      end

      if (@code.size == 1 || @labeled_pcs[@code.size] ||
          (%i(load store putc getc exit) + @jmp_ops.keys).include?(prev_op))
        @code << [[op, *args, lineno]]
      else
        @code[-1] << [op, *args, lineno]
      end
      prev_op = op
    end

    @data[0][0] = cur_data_addr

    @code = @code.each_with_index.map{|opa, i|
      opa.map do |op, *args, lineno|
        args = args.each_with_index.map do |a, j|
          if a.class == String
            if (@jmp_ops[op] && j == 0) || (op == :mov && j == 1)
              if !@labels[a]
                raise "undefined label #{a} at line #{lineno}"
              end
              @labels[a]
            else
              raise "unexpected label (#{a}) in #{op}"
            end
          elsif sym?(a)
            a
          else
            if a < INT_MIN || a > UINT_MAX
              raise "number (#{a}) out of bound at line #{i+1}"
            end
            a & UINT_MAX
          end
        end
        [op, *args, lineno]
      end
    }

    return [@code, @data]
  end

  def dump
    @code.each do |c|
      p c
    end
  end

end

if $0 == __FILE__
  asm = BFAsm.new
  asm.parse(File.read(ARGV[0]))
  asm.dump
end
