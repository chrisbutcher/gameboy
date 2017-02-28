require 'json'
require 'set'
require 'ostruct'
require 'byebug'

class GameBoyOpCodeRustifier
  attr_reader :o

  def initialize(json_file)
    @o = JSON.parse(File.read(json_file))
  end

  def print
    mappings, fns = tetris_structs_to_rust_mappings_and_fns(tetris_structs)
    cb_mappings, cb_fns = tetris_structs_to_rust_mappings_and_fns(tetris_cb_structs)

    # puts cb_mappings
    # puts cb_fns

    puts fns
    puts mappings
  end

  private

  def tetris_structs_to_rust_mappings_and_fns(structs)
    mappings = []
    fns = {}

    rust_fns = structs.each do |ocs|
      mnemonic = ocs.mnemonic.downcase
      cycles = ocs.cycles
      operands = case ocs.operand_count
      when 0
        []
      when 1
        [ocs.operand1]
      else
        [ocs.operand1, ocs.operand2]
      end

      operands_sig = operands.map(&:downcase).map do |operand|
        operand_to_sig.fetch(operand, "#{operand}?sig?")
      end.compact.join("_").gsub(/[\(\)]/, '') if operands.any?
      operands_sig = "_#{operands_sig}" if operands_sig


      operands_args = operands.map do |operand|
        operands_to_args.fetch(operand, "#{operand}!arg!")
      end.compact if operands.any?
      args = operands_args&.join(', ')

      byebug if ocs.op_code == '0xcb'

      fn_name = "op_#{mnemonic}#{operands_sig}"
      fns[fn_name] = <<~RUST
        fn #{fn_name}(&mut self, #{args + ',' if args}mmu: &mut mmu::MMU) {
          // #{ocs.inspect}
        }

      RUST

      mappings << <<~RUST
        "#{ocs.op_code}" => {
          // #{ocs.inspect}
          self.#{fn_name}(#{args + ', ' if args}mmu); #{cycles.first}#{'// ' + cycles[1].to_s if cycles.size > 1 }
        },
      RUST
    end

    [mappings, fns.values]
  end

  def operand_to_sig
    @operand_to_sig ||= begin
      {
        'd8' => 'n',
        'd16' => 'nn',
        'a' => 'n',
        'f' => 'n',
        'b' => 'n',
        'c' => 'n',
        'd' => 'n',
        'e' => 'n',
        'h' => 'n',
        'l' => 'n',
        'af' => 'nn',
        'bc' => 'nn',
        'de' => 'nn',
        'hl' => 'nn',
        '(c)' => 'n_ind',
        '(hl)' => 'nn_ind',
        '(hl+)' => 'nn_ind',
        '(hl-)' => 'nn_ind',
        '(bc)' => 'nn_ind',
        '(de)' => 'nn_ind',
        'sp' => 'sp',
        'sp+r8' => 'sp_r8',
        'z' => 'zflag',
        'c' => 'cflag',
        'nz' => 'notzflag',
        'nc' => 'notcflag',
        '0' => 'b',
        '1' => 'b',
        '2' => 'b',
        '3' => 'b',
        '4' => 'b',
        '5' => 'b',
        '6' => 'b',
        '7' => 'b',
        'a8' => 'b_adr',
        'a16' => 'w_adr',
        '(a8)' => 'b_adr_ptr',
        '(a16)' => 'w_adr_ptr',
        'cb' => nil,
      }
    end
  end

  def operands_to_args
    @operands_to_args ||= begin
      byte_sig = "byte: Byte"
      word_sig = "word: Word"
      word_ptr_sig = "wordI: WordImmediate"
      byte_addr_sig = "byteAddr: Byte"
      word_addr_sig = "wordAddr: Word"
      word_addr_ptr_sig = "wordAddrImmediate: Word"

      {
        "A" => byte_sig,
        "F" => byte_sig,
        "B" => byte_sig,
        "C" => byte_sig,
        "D" => byte_sig,
        "E" => byte_sig,
        "H" => byte_sig,
        "L" => byte_sig,
        "AF" => word_sig,
        "BC" => word_sig,
        "DE" => word_sig,
        "HL" => word_sig,
        "(DE)" => word_addr_ptr_sig,
        "(HL)" => word_addr_ptr_sig,
        "(a8)" => word_addr_ptr_sig,
        "a16" => word_addr_sig,
        "(a16)" => word_addr_ptr_sig,
        "d8" => byte_sig,
        "d16" => word_sig,
        "CB" => nil,
      }
    end
  end

  def tetris_structs
    @tetris_structs ||= begin
      op_code_structs.select do |ocs|
        ocs.category == 'unprefixed' && tetris_op_codes.include?(ocs.op_code)
      end.sort_by(&:op_code)
    end
  end

  def tetris_cb_structs
    @tetris_cb_structs ||= begin
      op_code_structs.select do |ocs|
        ocs.category == 'cbprefixed' && tetris_cb_op_codes.include?(ocs.op_code)
      end.sort_by(&:op_code)
    end
  end

  def op_code_structs
    @op_code_structs ||= begin
      o.flat_map do |category, ops|
        ops.map do |op_code, deets|
          OpenStruct.new(deets.merge(op_code: op_code, category: category))
        end
      end.flatten
    end
  end

  def tetris_op_codes
    Set.new(['0x00', '0xc3', '0xaf', '0x21', '0x0e', '0x06', '0x32', '0x05', '0x20', '0x0d', '0x3e', '0xf3', '0xe0', '0xf0', '0xfe', '0x36', '0xea', '0x31', '0x2a', '0xe2', '0x0c', '0xcd', '0xfb', '0xe6', '0xca', '0x7e', '0xa7', '0x28', '0x35', '0x2c', '0xef', '0x29', '0x1c', '0x3d', '0x1d', '0xa8', '0x12', '0xdf', '0x61', '0x81', '0x19', '0x04', '0x14', '0x6b', '0x1a', '0x1b', '0x1e', '0x1f', '0x7a', '0x89', '0x15', '0x23', '0x16', '0x17', '0x77', '0xe4', '0x01', '0x0b', '0x78', '0xb1', '0xc9', '0x2f', '0xcb', '0x47', '0xb0', '0x4f', '0xa9', '0xa1', '0x79', '0xf5', '0xc5', '0xd5', '0xe5', '0xfa', '0xe1', '0xd1', '0xc1', '0xf1', '0x18', '0x34', '0x80', '0x30', '0xc8', '0x09', '0x4e', '0x46', '0x69', '0x60', '0x7c', '0x11', '0xe9', '0xbe', '0x2d', '0x0a', '0x03', '0x85', '0x6f', '0x62', '0x5f', '0x57', '0x73', '0x72', '0x71', '0x3a', '0x13', '0x7b', '0x22', '0x7d', '0xc6', '0x54', '0xc2', '0x67', '0x5d', '0xf6', '0x3c', '0x26', '0x5e'])
  end

  def tetris_cb_op_codes
   Set.new(['0x37', '0xbe', '0xfe', '0x27', '0x46', '0xd0', '0xf0', '0xd8', '0xf8', '0x7f', '0x7e', '0x86', '0x3f', '0x40', '0x33', '0x5f'])
  end
end

GameBoyOpCodeRustifier.new('opcodes.json').print
