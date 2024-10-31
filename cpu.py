from memory import Memory
from opcodes import (
    DAA,
    INC_BC, DEC_BC, INC_DE, DEC_DE, INC_HL, DEC_HL, INC_SP, DEC_SP,
    INC_B, DEC_B, INC_C, DEC_C, INC_D, DEC_D, INC_E, DEC_E, INC_H, DEC_H, INC_L, DEC_L, INC__HL_, DEC__HL_, INC_A, DEC_A,
    ADD_HL_BC, ADD_HL_DE, ADD_HL_HL, ADD_HL_SP,
    ADD_A_B, ADD_A_C, ADD_A_D, ADD_A_E, ADD_A_H, ADD_A_L, ADD_A__HL_, ADD_A_A,
    ADC_A_B, ADC_A_C, ADC_A_D, ADC_A_E, ADC_A_H, ADC_A_L, ADC_A__HL_, ADC_A_A,
    SUB_A_B, SUB_A_C, SUB_A_D, SUB_A_E, SUB_A_H, SUB_A_L, SUB_A__HL_, SUB_A_A,
    SBC_A_B, SBC_A_C, SBC_A_D, SBC_A_E, SBC_A_H, SBC_A_L, SBC_A__HL_, SBC_A_A,
    AND_A_B, AND_A_C, AND_A_D, AND_A_E, AND_A_H, AND_A_L, AND_A__HL_, AND_A_A,
    XOR_A_B, XOR_A_C, XOR_A_D, XOR_A_E, XOR_A_H, XOR_A_L, XOR_A__HL_, XOR_A_A,
    OR_A_B, OR_A_C, OR_A_D, OR_A_E, OR_A_H, OR_A_L, OR_A__HL_, OR_A_A,
    CP_A_B, CP_A_C, CP_A_D, CP_A_E, CP_A_H, CP_A_L, CP_A__HL_, CP_A_A,
    ADD_A_IMM, ADC_A_IMM, SUB_A_IMM, SBC_A_IMM,
    AND_A_IMM, XOR_A_IMM, OR_A_IMM, CP_A_IMM
)

FLAG_Z = 0b10000000  # Zero Flag
FLAG_N = 0b01000000  # Subtract Flag
FLAG_H = 0b00100000  # Half Carry Flag
FLAG_C = 0b00010000  # Carry Flag

START_PC = 0x0100
START_SP = 0xFFFE

REGISTER_A = 0
REGISTER_F = 1
REGISTER_B = 2
REGISTER_C = 3
REGISTER_D = 4
REGISTER_E = 5
REGISTER_H = 6
REGISTER_L = 7

class CPU:
    def __init__(self, memory: Memory):
        self.memory = memory
        # Registers A, F, B, C, D, E, H, L
        self.registers = [0x00] * 8
        self.pc = START_PC
        self.sp = START_SP
        self.INSTRUCTION_MAP = {}
        self._fill_instruction_map()

    # Properties for register pairs (e.g., BC, DE, HL)
    @property
    def BC(self):
        return (self.registers[REGISTER_B] << 8) | self.registers[REGISTER_C]

    @BC.setter
    def BC(self, value16):
        self.registers[REGISTER_B] = (value16 >> 8) & 0xFF
        self.registers[REGISTER_C] = value16 & 0xFF

    @property
    def DE(self):
        return (self.registers[REGISTER_D] << 8) | self.registers[REGISTER_E]

    @DE.setter
    def DE(self, value16):
        self.registers[REGISTER_D] = (value16 >> 8) & 0xFF
        self.registers[REGISTER_E] = value16 & 0xFF

    @property
    def HL(self):
        return (self.registers[REGISTER_H] << 8) | self.registers[REGISTER_L]

    @HL.setter
    def HL(self, value16):
        self.registers[REGISTER_H] = (value16 >> 8) & 0xFF
        self.registers[REGISTER_L] = value16 & 0xFF

    def _fill_instruction_map(self):
        self.INSTRUCTION_MAP[DAA] = lambda self: self._daa()
        def _map_inc_dec_opcode_register_pairs_to_operation(opcode_register_groups, operation, **kwargs):
            for opcode, register in opcode_register_groups:
                self.INSTRUCTION_MAP[opcode] = lambda self, reg=register: operation(self, reg, **kwargs)
        def _map_opcode_inc_dec_16_pairs_to_operation(opcode_register_groups, operation, **kwargs):
            for opcode, register16 in opcode_register_groups:
                self.INSTRUCTION_MAP[opcode] = lambda self, reg16=register16: operation(self, reg16, **kwargs)
        def _map_opcode_add_hl_register16_pairs_to_operation(opcode_register_groups, operation, **kwargs):
            for opcode, register_pair in opcode_register_groups:
                self.INSTRUCTION_MAP[opcode] = lambda self, reg_pair=register_pair: operation(self, getattr(self, reg_pair), **kwargs)
        def _map_opcode_register_pairs_to_operation(opcode_register_pairs, operation, **kwargs):
            for opcode, register in opcode_register_pairs:
                self.INSTRUCTION_MAP[opcode] = lambda self, reg=register: operation(self, self.registers[reg], **kwargs)
        def _map_opcode_hl_imm_to_operation(opcode_hl, opcode_imm, operation, **kwargs):
            self.INSTRUCTION_MAP[opcode_hl] = lambda self: operation(self, self._read_byte_at_memory_hl(), **kwargs)
            self.INSTRUCTION_MAP[opcode_imm] = lambda self: (operation(self, self.memory.read_byte(self.pc), **kwargs), setattr(self, 'pc', self.pc + 1))
        # Define the opcode-register pairs and call add_instruction_map with the appropriate function and flags
        _map_opcode_inc_dec_16_pairs_to_operation( [(INC_BC, 'BC'), (INC_DE, 'DE'), (INC_HL, 'HL'), (INC_SP, 'sp')],
            lambda self, value: self._inc16(value) )
        _map_opcode_inc_dec_16_pairs_to_operation( [(DEC_BC, 'BC'), (DEC_DE, 'DE'), (DEC_HL, 'HL'), (DEC_SP, 'sp') ],
            lambda self, value: self._dec16(value) )
        _map_inc_dec_opcode_register_pairs_to_operation( [ (INC_B, REGISTER_B), (INC_C, REGISTER_C), (INC_D, REGISTER_D), (INC_E, REGISTER_E), (INC_H, REGISTER_H), (INC_L, REGISTER_L), (INC_A, REGISTER_A) ],
                            lambda self, register: self._inc(register))
        _map_inc_dec_opcode_register_pairs_to_operation( [ (DEC_B, REGISTER_B), (DEC_C, REGISTER_C), (DEC_D, REGISTER_D), (DEC_E, REGISTER_E), (DEC_H, REGISTER_H), (DEC_L, REGISTER_L), (DEC_A, REGISTER_A) ],
                            lambda self, register: self._dec(register))
        self.INSTRUCTION_MAP[INC__HL_] = lambda self: self._inc__hl_()
        self.INSTRUCTION_MAP[DEC__HL_] = lambda self: self._dec__hl_()
        _map_opcode_add_hl_register16_pairs_to_operation( [(ADD_HL_BC, 'BC'), (ADD_HL_DE, 'DE'), (ADD_HL_HL, 'HL'), (ADD_HL_SP, 'sp') ],
            lambda self, value: self._add_hl_16(value))
        _map_opcode_register_pairs_to_operation([ (ADD_A_A, REGISTER_A), (ADD_A_B, REGISTER_B), (ADD_A_C, REGISTER_C), (ADD_A_D, REGISTER_D), (ADD_A_E, REGISTER_E), (ADD_A_H, REGISTER_H), (ADD_A_L, REGISTER_L) ],
                            lambda self, value: self._add_a(value))
        _map_opcode_register_pairs_to_operation( [ (ADC_A_A, REGISTER_A), (ADC_A_B, REGISTER_B), (ADC_A_C, REGISTER_C), (ADC_A_D, REGISTER_D), (ADC_A_E, REGISTER_E), (ADC_A_H, REGISTER_H), (ADC_A_L, REGISTER_L) ],
                            lambda self, value: self._add_a(value, use_carry=True))
        _map_opcode_register_pairs_to_operation( [ (SUB_A_A, REGISTER_A), (SUB_A_B, REGISTER_B), (SUB_A_C, REGISTER_C), (SUB_A_D, REGISTER_D), (SUB_A_E, REGISTER_E), (SUB_A_H, REGISTER_H), (SUB_A_L, REGISTER_L) ],
                            lambda self, value: self._sub_a(value))
        _map_opcode_register_pairs_to_operation( [ (SBC_A_A, REGISTER_A), (SBC_A_B, REGISTER_B), (SBC_A_C, REGISTER_C), (SBC_A_D, REGISTER_D), (SBC_A_E, REGISTER_E), (SBC_A_H, REGISTER_H), (SBC_A_L, REGISTER_L) ],
                            lambda self, value: self._sub_a(value, use_carry=True))
        _map_opcode_register_pairs_to_operation( [ (AND_A_A, REGISTER_A), (AND_A_B, REGISTER_B), (AND_A_C, REGISTER_C), (AND_A_D, REGISTER_D), (AND_A_E, REGISTER_E), (AND_A_H, REGISTER_H), (AND_A_L, REGISTER_L) ],
                            lambda self, value: self._and_a(value))
        _map_opcode_register_pairs_to_operation( [ (XOR_A_A, REGISTER_A), (XOR_A_B, REGISTER_B), (XOR_A_C, REGISTER_C), (XOR_A_D, REGISTER_D), (XOR_A_E, REGISTER_E), (XOR_A_H, REGISTER_H), (XOR_A_L, REGISTER_L) ],
                            lambda self, value: self._xor_a(value))
        _map_opcode_register_pairs_to_operation( [ (OR_A_A, REGISTER_A), (OR_A_B, REGISTER_B), (OR_A_C, REGISTER_C), (OR_A_D, REGISTER_D), (OR_A_E, REGISTER_E), (OR_A_H, REGISTER_H), (OR_A_L, REGISTER_L) ],
                            lambda self, value: self._or_a(value))
        _map_opcode_register_pairs_to_operation( [ (CP_A_A, REGISTER_A), (CP_A_B, REGISTER_B), (CP_A_C, REGISTER_C), (CP_A_D, REGISTER_D), (CP_A_E, REGISTER_E), (CP_A_H, REGISTER_H), (CP_A_L, REGISTER_L) ],
                            lambda self, value: self._sub_a(value, compare=True))
        # Define each set of operations with their HL and IMM opcodes
        _map_opcode_hl_imm_to_operation(ADD_A__HL_, ADD_A_IMM, lambda self, value: self._add_a(value))
        _map_opcode_hl_imm_to_operation(ADC_A__HL_, ADC_A_IMM, lambda self, value: self._add_a(value, use_carry=True))
        _map_opcode_hl_imm_to_operation(SUB_A__HL_, SUB_A_IMM, lambda self, value: self._sub_a(value))
        _map_opcode_hl_imm_to_operation(SBC_A__HL_, SBC_A_IMM, lambda self, value: self._sub_a(value, use_carry=True))
        _map_opcode_hl_imm_to_operation(AND_A__HL_, AND_A_IMM, lambda self, value: self._and_a(value))
        _map_opcode_hl_imm_to_operation(XOR_A__HL_, XOR_A_IMM, lambda self, value: self._xor_a(value))
        _map_opcode_hl_imm_to_operation(OR_A__HL_, OR_A_IMM, lambda self, value: self._or_a(value))
        _map_opcode_hl_imm_to_operation(CP_A__HL_, CP_A_IMM, lambda self, value: self._sub_a(value, compare=True))

    def step(self):
        opcode = self.memory.read_byte(self.pc)
        self.pc += 1
        self.INSTRUCTION_MAP[opcode](self)

    def _read_byte_at_memory_hl(self):
        address = (self.registers[REGISTER_H] << 8) | self.registers[REGISTER_L]
        memory_value = self.memory.read_byte(address)
        return memory_value

    def _daa(self):
        a = self.registers[REGISTER_A]
        flag_n = self.registers[REGISTER_F] & FLAG_N
        adjust = 0
        carry = False

        # Check flags to determine adjustment values
        if (self.registers[REGISTER_F] & FLAG_H) or (not (flag_n) and (a & 0x0F) > 0x09):
            adjust |= 0x06  # Add 0x06 to fix lower nibble
        if (self.registers[REGISTER_F] & FLAG_C) or (not (flag_n) and a > 0x99):
            adjust |= 0x60  # Add 0x60 to fix upper nibble
            carry = True

        # Apply adjustment based on N flag (add or subtract)
        a = a + (-adjust if flag_n else adjust)
        a &= 0xFF  # Keep result within 8 bits

        # Update flags
        self.registers[REGISTER_A] = a
        self.registers[REGISTER_F] &= ~(FLAG_Z | FLAG_H)  # Clear Z and H flags
        if a == 0:
            self.registers[REGISTER_F] |= FLAG_Z  # Set Z if result is zero
        if carry:
            self.registers[REGISTER_F] |= FLAG_C  # Preserve or set C flag

    def _inc16(self, register_name):
        previous_value = getattr(self, register_name)
        next_value = (previous_value + 1) & 0xFFFF
        setattr(self, register_name, next_value)

    def _dec16(self, register_name):
        previous_value = getattr(self, register_name)
        next_value = (previous_value - 1) & 0xFFFF
        setattr(self, register_name, next_value)

    def _inc(self, register):
        """Increment the specified register by 1."""
        self.registers[register] += 1
        self.registers[register] &= 0xFF  # Ensure it wraps around at 8 bits

        # Update flags
        if self.registers[register] == 0:
            self.registers[REGISTER_F] |= FLAG_Z  # Set Zero flag
        else:
            self.registers[REGISTER_F] &= ~FLAG_Z  # Clear Zero flag

        # Half carry is set if the lower 4 bits go from 0x0F to 0x10
        if (self.registers[register] & 0x0F) == 0x00:
            self.registers[REGISTER_F] |= FLAG_H  # Set Half Carry flag
        else:
            self.registers[REGISTER_F] &= ~FLAG_H  # Clear Half Carry flag

        self.registers[REGISTER_F] &= ~FLAG_N  # Clear Subtract flag

    def _dec(self, register):
        """Decrement the specified register by 1."""
        self.registers[register] -= 1
        self.registers[register] &= 0xFF  # Ensure it wraps around at 8 bits

        # Update flags
        if self.registers[register] == 0:
            self.registers[REGISTER_F] |= FLAG_Z  # Set Zero flag
        else:
            self.registers[REGISTER_F] &= ~FLAG_Z  # Clear Zero flag

        # Half carry is set if the lower 4 bits go from 0x00 to 0x0F
        if (self.registers[register] & 0x0F) == 0x0F:
            self.registers[REGISTER_F] |= FLAG_H  # Set Half Carry flag
        else:
            self.registers[REGISTER_F] &= ~FLAG_H  # Clear Half Carry flag

        self.registers[REGISTER_F] |= FLAG_N  # Set Subtract flag

    def _inc__hl_(self):
        """Increment the value at the address pointed to by HL."""
        address = (self.registers[REGISTER_H] << 8) | self.registers[REGISTER_L]
        value = self.memory.read_byte(address)
        incremented_value = (value + 1) & 0xFF  # Increment and ensure it wraps around
        self.memory.write_byte(address, incremented_value)

        # Update flags
        if incremented_value == 0:
            self.registers[REGISTER_F] |= FLAG_Z  # Set Zero flag
        else:
            self.registers[REGISTER_F] &= ~FLAG_Z  # Clear Zero flag

        if (value & 0x0F) == 0x0F:
            self.registers[REGISTER_F] |= FLAG_H  # Set Half Carry flag
        else:
            self.registers[REGISTER_F] &= ~FLAG_H  # Clear Half Carry flag

        self.registers[REGISTER_F] &= ~FLAG_N  # Clear Subtract flag

    def _dec__hl_(self):
        """Decrement the value at the address pointed to by HL."""
        address = (self.registers[REGISTER_H] << 8) | self.registers[REGISTER_L]
        value = self.memory.read_byte(address)
        decremented_value = (value - 1) & 0xFF  # Decrement and ensure it wraps around
        self.memory.write_byte(address, decremented_value)

        # Update flags
        if decremented_value == 0:
            self.registers[REGISTER_F] |= FLAG_Z  # Set Zero flag
        else:
            self.registers[REGISTER_F] &= ~FLAG_Z  # Clear Zero flag

        if (value & 0x0F) == 0x00:
            self.registers[REGISTER_F] |= FLAG_H  # Set Half Carry flag
        else:
            self.registers[REGISTER_F] &= ~FLAG_H  # Clear Half Carry flag

        self.registers[REGISTER_F] |= FLAG_N  # Set Subtract flag

    def _add_hl_16(self, operand_16):
        previous_hl = (self.registers[REGISTER_H] << 8) | self.registers[REGISTER_L]
        result = previous_hl + operand_16

        # Update H and L with the result, only keeping the lower 16 bits
        self.registers[REGISTER_H] = (result >> 8) & 0xFF
        self.registers[REGISTER_L] = result & 0xFF
        # Clear the N flag as this is an addition
        self.registers[REGISTER_F] &= ~FLAG_N
        # Set the H flag if there is a half-carry from bit 11 to bit 12
        if ((previous_hl & 0x0FFF) + (operand_16 & 0x0FFF)) > 0x0FFF: self.registers[REGISTER_F] |= FLAG_H
        else: self.registers[REGISTER_F] &= ~FLAG_H
        # Set the C flag if there is a carry from bit 15
        if result > 0xFFFF: self.registers[REGISTER_F] |= FLAG_C
        else: self.registers[REGISTER_F] &= ~FLAG_C

    def _add_a(self, operand_2, use_carry=False):
        carry = (self.registers[REGISTER_F] & FLAG_C) >> 4  if use_carry else 0
        operand_1 = self.registers[REGISTER_A]
        result = operand_1 + operand_2 + carry
        self.registers[REGISTER_A] = result & 0xFF

        self.registers[REGISTER_F] = 0x00
        if self.registers[REGISTER_A] == 0:
            self.registers[REGISTER_F] |= FLAG_Z
        self.registers[REGISTER_F] &= ~FLAG_N
        if result > 0xFF:
            self.registers[REGISTER_F] |= FLAG_C
        if ((operand_1 & 0x0F) + (operand_2 & 0x0F) + carry) > 0x0F:
            self.registers[REGISTER_F] |= FLAG_H

    def _sub_a(self, operand_2, use_carry=False, compare=False):
        carry = (self.registers[REGISTER_F] & FLAG_C) >> 4 if use_carry else 0
        operand_1 = self.registers[REGISTER_A]
        result = operand_1 - operand_2 - carry
        if not compare: self.registers[REGISTER_A] = result & 0xFF

        self.registers[REGISTER_F] = 0x00
        if result & 0xFF == 0:
            self.registers[REGISTER_F] |= FLAG_Z
        self.registers[REGISTER_F] |= FLAG_N
        if result < 0:
            self.registers[REGISTER_F] |= FLAG_C
        if ((operand_1 & 0x0F) < (operand_2 & 0x0F) + carry):
            self.registers[REGISTER_F] |= FLAG_H

    def _and_a(self, operand2):
        self.registers[REGISTER_A] &= operand2  # AND operation

        # Set flags
        self.registers[REGISTER_F] = 0
        if self.registers[REGISTER_A] == 0:
            self.registers[REGISTER_F] |= FLAG_Z  # Set zero flag
        # Half carry is always set for AND
        self.registers[REGISTER_F] |= FLAG_H 

    def _xor_a(self, operand2):
        self.registers[REGISTER_A] ^= operand2  # XOR operation

        # Set flags
        self.registers[REGISTER_F] = 0
        if self.registers[REGISTER_A] == 0:
            self.registers[REGISTER_F] |= FLAG_Z  # Set zero flag

    def _or_a(self, operand2):
        self.registers[REGISTER_A] |= operand2  # OR operation

        # Set flags
        self.registers[REGISTER_F] = 0
        if self.registers[REGISTER_A] == 0:
            self.registers[REGISTER_F] |= FLAG_Z  # Set zero flag
