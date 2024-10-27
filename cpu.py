from memory import Memory
from opcodes import (
    ADD_A_B, ADD_A_C, ADD_A_D, ADD_A_E, ADD_A_H, ADD_A_L, ADD_A_HL, ADD_A_A,
    ADC_A_B, ADC_A_C, ADC_A_D, ADC_A_E, ADC_A_H, ADC_A_L, ADC_A_HL, ADC_A_A,
    SUB_A_B, SUB_A_C, SUB_A_D, SUB_A_E, SUB_A_H, SUB_A_L, SUB_A_HL, SUB_A_A,
    SBC_A_B, SBC_A_C, SBC_A_D, SBC_A_E, SBC_A_H, SBC_A_L, SBC_A_HL, SBC_A_A,
    AND_A_B, AND_A_C, AND_A_D, AND_A_E, AND_A_H, AND_A_L, AND_A_HL, AND_A_A,
    XOR_A_B, XOR_A_C, XOR_A_D, XOR_A_E, XOR_A_H, XOR_A_L, XOR_A_HL, XOR_A_A,
    OR_A_B, OR_A_C, OR_A_D, OR_A_E, OR_A_H, OR_A_L, OR_A_HL, OR_A_A,
    CP_A_B, CP_A_C, CP_A_D, CP_A_E, CP_A_H, CP_A_L, CP_A_HL, CP_A_A,
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

    def _fill_instruction_map(self):
        def _map_opcode_register_pairs_to_operation(opcode_register_pairs, operation, **kwargs):
            for opcode, register in opcode_register_pairs:
                self.INSTRUCTION_MAP[opcode] = lambda self, reg=register: operation(self, self.registers[reg], **kwargs)
        def _map_opcode_hl_imm_to_operation(opcode_hl, opcode_imm, operation, **kwargs):
            self.INSTRUCTION_MAP[opcode_hl] = lambda self: operation(self, self._read_byte_at_memory_hl(), **kwargs)
            self.INSTRUCTION_MAP[opcode_imm] = lambda self: (operation(self, self.memory.read_byte(self.pc), **kwargs), setattr(self, 'pc', self.pc + 1))
        # Define the opcode-register pairs and call add_instruction_map with the appropriate function and flags
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
        _map_opcode_hl_imm_to_operation(ADD_A_HL, ADD_A_IMM, lambda self, value: self._add_a(value))
        _map_opcode_hl_imm_to_operation(ADC_A_HL, ADC_A_IMM, lambda self, value: self._add_a(value, use_carry=True))
        _map_opcode_hl_imm_to_operation(SUB_A_HL, SUB_A_IMM, lambda self, value: self._sub_a(value))
        _map_opcode_hl_imm_to_operation(SBC_A_HL, SBC_A_IMM, lambda self, value: self._sub_a(value, use_carry=True))
        _map_opcode_hl_imm_to_operation(AND_A_HL, AND_A_IMM, lambda self, value: self._and_a(value))
        _map_opcode_hl_imm_to_operation(XOR_A_HL, XOR_A_IMM, lambda self, value: self._xor_a(value))
        _map_opcode_hl_imm_to_operation(OR_A_HL, OR_A_IMM, lambda self, value: self._or_a(value))
        _map_opcode_hl_imm_to_operation(CP_A_HL, CP_A_IMM, lambda self, value: self._sub_a(value, compare=True))

    def step(self):
        opcode = self.memory.read_byte(self.pc)
        self.pc += 1
        self.INSTRUCTION_MAP[opcode](self)

    def _read_byte_at_memory_hl(self):
        address = (self.registers[REGISTER_H] << 8) | self.registers[REGISTER_L]
        memory_value = self.memory.read_byte(address)
        return memory_value

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
