from memory import Memory
from opcodes import (
    ADD_A_B, ADD_A_C, ADD_A_D, ADD_A_E, ADD_A_H, ADD_A_L, ADD_A_HL, ADD_A_A,
    ADC_A_B, ADC_A_C, ADC_A_D, ADC_A_E, ADC_A_H, ADC_A_L, ADC_A_HL, ADC_A_A,
    SUB_A_B, SUB_A_C, SUB_A_D, SUB_A_E, SUB_A_H, SUB_A_L, SUB_A_HL, SUB_A_A,
    SBC_A_B, SBC_A_C, SBC_A_D, SBC_A_E, SBC_A_H, SBC_A_L, SBC_A_HL, SBC_A_A,
    AND_B, AND_C, AND_D, AND_E, AND_H, AND_L, AND_HL, AND_A,
    XOR_B, XOR_C, XOR_D, XOR_E, XOR_H, XOR_L, XOR_HL, XOR_A,
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

INSTRUCTION_MAP = {}
INSTRUCTION_MAP.update({
    ADD_A_A:    lambda self: self.add_register(REGISTER_A),
    ADD_A_B:    lambda self: self.add_register(REGISTER_B),
    ADD_A_C:    lambda self: self.add_register(REGISTER_C),
    ADD_A_D:    lambda self: self.add_register(REGISTER_D),
    ADD_A_E:    lambda self: self.add_register(REGISTER_E),
    ADD_A_H:    lambda self: self.add_register(REGISTER_H),
    ADD_A_L:    lambda self: self.add_register(REGISTER_L),
    ADD_A_HL:   lambda self: self.add_hl(),
})
INSTRUCTION_MAP.update({
    ADC_A_A:    lambda self: self.adc_register(REGISTER_A),
    ADC_A_B:    lambda self: self.adc_register(REGISTER_B),
    ADC_A_C:    lambda self: self.adc_register(REGISTER_C),
    ADC_A_D:    lambda self: self.adc_register(REGISTER_D),
    ADC_A_E:    lambda self: self.adc_register(REGISTER_E),
    ADC_A_H:    lambda self: self.adc_register(REGISTER_H),
    ADC_A_L:    lambda self: self.adc_register(REGISTER_L),
    ADC_A_HL:   lambda self: self.adc_hl(),
})
INSTRUCTION_MAP.update({
    SUB_A_A:    lambda self: self.sub_register(REGISTER_A),
    SUB_A_B:    lambda self: self.sub_register(REGISTER_B),
    SUB_A_C:    lambda self: self.sub_register(REGISTER_C),
    SUB_A_D:    lambda self: self.sub_register(REGISTER_D),
    SUB_A_E:    lambda self: self.sub_register(REGISTER_E),
    SUB_A_H:    lambda self: self.sub_register(REGISTER_H),
    SUB_A_L:    lambda self: self.sub_register(REGISTER_L),
    SUB_A_HL:   lambda self: self.sub_hl(),
})
INSTRUCTION_MAP.update({
    SBC_A_A:    lambda self: self.sbc_register(REGISTER_A),
    SBC_A_B:    lambda self: self.sbc_register(REGISTER_B),
    SBC_A_C:    lambda self: self.sbc_register(REGISTER_C),
    SBC_A_D:    lambda self: self.sbc_register(REGISTER_D),
    SBC_A_E:    lambda self: self.sbc_register(REGISTER_E),
    SBC_A_H:    lambda self: self.sbc_register(REGISTER_H),
    SBC_A_L:    lambda self: self.sbc_register(REGISTER_L),
    SBC_A_HL:   lambda self: self.sbc_hl(),
})
INSTRUCTION_MAP.update({
    AND_A:    lambda self: self.and_register(REGISTER_A),
    AND_B:    lambda self: self.and_register(REGISTER_B),
    AND_C:    lambda self: self.and_register(REGISTER_C),
    AND_D:    lambda self: self.and_register(REGISTER_D),
    AND_E:    lambda self: self.and_register(REGISTER_E),
    AND_H:    lambda self: self.and_register(REGISTER_H),
    AND_L:    lambda self: self.and_register(REGISTER_L),
    AND_HL:   lambda self: self.and_hl(),
})
INSTRUCTION_MAP.update({
    XOR_A:    lambda self: self.xor_register(REGISTER_A),
    XOR_B:    lambda self: self.xor_register(REGISTER_B),
    XOR_C:    lambda self: self.xor_register(REGISTER_C),
    XOR_D:    lambda self: self.xor_register(REGISTER_D),
    XOR_E:    lambda self: self.xor_register(REGISTER_E),
    XOR_H:    lambda self: self.xor_register(REGISTER_H),
    XOR_L:    lambda self: self.xor_register(REGISTER_L),
    XOR_HL:   lambda self: self.xor_hl(),
})

class CPU:
    def __init__(self, memory: Memory):
        self.memory = memory
        # Registers A, F, B, C, D, E, H, L
        self.registers = [0x00] * 8
        self.pc = START_PC
        self.sp = START_SP

    def fetch_instruction(self):
        opcode = self.memory.read_byte(self.pc)
        self.pc += 1
        return opcode

    def step(self):
        opcode = self.fetch_instruction()
        INSTRUCTION_MAP[opcode](self)

    def _read_hl(self):
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

    def _sub_a(self, operand_2, use_carry=False):
        carry = (self.registers[REGISTER_F] & FLAG_C) >> 4  if use_carry else 0
        operand_1 = self.registers[REGISTER_A]
        result = operand_1 - operand_2 - carry
        self.registers[REGISTER_A] = result & 0xFF

        self.registers[REGISTER_F] = 0x00
        if result & 0xFF == 0:
            self.registers[REGISTER_F] |= FLAG_Z
        self.registers[REGISTER_F] |= FLAG_N
        if result < 0:
            self.registers[REGISTER_F] |= FLAG_C
        if ((operand_1 & 0x0F) < (operand_2 & 0x0F) + carry):
            self.registers[REGISTER_F] |= FLAG_H

    # Instruction implementations
    def add_register(self, register):
        self._add_a( self.registers[register] )

    def add_hl(self):
        memory_value = self._read_hl()
        self._add_a(memory_value)

    def adc_register(self, register):
        self._add_a(self.registers[register], use_carry=True)

    def adc_hl(self):
        memory_value = self._read_hl()
        self._add_a(memory_value, use_carry=True)

    def sub_register(self, register):
        self._sub_a(self.registers[register])

    def sub_hl(self):
        memory_value = self._read_hl()
        self._sub_a(memory_value)

    def sbc_register(self, register):
        self._sub_a(self.registers[register], use_carry=True)

    def sbc_hl(self):
        memory_value = self._read_hl()
        self._sub_a(memory_value, use_carry=True)

    def _and_a(self, operand2):
        self.registers[REGISTER_A] &= operand2  # AND operation

        # Set flags
        self.registers[REGISTER_F] = 0
        if self.registers[REGISTER_A] == 0:
            self.registers[REGISTER_F] |= FLAG_Z  # Set zero flag
        # Half carry is always set for AND
        self.registers[REGISTER_F] |= FLAG_H 

    def and_register(self, register):
        self._and_a( self.registers[register] )

    def and_hl(self):
        self._and_a( self._read_hl() )

    def _xor_a(self, operand2):
        self.registers[REGISTER_A] ^= operand2  # XOR operation

        # Set flags
        self.registers[REGISTER_F] = 0
        if self.registers[REGISTER_A] == 0:
            self.registers[REGISTER_F] |= FLAG_Z  # Set zero flag

    def xor_register(self, register):
        self._xor_a( self.registers[register] )

    def xor_hl(self):
        self._xor_a( self._read_hl() )
