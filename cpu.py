from memory import Memory
from opcodes import ADD_A_B, ADD_A_C, ADD_A_D, ADD_A_E, ADD_A_H, ADD_A_L, ADD_A_HL, ADD_A_A
from opcodes import ADC_A_B, ADC_A_C, ADC_A_D, ADC_A_E, ADC_A_H, ADC_A_L, ADC_A_HL, ADC_A_A

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
    ADD_A_B:    lambda self: self.add_register(REGISTER_B),
    ADD_A_C:    lambda self: self.add_register(REGISTER_C),
    ADD_A_D:    lambda self: self.add_register(REGISTER_D),
    ADD_A_E:    lambda self: self.add_register(REGISTER_E),
    ADD_A_H:    lambda self: self.add_register(REGISTER_H),
    ADD_A_L:    lambda self: self.add_register(REGISTER_L),
    ADD_A_HL:   lambda self: self.add_hl(),
    ADD_A_A:    lambda self: self.add_register(REGISTER_A),
})
INSTRUCTION_MAP.update({
    ADC_A_B:    lambda self: self.adc_register(REGISTER_B),
    ADC_A_C:    lambda self: self.adc_register(REGISTER_C),
    ADC_A_D:    lambda self: self.adc_register(REGISTER_D),
    ADC_A_E:    lambda self: self.adc_register(REGISTER_E),
    ADC_A_H:    lambda self: self.adc_register(REGISTER_H),
    ADC_A_L:    lambda self: self.adc_register(REGISTER_L),
    ADC_A_HL:   lambda self: self.adc_hl(),
    ADC_A_A:    lambda self: self.adc_register(REGISTER_A),
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

    # Instruction implementations
    def _update_flags(self, result, subtraction=False):
        self.registers[REGISTER_F] = 0x00
        if result & 0xFF == 0:
            self.registers[REGISTER_F] |= FLAG_Z
        if subtraction:
            self.registers[REGISTER_F] |= FLAG_N
        if result > 0xFF:
            self.registers[REGISTER_F] |= FLAG_C

    def _add_a(self, value):
        result = self.registers[REGISTER_A] + value
        self.registers[REGISTER_A] = result & 0xFF
        self._update_flags(result)

    def add_register(self, register):
        self._add_a( self.registers[register] )

    def add_hl(self):
        address = (self.registers[REGISTER_H] << 8) | self.registers[REGISTER_L]
        memory_value = self.memory.read_byte(address)
        self._add_a(memory_value)

    def _adc_a(self, value):
        carry = (self.registers[REGISTER_F] & FLAG_C) >> 4  # Extract the carry flag
        result = self.registers[REGISTER_A] + value + carry
        self.registers[REGISTER_A] = result & 0xFF
        self._update_flags(result)

    def adc_register(self, register):
        self._adc_a(self.registers[register])

    def adc_hl(self):
        address = (self.registers[REGISTER_H] << 8) | self.registers[REGISTER_L]
        memory_value = self.memory.read_byte(address)
        self._adc_a(memory_value)