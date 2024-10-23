from memory import Memory
from opcodes import ADD_A_B, ADD_A_C, ADD_A_D, ADD_A_E, ADD_A_H, ADD_A_L, ADD_A_HL, ADD_A_A
from opcodes import ADC_A_B, ADC_A_C, ADC_A_D, ADC_A_E, ADC_A_H, ADC_A_L, ADC_A_HL, ADC_A_A

FLAG_ZERO = 0b10000000
FLAG_SUBS = 0b01000000
FLAG_CARR = 0b00010000

START_PC = 0x0100
START_SP = 0xFFFE

INSTRUCTION_MAP = {}
INSTRUCTION_MAP.update({
    ADD_A_B:    lambda self: self.add_register('B'),
    ADD_A_C:    lambda self: self.add_register('C'),
    ADD_A_D:    lambda self: self.add_register('D'),
    ADD_A_E:    lambda self: self.add_register('E'),
    ADD_A_H:    lambda self: self.add_register('H'),
    ADD_A_L:    lambda self: self.add_register('L'),
    ADD_A_HL:   lambda self: self.add_hl(),
    ADD_A_A:    lambda self: self.add_register('A'),
})
INSTRUCTION_MAP.update({
    ADC_A_B:    lambda self: self.adc_register('B'),
    ADC_A_C:    lambda self: self.adc_register('C'),
    ADC_A_D:    lambda self: self.adc_register('D'),
    ADC_A_E:    lambda self: self.adc_register('E'),
    ADC_A_H:    lambda self: self.adc_register('H'),
    ADC_A_L:    lambda self: self.adc_register('L'),
    ADC_A_HL:   lambda self: self.adc_hl(),
    ADC_A_A:    lambda self: self.adc_register('A'),
})

class CPU:
    def __init__(self, memory: Memory):
        self.memory = memory
        self.registers = {
            'A': 0x00, 'F': 0x00,  # Flags
            'B': 0x00, 'C': 0x00,
            'D': 0x00, 'E': 0x00,
            'H': 0x00, 'L': 0x00,
        }
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
        self.registers['F'] = 0x00
        if result & 0xFF == 0:
            self.registers['F'] |= FLAG_ZERO
        if subtraction:
            self.registers['F'] |= FLAG_SUBS
        if result > 0xFF:
            self.registers['F'] |= FLAG_CARR

    def _add_a(self, value):
        result = self.registers['A'] + value
        self.registers['A'] = result & 0xFF
        self._update_flags(result)

    def add_register(self, register: str):
        self._add_a( self.registers[register] )

    def add_hl(self):
        address = (self.registers['H'] << 8) | self.registers['L']
        memory_value = self.memory.read_byte(address)
        self._add_a(memory_value)

    def _adc_a(self, value):
        carry = (self.registers['F'] & FLAG_CARR) >> 4  # Extract the carry flag
        result = self.registers['A'] + value + carry
        self.registers['A'] = result & 0xFF
        self._update_flags(result)

    def adc_register(self, register: str):
        self._adc_a(self.registers[register])

    def adc_hl(self):
        address = (self.registers['H'] << 8) | self.registers['L']
        memory_value = self.memory.read_byte(address)
        self._adc_a(memory_value)