from memory import Memory
from opcodes import ADD_A_B, ADD_A_C, ADD_A_D, ADD_A_E, ADD_A_H, ADD_A_L, ADD_A_HL, ADD_A_A

START_PC = 0x0100
START_SP = 0xFFFE

INSTRUCTION_MAP = {
    ADD_A_B:    lambda self: self.add_register('B'),
    ADD_A_C:    lambda self: self.add_register('C'),
    ADD_A_D:    lambda self: self.add_register('D'),
    ADD_A_E:    lambda self: self.add_register('E'),
    ADD_A_H:    lambda self: self.add_register('H'),
    ADD_A_L:    lambda self: self.add_register('L'),
    ADD_A_HL:   lambda self: self.add_hl(),
    ADD_A_A:    lambda self: self.add_register('A'),
}

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
            self.registers['F'] |= 0x80  # Zero flag
        if subtraction:
            self.registers['F'] |= 0x40  # Subtraction flag
        if result > 0xFF:
            self.registers['F'] |= 0x10  # Carry flag

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
