import unittest
from cpu import CPU
from memory import Memory
import opcodes

class TestCPU(unittest.TestCase):
    def setUp(self):
        self.memory = Memory()
        self.cpu = CPU(self.memory)

    def test_add_register(self):
        # Set registers
        self.cpu.registers['B'] = 0x02
        self.cpu.registers['C'] = 0x03
        self.cpu.registers['D'] = 0x04
        self.cpu.registers['E'] = 0x05
        self.cpu.registers['H'] = 0x06
        self.cpu.registers['L'] = 0x07

        EXPECTED_RESULTS = {
            opcodes.ADD_A_A : 0x02,
            opcodes.ADD_A_B : 0x03,
            opcodes.ADD_A_C : 0x04,
            opcodes.ADD_A_D : 0x05,
            opcodes.ADD_A_E : 0x06,
            opcodes.ADD_A_H : 0x07,
            opcodes.ADD_A_L : 0x08,
        }
        for opcode, expected_result in EXPECTED_RESULTS.items():
            # Execute instruction
            self.cpu.registers['A'] = 0x01
            self.memory.write_byte(self.cpu.pc, opcode)
            self.cpu.step()
            # Check result
            self.assertEqual(self.cpu.registers['A'], expected_result)
            self.assertEqual(self.cpu.registers['F'], 0x00)

    def test_add_hl(self):
        # Set registers and memory
        self.cpu.registers['A'] = 0x01
        self.cpu.registers['H'] = 0x00
        self.cpu.registers['L'] = 0x10

        self.memory.write_byte(0x0010, 0x02)  # Set memory at HL to 2

        # Execute instruction
        self.memory.write_byte(self.cpu.pc, opcodes.ADD_A_HL)
        self.cpu.step()
        
        # Check result
        self.assertEqual(self.cpu.registers['A'], 0x03)
        self.assertEqual(self.cpu.registers['F'], 0x00)

if __name__ == '__main__':
    unittest.main()
