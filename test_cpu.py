import unittest
from cpu import CPU, FLAG_ZERO, FLAG_CARR, FLAG_SUBS
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
        addr_high = 0x00
        addr_low = 0x10
        # Set registers and memory
        self.cpu.registers['A'] = 0x01
        self.cpu.registers['H'] = addr_high
        self.cpu.registers['L'] = addr_low

        self.memory.write_byte( (addr_high << 8) | addr_low, 0x02 )  # Set memory at HL to 2

        # Execute instruction
        self.memory.write_byte(self.cpu.pc, opcodes.ADD_A_HL)
        self.cpu.step()
        
        # Check result
        self.assertEqual(self.cpu.registers['A'], 0x03)
        self.assertEqual(self.cpu.registers['F'], 0x00)

    def test_adc_register(self):
        # Set registers
        self.cpu.registers['B'] = 0x02
        self.cpu.registers['C'] = 0x03
        self.cpu.registers['D'] = 0x04
        self.cpu.registers['E'] = 0x05
        self.cpu.registers['H'] = 0x06
        self.cpu.registers['L'] = 0x07

        EXPECTED_RESULTS = {
            opcodes.ADC_A_A : 0x03,  # A = 1 + 1 (A) + 1 (carry)
            opcodes.ADC_A_B : 0x04,  # A = 1 + 2 (B) + 1 (carry)
            opcodes.ADC_A_C : 0x05,  # A = 1 + 3 (C) + 1 (carry)
            opcodes.ADC_A_D : 0x06,  # A = 1 + 4 (D) + 1 (carry)
            opcodes.ADC_A_E : 0x07,  # A = 1 + 5 (E) + 1 (carry)
            opcodes.ADC_A_H : 0x08,  # A = 1 + 6 (H) + 1 (carry)
            opcodes.ADC_A_L : 0x09,  # A = 1 + 7 (L) + 1 (carry)
        }

        for opcode, expected_result in EXPECTED_RESULTS.items():
            # Simulate carry flag set (carry flag is bit 4)
            self.cpu.registers['F'] = 0x10  # Set carry flag
            # Execute instruction
            self.cpu.registers['A'] = 0x01
            self.memory.write_byte(self.cpu.pc, opcode)
            self.cpu.step()
            # Check result
            self.assertEqual(self.cpu.registers['A'], expected_result)
            self.assertEqual(self.cpu.registers['F'] & FLAG_ZERO, 0x00)
            self.assertEqual(self.cpu.registers['F'] & FLAG_SUBS, 0x00)

    def test_adc_hl(self):
        addr_high = 0x00
        addr_low = 0x10
        # Set registers and memory
        self.cpu.registers['A'] = 0x01
        self.cpu.registers['H'] = addr_high
        self.cpu.registers['L'] = addr_low

        # Simulate carry flag set
        self.cpu.registers['F'] = 0x10  # Set carry flag
        self.memory.write_byte((addr_high << 8) | addr_low, 0x02)  # Set memory at HL to 2

        # Execute instruction
        self.memory.write_byte(self.cpu.pc, opcodes.ADC_A_HL)
        self.cpu.step()

        # Check result
        self.assertEqual(self.cpu.registers['A'], 0x04)  # A = 1 + 2 + 1 (carry)
        self.assertEqual(self.cpu.registers['F'] & FLAG_ZERO, 0x00)
        self.assertEqual(self.cpu.registers['F'] & FLAG_SUBS, 0x00)

if __name__ == '__main__':
    unittest.main()
