import unittest
from cpu import CPU, FLAG_Z, FLAG_C, FLAG_N, REGISTER_A, REGISTER_F, REGISTER_B, REGISTER_C, REGISTER_D, REGISTER_E, REGISTER_H, REGISTER_L
from memory import Memory
import opcodes

class TestCPU(unittest.TestCase):
    def setUp(self):
        self.memory = Memory()
        self.cpu = CPU(self.memory)

    def test_add_register(self):
        # Set registers
        self.cpu.registers[REGISTER_B] = 0x02
        self.cpu.registers[REGISTER_C] = 0x03
        self.cpu.registers[REGISTER_D] = 0x04
        self.cpu.registers[REGISTER_E] = 0x05
        self.cpu.registers[REGISTER_H] = 0x06
        self.cpu.registers[REGISTER_L] = 0x07

        EXPECTED_RESULTS = {
            opcodes.ADD_A_A: 0x02,
            opcodes.ADD_A_B: 0x03,
            opcodes.ADD_A_C: 0x04,
            opcodes.ADD_A_D: 0x05,
            opcodes.ADD_A_E: 0x06,
            opcodes.ADD_A_H: 0x07,
            opcodes.ADD_A_L: 0x08,
        }

        for opcode, expected_result in EXPECTED_RESULTS.items():
            # Execute instruction
            self.cpu.registers[REGISTER_A] = 0x01
            self.memory.write_byte(self.cpu.pc, opcode)
            self.cpu.step()
            # Check result
            self.assertEqual(self.cpu.registers[REGISTER_A], expected_result)
            self.assertEqual(self.cpu.registers[REGISTER_F], 0x00)

    def test_add_hl(self):
        addr_high = 0x00
        addr_low = 0x10
        # Set registers and memory
        self.cpu.registers[REGISTER_A] = 0x01
        self.cpu.registers[REGISTER_H] = addr_high
        self.cpu.registers[REGISTER_L] = addr_low

        self.memory.write_byte((addr_high << 8) | addr_low, 0x02)  # Set memory at HL to 2

        # Execute instruction
        self.memory.write_byte(self.cpu.pc, opcodes.ADD_A_HL)
        self.cpu.step()

        # Check result
        self.assertEqual(self.cpu.registers[REGISTER_A], 0x03)
        self.assertEqual(self.cpu.registers[REGISTER_F], 0x00)

    def test_adc_register(self):
        # Set registers
        self.cpu.registers[REGISTER_B] = 0x02
        self.cpu.registers[REGISTER_C] = 0x03
        self.cpu.registers[REGISTER_D] = 0x04
        self.cpu.registers[REGISTER_E] = 0x05
        self.cpu.registers[REGISTER_H] = 0x06
        self.cpu.registers[REGISTER_L] = 0x07

        EXPECTED_RESULTS = {
            opcodes.ADC_A_A: 0x03,  # A = 1 + 1 (A) + 1 (carry)
            opcodes.ADC_A_B: 0x04,  # A = 1 + 2 (B) + 1 (carry)
            opcodes.ADC_A_C: 0x05,  # A = 1 + 3 (C) + 1 (carry)
            opcodes.ADC_A_D: 0x06,  # A = 1 + 4 (D) + 1 (carry)
            opcodes.ADC_A_E: 0x07,  # A = 1 + 5 (E) + 1 (carry)
            opcodes.ADC_A_H: 0x08,  # A = 1 + 6 (H) + 1 (carry)
            opcodes.ADC_A_L: 0x09,  # A = 1 + 7 (L) + 1 (carry)
        }

        for opcode, expected_result in EXPECTED_RESULTS.items():
            # Simulate carry flag set (carry flag is bit 4)
            self.cpu.registers[REGISTER_F] = 0x10  # Set carry flag
            # Execute instruction
            self.cpu.registers[REGISTER_A] = 0x01
            self.memory.write_byte(self.cpu.pc, opcode)
            self.cpu.step()
            # Check result
            self.assertEqual(self.cpu.registers[REGISTER_A], expected_result)
            self.assertEqual(self.cpu.registers[REGISTER_F] & FLAG_Z, 0x00)
            self.assertEqual(self.cpu.registers[REGISTER_F] & FLAG_N, 0x00)

    def test_adc_hl(self):
        addr_high = 0x00
        addr_low = 0x10
        # Set registers and memory
        self.cpu.registers[REGISTER_A] = 0x01
        self.cpu.registers[REGISTER_H] = addr_high
        self.cpu.registers[REGISTER_L] = addr_low

        # Simulate carry flag set
        self.cpu.registers[REGISTER_F] = 0x10  # Set carry flag
        self.memory.write_byte((addr_high << 8) | addr_low, 0x02)  # Set memory at HL to 2

        # Execute instruction
        self.memory.write_byte(self.cpu.pc, opcodes.ADC_A_HL)
        self.cpu.step()

        # Check result
        self.assertEqual(self.cpu.registers[REGISTER_A], 0x04)  # A = 1 + 2 + 1 (carry)
        self.assertEqual(self.cpu.registers[REGISTER_F] & FLAG_Z, 0x00)
        self.assertEqual(self.cpu.registers[REGISTER_F] & FLAG_N, 0x00)

    def test_sub_register(self):
        # Set registers
        self.cpu.registers[REGISTER_B] = 0x02
        self.cpu.registers[REGISTER_C] = 0x03
        self.cpu.registers[REGISTER_D] = 0x04
        self.cpu.registers[REGISTER_E] = 0x05
        self.cpu.registers[REGISTER_H] = 0x06
        self.cpu.registers[REGISTER_L] = 0x07

        EXPECTED_RESULTS = {
            opcodes.SUB_A_A: 0x00,  # A - A = 1 - 1
            opcodes.SUB_A_B: 0xFF,  # A - B = 1 - 2
            opcodes.SUB_A_C: 0xFE,  # A - C = 1 - 3
            opcodes.SUB_A_D: 0xFD,  # A - D = 1 - 4
            opcodes.SUB_A_E: 0xFC,  # A - E = 1 - 5
            opcodes.SUB_A_H: 0xFB,  # A - H = 1 - 6
            opcodes.SUB_A_L: 0xFA,  # A - L = 1 - 7
        }

        for opcode, expected_result in EXPECTED_RESULTS.items():
            # Execute instruction
            self.cpu.registers[REGISTER_A] = 0x01
            self.memory.write_byte(self.cpu.pc, opcode)
            self.cpu.step()
            # Check result
            self.assertEqual(self.cpu.registers[REGISTER_A], expected_result)
            
            # Check flags
            if expected_result == 0x00:
                # If the result is zero, Z flag should be set
                self.assertEqual(self.cpu.registers[REGISTER_F] & FLAG_Z, FLAG_Z)
            else:
                # If the result is non-zero, Z flag should not be set
                self.assertEqual(self.cpu.registers[REGISTER_F] & FLAG_Z, 0x00)
            
            # N flag should always be set for subtraction
            self.assertEqual(self.cpu.registers[REGISTER_F] & FLAG_N, FLAG_N)
            
            # C flag should be set if the result caused an underflow
            if expected_result > 0xFF:
                self.assertEqual(self.cpu.registers[REGISTER_F] & FLAG_C, FLAG_C)
            else:
                self.assertEqual(self.cpu.registers[REGISTER_F] & FLAG_C, 0x00)

    def test_sub_hl(self):
        addr_high = 0x00
        addr_low = 0x10
        # Set registers and memory
        self.cpu.registers[REGISTER_A] = 0x03
        self.cpu.registers[REGISTER_H] = addr_high
        self.cpu.registers[REGISTER_L] = addr_low

        self.memory.write_byte((addr_high << 8) | addr_low, 0x02)  # Set memory at HL to 2

        # Execute instruction
        self.memory.write_byte(self.cpu.pc, opcodes.SUB_A_HL)
        self.cpu.step()

        # Check result
        self.assertEqual(self.cpu.registers[REGISTER_A], 0x01)  # A = 3 - 2
        self.assertEqual(self.cpu.registers[REGISTER_F] & FLAG_Z, 0x00)  # Z flag should be 0
        self.assertEqual(self.cpu.registers[REGISTER_F] & FLAG_N, FLAG_N)  # N flag should be set

    def test_sbc_register(self):
        # Set registers
        self.cpu.registers[REGISTER_B] = 0x02
        self.cpu.registers[REGISTER_C] = 0x03
        self.cpu.registers[REGISTER_D] = 0x04
        self.cpu.registers[REGISTER_E] = 0x05
        self.cpu.registers[REGISTER_H] = 0x06
        self.cpu.registers[REGISTER_L] = 0x07

        EXPECTED_RESULTS = {
            opcodes.SBC_A_A: 0xFF,  # A = 1 - 1 - 1 (carry)
            opcodes.SBC_A_B: 0xFE,  # A = 1 - 2 - 1 (carry)
            opcodes.SBC_A_C: 0xFD,  # A = 1 - 3 - 1 (carry)
            opcodes.SBC_A_D: 0xFC,  # A = 1 - 4 - 1 (carry)
            opcodes.SBC_A_E: 0xFB,  # A = 1 - 5 - 1 (carry)
            opcodes.SBC_A_H: 0xFA,  # A = 1 - 6 - 1 (carry)
            opcodes.SBC_A_L: 0xF9,  # A = 1 - 7 - 1 (carry)
        }

        for opcode, expected_result in EXPECTED_RESULTS.items():
            # Simulate carry flag set
            self.cpu.registers[REGISTER_F] = FLAG_C  # Set carry flag
            # Execute instruction
            self.cpu.registers[REGISTER_A] = 0x01
            self.memory.write_byte(self.cpu.pc, opcode)
            self.cpu.step()
            # Check result
            self.assertEqual(self.cpu.registers[REGISTER_A], expected_result)
            self.assertEqual(self.cpu.registers[REGISTER_F] & FLAG_Z, 0x00)  # Z flag should be 0
            self.assertEqual(self.cpu.registers[REGISTER_F] & FLAG_N, FLAG_N)  # N flag should be set

    def test_sbc_hl(self):
        addr_high = 0x00
        addr_low = 0x10
        # Set registers and memory
        self.cpu.registers[REGISTER_A] = 0x03
        self.cpu.registers[REGISTER_H] = addr_high
        self.cpu.registers[REGISTER_L] = addr_low

        # Simulate carry flag set
        self.cpu.registers[REGISTER_F] = FLAG_C  # Set carry flag
        self.memory.write_byte((addr_high << 8) | addr_low, 0x02)  # Set memory at HL to 2

        # Execute instruction
        self.memory.write_byte(self.cpu.pc, opcodes.SBC_A_HL)
        self.cpu.step()

        # Check result
        self.assertEqual(self.cpu.registers[REGISTER_A], 0x00)  # A = 3 - 2 - 1 (carry)
        self.assertEqual(self.cpu.registers[REGISTER_F] & FLAG_Z, FLAG_Z)  # Z flag should be set
        self.assertEqual(self.cpu.registers[REGISTER_F] & FLAG_N, FLAG_N)  # N flag should be set

if __name__ == '__main__':
    unittest.main()
