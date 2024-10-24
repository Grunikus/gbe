import unittest
from cpu import CPU, FLAG_Z, FLAG_N, FLAG_H, FLAG_C, REGISTER_A, REGISTER_F, REGISTER_B, REGISTER_C, REGISTER_D, REGISTER_E, REGISTER_H, REGISTER_L
from memory import Memory
import opcodes

class TestCPU(unittest.TestCase):
    def setUp(self):
        self.memory = Memory()
        self.cpu = CPU(self.memory)
        # Initialize registers to trigger Z, H, and C flags across operations
        self.cpu.registers[REGISTER_A] = 0x01
        self.cpu.registers[REGISTER_B] = 0x01  # SUB A, r: A = 0x01 - 0x01 = 0x00 (Z = 1, N = 1, H = 0, C = 0)
        self.cpu.registers[REGISTER_C] = 0x02  # SUB A, r: A = 0x01 - 0x02 = 0xFF (C = 1, Z = 0, H = 1)
        self.cpu.registers[REGISTER_D] = 0x0F  # ADD A, r: A = 0x01 + 0x0F = 0x10 (H = 1, Z = 0, C = 0)
        self.cpu.registers[REGISTER_E] = 0x11  # SUB A, r: A = 0x21 - 0x11 = 0x10 (H = 1, Z = 0, C = 0)
        self.cpu.registers[REGISTER_H] = 0x06
        self.cpu.registers[REGISTER_L] = 0xFF  # ADD A, r: A = 0x01 + 0xFF = 0x00 (Z = 1, H = 1, C = 1)

    def _initialize_memory_at_hl(self, hl_high, hl_low, value):
        """Helper function to set registers H, L, and memory at the HL address."""
        self.cpu.registers[REGISTER_H] = hl_high
        self.cpu.registers[REGISTER_L] = hl_low
        self.memory.write_byte((hl_high << 8) | hl_low, value)

    def _calculate_flags(self, result, carry_out, half_carry, is_subtraction):
        z_flag = FLAG_Z if result == 0 else 0
        n_flag = FLAG_N if is_subtraction else 0
        h_flag = FLAG_H if half_carry else 0
        c_flag = FLAG_C if carry_out else 0
        return z_flag | n_flag | h_flag | c_flag

    def _add(self, operand1, operand2, carry_in):
        carry_in = 1 if carry_in else 0
        result = operand1 + operand2 + carry_in

        return_value = result & 0xFF

        carry_out = result > 0xFF
        half_carry = ((operand1 & 0x0F) + (operand2 & 0x0F) + carry_in) > 0x0F
        flags = self._calculate_flags(return_value, carry_out, half_carry, is_subtraction=False)
        return return_value,flags

    def _sub(self, operand1, operand2, carry_in):
        carry_in = 1 if carry_in else 0
        result = operand1 - operand2 - carry_in

        return_value = result & 0xFF

        carry_out = result < 0
        half_carry = ((operand1 & 0x0F) - (operand2 & 0x0F) - carry_in) < 0
        flags = self._calculate_flags(return_value, carry_out, half_carry, is_subtraction=True)
        return return_value,flags

    def test_add_register(self):
        OPERANDS = {
            opcodes.ADD_A_A: REGISTER_A,
            opcodes.ADD_A_B: REGISTER_B,
            opcodes.ADD_A_C: REGISTER_C,
            opcodes.ADD_A_D: REGISTER_D,
            opcodes.ADD_A_E: REGISTER_E,
            opcodes.ADD_A_H: REGISTER_H,
            opcodes.ADD_A_L: REGISTER_L,
        }

        for opcode, register in OPERANDS.items():
            OPERAND1 = 0x01
            OPERAND2 = self.cpu.registers[register]
            self.cpu.registers[REGISTER_A] = OPERAND1

            expected_result, flags = self._add(OPERAND1, OPERAND2, carry_in= self.cpu.registers[REGISTER_F] & FLAG_C)

            self.memory.write_byte(self.cpu.pc, opcode)
            self.cpu.step()

            self.assertEqual(self.cpu.registers[REGISTER_A], expected_result, f"{opcode=},{register=}")
            self.assertEqual(self.cpu.registers[REGISTER_F], flags, f"{opcode=},{register=}")

    def test_add_hl(self):
        opcode = opcodes.ADD_A_HL
        OPERAND1 = 0x01
        OPERAND2 = 0x02
        INITIAL_CARRY_STATUS = 1
        expected_result, flags = self._add(OPERAND1, OPERAND2, carry_in= 0)
 
        ADDR_HIGH, ADDR_LOW = 0x00, 0x10
        self._initialize_memory_at_hl(ADDR_HIGH, ADDR_LOW, OPERAND2)
        self.cpu.registers[REGISTER_A] = OPERAND1
        if INITIAL_CARRY_STATUS:
            self.cpu.registers[REGISTER_F] |= FLAG_C
        else:
            self.cpu.registers[REGISTER_F] &= ~FLAG_C
        self.memory.write_byte(self.cpu.pc, opcode)
        self.cpu.step()

        self.assertEqual(self.cpu.registers[REGISTER_A], expected_result)
        self.assertEqual(self.cpu.registers[REGISTER_F], flags)

    def test_adc_register(self):
        OPERANDS = {
            opcodes.ADC_A_A: REGISTER_A,
            opcodes.ADC_A_B: REGISTER_B,
            opcodes.ADC_A_C: REGISTER_C,
            opcodes.ADC_A_D: REGISTER_D,
            opcodes.ADC_A_E: REGISTER_E,
            opcodes.ADC_A_H: REGISTER_H,
            opcodes.ADC_A_L: REGISTER_L,
        }

        for opcode, register in OPERANDS.items():
            OPERAND1 = 0x01
            OPERAND2 = self.cpu.registers[register]
            INITIAL_CARRY_STATUS = 1
            expected_result, flags = self._add(OPERAND1, OPERAND2, carry_in= INITIAL_CARRY_STATUS)

            self.cpu.registers[REGISTER_A] = OPERAND1
            if INITIAL_CARRY_STATUS:
                self.cpu.registers[REGISTER_F] |= FLAG_C
            else:
                self.cpu.registers[REGISTER_F] &= ~FLAG_C
            self.memory.write_byte(self.cpu.pc, opcode)
            self.cpu.step()

            self.assertEqual(self.cpu.registers[REGISTER_A], expected_result)
            self.assertEqual(self.cpu.registers[REGISTER_F], flags)

    def test_adc_hl(self):
        opcode = opcodes.ADC_A_HL
        OPERAND1 = 0x01
        OPERAND2 = 0x02
        INITIAL_CARRY_STATUS = 1
        expected_result, flags = self._add(OPERAND1, OPERAND2, carry_in= INITIAL_CARRY_STATUS)

        ADDR_HIGH, ADDR_LOW = 0x00, 0x10
        self._initialize_memory_at_hl(ADDR_HIGH, ADDR_LOW, OPERAND2)

        self.cpu.registers[REGISTER_A] = OPERAND1
        if INITIAL_CARRY_STATUS:
            self.cpu.registers[REGISTER_F] |= FLAG_C
        else:
            self.cpu.registers[REGISTER_F] &= ~FLAG_C
        self.memory.write_byte(self.cpu.pc, opcode)
        self.cpu.step()

        self.assertEqual(self.cpu.registers[REGISTER_A], expected_result)
        self.assertEqual(self.cpu.registers[REGISTER_F], flags)

    def test_sub_register(self):
        OPERANDS = {
            opcodes.SUB_A_A: REGISTER_A,
            opcodes.SUB_A_B: REGISTER_B,
            opcodes.SUB_A_C: REGISTER_C,
            opcodes.SUB_A_D: REGISTER_D,
            opcodes.SUB_A_E: REGISTER_E,
            opcodes.SUB_A_H: REGISTER_H,
            opcodes.SUB_A_L: REGISTER_L,
        }

        for opcode, register in OPERANDS.items():
            A_INITIAL = 0x01
            operand = self.cpu.registers[register]

            self.cpu.registers[REGISTER_A] = A_INITIAL
            self.memory.write_byte(self.cpu.pc, opcode)
            self.cpu.step()

            # Calculate expected result
            expected_result = (A_INITIAL - operand) & 0xFF
            carry_out = A_INITIAL < operand
            half_carry = (A_INITIAL & 0x0F) < (operand & 0x0F)
            flags = self._calculate_flags(expected_result, carry_out, half_carry, is_subtraction=True)

            # Check result in register A and flags
            self.assertEqual(self.cpu.registers[REGISTER_A], expected_result)
            self.assertEqual(self.cpu.registers[REGISTER_F], flags)

    def test_sub_hl(self):
        opcode = opcodes.SUB_A_HL
        OPERAND1 = 0x03
        OPERAND2 = 0x02
        INITIAL_CARRY_STATUS = 1
        expected_result, flags = self._sub(OPERAND1, OPERAND2, carry_in=0)

        ADDR_HIGH, ADDR_LOW = 0x00, 0x10
        self._initialize_memory_at_hl(ADDR_HIGH, ADDR_LOW, OPERAND2)

        self.cpu.registers[REGISTER_A] = OPERAND1
        if INITIAL_CARRY_STATUS:
            self.cpu.registers[REGISTER_F] |= FLAG_C
        else:
            self.cpu.registers[REGISTER_F] &= ~FLAG_C
        self.memory.write_byte(self.cpu.pc, opcode)
        self.cpu.step()

        self.assertEqual(self.cpu.registers[REGISTER_A], expected_result)
        self.assertEqual(self.cpu.registers[REGISTER_F], flags)

    def test_sbc_register(self):
        OPERANDS = {
            opcodes.SBC_A_A: REGISTER_A,
            opcodes.SBC_A_B: REGISTER_B,
            opcodes.SBC_A_C: REGISTER_C,
            opcodes.SBC_A_D: REGISTER_D,
            opcodes.SBC_A_E: REGISTER_E,
            opcodes.SBC_A_H: REGISTER_H,
            opcodes.SBC_A_L: REGISTER_L,
        }

        for opcode, register in OPERANDS.items():
            OPERAND1 = 0x01
            OPERAND2 = self.cpu.registers[register]
            INITIAL_CARRY_STATUS = 1
            expected_result, flags = self._sub(OPERAND1, OPERAND2, carry_in= INITIAL_CARRY_STATUS)

            self.cpu.registers[REGISTER_A] = OPERAND1
            if INITIAL_CARRY_STATUS:
                self.cpu.registers[REGISTER_F] |= FLAG_C
            else:
                self.cpu.registers[REGISTER_F] &= ~FLAG_C
            self.memory.write_byte(self.cpu.pc, opcode)
            self.cpu.step()

            self.assertEqual(self.cpu.registers[REGISTER_A], expected_result)
            self.assertEqual(self.cpu.registers[REGISTER_F], flags)

    def test_sbc_hl(self):
        opcode = opcodes.SBC_A_HL
        OPERAND1 = 0x03
        OPERAND2 = 0x02
        INITIAL_CARRY_STATUS = 1
        expected_result, flags = self._sub(OPERAND1, OPERAND2, carry_in= INITIAL_CARRY_STATUS)

        ADDR_HIGH, ADDR_LOW = 0x00, 0x10
        self._initialize_memory_at_hl(ADDR_HIGH, ADDR_LOW, OPERAND2)

        self.cpu.registers[REGISTER_A] = OPERAND1
        if INITIAL_CARRY_STATUS:
            self.cpu.registers[REGISTER_F] |= FLAG_C
        else:
            self.cpu.registers[REGISTER_F] &= ~FLAG_C
        self.memory.write_byte(self.cpu.pc, opcode)
        self.cpu.step()

        self.assertEqual(self.cpu.registers[REGISTER_A], expected_result)
        self.assertEqual(self.cpu.registers[REGISTER_F], flags)

if __name__ == '__main__':
    unittest.main()
