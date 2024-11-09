import random
import unittest
import cpu
from cpu import CPU, FLAG_Z, FLAG_N, FLAG_H, FLAG_C, REGISTER_A, REGISTER_F, REGISTER_B, REGISTER_C, REGISTER_D, REGISTER_E, REGISTER_H, REGISTER_L
from memory import Memory
# Maybe we shouldn't import opcodes but hardcode them in the tests
import opcodes

IMMEDIATE_OPCODES = ( opcodes.ADD_A_IMM, opcodes.ADC_A_IMM, opcodes.SUB_A_IMM, opcodes.SBC_A_IMM, opcodes.AND_A_IMM, opcodes.XOR_A_IMM, opcodes.OR_A_IMM,  opcodes.CP_A_IMM, 
                      opcodes.LD_A_IMM,  opcodes.LD_B_IMM,  opcodes.LD_C_IMM,  opcodes.LD_D_IMM,  opcodes.LD_E_IMM,  opcodes.LD_H_IMM,  opcodes.LD_L_IMM, )

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

    def _random_byte(self):
        return random.randint(0x00, 0xFF)

    def _step_and_assert_flags(self, opcode, expected_flags):
        self.memory.write_byte(self.cpu.pc, opcode)
        self.cpu.step()
        self.assertEqual(self.cpu.registers[REGISTER_F], expected_flags,  f"{opcode=}")

    def _set_register_a_step_assert(self, opcode, operand1, expected_result, expected_flags):
        self.cpu.registers[REGISTER_A] = operand1
        self._step_and_assert_flags(opcode, expected_flags)
        self.assertEqual(self.cpu.registers[REGISTER_A], expected_result, f"{opcode=}")

    def _initialize_hl_and_memory(self, address_byte_hi, address_byte_lo, memory_at__hl__value, immediate_value):
        """Helper function to set registers H, L, and memory at the HL address."""
        self.cpu.registers[REGISTER_H] = address_byte_hi
        self.cpu.registers[REGISTER_L] = address_byte_lo
        self.memory.write_byte((address_byte_hi << 8) | address_byte_lo, memory_at__hl__value)
        self.memory.write_byte(self.cpu.pc+1, immediate_value)

    def _register_f_from_flags(self, zero=None, is_subtraction=None, half_carry=None, carry_out=None, modify_z=True, modify_n=True, modify_h=True, modify_c=True):
        flag_z = self.cpu.registers[REGISTER_F] & FLAG_Z if not modify_z else FLAG_Z if zero           else 0
        flag_n = self.cpu.registers[REGISTER_F] & FLAG_N if not modify_n else FLAG_N if is_subtraction else 0
        flag_h = self.cpu.registers[REGISTER_F] & FLAG_H if not modify_h else FLAG_H if half_carry     else 0
        flag_c = self.cpu.registers[REGISTER_F] & FLAG_C if not modify_c else FLAG_C if carry_out      else 0
        return flag_z | flag_n | flag_h | flag_c

    def _carry_update(self, INITIAL_CARRY_STATUS):
        if INITIAL_CARRY_STATUS:
            self.cpu.registers[REGISTER_F] |= FLAG_C
        else:
            self.cpu.registers[REGISTER_F] &= ~FLAG_C

    def _inc16(self, operand):
        result = (operand + 1) & 0xFFFF
        expected_flags = self.cpu.registers[REGISTER_F]
        return result, expected_flags

    def _dec16(self, operand):
        result = (operand + -1) & 0xFFFF
        expected_flags = self.cpu.registers[REGISTER_F]
        return result, expected_flags

    def _inc(self, operand):
        result = (operand + 1) & 0xFF
        half_carry = (operand & 0x0F) == 0x0F
        flags = self._register_f_from_flags(result==0, carry_out=False, half_carry=half_carry, is_subtraction=False, modify_c=False)
        return result, flags

    def _dec(self, operand):
        result = (operand - 1) & 0xFF
        half_carry = (operand & 0x0F) == 0
        flags = self._register_f_from_flags(result==0, carry_out=False, half_carry=half_carry, is_subtraction=True, modify_c=False)
        return result, flags

    def _add(self, operand1, operand2, carry_in):
        carry_in = 1 if carry_in else 0
        result = operand1 + operand2 + carry_in

        return_value = result & 0xFF

        carry_out = result > 0xFF
        half_carry = ((operand1 & 0x0F) + (operand2 & 0x0F) + carry_in) > 0x0F
        flags = self._register_f_from_flags(return_value==0, carry_out=carry_out, half_carry=half_carry, is_subtraction=False)
        return return_value,flags

    def _sub(self, operand1, operand2, carry_in):
        carry_in = 1 if carry_in else 0
        result = operand1 - operand2 - carry_in

        return_value = result & 0xFF

        carry_out = result < 0
        half_carry = ((operand1 & 0x0F) - (operand2 & 0x0F) - carry_in) < 0
        flags = self._register_f_from_flags(return_value==0, carry_out=carry_out, half_carry=half_carry, is_subtraction=True)
        return return_value,flags

    def _add_16bit(self, operand1, operand2):
        result = operand1 + operand2
        return_value = result & 0xFFFF
        carry_out = result > 0xFFFF
        half_carry = ((operand1 & 0x0FFF) + (operand2 & 0x0FFF)) > 0x0FFF
        return return_value, self._register_f_from_flags(return_value==0, carry_out=carry_out, half_carry=half_carry, is_subtraction=False, modify_z=False)

    def _set_register_a_and_flags_step_assert_value_a(self, opcode, value_a_input, value_a_output, flag_z=False, flag_n=False, flag_h=False, flag_c=False, modify_z=True, modify_n=True, modify_h=True, modify_c=True):
        self.cpu.registers[REGISTER_A] = value_a_input & 0xFF
        self.cpu.registers[REGISTER_F] = self._register_f_from_flags(zero=flag_z, is_subtraction=flag_n, half_carry=flag_h, carry_out=flag_c, modify_z=modify_z, modify_n=modify_n, modify_h=modify_h, modify_c=modify_c)
        self.memory.write_byte(self.cpu.pc, opcode)
        self.cpu.step()
        self.assertEqual(self.cpu.registers[REGISTER_A], value_a_output)

    def test_nop(self):
        opcode = opcodes.NOP
        previous_cpu = CPU(self.cpu)

        self.memory.write_byte(self.cpu.pc, opcode)
        self.cpu.step()
        for i in range(8):
            self.assertEqual(self.cpu.registers[i], previous_cpu.registers[i])
        self.assertEqual(self.cpu.sp, previous_cpu.sp)
        self.assertEqual(self.cpu.pc, previous_cpu.pc + 1)
        # self.assertEqual(self.cpu.memory, previous_cpu.memory)

    def test_daa(self):
        opcode = opcodes.DAA
        # Test after ADD without any flags set
        self._set_register_a_and_flags_step_assert_value_a(opcode, 0x15, 0x15)
        self.assertEqual(self.cpu.registers[REGISTER_F], 0x00)  # No flags set
        # Test ADD that produces half-carry
        self._set_register_a_and_flags_step_assert_value_a(opcode, 0x0A, 0x10, flag_h=True)
        self.assertEqual(self.cpu.registers[REGISTER_F] & FLAG_Z, 0)  # Zero flag should not be set
        self.assertEqual(self.cpu.registers[REGISTER_F] & FLAG_H, 0)  # Half-carry should be cleared
        # Test ADD that produces carry
        self._set_register_a_and_flags_step_assert_value_a(opcode, 0x9A, 0x00, flag_c=True)
        self.assertTrue(self.cpu.registers[REGISTER_F] & FLAG_Z)  # Zero flag should be set
        self.assertTrue(self.cpu.registers[REGISTER_F] & FLAG_C)  # Carry flag should remain set
        # Test SUB without any flags set
        self._set_register_a_and_flags_step_assert_value_a(opcode, 0x15, 0x15, flag_n=True)
        self.assertEqual(self.cpu.registers[REGISTER_F] & FLAG_C, 0)  # Carry should remain clear
        # Test SUB that produced a half-carry
        self._set_register_a_and_flags_step_assert_value_a(opcode, 0x6B, 0x65, flag_n=True, flag_h=True)
        self.assertEqual(self.cpu.registers[REGISTER_F] & FLAG_H, 0)  # Half-carry should be cleared
        # Test SUB that produced carry
        self._set_register_a_and_flags_step_assert_value_a(opcode, 0xA0, 0x40, flag_n=True, flag_c=True)
        self.assertTrue(self.cpu.registers[REGISTER_F] & FLAG_C)  # Carry flag should remain set

    def test_cpl(self):
        opcode = opcodes.CPL
        # N & H flags should be set, Z & C flags should be unchanged
        flags_expected_status = (self.cpu.registers[REGISTER_F] & (FLAG_Z | FLAG_C)) | FLAG_N | FLAG_H

        self._set_register_a_and_flags_step_assert_value_a(opcode, 0x00, 0xFF, modify_z=False, modify_n=False, modify_h=False, modify_c=False)
        self.assertEqual(self.cpu.registers[REGISTER_F], flags_expected_status)

        self._set_register_a_and_flags_step_assert_value_a(opcode, 0xFF, 0x00, modify_z=False, modify_n=False, modify_h=False, modify_c=False)
        self.assertEqual(self.cpu.registers[REGISTER_F], flags_expected_status)

        self._set_register_a_and_flags_step_assert_value_a(opcode, 0x5A, 0xA5, modify_z=False, modify_n=False, modify_h=False, modify_c=False)
        self.assertEqual(self.cpu.registers[REGISTER_F], flags_expected_status)

    def test_scf(self):
        opcode = opcodes.SCF  # Assume SCF opcode is correctly defined in the opcodes module

        # Test when no flags are initially set
        self.cpu.registers[REGISTER_F] = 0x00  # Clear all flags
        self.memory.write_byte(self.cpu.pc, opcode)
        self.cpu.step()
        # Check that carry flag (C) is set
        self.assertTrue(self.cpu.registers[REGISTER_F] & FLAG_C, "Carry flag should be set")
        # Check that half-carry (H) and subtraction (N) flags are cleared
        self.assertFalse(self.cpu.registers[REGISTER_F] & FLAG_H, "Half-carry flag should be cleared")
        self.assertFalse(self.cpu.registers[REGISTER_F] & FLAG_N, "Subtraction flag should be cleared")
        # Check that Zero flag (Z) remains unchanged
        self.assertFalse(self.cpu.registers[REGISTER_F] & FLAG_Z, "Zero flag should remain unchanged")

        # Test when Zero (Z) flag is initially set
        self.cpu.registers[REGISTER_F] = 0xF0  # Set all flags
        self.memory.write_byte(self.cpu.pc, opcode)
        self.cpu.step()
        # Carry flag (C) should still be set
        self.assertTrue(self.cpu.registers[REGISTER_F] & FLAG_C, "Carry flag should be set")
        # Half-carry (H) and subtraction (N) flags should be cleared
        self.assertFalse(self.cpu.registers[REGISTER_F] & FLAG_H, "Half-carry flag should be cleared")
        self.assertFalse(self.cpu.registers[REGISTER_F] & FLAG_N, "Subtraction flag should be cleared")
        # Zero flag (Z) should remain set
        self.assertTrue(self.cpu.registers[REGISTER_F] & FLAG_Z, "Zero flag should remain unchanged")

    def test_ccf(self):
        opcode = opcodes.CCF

        # Case 1: Carry flag is set initially
        self.cpu.registers[REGISTER_F] = FLAG_C  # Set the carry flag initially
        self.memory.write_byte(self.cpu.pc, opcode)
        self.cpu.step()
        self.assertEqual(self.cpu.registers[REGISTER_F] & (FLAG_Z | FLAG_C | FLAG_H | FLAG_N), 0)  # No flags should be set

        # Case 2: Carry flag is cleared initially
        self.cpu.registers[REGISTER_F] = FLAG_Z | FLAG_H | FLAG_N  # Clear only the carry flag initially
        self.memory.write_byte(self.cpu.pc, opcode)
        self.cpu.step()
        self.assertEqual(self.cpu.registers[REGISTER_F] & (FLAG_Z | FLAG_C | FLAG_H | FLAG_N), FLAG_Z|FLAG_C)  # Only Zero and Carry flags should be set

        # Case 3: Check that the zero flag (if set) is unaffected
        self.cpu.registers[REGISTER_F] = 0xF0  # Set all flags
        self.memory.write_byte(self.cpu.pc, opcode)
        self.cpu.step()
        self.assertEqual(self.cpu.registers[REGISTER_F] & (FLAG_Z | FLAG_C | FLAG_H | FLAG_N), FLAG_Z)  # Only Zero flag should remain set

    def test_inc16_register_pairs(self):
        OPCODES_TO_ITERATE = {
            opcodes.INC_BC: "BC",
            opcodes.INC_DE: "DE",
            opcodes.INC_HL: "HL",
            opcodes.INC_SP: "sp",
        }
        for opcode, register_pair in OPCODES_TO_ITERATE.items():
            initial_value = getattr(self.cpu, register_pair)
            
            # Adjust expected_result to handle 16-bit overflow behavior
            expected_result, expected_flags = self._inc16(initial_value)
            
            self.memory.write_byte(self.cpu.pc, opcode)
            self.cpu.step()
            
            # Assert the updated values match expectations
            self.assertEqual(getattr(self.cpu, register_pair), expected_result, f"{opcode=}")
            self.assertEqual(self.cpu.registers[REGISTER_F], expected_flags, f"{opcode=}")

    def test_dec16_register_pairs(self):
        OPCODES_TO_ITERATE = {
            opcodes.DEC_BC: "BC",
            opcodes.DEC_DE: "DE",
            opcodes.DEC_HL: "HL",
            opcodes.DEC_SP: "sp",
        }
        for opcode, register_pair in OPCODES_TO_ITERATE.items():
            initial_value = getattr(self.cpu, register_pair)
            
            # Adjust expected_result to handle 16-bit overflow behavior
            expected_result, expected_flags = self._dec16(initial_value)
            
            self.memory.write_byte(self.cpu.pc, opcode)
            self.cpu.step()
            
            # Assert the updated values match expectations
            self.assertEqual(getattr(self.cpu, register_pair), expected_result, f"{opcode=}")
            self.assertEqual(self.cpu.registers[REGISTER_F], expected_flags, f"{opcode=}")

    def test_inc_register(self):
        OPCODES_TO_ITERATE = {
            opcodes.INC_B: REGISTER_B,
            opcodes.INC_C: REGISTER_C,
            opcodes.INC_D: REGISTER_D,
            opcodes.INC_E: REGISTER_E,
            opcodes.INC_H: REGISTER_H,
            opcodes.INC_L: REGISTER_L,
            opcodes.INC_A: REGISTER_A,
        }
        for opcode, register in OPCODES_TO_ITERATE.items():
            initial_value = self.cpu.registers[register]
            expected_result, expected_flags = self._inc(initial_value)
            
            self._step_and_assert_flags(opcode, expected_flags)
            self.assertEqual(self.cpu.registers[register], expected_result, f"{opcode=}")

    def test_inc_non_register(self):
        opcode = opcodes.INC__HL_
        INITIAL_CARRY_STATUS = random.randint(0, 1)
        self._carry_update(INITIAL_CARRY_STATUS)
        OPERAND = 0xFF
        EXPECTED_RESULT, EXPECTED_FLAGS = self._inc(OPERAND)
        ADDR_HIGH, ADDR_LOW = 0x00, 0x10
        MEMORY_VALUE, IMMEDIATE_VALUE = (0, OPERAND) if opcode in IMMEDIATE_OPCODES else (OPERAND, 0)
        self._initialize_hl_and_memory(ADDR_HIGH, ADDR_LOW, MEMORY_VALUE, IMMEDIATE_VALUE)
        self._step_and_assert_flags(opcode, EXPECTED_FLAGS)
        self.assertEqual(self.memory.read_byte((ADDR_HIGH << 8) | ADDR_LOW), EXPECTED_RESULT, f"{opcode=}")

    # TODO: Review register base values for test coverage: Ensure Half Carry flag is set in any of the cases
    def test_dec_register(self):
        OPCODES_TO_ITERATE = {
            opcodes.DEC_B: REGISTER_B,
            opcodes.DEC_C: REGISTER_C,
            opcodes.DEC_D: REGISTER_D,
            opcodes.DEC_E: REGISTER_E,
            opcodes.DEC_H: REGISTER_H,
            opcodes.DEC_L: REGISTER_L,
            opcodes.DEC_A: REGISTER_A,
        }
        for opcode, register in OPCODES_TO_ITERATE.items():
            initial_value = self.cpu.registers[register]
            expected_result, expected_flags = self._dec(initial_value)
            
            self._step_and_assert_flags(opcode, expected_flags)
            self.assertEqual(self.cpu.registers[register], expected_result, f"{opcode=}")

    def test_dec_non_register(self):
        opcode = opcodes.DEC__HL_
        INITIAL_CARRY_STATUS = random.randint(0, 1)
        self._carry_update(INITIAL_CARRY_STATUS)
        OPERAND = 0xFF
        EXPECTED_RESULT, EXPECTED_FLAGS = self._dec(OPERAND)
        ADDR_HIGH, ADDR_LOW = 0x00, 0x10
        MEMORY_VALUE, IMMEDIATE_VALUE = (0, OPERAND) if opcode in IMMEDIATE_OPCODES else (OPERAND, 0)
        self._initialize_hl_and_memory(ADDR_HIGH, ADDR_LOW, MEMORY_VALUE, IMMEDIATE_VALUE)
        self._step_and_assert_flags(opcode, EXPECTED_FLAGS)
        self.assertEqual(self.memory.read_byte((ADDR_HIGH << 8) | ADDR_LOW), EXPECTED_RESULT, f"{opcode=}")

    def test_add_hl_register(self):
        OPCODES_TO_ITERATE = {
            opcodes.ADD_HL_BC: 'BC',
            opcodes.ADD_HL_DE: 'DE',
            opcodes.ADD_HL_HL: 'HL',
            opcodes.ADD_HL_SP: 'sp'
        }
        for opcode, register_pair in OPCODES_TO_ITERATE.items():
            if opcode == opcodes.ADD_HL_HL:
                self.cpu.HL = 0  # Clear HL for HL + HL case
            OPERAND_1 = self.cpu.HL
            OPERAND_2 = getattr(self.cpu, register_pair)
            expected_result, expected_flags = self._add_16bit(OPERAND_1, OPERAND_2)
            self._step_and_assert_flags(opcode, expected_flags)
            hl_value = (self.cpu.registers[REGISTER_H] << 8) | self.cpu.registers[REGISTER_L]
            self.assertEqual(hl_value, expected_result, f"{opcode=}")

    def test_add_a_register(self):
        OPCODES_TO_ITERATE = {
            opcodes.ADD_A_B: REGISTER_B,
            opcodes.ADD_A_C: REGISTER_C,
            opcodes.ADD_A_D: REGISTER_D,
            opcodes.ADD_A_E: REGISTER_E,
            opcodes.ADD_A_H: REGISTER_H,
            opcodes.ADD_A_L: REGISTER_L,
            opcodes.ADD_A_A: REGISTER_A,
        }
        for opcode, register in OPCODES_TO_ITERATE.items():
            OPERAND1 = self._random_byte()
            OPERAND2 = self.cpu.registers[register] if register!=REGISTER_A else OPERAND1
            EXPECTED_RESULT, EXPECTED_FLAGS = self._add(OPERAND1, OPERAND2, carry_in= 0)

            self._set_register_a_step_assert(opcode, OPERAND1, EXPECTED_RESULT, EXPECTED_FLAGS)

    def test_add_a_non_register(self):
        OPCODES_TO_ITERATE = { opcodes.ADD_A__HL_, opcodes.ADD_A_IMM }
        INITIAL_CARRY_STATUS = random.randint(0, 1)
        for opcode in OPCODES_TO_ITERATE:
            OPERAND1 = self._random_byte()
            OPERAND2 = self._random_byte()
            EXPECTED_RESULT, EXPECTED_FLAGS = self._add(OPERAND1, OPERAND2, carry_in= 0)
            ADDR_HIGH, ADDR_LOW = self._random_byte(), self._random_byte()
            MEMORY_VALUE, IMMEDIATE_VALUE = (0, OPERAND2) if opcode in IMMEDIATE_OPCODES else (OPERAND2, 0)
            self._initialize_hl_and_memory(ADDR_HIGH, ADDR_LOW, MEMORY_VALUE, IMMEDIATE_VALUE)
            self._carry_update(INITIAL_CARRY_STATUS)
            self._set_register_a_step_assert(opcode, OPERAND1, EXPECTED_RESULT, EXPECTED_FLAGS)

    def test_adc_a_register(self):
        OPCODES_TO_ITERATE = {
            opcodes.ADC_A_B: REGISTER_B,
            opcodes.ADC_A_C: REGISTER_C,
            opcodes.ADC_A_D: REGISTER_D,
            opcodes.ADC_A_E: REGISTER_E,
            opcodes.ADC_A_H: REGISTER_H,
            opcodes.ADC_A_L: REGISTER_L,
            opcodes.ADC_A_A: REGISTER_A,
        }
        INITIAL_CARRY_STATUS = random.randint(0, 1)
        for opcode, register in OPCODES_TO_ITERATE.items():
            OPERAND1 = self._random_byte()
            OPERAND2 = self.cpu.registers[register] if register!=REGISTER_A else OPERAND1
            EXPECTED_RESULT, EXPECTED_FLAGS = self._add(OPERAND1, OPERAND2, carry_in= INITIAL_CARRY_STATUS)

            self._carry_update(INITIAL_CARRY_STATUS)
            self._set_register_a_step_assert(opcode, OPERAND1, EXPECTED_RESULT, EXPECTED_FLAGS)

    def test_adc_a_non_register(self):
        OPCODES_TO_ITERATE = { opcodes.ADC_A__HL_, opcodes.ADC_A_IMM }
        INITIAL_CARRY_STATUS = random.randint(0, 1)
        for opcode in OPCODES_TO_ITERATE:
            OPERAND1 = self._random_byte()
            OPERAND2 = self._random_byte()
            EXPECTED_RESULT, EXPECTED_FLAGS = self._add(OPERAND1, OPERAND2, carry_in= INITIAL_CARRY_STATUS)
            ADDR_HIGH, ADDR_LOW = self._random_byte(), self._random_byte()
            MEMORY_VALUE, IMMEDIATE_VALUE = (0, OPERAND2) if opcode in IMMEDIATE_OPCODES else (OPERAND2, 0)
            self._initialize_hl_and_memory(ADDR_HIGH, ADDR_LOW, MEMORY_VALUE, IMMEDIATE_VALUE)
            self._carry_update(INITIAL_CARRY_STATUS)
            self._set_register_a_step_assert(opcode, OPERAND1, EXPECTED_RESULT, EXPECTED_FLAGS)

    def test_sub_a_register(self, compare=False):
        SUB_OPERANDS = {
            opcodes.SUB_A_B: REGISTER_B,
            opcodes.SUB_A_C: REGISTER_C,
            opcodes.SUB_A_D: REGISTER_D,
            opcodes.SUB_A_E: REGISTER_E,
            opcodes.SUB_A_H: REGISTER_H,
            opcodes.SUB_A_L: REGISTER_L,
            opcodes.SUB_A_A: REGISTER_A,
        }
        CP_OPERANDS = {
            # Add CP opcodes for comparison tests
            opcodes.CP_A_B: REGISTER_B,
            opcodes.CP_A_C: REGISTER_C,
            opcodes.CP_A_D: REGISTER_D,
            opcodes.CP_A_E: REGISTER_E,
            opcodes.CP_A_H: REGISTER_H,
            opcodes.CP_A_L: REGISTER_L,
            opcodes.CP_A_A: REGISTER_A,
        }
        OPCODES_TO_ITERATE = CP_OPERANDS if compare else SUB_OPERANDS
        INITIAL_CARRY_STATUS = random.randint(0, 1)
        for opcode, register in OPCODES_TO_ITERATE.items():
            OPERAND1 = self._random_byte()
            OPERAND2 = self.cpu.registers[register] if register!=REGISTER_A else OPERAND1
            EXPECTED_RESULT, EXPECTED_FLAGS = self._sub(OPERAND1, OPERAND2, carry_in=0)

            self._carry_update(INITIAL_CARRY_STATUS)
            self._set_register_a_step_assert(opcode, OPERAND1, OPERAND1 if compare else EXPECTED_RESULT, EXPECTED_FLAGS)

    def test_sub_a_non_register(self, compare=False):
        OPCODES_TO_ITERATE = { opcodes.CP_A__HL_, opcodes.CP_A_IMM } if compare else { opcodes.SUB_A__HL_, opcodes.SUB_A_IMM }
        INITIAL_CARRY_STATUS = random.randint(0, 1)
        for opcode in OPCODES_TO_ITERATE:
            OPERAND1 = self._random_byte()
            OPERAND2 = self._random_byte()
            EXPECTED_RESULT, EXPECTED_FLAGS = self._sub(OPERAND1, OPERAND2, carry_in=0)
            ADDR_HIGH, ADDR_LOW = self._random_byte(), self._random_byte()
            MEMORY_VALUE, IMMEDIATE_VALUE = (0, OPERAND2) if opcode in IMMEDIATE_OPCODES else (OPERAND2, 0)
            self._initialize_hl_and_memory(ADDR_HIGH, ADDR_LOW, MEMORY_VALUE, IMMEDIATE_VALUE)
            self._carry_update(INITIAL_CARRY_STATUS)
            self._set_register_a_step_assert(opcode, OPERAND1, OPERAND1 if compare else EXPECTED_RESULT, EXPECTED_FLAGS)

    def test_cp_a_register(self):
        self.test_sub_a_register(compare=True)

    def test_cp_a_non_register(self):
        self.test_sub_a_non_register(compare=True)

    def test_sbc_a_register(self):
        OPCODES_TO_ITERATE = {
            opcodes.SBC_A_B: REGISTER_B,
            opcodes.SBC_A_C: REGISTER_C,
            opcodes.SBC_A_D: REGISTER_D,
            opcodes.SBC_A_E: REGISTER_E,
            opcodes.SBC_A_H: REGISTER_H,
            opcodes.SBC_A_L: REGISTER_L,
            opcodes.SBC_A_A: REGISTER_A,
        }
        INITIAL_CARRY_STATUS = random.randint(0, 1)
        for opcode, register in OPCODES_TO_ITERATE.items():
            OPERAND1 = self._random_byte()
            OPERAND2 = self.cpu.registers[register] if register!=REGISTER_A else OPERAND1
            EXPECTED_RESULT, EXPECTED_FLAGS = self._sub(OPERAND1, OPERAND2, carry_in=INITIAL_CARRY_STATUS)

            self._carry_update(INITIAL_CARRY_STATUS)
            self._set_register_a_step_assert(opcode, OPERAND1, EXPECTED_RESULT, EXPECTED_FLAGS)

    def test_sbc_a_non_register(self):
        OPCODES_TO_ITERATE = { opcodes.SBC_A__HL_, opcodes.SBC_A_IMM }
        INITIAL_CARRY_STATUS = random.randint(0, 1)
        for opcode in OPCODES_TO_ITERATE:
            OPERAND1 = self._random_byte()
            OPERAND2 = self._random_byte()
            EXPECTED_RESULT, EXPECTED_FLAGS = self._sub(OPERAND1, OPERAND2, carry_in= INITIAL_CARRY_STATUS)
            ADDR_HIGH, ADDR_LOW = self._random_byte(), self._random_byte()
            MEMORY_VALUE, IMMEDIATE_VALUE = (0, OPERAND2) if opcode in IMMEDIATE_OPCODES else (OPERAND2, 0)
            self._initialize_hl_and_memory(ADDR_HIGH, ADDR_LOW, MEMORY_VALUE, IMMEDIATE_VALUE)
            self._carry_update(INITIAL_CARRY_STATUS)
            self._set_register_a_step_assert(opcode, OPERAND1, EXPECTED_RESULT, EXPECTED_FLAGS)

    def test_and_a_register(self):
        OPCODES_TO_ITERATE = {
            opcodes.AND_A_A: REGISTER_A,
            opcodes.AND_A_B: REGISTER_B,
            opcodes.AND_A_C: REGISTER_C,
            opcodes.AND_A_D: REGISTER_D,
            opcodes.AND_A_E: REGISTER_E,
            opcodes.AND_A_H: REGISTER_H,
            opcodes.AND_A_L: REGISTER_L,
        }
        for opcode, register in OPCODES_TO_ITERATE.items():
            OPERAND1 = self._random_byte()
            OPERAND2 = self.cpu.registers[register] if register!=REGISTER_A else OPERAND1
            EXPECTED_RESULT = OPERAND1 & OPERAND2
            EXPECTED_FLAGS = (FLAG_Z if EXPECTED_RESULT == 0 else 0) | FLAG_H  # Half carry is always set for AND operations
            self._set_register_a_step_assert(opcode, OPERAND1, EXPECTED_RESULT, EXPECTED_FLAGS)

    def test_xor_a_register(self):
        OPERANDS = {
            opcodes.XOR_A_A: REGISTER_A,
            opcodes.XOR_A_B: REGISTER_B,
            opcodes.XOR_A_C: REGISTER_C,
            opcodes.XOR_A_D: REGISTER_D,
            opcodes.XOR_A_E: REGISTER_E,
            opcodes.XOR_A_H: REGISTER_H,
            opcodes.XOR_A_L: REGISTER_L,
        }
        for opcode, register in OPERANDS.items():
            OPERAND1 = self._random_byte()
            OPERAND2 = self.cpu.registers[register] if register!=REGISTER_A else OPERAND1
            EXPECTED_RESULT = OPERAND1 ^ OPERAND2
            EXPECTED_FLAGS = FLAG_Z if EXPECTED_RESULT == 0 else 0
            self._set_register_a_step_assert(opcode, OPERAND1, EXPECTED_RESULT, EXPECTED_FLAGS)

    def test_and_a_hl(self):
        OPCODES_TO_ITERATE = { opcodes.AND_A__HL_, opcodes.AND_A_IMM }

        for opcode in OPCODES_TO_ITERATE:
            OPERAND1, OPERAND2 = self._random_byte(), self._random_byte()
            EXPECTED_RESULT = OPERAND1 & OPERAND2
            EXPECTED_FLAGS = (FLAG_Z if EXPECTED_RESULT == 0 else 0) | FLAG_H  # Half carry is always set for AND operations
            ADDR_HIGH, ADDR_LOW = self._random_byte(), self._random_byte()
            MEMORY_VALUE, IMMEDIATE_VALUE = (0, OPERAND2) if opcode in IMMEDIATE_OPCODES else (OPERAND2, 0)
            self._initialize_hl_and_memory(ADDR_HIGH, ADDR_LOW, MEMORY_VALUE, IMMEDIATE_VALUE)
            self._set_register_a_step_assert(opcode, OPERAND1, EXPECTED_RESULT, EXPECTED_FLAGS)

    def test_xor_a_hl(self):
        OPCODES_TO_ITERATE = { opcodes.XOR_A__HL_, opcodes.XOR_A_IMM }
        for opcode in OPCODES_TO_ITERATE:
            OPERAND1, OPERAND2 = self._random_byte(), self._random_byte()
            EXPECTED_RESULT = OPERAND1 ^ OPERAND2
            EXPECTED_FLAGS = FLAG_Z if EXPECTED_RESULT == 0 else 0
            ADDR_HIGH, ADDR_LOW = self._random_byte(), self._random_byte()
            MEMORY_VALUE, IMMEDIATE_VALUE = (0, OPERAND2) if opcode in IMMEDIATE_OPCODES else (OPERAND2, 0)
            self._initialize_hl_and_memory(ADDR_HIGH, ADDR_LOW, MEMORY_VALUE, IMMEDIATE_VALUE)
            self._set_register_a_step_assert(opcode, OPERAND1, EXPECTED_RESULT, EXPECTED_FLAGS)

    def test_or_a_register(self):
        OPCODES_TO_ITERATE = {
            opcodes.OR_A_B: REGISTER_B,
            opcodes.OR_A_C: REGISTER_C,
            opcodes.OR_A_D: REGISTER_D,
            opcodes.OR_A_E: REGISTER_E,
            opcodes.OR_A_H: REGISTER_H,
            opcodes.OR_A_L: REGISTER_L,
            opcodes.OR_A_A: REGISTER_A,
        }
        for opcode, register in OPCODES_TO_ITERATE.items():
            OPERAND1 = self._random_byte()
            OPERAND2 = self.cpu.registers[register] if register!=REGISTER_A else OPERAND1
            EXPECTED_RESULT = OPERAND1 | OPERAND2
            EXPECTED_FLAGS = FLAG_Z if EXPECTED_RESULT == 0 else 0

            self._set_register_a_step_assert(opcode, OPERAND1, EXPECTED_RESULT, EXPECTED_FLAGS)

    def test_or_a_hl(self):
        OPCODES_TO_ITERATE = { opcodes.OR_A__HL_, opcodes.OR_A_IMM }
        for opcode in OPCODES_TO_ITERATE:
            OPERAND1, OPERAND2 = self._random_byte(), self._random_byte()
            EXPECTED_RESULT = OPERAND1 | OPERAND2
            EXPECTED_FLAGS = FLAG_Z if EXPECTED_RESULT == 0 else 0
            ADDR_HIGH, ADDR_LOW = self._random_byte(), self._random_byte()
            MEMORY_VALUE, IMMEDIATE_VALUE = (0, OPERAND2) if opcode in IMMEDIATE_OPCODES else (OPERAND2, 0)
            self._initialize_hl_and_memory(ADDR_HIGH, ADDR_LOW, MEMORY_VALUE, IMMEDIATE_VALUE)
            self._set_register_a_step_assert(opcode, OPERAND1, EXPECTED_RESULT, EXPECTED_FLAGS)

    def _set_sp_and_execute_opcode(self, opcode_reg16, sp_initial, offset, reg16_expected, flag_h=False, flag_c=False):
        opcode, reg16 = opcode_reg16
        self.cpu.sp = sp_initial
        self.memory.write_byte(self.cpu.pc, opcode)
        self.memory.write_byte(self.cpu.pc + 1, offset)
        self.cpu.step()

        self.assertEqual( getattr(self.cpu, reg16), reg16_expected, f"{reg16} after {opcode=} with {sp_initial=}, offset={offset:#04x}")
        self.assertEqual(bool(self.cpu.registers[REGISTER_F] & FLAG_H), flag_h, "Half-carry flag mismatch")
        self.assertEqual(bool(self.cpu.registers[REGISTER_F] & FLAG_C), flag_c, "Carry flag mismatch")

    def test_ld_hl_s8(self):
        opcode_reg16 = opcodes.LD_HL_S8, 'HL'
        self._set_sp_and_execute_opcode(opcode_reg16=opcode_reg16, sp_initial=0x0000, offset=0x05, reg16_expected=0x0005, flag_h=False, flag_c=False)
        self._set_sp_and_execute_opcode(opcode_reg16=opcode_reg16, sp_initial=0xEFFF, offset=0x01, reg16_expected=0xF000, flag_h=True,  flag_c=False)
        self._set_sp_and_execute_opcode(opcode_reg16=opcode_reg16, sp_initial=0xFFFF, offset=0x10, reg16_expected=0x000F, flag_h=False, flag_c=True )
        self._set_sp_and_execute_opcode(opcode_reg16=opcode_reg16, sp_initial=0xFFFF, offset=0x01, reg16_expected=0x0000, flag_h=True,  flag_c=True )
        self._set_sp_and_execute_opcode(opcode_reg16=opcode_reg16, sp_initial=0x0010, offset=0xF0, reg16_expected=0x0000, flag_h=False, flag_c=False)

    def test_add_sp_imm(self):
        opcode_reg16 = opcodes.ADD_SP_IMM, 'sp'
        self._set_sp_and_execute_opcode(opcode_reg16=opcode_reg16, sp_initial=0x0000, offset=0x05, reg16_expected=0x0005, flag_h=False, flag_c=False)
        self._set_sp_and_execute_opcode(opcode_reg16=opcode_reg16, sp_initial=0xEFFF, offset=0x01, reg16_expected=0xF000, flag_h=True,  flag_c=False)
        self._set_sp_and_execute_opcode(opcode_reg16=opcode_reg16, sp_initial=0xFFFF, offset=0x10, reg16_expected=0x000F, flag_h=False, flag_c=True )
        self._set_sp_and_execute_opcode(opcode_reg16=opcode_reg16, sp_initial=0xFFFF, offset=0x01, reg16_expected=0x0000, flag_h=True,  flag_c=True )
        self._set_sp_and_execute_opcode(opcode_reg16=opcode_reg16, sp_initial=0x0010, offset=0xF0, reg16_expected=0x0000, flag_h=False, flag_c=False)

    def _execute_ld_instruction_and_assert_value(self, opcode, target_register, initial_value, expected_value):
        self.cpu.registers[target_register] = initial_value
        self.memory.write_byte(self.cpu.pc, opcode)
        self.cpu.step()
        self.assertEqual(self.cpu.registers[target_register], expected_value, f"{opcode=}")

    def test_ld_r8_r8(self):
        for register_1 in ('A', 'B', 'C', 'D', 'E', 'H', 'L'):
            for register_2 in ('A', 'B', 'C', 'D', 'E', 'H', 'L'):
                opcode = getattr(opcodes, f'LD_{register_1}_{register_2}')
                register_to_check = getattr(cpu, f'REGISTER_{register_2}')
                self._execute_ld_instruction_and_assert_value( opcode, getattr(cpu, f'REGISTER_{register_1}'),
                    initial_value:= self._random_byte(),
                    expected_value= initial_value if register_2 == register_1 else self.cpu.registers[register_to_check]
                )

    def test_ld_r8__hl__and_imm(self):
        for register in ('A', 'B', 'C', 'D', 'E', 'H', 'L'):
            OPCODES_TO_ITERATE = ( getattr(opcodes, f'LD_{register}__HL_'), getattr(opcodes, f'LD_{register}_IMM') )
            for opcode in OPCODES_TO_ITERATE:
                test_value = self._random_byte()
                ADDR_HIGH, ADDR_LOW = self._random_byte(), self._random_byte()
                MEMORY_VALUE, IMMEDIATE_VALUE = (0, test_value) if opcode in IMMEDIATE_OPCODES else (test_value, 0)
                self._initialize_hl_and_memory(ADDR_HIGH, ADDR_LOW, MEMORY_VALUE, IMMEDIATE_VALUE)

                self.memory.write_byte(self.cpu.pc, opcode)
                self.cpu.step()
                self.assertEqual(self.cpu.registers[ getattr(cpu, f'REGISTER_{register}') ], test_value, f"{opcode=}")

    def test_ld__hl__r8(self):
        for register in ('A', 'B', 'C', 'D', 'E', 'H', 'L'):
            opcode = getattr(opcodes, f'LD__HL__{register}')
            reg_value = self._random_byte()
            self.cpu.registers[getattr(cpu, f'REGISTER_{register}')] = reg_value
            self.memory.write_byte(self.cpu.pc, opcode)
            self.cpu.step()
            self.assertEqual( self.memory.read_byte( (self.cpu.registers[REGISTER_H] << 8) | self.cpu.registers[REGISTER_L] ), reg_value, f"{opcode=}")

    def test_ld__hl__imm(self):
        opcode = opcodes.LD__HL__IMM
        imm_value = self._random_byte()
        self.memory.write_byte(self.cpu.pc, opcode)
        self.memory.write_byte(self.cpu.pc + 1, imm_value)
        self.cpu.step()
        self.assertEqual( self.memory.read_byte( (self.cpu.registers[REGISTER_H] << 8) | self.cpu.registers[REGISTER_L] ), imm_value, f"{opcode=}")

    def test_ld__indirect__a(self):
        for indirect in ('NN', 'C', 'IMM16', 'BC', 'DE', 'HL_INC', 'HL_DEC'):
            opcode = getattr(opcodes, f'LD__{indirect}__A')
            previous_hl = (self.cpu.registers[REGISTER_H] << 8) | self.cpu.registers[REGISTER_L]
            imm_value = self.cpu.registers[REGISTER_A]

            match indirect:
                case 'NN':
                    addr_hi = 0xFF
                    addr_lo = self._random_byte()
                    self.memory.write_byte(self.cpu.pc+1, addr_lo)
                case 'C':
                    addr_hi = 0xFF
                    addr_lo = self.cpu.registers[REGISTER_C]
                case 'IMM16':
                    addr_hi = self._random_byte()
                    addr_lo = self._random_byte()
                    self.memory.write_byte(self.cpu.pc+1, addr_hi)
                    self.memory.write_byte(self.cpu.pc+2, addr_lo)
                case 'BC':
                    addr_hi = self.cpu.registers[REGISTER_B]
                    addr_lo = self.cpu.registers[REGISTER_C]
                case 'DE':
                    addr_hi = self.cpu.registers[REGISTER_D]
                    addr_lo = self.cpu.registers[REGISTER_E]
                case _:
                    addr_hi = self.cpu.registers[REGISTER_H]
                    addr_lo = self.cpu.registers[REGISTER_L]
            self.memory.write_byte(self.cpu.pc, opcode)
            self.cpu.step()

            self.assertEqual( self.memory.read_byte( (addr_hi << 8) | addr_lo ), imm_value, f"{opcode=}")
            if indirect in ('HL_INC', 'HL_DEC'):
                self.assertEqual( self.cpu.HL, (previous_hl + (1 if indirect=='HL_INC' else -1) ) & 0xFFFF , f"{opcode=}")

    def test_ld_a__indirect_(self):
        for indirect in ('NN', 'C', 'IMM16', 'BC', 'DE', 'HL_INC', 'HL_DEC'):
            opcode = getattr(opcodes, f'LD_A__{indirect}_')
            previous_hl = (self.cpu.registers[REGISTER_H] << 8) | self.cpu.registers[REGISTER_L]
            imm_value = self._random_byte()

            match indirect:
                case 'NN':
                    addr_hi = 0xFF
                    addr_lo = self._random_byte()
                    self.memory.write_byte(self.cpu.pc+1, addr_lo)
                case 'C':
                    addr_hi = 0xFF
                    addr_lo = self.cpu.registers[REGISTER_C]
                case 'IMM16':
                    addr_hi = self._random_byte()
                    addr_lo = self._random_byte()
                    self.memory.write_byte(self.cpu.pc+1, addr_hi)
                    self.memory.write_byte(self.cpu.pc+2, addr_lo)
                case 'BC':
                    addr_hi = self.cpu.registers[REGISTER_B]
                    addr_lo = self.cpu.registers[REGISTER_C]
                case 'DE':
                    addr_hi = self.cpu.registers[REGISTER_D]
                    addr_lo = self.cpu.registers[REGISTER_E]
                case _:
                    addr_hi = self.cpu.registers[REGISTER_H]
                    addr_lo = self.cpu.registers[REGISTER_L]
            self.memory.write_byte((addr_hi << 8) | addr_lo, imm_value)
            self.memory.write_byte(self.cpu.pc, opcode)
            self.cpu.step()

            self.assertEqual( self.cpu.registers[REGISTER_A], imm_value, f"{opcode=}")
            if indirect in ('HL_INC', 'HL_DEC'):
                self.assertEqual( self.cpu.HL, (previous_hl + (1 if indirect=='HL_INC' else -1) ) & 0xFFFF , f"{opcode=}")

    def test_ld_r16_imm16(self):
        for register_16 in ('BC', 'DE', 'HL', 'SP'):
            opcode = getattr(opcodes, f'LD_{register_16}_IMM16')
            imm_value_hi, imm_value_lo = self._random_byte(), self._random_byte()
            imm_value_16 = (imm_value_hi << 8) | imm_value_lo
            self.memory.write_byte(self.cpu.pc+1, imm_value_lo)
            self.memory.write_byte(self.cpu.pc+2, imm_value_hi)

            self.memory.write_byte(self.cpu.pc, opcode)
            self.cpu.step()
            self.assertEqual( self.cpu.sp if register_16 == 'SP' else getattr(self.cpu, register_16), imm_value_16, f"{opcode=}")

    def test_ld_indirect_sp(self):
        opcode = opcodes.LD__IMM16__SP
        addr_hi = self._random_byte()
        addr_lo = self._random_byte()
        self.memory.write_byte(self.cpu.pc+1, addr_hi)
        self.memory.write_byte(self.cpu.pc+2, addr_lo)
        self.memory.write_byte(self.cpu.pc, opcode)
        self.cpu.step()

        self.assertEqual( self.memory.read_byte( (addr_hi << 8) | addr_lo ), self.cpu.sp & 0x00FF, f"{opcode=}")
        self.assertEqual( self.memory.read_byte( (addr_hi << 8) | addr_lo + 1), self.cpu.sp >> 8, f"{opcode=}")

    def test_ld_sp_hl(self):
        opcode = opcodes.LD_SP_HL

        self.memory.write_byte(self.cpu.pc, opcode)
        self.cpu.step()
        self.assertEqual( self.cpu.sp, self.cpu.HL, f"{opcode=}")

    def test_r_a(self):
        for opcode in (opcodes.RLCA, opcodes.RLA, opcodes.RRCA, opcodes.RRA):
            for flags in range(0, 0b1111):
                reg_f_initial = flags << 4
                for byte in range(0xFF):
                    flags_mask = 0b1000_0000 if opcode in (opcodes.RLCA, opcodes.RLA) else 1
                    expected_flags = ( FLAG_C if byte & flags_mask else 0 )
                    if (opcode==opcodes.RLCA and expected_flags) or (opcode==opcodes.RLA and (reg_f_initial&FLAG_C)):
                        register_mask = 1
                    elif (opcode==opcodes.RRCA and expected_flags) or (opcode==opcodes.RRA and (reg_f_initial&FLAG_C)):
                        register_mask = 0b1000_0000
                    else:
                        register_mask = 0
                    expected_a = ( (byte << 1 & 0xFF) if opcode in (opcodes.RLCA, opcodes.RLA) else (byte >> 1) ) | register_mask
                    self.cpu.registers[REGISTER_A] = byte
                    self.cpu.registers[REGISTER_F] = reg_f_initial
                    self.memory.write_byte(self.cpu.pc, opcode)
                    self.cpu.step()
                    self.assertEqual(self.cpu.registers[REGISTER_A], expected_a)
                    self.assertEqual(self.cpu.registers[REGISTER_F], expected_flags)

    def test_jr_imm(self):
        for opcode in (opcodes.JR_IMM, opcodes.JR_Z_IMM, opcodes.JR_NZ_IMM, opcodes.JR_C_IMM, opcodes.JR_NC_IMM):
            jr_if_true = opcode in (opcodes.JR_Z_IMM, opcodes.JR_C_IMM)
            comparison_flag = FLAG_Z if opcode in (opcodes.JR_Z_IMM, opcodes.JR_NZ_IMM) else FLAG_C
            if opcode == opcodes.JR_IMM:
                expect_jump = True
            for flags in range(0, 0b1111):
                initial_flags = flags << 4
                if opcode != opcodes.JR_IMM:
                    expect_jump = jr_if_true if (initial_flags&comparison_flag) else not jr_if_true
                for offset in range(-128, 0, 127):
                    initial_pc = self.cpu.pc
                    expected_pc = (initial_pc + 2 + offset) & 0xFFFF if expect_jump else initial_pc + 2

                    self.cpu.registers[REGISTER_F] = initial_flags
                    self.memory.write_byte(self.cpu.pc, opcode)
                    self.memory.write_byte(self.cpu.pc + 1, offset)
                    self.cpu.step()
                    self.assertEqual(self.cpu.registers[REGISTER_F], initial_flags, f"{opcode=}")
                    self.assertEqual(self.cpu.pc, expected_pc, f"{opcode=}")

    def test_push_reg16(self):
        for register_pair in ('BC', 'DE', 'HL', 'AF'):
            opcode = getattr(opcodes, f"PUSH_{register_pair}" )
            register_high = getattr(cpu, f"REGISTER_{register_pair[0:1]}" )
            register_low  = getattr(cpu, f"REGISTER_{register_pair[-1:]}" )
            value_high, value_low = self._random_byte(), self._random_byte()

            # Set values in high and low registers for the pair
            self.cpu.registers[register_high] = value_high
            self.cpu.registers[register_low]  = value_low
            initial_sp = self.cpu.sp

            # Write the opcode and execute the push instruction
            self.memory.write_byte(self.cpu.pc, opcode)
            self.cpu.step()

            # Check if values were pushed to the stack and SP was decremented correctly
            self.assertEqual( self.cpu.sp, (initial_sp - 2) & 0xFFFF, f"{opcode=}" )
            self.assertEqual( self.memory.read_byte(self.cpu.sp), value_low, f"{opcode=}" )
            self.assertEqual( self.memory.read_byte(self.cpu.sp + 1), value_high, f"{opcode=}" )

    def test_pop_reg16(self):
        for register_pair in ('BC', 'DE', 'HL', 'AF'):
            opcode = getattr(opcodes, f"POP_{register_pair}")
            register_high = getattr(cpu, f"REGISTER_{register_pair[0:1]}")
            register_low  = getattr(cpu, f"REGISTER_{register_pair[-1:]}")
            value_high, value_low = self._random_byte(), self._random_byte()

            # Set up stack values in little-endian format and adjust the stack pointer
            self.memory.write_byte(self.cpu.sp, value_low)
            self.memory.write_byte(self.cpu.sp + 1, value_high)
            initial_sp = self.cpu.sp

            # Write the opcode and execute the pop instruction
            self.memory.write_byte(self.cpu.pc, opcode)
            self.cpu.step()

            # Check if the values were loaded correctly into the registers
            self.assertEqual(self.cpu.registers[register_low], value_low, f"{opcode=}")
            self.assertEqual(self.cpu.registers[register_high], value_high, f"{opcode=}")
            self.assertEqual(self.cpu.sp, (initial_sp + 2) & 0xFFFF, f"{opcode=}")


if __name__ == '__main__':
    unittest.main()
