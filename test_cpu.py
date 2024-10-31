import unittest
from cpu import CPU, FLAG_Z, FLAG_N, FLAG_H, FLAG_C, REGISTER_A, REGISTER_F, REGISTER_B, REGISTER_C, REGISTER_D, REGISTER_E, REGISTER_H, REGISTER_L
from memory import Memory
# Maybe we shouldn't import opcodes but hardcode them in the tests
import opcodes

IMMEDIATE_OPCODES = ( opcodes.ADD_A_IMM, opcodes.ADC_A_IMM, opcodes.SUB_A_IMM, opcodes.SBC_A_IMM, opcodes.AND_A_IMM, opcodes.XOR_A_IMM, opcodes.OR_A_IMM,  opcodes.CP_A_IMM, )

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

    def _step_and_assert_flags(self, opcode, expected_flags):
        self.memory.write_byte(self.cpu.pc, opcode)
        self.cpu.step()
        self.assertEqual(self.cpu.registers[REGISTER_F], expected_flags,  f"{opcode=}")

    def _set_register_a_step_assert(self, opcode, operand1, expected_result, expected_flags):
        self.cpu.registers[REGISTER_A] = operand1
        self._step_and_assert_flags(opcode, expected_flags)
        self.assertEqual(self.cpu.registers[REGISTER_A], expected_result, f"{opcode=}")

    def _initialize_hl_and_memory(self, H, L, hl_value, immediate_value):
        """Helper function to set registers H, L, and memory at the HL address."""
        self.cpu.registers[REGISTER_H] = H
        self.cpu.registers[REGISTER_L] = L
        self.memory.write_byte((H << 8) | L, hl_value)
        self.memory.write_byte(self.cpu.pc+1, immediate_value)

    def _register_f_from_flags(self, zero=None, is_subtraction=None, half_carry=None, carry_out=None, modify_z=True, modify_n=True, modify_h=True, modify_c=True):
        flag_z = self.cpu.registers[REGISTER_F] & FLAG_Z if not modify_z else FLAG_Z if zero           else 0
        flag_n = self.cpu.registers[REGISTER_F] & FLAG_N if not modify_n else FLAG_N if is_subtraction else 0
        flag_h = self.cpu.registers[REGISTER_F] & FLAG_H if not modify_h else FLAG_H if half_carry     else 0
        flag_c = self.cpu.registers[REGISTER_F] & FLAG_C if not modify_c else FLAG_C if carry_out      else 0
        return flag_z | flag_n | flag_h | flag_c

    def _carry_update(self, INITIAL_CARRY_STATUS):
        if INITIAL_CARRY_STATUS: self.cpu.registers[REGISTER_F] |= FLAG_C
        else: self.cpu.registers[REGISTER_F] &= ~FLAG_C

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

    def test_daa(self):

        def _set_register_a_and_flags_step_assert_value_a(opcode, value_a_input, value_a_output, flag_z=False, flag_n=False, flag_h=False, flag_c=False):
            self.cpu.registers[REGISTER_A] = value_a_input & 0xFF
            self.cpu.registers[REGISTER_F] = self._register_f_from_flags(zero=flag_z, is_subtraction=flag_n, half_carry=flag_h, carry_out=flag_c)
            self.memory.write_byte(self.cpu.pc, opcode)
            self.cpu.step()
            self.assertEqual(self.cpu.registers[REGISTER_A], value_a_output)

        opcode = opcodes.DAA
        # Test after ADD without any flags set
        _set_register_a_and_flags_step_assert_value_a(opcode, 0x15, 0x15)
        self.assertEqual(self.cpu.registers[REGISTER_F], 0x00)  # No flags set
        # Test ADD that produces half-carry
        _set_register_a_and_flags_step_assert_value_a(opcode, 0x0A, 0x10, flag_h=True)
        self.assertEqual(self.cpu.registers[REGISTER_F] & FLAG_Z, 0)  # Zero flag should not be set
        self.assertEqual(self.cpu.registers[REGISTER_F] & FLAG_H, 0)  # Half-carry should be cleared
        # Test ADD that produces carry
        _set_register_a_and_flags_step_assert_value_a(opcode, 0x9A, 0x00, flag_c=True)
        self.assertTrue(self.cpu.registers[REGISTER_F] & FLAG_Z)  # Zero flag should be set
        self.assertTrue(self.cpu.registers[REGISTER_F] & FLAG_C)  # Carry flag should remain set
        # Test SUB without any flags set
        _set_register_a_and_flags_step_assert_value_a(opcode, 0x15, 0x15, flag_n=True)
        self.assertEqual(self.cpu.registers[REGISTER_F] & FLAG_C, 0)  # Carry should remain clear
        # Test SUB that produced a half-carry
        _set_register_a_and_flags_step_assert_value_a(opcode, 0x6B, 0x65, flag_n=True, flag_h=True)
        self.assertEqual(self.cpu.registers[REGISTER_F] & FLAG_H, 0)  # Half-carry should be cleared
        # Test SUB that produced carry
        _set_register_a_and_flags_step_assert_value_a(opcode, 0xA0, 0x40, flag_n=True, flag_c=True)
        self.assertTrue(self.cpu.registers[REGISTER_F] & FLAG_C)  # Carry flag should remain set

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
        INITIAL_CARRY_STATUS = 1
        self._carry_update(INITIAL_CARRY_STATUS)
        OPERAND = 0xFF
        EXPECTED_RESULT, EXPECTED_FLAGS = self._inc(OPERAND)
        ADDR_HIGH, ADDR_LOW = 0x00, 0x10
        MEMORY_VALUE, IMMEDIATE_VALUE = (0, OPERAND) if opcode in IMMEDIATE_OPCODES else (OPERAND, 0)
        self._initialize_hl_and_memory(ADDR_HIGH, ADDR_LOW, MEMORY_VALUE, IMMEDIATE_VALUE)
        self._step_and_assert_flags(opcode, EXPECTED_FLAGS)
        self.assertEqual(self.memory.read_byte((ADDR_HIGH << 8) | ADDR_LOW), EXPECTED_RESULT, f"{opcode=}")

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
        INITIAL_CARRY_STATUS = 1
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
            if opcode == opcodes.ADD_HL_HL: self.cpu.HL = 0  # Clear HL for HL + HL case
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
        OPERAND1 = 0x01
        for opcode, register in OPCODES_TO_ITERATE.items():
            OPERAND2 = self.cpu.registers[register] if register!=REGISTER_A else OPERAND1
            EXPECTED_RESULT, EXPECTED_FLAGS = self._add(OPERAND1, OPERAND2, carry_in= 0)

            self._set_register_a_step_assert(opcode, OPERAND1, EXPECTED_RESULT, EXPECTED_FLAGS)

    def test_add_a_non_register(self):
        OPCODES_TO_ITERATE = { opcodes.ADD_A__HL_, opcodes.ADD_A_IMM }
        INITIAL_CARRY_STATUS = 1
        OPERAND1 = 0x01
        OPERAND2 = 0x02
        EXPECTED_RESULT, EXPECTED_FLAGS = self._add(OPERAND1, OPERAND2, carry_in= 0)
        ADDR_HIGH, ADDR_LOW = 0x00, 0x10
        for opcode in OPCODES_TO_ITERATE:
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
        INITIAL_CARRY_STATUS = 1
        OPERAND1 = 0x01
        for opcode, register in OPCODES_TO_ITERATE.items():
            OPERAND2 = self.cpu.registers[register] if register!=REGISTER_A else OPERAND1
            EXPECTED_RESULT, EXPECTED_FLAGS = self._add(OPERAND1, OPERAND2, carry_in= INITIAL_CARRY_STATUS)

            self._carry_update(INITIAL_CARRY_STATUS)
            self._set_register_a_step_assert(opcode, OPERAND1, EXPECTED_RESULT, EXPECTED_FLAGS)

    def test_adc_a_non_register(self):
        OPCODES_TO_ITERATE = { opcodes.ADC_A__HL_, opcodes.ADC_A_IMM }
        INITIAL_CARRY_STATUS = 1
        OPERAND1 = 0x01
        OPERAND2 = 0x02
        EXPECTED_RESULT, EXPECTED_FLAGS = self._add(OPERAND1, OPERAND2, carry_in= INITIAL_CARRY_STATUS)
        ADDR_HIGH, ADDR_LOW = 0x00, 0x10
        for opcode in OPCODES_TO_ITERATE:
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
        OPERAND1 = 0x01
        INITIAL_CARRY_STATUS = 1
        for opcode, register in OPCODES_TO_ITERATE.items():
            OPERAND2 = self.cpu.registers[register] if register!=REGISTER_A else OPERAND1
            EXPECTED_RESULT, EXPECTED_FLAGS = self._sub(OPERAND1, OPERAND2, carry_in=0)

            self._carry_update(INITIAL_CARRY_STATUS)
            self._set_register_a_step_assert(opcode, OPERAND1, OPERAND1 if compare else EXPECTED_RESULT, EXPECTED_FLAGS)

    def test_sub_a_non_register(self, compare=False):
        OPCODES_TO_ITERATE = { opcodes.CP_A__HL_, opcodes.CP_A_IMM } if compare else { opcodes.SUB_A__HL_, opcodes.SUB_A_IMM }
        INITIAL_CARRY_STATUS = 1
        OPERAND1 = 0x03
        OPERAND2 = 0x02
        EXPECTED_RESULT, EXPECTED_FLAGS = self._sub(OPERAND1, OPERAND2, carry_in=0)
        ADDR_HIGH, ADDR_LOW = 0x00, 0x10
        for opcode in OPCODES_TO_ITERATE:
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
        INITIAL_CARRY_STATUS = 1
        OPERAND1 = 0x01
        for opcode, register in OPCODES_TO_ITERATE.items():
            OPERAND2 = self.cpu.registers[register] if register!=REGISTER_A else OPERAND1
            EXPECTED_RESULT, EXPECTED_FLAGS = self._sub(OPERAND1, OPERAND2, carry_in=INITIAL_CARRY_STATUS)

            self._carry_update(INITIAL_CARRY_STATUS)
            self._set_register_a_step_assert(opcode, OPERAND1, EXPECTED_RESULT, EXPECTED_FLAGS)

    def test_sbc_a_non_register(self):
        OPCODES_TO_ITERATE = { opcodes.SBC_A__HL_, opcodes.SBC_A_IMM }
        INITIAL_CARRY_STATUS = 1
        OPERAND1 = 0x03
        OPERAND2 = 0x02
        EXPECTED_RESULT, EXPECTED_FLAGS = self._sub(OPERAND1, OPERAND2, carry_in= INITIAL_CARRY_STATUS)
        ADDR_HIGH, ADDR_LOW = 0x00, 0x10
        for opcode in OPCODES_TO_ITERATE:
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
        OPERAND1 = 0x0F
        for opcode, register in OPCODES_TO_ITERATE.items():
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
        OPERAND1 = 0x0F
        for opcode, register in OPERANDS.items():
            OPERAND2 = self.cpu.registers[register] if register!=REGISTER_A else OPERAND1
            EXPECTED_RESULT = OPERAND1 ^ OPERAND2
            EXPECTED_FLAGS = FLAG_Z if EXPECTED_RESULT == 0 else 0
            self._set_register_a_step_assert(opcode, OPERAND1, EXPECTED_RESULT, EXPECTED_FLAGS)

    def test_and_a_hl(self):
        OPCODES_TO_ITERATE = { opcodes.AND_A__HL_, opcodes.AND_A_IMM }
        OPERAND1, OPERAND2 = 0x0F, 0x03
        EXPECTED_RESULT = OPERAND1 & OPERAND2
        EXPECTED_FLAGS = FLAG_Z if EXPECTED_RESULT == 0 else 0 | FLAG_H  # Half carry is always set for AND operations
        ADDR_HIGH, ADDR_LOW = 0x00, 0x10

        for opcode in OPCODES_TO_ITERATE:
            MEMORY_VALUE, IMMEDIATE_VALUE = (0, OPERAND2) if opcode in IMMEDIATE_OPCODES else (OPERAND2, 0)
            self._initialize_hl_and_memory(ADDR_HIGH, ADDR_LOW, MEMORY_VALUE, IMMEDIATE_VALUE)
            self._set_register_a_step_assert(opcode, OPERAND1, EXPECTED_RESULT, EXPECTED_FLAGS)

    def test_xor_a_hl(self):
        OPCODES_TO_ITERATE = { opcodes.XOR_A__HL_, opcodes.XOR_A_IMM }
        OPERAND1, OPERAND2 = 0x0F, 0x03
        EXPECTED_RESULT = OPERAND1 ^ OPERAND2
        EXPECTED_FLAGS = FLAG_Z if EXPECTED_RESULT == 0 else 0
        ADDR_HIGH, ADDR_LOW = 0x00, 0x10
        for opcode in OPCODES_TO_ITERATE:
            MEMORY_VALUE, IMMEDIATE_VALUE = (0, OPERAND2) if opcode in IMMEDIATE_OPCODES else (OPERAND2, 0)
            self._initialize_hl_and_memory(ADDR_HIGH, ADDR_LOW, MEMORY_VALUE, IMMEDIATE_VALUE)
            self._set_register_a_step_assert(opcode, OPERAND1, EXPECTED_RESULT, EXPECTED_FLAGS)

    def test_or_a_register(self, OPERAND1 = 0x0F):
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
            OPERAND2 = self.cpu.registers[register] if register!=REGISTER_A else OPERAND1
            EXPECTED_RESULT = OPERAND1 | OPERAND2
            EXPECTED_FLAGS = FLAG_Z if EXPECTED_RESULT == 0 else 0

            self._set_register_a_step_assert(opcode, OPERAND1, EXPECTED_RESULT, EXPECTED_FLAGS)

    def test_or_a_hl(self):
        OPCODES_TO_ITERATE = { opcodes.OR_A__HL_, opcodes.OR_A_IMM }
        OPERAND1, OPERAND2 = 0x0F, 0x03
        EXPECTED_RESULT = OPERAND1 | OPERAND2
        EXPECTED_FLAGS = FLAG_Z if EXPECTED_RESULT == 0 else 0
        ADDR_HIGH, ADDR_LOW = 0x00, 0x10
        for opcode in OPCODES_TO_ITERATE:
            MEMORY_VALUE, IMMEDIATE_VALUE = (0, OPERAND2) if opcode in IMMEDIATE_OPCODES else (OPERAND2, 0)
            self._initialize_hl_and_memory(ADDR_HIGH, ADDR_LOW, MEMORY_VALUE, IMMEDIATE_VALUE)
            self._set_register_a_step_assert(opcode, OPERAND1, EXPECTED_RESULT, EXPECTED_FLAGS)

if __name__ == '__main__':
    unittest.main()
