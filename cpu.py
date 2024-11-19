from dataclasses import dataclass
from memory import Memory
from opcodes import (
    NOP,
    JR_IMM, JR_Z_IMM, JR_NZ_IMM, JR_C_IMM, JR_NC_IMM,
    JP_NZ, JP_ADDR, JP_Z, JP_C, JP_NC,
    RLCA, RRCA, RLA, RRA,
    DAA, CPL, SCF, CCF,
    INC_BC, DEC_BC, INC_DE, DEC_DE, INC_HL, DEC_HL, INC_SP, DEC_SP,
    INC_B, DEC_B, INC_C, DEC_C, INC_D, DEC_D, INC_E, DEC_E, INC_H, DEC_H, INC_L, DEC_L, INC__HL_, DEC__HL_, INC_A, DEC_A,
    LD_BC_IMM16, LD_DE_IMM16, LD_HL_IMM16, LD_SP_IMM16,
    LD__IMM16__SP, LD_SP_HL,
    LD_A_B, LD_A_C, LD_A_D, LD_A_E, LD_A_H, LD_A_L, LD_A__HL_, LD_A_A,
    LD_B_B, LD_B_C, LD_B_D, LD_B_E, LD_B_H, LD_B_L, LD_B__HL_, LD_B_A,
    LD_C_B, LD_C_C, LD_C_D, LD_C_E, LD_C_H, LD_C_L, LD_C__HL_, LD_C_A,
    LD_D_B, LD_D_C, LD_D_D, LD_D_E, LD_D_H, LD_D_L, LD_D__HL_, LD_D_A,
    LD_E_B, LD_E_C, LD_E_D, LD_E_E, LD_E_H, LD_E_L, LD_E__HL_, LD_E_A,
    LD_H_B, LD_H_C, LD_H_D, LD_H_E, LD_H_H, LD_H_L, LD_H__HL_, LD_H_A,
    LD_L_B, LD_L_C, LD_L_D, LD_L_E, LD_L_H, LD_L_L, LD_L__HL_, LD_L_A,
    LD_A_IMM, LD_B_IMM, LD_C_IMM, LD_D_IMM, LD_E_IMM, LD_H_IMM, LD_L_IMM, LD__HL__IMM,
    LD__HL__B, LD__HL__C, LD__HL__D, LD__HL__E, LD__HL__H, LD__HL__L, LD__HL__A,
    ADD_HL_BC, ADD_HL_DE, ADD_HL_HL, ADD_HL_SP,
    ADD_A_B, ADD_A_C, ADD_A_D, ADD_A_E, ADD_A_H, ADD_A_L, ADD_A__HL_, ADD_A_A,
    ADC_A_B, ADC_A_C, ADC_A_D, ADC_A_E, ADC_A_H, ADC_A_L, ADC_A__HL_, ADC_A_A,
    SUB_A_B, SUB_A_C, SUB_A_D, SUB_A_E, SUB_A_H, SUB_A_L, SUB_A__HL_, SUB_A_A,
    SBC_A_B, SBC_A_C, SBC_A_D, SBC_A_E, SBC_A_H, SBC_A_L, SBC_A__HL_, SBC_A_A,
    AND_A_B, AND_A_C, AND_A_D, AND_A_E, AND_A_H, AND_A_L, AND_A__HL_, AND_A_A,
    XOR_A_B, XOR_A_C, XOR_A_D, XOR_A_E, XOR_A_H, XOR_A_L, XOR_A__HL_, XOR_A_A,
    OR_A_B, OR_A_C, OR_A_D, OR_A_E, OR_A_H, OR_A_L, OR_A__HL_, OR_A_A,
    CP_A_B, CP_A_C, CP_A_D, CP_A_E, CP_A_H, CP_A_L, CP_A__HL_, CP_A_A,
    ADD_A_IMM, ADC_A_IMM, SUB_A_IMM, SBC_A_IMM,
    AND_A_IMM, XOR_A_IMM, OR_A_IMM, CP_A_IMM,
    ADD_SP_IMM, LD_HL_S8,
    LD__NN__A, LD__C__A, LD__IMM16__A, LD_A__NN_, LD_A__C_, LD_A__IMM16_,
    LD__BC__A, LD_A__BC_, LD__DE__A, LD_A__DE_, LD__HL_INC__A, LD_A__HL_INC_, LD__HL_DEC__A, LD_A__HL_DEC_
    , POP_BC, POP_DE, POP_HL, POP_AF
    , PUSH_BC, PUSH_DE, PUSH_HL, PUSH_AF
    , RET_NZ, RET_Z, RET, RET_C, RET_NC
    # , RETI
    , RST_00H, RST_08H, RST_10H, RST_18H, RST_20H, RST_28H, RST_30H, RST_38H
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

@dataclass
class CPU:
    memory: Memory
    registers = [0x00] * 8
    pc = START_PC
    sp = START_SP
    INSTRUCTION_MAP = {
        NOP : lambda _: _
    }

    def __post_init__(self):
        self._fill_instruction_map()

    # Properties for register pairs (e.g., BC, DE, HL)
    @property
    def BC(self):
        return (self.registers[REGISTER_B] << 8) | self.registers[REGISTER_C]

    @BC.setter
    def BC(self, value16):
        self.registers[REGISTER_B] = (value16 >> 8) & 0xFF
        self.registers[REGISTER_C] = value16 & 0xFF

    @property
    def DE(self):
        return (self.registers[REGISTER_D] << 8) | self.registers[REGISTER_E]

    @DE.setter
    def DE(self, value16):
        self.registers[REGISTER_D] = (value16 >> 8) & 0xFF
        self.registers[REGISTER_E] = value16 & 0xFF

    @property
    def HL(self):
        return (self.registers[REGISTER_H] << 8) | self.registers[REGISTER_L]

    @HL.setter
    def HL(self, value16):
        self.registers[REGISTER_H] = (value16 >> 8) & 0xFF
        self.registers[REGISTER_L] = value16 & 0xFF

    def _fill_instruction_map(self):
        self.INSTRUCTION_MAP[JR_IMM]    = lambda self: self._jr( self._read_byte_from__pc__and_inc_pc()                         )
        self.INSTRUCTION_MAP[JR_Z_IMM]  = lambda self: self._jr( self._read_byte_from__pc__and_inc_pc()           , zero = True )
        self.INSTRUCTION_MAP[JR_NZ_IMM] = lambda self: self._jr( self._read_byte_from__pc__and_inc_pc(), _not=True, zero = True )
        self.INSTRUCTION_MAP[JR_C_IMM]  = lambda self: self._jr( self._read_byte_from__pc__and_inc_pc()           , carry= True )
        self.INSTRUCTION_MAP[JR_NC_IMM] = lambda self: self._jr( self._read_byte_from__pc__and_inc_pc(), _not=True, carry= True )
        self.INSTRUCTION_MAP[JP_ADDR]   = lambda self: self._jp( self._read_byte_from__pc__and_inc_pc(), self._read_byte_from__pc__and_inc_pc()                         )
        self.INSTRUCTION_MAP[JP_Z]      = lambda self: self._jp( self._read_byte_from__pc__and_inc_pc(), self._read_byte_from__pc__and_inc_pc()           , zero = True )
        self.INSTRUCTION_MAP[JP_NZ]     = lambda self: self._jp( self._read_byte_from__pc__and_inc_pc(), self._read_byte_from__pc__and_inc_pc(), _not=True, zero = True )
        self.INSTRUCTION_MAP[JP_C]      = lambda self: self._jp( self._read_byte_from__pc__and_inc_pc(), self._read_byte_from__pc__and_inc_pc()           , carry= True )
        self.INSTRUCTION_MAP[JP_NC]     = lambda self: self._jp( self._read_byte_from__pc__and_inc_pc(), self._read_byte_from__pc__and_inc_pc(), _not=True, carry= True )
        self.INSTRUCTION_MAP[RET]    = lambda self: self._ret(                        )
        self.INSTRUCTION_MAP[RET_Z]  = lambda self: self._ret(            zero = True )
        self.INSTRUCTION_MAP[RET_NZ] = lambda self: self._ret( _not=True, zero = True )
        self.INSTRUCTION_MAP[RET_C]  = lambda self: self._ret(            carry= True )
        self.INSTRUCTION_MAP[RET_NC] = lambda self: self._ret( _not=True, carry= True )
        # self.INSTRUCTION_MAP[RETI]    = lambda self: self._reti( )
        self.INSTRUCTION_MAP[RLCA] = lambda self: self._rotate_a()
        self.INSTRUCTION_MAP[RRCA] = lambda self: self._rotate_a(left=False)
        self.INSTRUCTION_MAP[RLA]  = lambda self: self._rotate_a(carry=False)
        self.INSTRUCTION_MAP[RRA]  = lambda self: self._rotate_a(left=False, carry=False)
        self.INSTRUCTION_MAP[DAA]  = lambda self: self._daa()
        self.INSTRUCTION_MAP[CPL]  = lambda self: self._cpl()
        self.INSTRUCTION_MAP[SCF]  = lambda self: self._scf()
        self.INSTRUCTION_MAP[CCF]  = lambda self: self._ccf()
        def _map_inc_dec_opcode_register_pairs_to_operation(opcode_register_groups, operation, **kwargs):
            for opcode, register in opcode_register_groups:
                self.INSTRUCTION_MAP[opcode] = lambda self, reg=register: operation(self, reg, **kwargs)
        def _map_opcode_inc_dec_16_pairs_to_operation(opcode_register_groups, operation, **kwargs):
            for opcode, register16 in opcode_register_groups:
                self.INSTRUCTION_MAP[opcode] = lambda self, reg16=register16: operation(self, reg16, **kwargs)
        def _map_opcode_add_hl_register16_pairs_to_operation(opcode_register_groups, operation, **kwargs):
            for opcode, register_pair in opcode_register_groups:
                self.INSTRUCTION_MAP[opcode] = lambda self, reg_pair=register_pair: operation(self, getattr(self, reg_pair), **kwargs)
        def _map_opcode_register_trios_to_operation(opcode_register_trios, operation, **kwargs):
            for opcode, register1, register2 in opcode_register_trios:
                self.INSTRUCTION_MAP[opcode] = lambda self, reg1=register1, reg2=register2: operation(self, reg1, self.registers[reg2], **kwargs)
        def _map_opcode_register_pairs_to_operation(opcode_register_pairs, operation, **kwargs):
            for opcode, register in opcode_register_pairs:
                self.INSTRUCTION_MAP[opcode] = lambda self, reg=register: operation(self, self.registers[reg], **kwargs)
        def _map_opcode_hl_imm_to_ld_reg(opcode_hl_imm_register, operation, **kwargs):
            for opcode_hl, opcode_imm, register in opcode_hl_imm_register:
                self.INSTRUCTION_MAP[opcode_hl] = lambda self, reg=register: operation(self, reg, self.memory.read_byte( (self.registers[REGISTER_H] << 8) | self.registers[REGISTER_L] ), **kwargs)
                self.INSTRUCTION_MAP[opcode_imm] = lambda self, reg=register: (operation(self, reg, self._read_byte_from__pc__and_inc_pc(), **kwargs))
        def _map_opcode_hl_imm_to_operation(opcode_hl, opcode_imm, operation, **kwargs):
            self.INSTRUCTION_MAP[opcode_hl] = lambda self: operation(self, self.memory.read_byte( (self.registers[REGISTER_H] << 8) | self.registers[REGISTER_L] ), **kwargs)
            self.INSTRUCTION_MAP[opcode_imm] = lambda self: (operation(self, self._read_byte_from__pc__and_inc_pc(), **kwargs))
        def _map_opcode_ld_register16_pairs_to_operation(opcode_register_groups, operation, **kwargs):
            for opcode, register_pair in opcode_register_groups:
                self.INSTRUCTION_MAP[opcode] = lambda self, reg16=register_pair: operation(self, reg16, **kwargs)
        def _map_opcode_rst(opcodes):
            for opcode in opcodes:
                self.INSTRUCTION_MAP[opcode] = lambda self, value=opcode-RST_00H: self._rst(value)
        # Define the opcode-register pairs and call add_instruction_map with the appropriate function and flags
        _map_opcode_inc_dec_16_pairs_to_operation( [(INC_BC, 'BC'), (INC_DE, 'DE'), (INC_HL, 'HL'), (INC_SP, 'sp')],
            lambda self, value: self._inc16(value) )
        _map_opcode_inc_dec_16_pairs_to_operation( [(DEC_BC, 'BC'), (DEC_DE, 'DE'), (DEC_HL, 'HL'), (DEC_SP, 'sp') ],
            lambda self, value: self._dec16(value) )
        _map_inc_dec_opcode_register_pairs_to_operation( [ (INC_B, REGISTER_B), (INC_C, REGISTER_C), (INC_D, REGISTER_D), (INC_E, REGISTER_E), (INC_H, REGISTER_H), (INC_L, REGISTER_L), (INC_A, REGISTER_A) ],
                            lambda self, register: self._inc(register))
        _map_inc_dec_opcode_register_pairs_to_operation( [ (DEC_B, REGISTER_B), (DEC_C, REGISTER_C), (DEC_D, REGISTER_D), (DEC_E, REGISTER_E), (DEC_H, REGISTER_H), (DEC_L, REGISTER_L), (DEC_A, REGISTER_A) ],
                            lambda self, register: self._dec(register))
        self.INSTRUCTION_MAP[INC__HL_] = lambda self: self._inc__hl_()
        self.INSTRUCTION_MAP[DEC__HL_] = lambda self: self._dec__hl_()
        _map_opcode_add_hl_register16_pairs_to_operation( [(ADD_HL_BC, 'BC'), (ADD_HL_DE, 'DE'), (ADD_HL_HL, 'HL'), (ADD_HL_SP, 'sp') ],
            lambda self, value: self._add_hl_16(value))
        _map_opcode_register_trios_to_operation([ (LD_A_B, REGISTER_A, REGISTER_B), (LD_A_C, REGISTER_A, REGISTER_C), (LD_A_D, REGISTER_A, REGISTER_D), (LD_A_E, REGISTER_A, REGISTER_E), (LD_A_H, REGISTER_A, REGISTER_H), (LD_A_L, REGISTER_A, REGISTER_L), (LD_A_A, REGISTER_A, REGISTER_A),
                                                (LD_B_B, REGISTER_B, REGISTER_B), (LD_B_C, REGISTER_B, REGISTER_C), (LD_B_D, REGISTER_B, REGISTER_D), (LD_B_E, REGISTER_B, REGISTER_E), (LD_B_H, REGISTER_B, REGISTER_H), (LD_B_L, REGISTER_B, REGISTER_L), (LD_B_A, REGISTER_B, REGISTER_A),
                                                (LD_C_B, REGISTER_C, REGISTER_B), (LD_C_C, REGISTER_C, REGISTER_C), (LD_C_D, REGISTER_C, REGISTER_D), (LD_C_E, REGISTER_C, REGISTER_E), (LD_C_H, REGISTER_C, REGISTER_H), (LD_C_L, REGISTER_C, REGISTER_L), (LD_C_A, REGISTER_C, REGISTER_A),
                                                (LD_D_B, REGISTER_D, REGISTER_B), (LD_D_C, REGISTER_D, REGISTER_C), (LD_D_D, REGISTER_D, REGISTER_D), (LD_D_E, REGISTER_D, REGISTER_E), (LD_D_H, REGISTER_D, REGISTER_H), (LD_D_L, REGISTER_D, REGISTER_L), (LD_D_A, REGISTER_D, REGISTER_A),
                                                (LD_E_B, REGISTER_E, REGISTER_B), (LD_E_C, REGISTER_E, REGISTER_C), (LD_E_D, REGISTER_E, REGISTER_D), (LD_E_E, REGISTER_E, REGISTER_E), (LD_E_H, REGISTER_E, REGISTER_H), (LD_E_L, REGISTER_E, REGISTER_L), (LD_E_A, REGISTER_E, REGISTER_A),
                                                (LD_H_B, REGISTER_H, REGISTER_B), (LD_H_C, REGISTER_H, REGISTER_C), (LD_H_D, REGISTER_H, REGISTER_D), (LD_H_E, REGISTER_H, REGISTER_E), (LD_H_H, REGISTER_H, REGISTER_H), (LD_H_L, REGISTER_H, REGISTER_L), (LD_H_A, REGISTER_H, REGISTER_A),
                                                (LD_L_B, REGISTER_L, REGISTER_B), (LD_L_C, REGISTER_L, REGISTER_C), (LD_L_D, REGISTER_L, REGISTER_D), (LD_L_E, REGISTER_L, REGISTER_E), (LD_L_H, REGISTER_L, REGISTER_H), (LD_L_L, REGISTER_L, REGISTER_L), (LD_L_A, REGISTER_L, REGISTER_A)
         ],
                            lambda self, register, value: self._ld(register, value))
        _map_opcode_register_pairs_to_operation([(LD__HL__A, REGISTER_A),
                                                 (LD__HL__B, REGISTER_B), (LD__HL__C, REGISTER_C),
                                                 (LD__HL__D, REGISTER_D), (LD__HL__E, REGISTER_E),
                                                 (LD__HL__H, REGISTER_H), (LD__HL__L, REGISTER_L) ],
                            lambda self, value: self._ld__indirect_((self.registers[REGISTER_H] << 8) | self.registers[REGISTER_L], value) )
        _map_opcode_register_pairs_to_operation([ (ADD_A_A, REGISTER_A), (ADD_A_B, REGISTER_B), (ADD_A_C, REGISTER_C), (ADD_A_D, REGISTER_D), (ADD_A_E, REGISTER_E), (ADD_A_H, REGISTER_H), (ADD_A_L, REGISTER_L) ],
                            lambda self, value: self._add_a(value))
        _map_opcode_register_pairs_to_operation( [ (ADC_A_A, REGISTER_A), (ADC_A_B, REGISTER_B), (ADC_A_C, REGISTER_C), (ADC_A_D, REGISTER_D), (ADC_A_E, REGISTER_E), (ADC_A_H, REGISTER_H), (ADC_A_L, REGISTER_L) ],
                            lambda self, value: self._add_a(value, use_carry=True))
        _map_opcode_register_pairs_to_operation( [ (SUB_A_A, REGISTER_A), (SUB_A_B, REGISTER_B), (SUB_A_C, REGISTER_C), (SUB_A_D, REGISTER_D), (SUB_A_E, REGISTER_E), (SUB_A_H, REGISTER_H), (SUB_A_L, REGISTER_L) ],
                            lambda self, value: self._sub_a(value))
        _map_opcode_register_pairs_to_operation( [ (SBC_A_A, REGISTER_A), (SBC_A_B, REGISTER_B), (SBC_A_C, REGISTER_C), (SBC_A_D, REGISTER_D), (SBC_A_E, REGISTER_E), (SBC_A_H, REGISTER_H), (SBC_A_L, REGISTER_L) ],
                            lambda self, value: self._sub_a(value, use_carry=True))
        _map_opcode_register_pairs_to_operation( [ (AND_A_A, REGISTER_A), (AND_A_B, REGISTER_B), (AND_A_C, REGISTER_C), (AND_A_D, REGISTER_D), (AND_A_E, REGISTER_E), (AND_A_H, REGISTER_H), (AND_A_L, REGISTER_L) ],
                            lambda self, value: self._and_a(value))
        _map_opcode_register_pairs_to_operation( [ (XOR_A_A, REGISTER_A), (XOR_A_B, REGISTER_B), (XOR_A_C, REGISTER_C), (XOR_A_D, REGISTER_D), (XOR_A_E, REGISTER_E), (XOR_A_H, REGISTER_H), (XOR_A_L, REGISTER_L) ],
                            lambda self, value: self._xor_a(value))
        _map_opcode_register_pairs_to_operation( [ (OR_A_A, REGISTER_A), (OR_A_B, REGISTER_B), (OR_A_C, REGISTER_C), (OR_A_D, REGISTER_D), (OR_A_E, REGISTER_E), (OR_A_H, REGISTER_H), (OR_A_L, REGISTER_L) ],
                            lambda self, value: self._or_a(value))
        _map_opcode_register_pairs_to_operation( [ (CP_A_A, REGISTER_A), (CP_A_B, REGISTER_B), (CP_A_C, REGISTER_C), (CP_A_D, REGISTER_D), (CP_A_E, REGISTER_E), (CP_A_H, REGISTER_H), (CP_A_L, REGISTER_L) ],
                            lambda self, value: self._sub_a(value, compare=True))
        # Define each set of operations with their HL and IMM opcodes
        self.INSTRUCTION_MAP[LD__HL__IMM] = lambda self: ( self._ld__indirect_((self.registers[REGISTER_H] << 8) | self.registers[REGISTER_L], self._read_byte_from__pc__and_inc_pc()) )
        _map_opcode_hl_imm_to_ld_reg([(LD_A__HL_, LD_A_IMM, REGISTER_A),
                                      (LD_B__HL_, LD_B_IMM, REGISTER_B), (LD_C__HL_, LD_C_IMM, REGISTER_C),
                                      (LD_D__HL_, LD_D_IMM, REGISTER_D), (LD_E__HL_, LD_E_IMM, REGISTER_E),
                                      (LD_H__HL_, LD_H_IMM, REGISTER_H), (LD_L__HL_, LD_L_IMM, REGISTER_L)],
                                    lambda self, register, value: self._ld(register, value))
        _map_opcode_hl_imm_to_operation(ADD_A__HL_, ADD_A_IMM, lambda self, value: self._add_a(value))
        _map_opcode_hl_imm_to_operation(ADC_A__HL_, ADC_A_IMM, lambda self, value: self._add_a(value, use_carry=True))
        _map_opcode_hl_imm_to_operation(SUB_A__HL_, SUB_A_IMM, lambda self, value: self._sub_a(value))
        _map_opcode_hl_imm_to_operation(SBC_A__HL_, SBC_A_IMM, lambda self, value: self._sub_a(value, use_carry=True))
        _map_opcode_hl_imm_to_operation(AND_A__HL_, AND_A_IMM, lambda self, value: self._and_a(value))
        _map_opcode_hl_imm_to_operation(XOR_A__HL_, XOR_A_IMM, lambda self, value: self._xor_a(value))
        _map_opcode_hl_imm_to_operation(OR_A__HL_, OR_A_IMM, lambda self, value: self._or_a(value))
        _map_opcode_hl_imm_to_operation(CP_A__HL_, CP_A_IMM, lambda self, value: self._sub_a(value, compare=True))
        _map_opcode_ld_register16_pairs_to_operation( [(LD_BC_IMM16, 'BC'), (LD_DE_IMM16, 'DE'), (LD_HL_IMM16, 'HL'), (LD_SP_IMM16, 'sp') ],
            lambda self, register16: self._ld_r16(register16, self._read_byte_from__pc__and_inc_pc() | (self._read_byte_from__pc__and_inc_pc() << 8) )
        )
        _map_opcode_rst( [RST_00H, RST_08H, RST_10H, RST_18H, RST_20H, RST_28H, RST_30H, RST_38H] )
        self.INSTRUCTION_MAP[LD_SP_HL]     = lambda self: self._ld_r16('sp', self.HL )
        self.INSTRUCTION_MAP[POP_BC]       = lambda self: self._pop_r16( REGISTER_B, REGISTER_C )
        self.INSTRUCTION_MAP[POP_DE]       = lambda self: self._pop_r16( REGISTER_D, REGISTER_E )
        self.INSTRUCTION_MAP[POP_HL]       = lambda self: self._pop_r16( REGISTER_H, REGISTER_L )
        self.INSTRUCTION_MAP[POP_AF]       = lambda self: self._pop_r16( REGISTER_A, REGISTER_F )
        self.INSTRUCTION_MAP[PUSH_BC]      = lambda self: self._push_r16( REGISTER_B, REGISTER_C )
        self.INSTRUCTION_MAP[PUSH_DE]      = lambda self: self._push_r16( REGISTER_D, REGISTER_E )
        self.INSTRUCTION_MAP[PUSH_HL]      = lambda self: self._push_r16( REGISTER_H, REGISTER_L )
        self.INSTRUCTION_MAP[PUSH_AF]      = lambda self: self._push_r16( REGISTER_A, REGISTER_F )
        self.INSTRUCTION_MAP[ADD_SP_IMM]   = lambda self: self._add_sp_imm()
        self.INSTRUCTION_MAP[LD_HL_S8]     = lambda self: self._ld_hl_s8()
        self.INSTRUCTION_MAP[LD__BC__A]    = lambda self: self._ld__indirect_( (self.registers[REGISTER_B] << 8) | self.registers[REGISTER_C] , self.registers[REGISTER_A] )
        self.INSTRUCTION_MAP[LD__DE__A]    = lambda self: self._ld__indirect_( (self.registers[REGISTER_D] << 8) | self.registers[REGISTER_E] , self.registers[REGISTER_A] )
        self.INSTRUCTION_MAP[LD__HL_INC__A]= lambda self: self._ld__indirect_( self._hl(+1) , self.registers[REGISTER_A] )
        self.INSTRUCTION_MAP[LD__HL_DEC__A]= lambda self: self._ld__indirect_( self._hl(-1) , self.registers[REGISTER_A] )
        self.INSTRUCTION_MAP[LD__NN__A]    = lambda self: self._ld__indirect_( 0xFF00 | self._read_byte_from__pc__and_inc_pc() , self.registers[REGISTER_A] )
        self.INSTRUCTION_MAP[LD__C__A]     = lambda self: self._ld__indirect_( 0xFF00 | self.registers[REGISTER_C] , self.registers[REGISTER_A] )
        self.INSTRUCTION_MAP[LD__IMM16__A] = lambda self: self._ld__indirect_( (self._read_byte_from__pc__and_inc_pc() << 8) | self._read_byte_from__pc__and_inc_pc() , self.registers[REGISTER_A] )
        self.INSTRUCTION_MAP[LD_A__BC_]    = lambda self: self._ld(REGISTER_A, self.memory.read_byte( (self.registers[REGISTER_B] << 8) | self.registers[REGISTER_C] ))
        self.INSTRUCTION_MAP[LD_A__DE_]    = lambda self: self._ld(REGISTER_A, self.memory.read_byte( (self.registers[REGISTER_D] << 8) | self.registers[REGISTER_E] ))
        self.INSTRUCTION_MAP[LD_A__HL_INC_]= lambda self: self._ld(REGISTER_A, self.memory.read_byte( self._hl(+1) ))
        self.INSTRUCTION_MAP[LD_A__HL_DEC_]= lambda self: self._ld(REGISTER_A, self.memory.read_byte( self._hl(-1) ))
        self.INSTRUCTION_MAP[LD_A__NN_]    = lambda self: self._ld(REGISTER_A, self.memory.read_byte( 0xFF00 | self._read_byte_from__pc__and_inc_pc() ))
        self.INSTRUCTION_MAP[LD_A__C_]     = lambda self: self._ld(REGISTER_A, self.memory.read_byte( 0xFF00 | self.registers[REGISTER_C] ))
        self.INSTRUCTION_MAP[LD_A__IMM16_] = lambda self: self._ld(REGISTER_A, self.memory.read_byte( (self._read_byte_from__pc__and_inc_pc() << 8) | self._read_byte_from__pc__and_inc_pc() ))
        self.INSTRUCTION_MAP[LD__IMM16__SP]   = lambda self: self._ld__indirect_16((self._read_byte_from__pc__and_inc_pc() << 8) | self._read_byte_from__pc__and_inc_pc(), self.sp )

    def _read_byte_from__pc__and_inc_pc(self):
        value = self.memory.read_byte(self.pc)
        self.pc = (self.pc + 1) & 0xFFFF
        return value

    def step(self):
        opcode = self._read_byte_from__pc__and_inc_pc()
        self.INSTRUCTION_MAP[opcode](self)

    def _jr(self, byte, _not=False, zero=False, carry=False ):
        if( (
                not zero and not carry
            ) or (
                zero and (
                    ( not _not and (self.registers[REGISTER_F]&FLAG_Z) )
                    or
                    ( _not and not (self.registers[REGISTER_F]&FLAG_Z) )
                )
            ) or (
                carry and (
                    ( not _not and (self.registers[REGISTER_F]&FLAG_C) )
                    or
                    ( _not and not (self.registers[REGISTER_F]&FLAG_C) )
                )
            ) ):
            self.pc = (self.pc + byte) & 0xFFFF

    def _jp(self, byte_lo, byte_hi, _not=False, zero=False, carry=False ):
        if( (
                not zero and not carry
            ) or (
                zero and (
                    ( not _not and (self.registers[REGISTER_F]&FLAG_Z) )
                    or
                    ( _not and not (self.registers[REGISTER_F]&FLAG_Z) )
                )
            ) or (
                carry and (
                    ( not _not and (self.registers[REGISTER_F]&FLAG_C) )
                    or
                    ( _not and not (self.registers[REGISTER_F]&FLAG_C) )
                )
            ) ):
            self.pc = (byte_hi << 8) + byte_lo

    def _ret(self, _not=False, zero=False, carry=False ):
        """
        Pop from the memory stack the program counter PC value pushed when the subroutine was called, returning control to the source program.

        The contents of the address specified by the stack pointer SP are loaded in the lower-order byte of PC, and the contents of SP are incremented by 1.
        The contents of the address specified by the new SP value are then loaded in the higher-order byte of PC, and the contents of SP are incremented by 1 again.
        (The value of SP is 2 larger than before instruction execution.) The next instruction is fetched from the address specified by the content of PC (as usual).
        """
        if( (
                not zero and not carry
            ) or (
                zero and (
                    ( not _not and (self.registers[REGISTER_F]&FLAG_Z) )
                    or
                    ( _not and not (self.registers[REGISTER_F]&FLAG_Z) )
                )
            ) or (
                carry and (
                    ( not _not and (self.registers[REGISTER_F]&FLAG_C) )
                    or
                    ( _not and not (self.registers[REGISTER_F]&FLAG_C) )
                )
            ) ):
            self.pc = self.memory.read_byte( self.sp )
            self.sp = (self.sp + 1) & 0xFFFF
            self.pc |= (self.memory.read_byte( self.sp ) << 8)
            self.sp = (self.sp + 1) & 0xFFFF

    def _rotate_a(self, left=True, carry=True):

        def _rl(previous_register_a:int, use_carry=False, previous_c=0):
            result = previous_register_a << 1
            output_carry = previous_register_a & 0b1000_0000
            if ((use_carry and output_carry)
             or (not use_carry and previous_c)
            ):
                result |= 0b0000_0001

            return result, output_carry

        def _rr(previous_register_a:int, use_carry=False, previous_c=0):
            result = previous_register_a >> 1
            output_carry = previous_register_a & 0b0000_0001
            if ((use_carry and output_carry)
             or (not use_carry and previous_c)
            ):
                result |= 0b1000_0000

            return result, output_carry

        new_a, new_carry = (_rl if left else _rr) ( self.registers[REGISTER_A] , carry, self.registers[REGISTER_F]&FLAG_C)
        self.registers[REGISTER_F] = FLAG_C if new_carry else 0
        self.registers[REGISTER_A] = new_a & 0xFF

    def _daa(self):
        a = self.registers[REGISTER_A]
        flag_n = self.registers[REGISTER_F] & FLAG_N
        adjust = 0
        carry = False

        # Check flags to determine adjustment values
        if (self.registers[REGISTER_F] & FLAG_H) or (not (flag_n) and (a & 0x0F) > 0x09):
            adjust |= 0x06  # Add 0x06 to fix lower nibble
        if (self.registers[REGISTER_F] & FLAG_C) or (not (flag_n) and a > 0x99):
            adjust |= 0x60  # Add 0x60 to fix upper nibble
            carry = True

        # Apply adjustment based on N flag (add or subtract)
        a = a + (-adjust if flag_n else adjust)
        a &= 0xFF  # Keep result within 8 bits

        # Update flags
        self.registers[REGISTER_A] = a
        self.registers[REGISTER_F] &= ~(FLAG_Z | FLAG_H)  # Clear Z and H flags
        if a == 0:
            self.registers[REGISTER_F] |= FLAG_Z  # Set Z if result is zero
        if carry:
            self.registers[REGISTER_F] |= FLAG_C  # Preserve or set C flag

    def _cpl(self):
        # Complement the A register
        self.registers[REGISTER_A] = ~self.registers[REGISTER_A] & 0xFF
        
        # Set the subtraction flag (N = 1)
        self.registers[REGISTER_F] |= FLAG_N
        
        # Set the half-carry flag (H = 1)
        self.registers[REGISTER_F] |= FLAG_H

    def _scf(self):
        self.registers[REGISTER_F] |= FLAG_C
        self.registers[REGISTER_F] &= ~FLAG_H & ~FLAG_N

    def _ccf(self):
        if self.registers[REGISTER_F] & FLAG_C:
            self.registers[REGISTER_F] &= ~FLAG_C
        else:
            self.registers[REGISTER_F] |= FLAG_C
        self.registers[REGISTER_F] &= ~FLAG_H & ~FLAG_N

    def _inc16(self, register_name):
        previous_value = getattr(self, register_name)
        next_value = (previous_value + 1) & 0xFFFF
        setattr(self, register_name, next_value)

    def _dec16(self, register_name):
        previous_value = getattr(self, register_name)
        next_value = (previous_value - 1) & 0xFFFF
        setattr(self, register_name, next_value)

    def _inc(self, register):
        """Increment the specified register by 1."""
        self.registers[register] += 1
        self.registers[register] &= 0xFF  # Ensure it wraps around at 8 bits

        # Update flags
        if self.registers[register] == 0:
            self.registers[REGISTER_F] |= FLAG_Z  # Set Zero flag
        else:
            self.registers[REGISTER_F] &= ~FLAG_Z  # Clear Zero flag

        # Half carry is set if the lower 4 bits go from 0x0F to 0x10
        if (self.registers[register] & 0x0F) == 0x00:
            self.registers[REGISTER_F] |= FLAG_H  # Set Half Carry flag
        else:
            self.registers[REGISTER_F] &= ~FLAG_H  # Clear Half Carry flag

        self.registers[REGISTER_F] &= ~FLAG_N  # Clear Subtract flag

    def _dec(self, register):
        """Decrement the specified register by 1."""
        self.registers[register] -= 1
        self.registers[register] &= 0xFF  # Ensure it wraps around at 8 bits

        # Update flags
        if self.registers[register] == 0:
            self.registers[REGISTER_F] |= FLAG_Z  # Set Zero flag
        else:
            self.registers[REGISTER_F] &= ~FLAG_Z  # Clear Zero flag

        # Half carry is set if the lower 4 bits go from 0x00 to 0x0F
        if (self.registers[register] & 0x0F) == 0x0F:
            self.registers[REGISTER_F] |= FLAG_H  # Set Half Carry flag
        else:
            self.registers[REGISTER_F] &= ~FLAG_H  # Clear Half Carry flag

        self.registers[REGISTER_F] |= FLAG_N  # Set Subtract flag

    def _inc__hl_(self):
        """Increment the value at the address pointed to by HL."""
        address = (self.registers[REGISTER_H] << 8) | self.registers[REGISTER_L]
        value = self.memory.read_byte(address)
        incremented_value = (value + 1) & 0xFF  # Increment and ensure it wraps around
        self.memory.write_byte(address, incremented_value)

        # Update flags
        if incremented_value == 0:
            self.registers[REGISTER_F] |= FLAG_Z  # Set Zero flag
        else:
            self.registers[REGISTER_F] &= ~FLAG_Z  # Clear Zero flag

        if (value & 0x0F) == 0x0F:
            self.registers[REGISTER_F] |= FLAG_H  # Set Half Carry flag
        else:
            self.registers[REGISTER_F] &= ~FLAG_H  # Clear Half Carry flag

        self.registers[REGISTER_F] &= ~FLAG_N  # Clear Subtract flag

    def _dec__hl_(self):
        """Decrement the value at the address pointed to by HL."""
        address = (self.registers[REGISTER_H] << 8) | self.registers[REGISTER_L]
        value = self.memory.read_byte(address)
        decremented_value = (value - 1) & 0xFF  # Decrement and ensure it wraps around
        self.memory.write_byte(address, decremented_value)

        # Update flags
        if decremented_value == 0:
            self.registers[REGISTER_F] |= FLAG_Z  # Set Zero flag
        else:
            self.registers[REGISTER_F] &= ~FLAG_Z  # Clear Zero flag

        if (value & 0x0F) == 0x00:
            self.registers[REGISTER_F] |= FLAG_H  # Set Half Carry flag
        else:
            self.registers[REGISTER_F] &= ~FLAG_H  # Clear Half Carry flag

        self.registers[REGISTER_F] |= FLAG_N  # Set Subtract flag

    def _add_hl_16(self, operand_16):
        previous_hl = (self.registers[REGISTER_H] << 8) | self.registers[REGISTER_L]
        result = previous_hl + operand_16

        # Update H and L with the result, only keeping the lower 16 bits
        self.registers[REGISTER_H] = (result >> 8) & 0xFF
        self.registers[REGISTER_L] = result & 0xFF
        # Clear the N flag as this is an addition
        self.registers[REGISTER_F] &= ~FLAG_N
        # Set the H flag if there is a half-carry from bit 11 to bit 12
        if ((previous_hl & 0x0FFF) + (operand_16 & 0x0FFF)) > 0x0FFF:
            self.registers[REGISTER_F] |= FLAG_H
        else:
            self.registers[REGISTER_F] &= ~FLAG_H
        # Set the C flag if there is a carry from bit 15
        if result > 0xFFFF:
            self.registers[REGISTER_F] |= FLAG_C
        else:
            self.registers[REGISTER_F] &= ~FLAG_C

    def _hl(self, operand):
        previous_hl = self.HL
        self.HL = (self.HL + operand) & 0xFFFF
        return previous_hl

    def _ld(self, register_index, value_to_load):
        self.registers[register_index] = value_to_load

    def _ld_r16(self, register_name, value_to_load):
        setattr(self, register_name, value_to_load)

    def _ld__indirect_(self, address, value_to_load):
        self.memory.write_byte(address, value_to_load)

    def _ld__indirect_16(self, address, value_to_load):
        self.memory.write_byte(address, value_to_load & 0x00FF)
        self.memory.write_byte(address + 1, value_to_load >> 8)

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

    def _sub_a(self, operand_2, use_carry=False, compare=False):
        carry = (self.registers[REGISTER_F] & FLAG_C) >> 4 if use_carry else 0
        operand_1 = self.registers[REGISTER_A]
        result = operand_1 - operand_2 - carry
        if not compare:
            self.registers[REGISTER_A] = result & 0xFF

        self.registers[REGISTER_F] = 0x00
        if result & 0xFF == 0:
            self.registers[REGISTER_F] |= FLAG_Z
        self.registers[REGISTER_F] |= FLAG_N
        if result < 0:
            self.registers[REGISTER_F] |= FLAG_C
        if ((operand_1 & 0x0F) < (operand_2 & 0x0F) + carry):
            self.registers[REGISTER_F] |= FLAG_H

    def _and_a(self, operand2):
        self.registers[REGISTER_A] &= operand2  # AND operation

        # Set flags
        self.registers[REGISTER_F] = 0
        if self.registers[REGISTER_A] == 0:
            self.registers[REGISTER_F] |= FLAG_Z  # Set zero flag
        # Half carry is always set for AND
        self.registers[REGISTER_F] |= FLAG_H 

    def _xor_a(self, operand2):
        self.registers[REGISTER_A] ^= operand2  # XOR operation

        # Set flags
        self.registers[REGISTER_F] = 0
        if self.registers[REGISTER_A] == 0:
            self.registers[REGISTER_F] |= FLAG_Z  # Set zero flag

    def _or_a(self, operand2):
        self.registers[REGISTER_A] |= operand2  # OR operation

        # Set flags
        self.registers[REGISTER_F] = 0
        if self.registers[REGISTER_A] == 0:
            self.registers[REGISTER_F] |= FLAG_Z  # Set zero flag

    def _signed_byte(self, offset):
        return offset if offset < 0x80 else offset - 0x100

    def _add_sp_imm(self):
        offset = self._read_byte_from__pc__and_inc_pc()
        offset_signed = self._signed_byte(offset)
        sp_initial = self.sp
        result = sp_initial + offset_signed
        flag_h = FLAG_H if ((sp_initial & 0x0F) + (offset_signed & 0x0F)) > 0x0F else 0
        flag_c = FLAG_C if result > 0xFFFF or result < 0 else 0

        self.sp = result & 0xFFFF
        self.registers[REGISTER_F] = flag_h | flag_c

    def _ld_hl_s8(self):
        offset = self._read_byte_from__pc__and_inc_pc()
        offset_signed = self._signed_byte(offset)
        sp_initial = self.sp
        result = sp_initial + offset_signed
        flag_h = FLAG_H if ((sp_initial & 0x0F) + (offset_signed & 0x0F)) > 0x0F else 0
        flag_c = FLAG_C if result > 0xFFFF or result < 0 else 0

        self.HL = result & 0xFFFF
        self.registers[REGISTER_F] = flag_h | flag_c

    def _pop_r16(self, reg_h, reg_l ):
        self.registers[reg_l] = self.memory.read_byte( self.sp )
        self.sp = (self.sp + 1) & 0xFFFF
        self.registers[reg_h] = self.memory.read_byte( self.sp )
        self.sp = (self.sp + 1) & 0xFFFF

    def _push_r16(self, reg_h, reg_l ):
        self.sp = (self.sp - 1) & 0xFFFF
        self.memory.write_byte( self.sp, self.registers[reg_h] )
        self.sp = (self.sp - 1) & 0xFFFF
        self.memory.write_byte( self.sp, self.registers[reg_l] )

    def _rst(self, value):
        self.sp = (self.sp - 1) & 0xFFFF
        self.memory.write_byte( self.sp, (self.pc & 0xFF00) >> 8 )
        self.sp = (self.sp - 1) & 0xFFFF
        self.memory.write_byte( self.sp, self.pc & 0xFF )
        self.pc = value
