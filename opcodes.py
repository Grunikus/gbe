"""Opcode constants from: https://izik1.github.io/gbops/index.html
Explanations found at: https://meganesu.github.io/generate-gb-opcodes/"""
"""
NOP          = 0x00  # No operation
LD_BC_NN    = 0x01  # LD BC, nn
LD_BC_A     = 0x02  # LD (BC), A
INC_BC      = 0x03  # INC BC
LD_B_IMM    = 0x06  # LD B, imm
RLCA         = 0x07  # RLCA
LD_NN_SP    = 0x08  # LD (nn), SP
LD_A_BC     = 0x0A  # LD A, (BC)
DEC_BC      = 0x0B  # DEC BC
LD_C_IMM    = 0x0E  # LD C, imm
RRCA         = 0x0F  # RRCA
STOP         = 0x10  # STOP
LD_DE_NN    = 0x11  # LD DE, nn
LD_DE_A     = 0x12  # LD (DE), A
INC_DE      = 0x13  # INC DE
LD_D_IMM    = 0x16  # LD D, imm
RLA          = 0x17  # RLA
JR_IMM      = 0x18  # JR imm
LD_A_DE     = 0x1A  # LD A, (DE)
DEC_DE      = 0x1B  # DEC DE
LD_E_IMM    = 0x1E  # LD E, imm
RRA          = 0x1F  # RRA
JR_Z_IMM    = 0x20  # JR Z, imm
LD_HL_NN    = 0x21  # LD HL, nn
LD_HL_A     = 0x22  # LD (HL+), A
INC_HL      = 0x23  # INC HL
LD_H_IMM    = 0x26  # LD H, imm
DAA          = 0x27  # DAA
JR_NZ_IMM   = 0x28  # JR NZ, imm
LD_A_HL     = 0x2A  # LD A, (HL+)
DEC_HL      = 0x2B  # DEC HL
LD_L_IMM    = 0x2E  # LD L, imm
CPL          = 0x2F  # CPL
JR_C_IMM    = 0x30  # JR C, imm
LD_SP_NN    = 0x31  # LD SP, nn
LD_HL_A_INC = 0x32  # LD (HL+), A
INC_SP      = 0x33  # INC SP
LD_A_IMM    = 0x36  # LD A, imm
SCF          = 0x37  # SCF
JR_NC_IMM   = 0x38  # JR NC, imm
LD_A_HL_INC = 0x3A  # LD A, (HL+)
DEC_SP      = 0x3B  # DEC SP
LD_A_IMM    = 0x3E  # LD A, imm
CCF          = 0x3F  # CCF
LD_B_B      = 0x40  # LD B, B
LD_B_C      = 0x41  # LD B, C
LD_B_D      = 0x42  # LD B, D
LD_B_E      = 0x43  # LD B, E
LD_B_H      = 0x44  # LD B, H
LD_B_L      = 0x45  # LD B, L
LD_B_HL     = 0x46  # LD B, (HL)
LD_B_A      = 0x47  # LD B, A
LD_C_B      = 0x48  # LD C, B
LD_C_C      = 0x49  # LD C, C
LD_C_D      = 0x4A  # LD C, D
LD_C_E      = 0x4B  # LD C, E
LD_C_H      = 0x4C  # LD C, H
LD_C_L      = 0x4D  # LD C, L
LD_C_HL     = 0x4E  # LD C, (HL)
LD_C_A      = 0x4F  # LD C, A
LD_D_B      = 0x50  # LD D, B
LD_D_C      = 0x51  # LD D, C
LD_D_D      = 0x52  # LD D, D
LD_D_E      = 0x53  # LD D, E
LD_D_H      = 0x54  # LD D, H
LD_D_L      = 0x55  # LD D, L
LD_D_HL     = 0x56  # LD D, (HL)
LD_D_A      = 0x57  # LD D, A
LD_E_B      = 0x58  # LD E, B
LD_E_C      = 0x59  # LD E, C
LD_E_D      = 0x5A  # LD E, D
LD_E_E      = 0x5B  # LD E, E
LD_E_H      = 0x5C  # LD E, H
LD_E_L      = 0x5D  # LD E, L
LD_E_HL     = 0x5E  # LD E, (HL)
LD_E_A      = 0x5F  # LD E, A
LD_H_B      = 0x60  # LD H, B
LD_H_C      = 0x61  # LD H, C
LD_H_D      = 0x62  # LD H, D
LD_H_E      = 0x63  # LD H, E
LD_H_H      = 0x64  # LD H, H
LD_H_L      = 0x65  # LD H, L
LD_H_HL     = 0x66  # LD H, (HL)
LD_H_A      = 0x67  # LD H, A
LD_L_B      = 0x68  # LD L, B
LD_L_C      = 0x69  # LD L, C
LD_L_D      = 0x6A  # LD L, D
LD_L_E      = 0x6B  # LD L, E
LD_L_H      = 0x6C  # LD L, H
LD_L_L      = 0x6D  # LD L, L
LD_L_HL     = 0x6E  # LD L, (HL)
LD_L_A      = 0x6F  # LD L, A
LD_HL_B    = 0x70  # LD (HL), B
LD_HL_C    = 0x71  # LD (HL), C
LD_HL_D    = 0x72  # LD (HL), D
LD_HL_E    = 0x73  # LD (HL), E
LD_HL_H    = 0x74  # LD (HL), H
LD_HL_L    = 0x75  # LD (HL), L
LD_HL_A    = 0x76  # LD (HL), A
LD_A_B      = 0x77  # LD A, B
LD_A_C      = 0x78  # LD A, C
LD_A_D      = 0x79  # LD A, D
LD_A_E      = 0x7A  # LD A, E
LD_A_H      = 0x7B  # LD A, H
LD_A_L      = 0x7C  # LD A, L
LD_A_HL     = 0x7D  # LD A, (HL)
LD_A_A      = 0x7E  # LD A, A
"""
INC_B       = 0x04  # INC B
DEC_B       = 0x05  # DEC B
ADD_HL_BC   = 0x09  # ADD HL, BC
INC_C       = 0x0C  # INC C
DEC_C       = 0x0D  # DEC C
INC_D       = 0x14  # INC D
DEC_D       = 0x15  # DEC D
ADD_HL_DE   = 0x19  # ADD HL, DE
INC_E       = 0x1C  # INC E
DEC_E       = 0x1D  # DEC E
INC_H       = 0x24  # INC H
DEC_H       = 0x25  # DEC H
ADD_HL_HL   = 0x29  # ADD HL, HL
INC_L       = 0x2C  # INC L
DEC_L       = 0x2D  # DEC L
INC__HL_    = 0x34  # INC (HL)
DEC__HL_    = 0x35  # DEC (HL)
ADD_HL_SP   = 0x39  # ADD HL, SP
INC_A       = 0x3C  # INC A
DEC_A       = 0x3D  # DEC A
ADD_A_B     = 0x80  # ADD A, B
ADD_A_C     = 0x81  # ADD A, C
ADD_A_D     = 0x82  # ADD A, D
ADD_A_E     = 0x83  # ADD A, E
ADD_A_H     = 0x84  # ADD A, H
ADD_A_L     = 0x85  # ADD A, L
ADD_A__HL_  = 0x86  # ADD A, (HL)
ADD_A_A     = 0x87  # ADD A, A
ADC_A_B     = 0x88  # ADC A, B
ADC_A_C     = 0x89  # ADC A, C
ADC_A_D     = 0x8A  # ADC A, D
ADC_A_E     = 0x8B  # ADC A, E
ADC_A_H     = 0x8C  # ADC A, H
ADC_A_L     = 0x8D  # ADC A, L
ADC_A__HL_  = 0x8E  # ADC A, (HL)
ADC_A_A     = 0x8F  # ADC A, A
SUB_A_B     = 0x90  # SUB A, B
SUB_A_C     = 0x91  # SUB A, C
SUB_A_D     = 0x92  # SUB A, D
SUB_A_E     = 0x93  # SUB A, E
SUB_A_H     = 0x94  # SUB A, H
SUB_A_L     = 0x95  # SUB A, L
SUB_A__HL_  = 0x96  # SUB A, (HL)
SUB_A_A     = 0x97  # SUB A, A
SBC_A_B     = 0x98  # SBC A, B
SBC_A_C     = 0x99  # SBC A, C
SBC_A_D     = 0x9A  # SBC A, D
SBC_A_E     = 0x9B  # SBC A, E
SBC_A_H     = 0x9C  # SBC A, H
SBC_A_L     = 0x9D  # SBC A, L
SBC_A__HL_  = 0x9E  # SBC A, (HL)
SBC_A_A     = 0x9F  # SBC A, A
AND_A_B     = 0xA0  # AND A, B
AND_A_C     = 0xA1  # AND A, C
AND_A_D     = 0xA2  # AND A, D
AND_A_E     = 0xA3  # AND A, E
AND_A_H     = 0xA4  # AND A, H
AND_A_L     = 0xA5  # AND A, L
AND_A__HL_  = 0xA6  # AND A, (HL)
AND_A_A     = 0xA7  # AND A, A
XOR_A_B     = 0xA8  # XOR A, B
XOR_A_C     = 0xA9  # XOR A, C
XOR_A_D     = 0xAA  # XOR A, D
XOR_A_E     = 0xAB  # XOR A, E
XOR_A_H     = 0xAC  # XOR A, H
XOR_A_L     = 0xAD  # XOR A, L
XOR_A__HL_  = 0xAE  # XOR A, (HL)
XOR_A_A     = 0xAF  # XOR A, A
OR_A_B      = 0xB0  # OR A, B
OR_A_C      = 0xB1  # OR A, C
OR_A_D      = 0xB2  # OR A, D
OR_A_E      = 0xB3  # OR A, E
OR_A_H      = 0xB4  # OR A, H
OR_A_L      = 0xB5  # OR A, L
OR_A__HL_   = 0xB6  # OR A, (HL)
OR_A_A      = 0xB7  # OR A, A
CP_A_B      = 0xB8  # CP A, B
CP_A_C      = 0xB9  # CP A, C
CP_A_D      = 0xBA  # CP A, D
CP_A_E      = 0xBB  # CP A, E
CP_A_H      = 0xBC  # CP A, H
CP_A_L      = 0xBD  # CP A, L
CP_A__HL_   = 0xBE  # CP A, (HL)
CP_A_A      = 0xBF  # CP A, A
ADD_A_IMM   = 0xC6  # ADD A, imm
ADC_A_IMM   = 0xCE  # ADC A, imm
SUB_A_IMM   = 0xD6  # SUB A, imm
SBC_A_IMM   = 0xDE  # SUB A, imm
AND_A_IMM   = 0xE6  # AND A, imm
XOR_A_IMM   = 0xEE  # XOR A, imm
OR_A_IMM    = 0xF6  # OR A, imm
CP_A_IMM    = 0xFE  # CP A, imm
"""
RET_NZ      = 0xC0  # RET NZ
POP_BC      = 0xC1  # POP BC
JP_NZ       = 0xC2  # JP NZ, nn
JP_ADDR     = 0xC3  # JP nn
CALL_NZ     = 0xC4  # CALL NZ, nn
PUSH_BC     = 0xC5  # PUSH BC
ADD_A_IMM   = 0xC6  # ADD A, imm
RST_00      = 0xC7  # RST 00H
RET_Z       = 0xC8  # RET Z
RET         = 0xC9  # RET
JP_Z        = 0xCA  # JP Z, nn
PREFIX_CB   = 0xCB  # CB Prefix
CALL_Z      = 0xCC  # CALL Z, nn
CALL        = 0xCD  # CALL nn
ADC_A_IMM   = 0xCE  # ADC A, imm
RST_08      = 0xCF  # RST 08H
RET_C       = 0xD0  # RET C
POP_DE      = 0xD1  # POP DE
JP_C        = 0xD2  # JP C, nn
OUT_C       = 0xD3  # OUT (C), A
CALL_C      = 0xD4  # CALL C, nn
PUSH_DE     = 0xD5  # PUSH DE
SUB_IMM     = 0xD6  # SUB A, imm
RST_10      = 0xD7  # RST 10H
RET_NC      = 0xD8  # RET NC
RETI        = 0xD9  # RETI
JP_NC       = 0xDA  # JP NC, nn
IN_C        = 0xDB  # IN A, (C)
CALL_NC     = 0xDC  # CALL NC, nn
ADC_IMM     = 0xDD  # ADC A, imm
RST_18      = 0xDF  # RST 18H
LDH_NN_A    = 0xE0  # LDH (nn), A
POP_HL      = 0xE1  # POP HL
LD_HL_NN    = 0xE2  # LD (C), A
LDH_A_NN    = 0xE3  # LD A, (nn)
CALL_HL     = 0xE4  # CALL nn
PUSH_HL     = 0xE5  # PUSH HL
AND_IMM     = 0xE6  # AND imm
RST_20      = 0xE7  # RST 20H
ADD_SP_IMM  = 0xE8  # ADD SP, imm
JP_HL       = 0xE9  # JP (HL)
LD_A_HL     = 0xEA  # LD A, (HL)
LD_A_NN     = 0xEB  # LD A, (nn)
CALL_HL     = 0xEC  # CALL nn
ADC_SP_IMM  = 0xED  # ADC SP, imm
RST_28      = 0xEF  # RST 28H
LDH_A_NN    = 0xF0  # LDH A, (nn)
POP_AF      = 0xF1  # POP AF
LD_A_HL     = 0xF2  # LD A, (HL)
DI          = 0xF3  # DI
CALL_NN     = 0xF4  # CALL nn
PUSH_AF     = 0xF5  # PUSH AF
OR_IMM      = 0xF6  # OR imm
RST_30      = 0xF7  # RST 30H
LD_SP_HL    = 0xF8  # LD SP, HL
LD_A_C      = 0xF9  # LD A, (C)
LD_A_NN     = 0xFA  # LD A, (nn)
EI          = 0xFB  # EI
CALL_NN     = 0xFC  # CALL nn
CP_IMM      = 0xFD  # CP imm
RST_38      = 0xFF  # RST 38H
"""