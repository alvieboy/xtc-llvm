//===-- NewcpuDisassembler.cpp - Disassembler for MicroBlaze  -------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file is part of the Newcpu Disassembler. It contains code to translate
// the data produced by the decoder into MCInsts.
//
//===----------------------------------------------------------------------===//

#include "NewcpuDisassembler.h"
#include "Newcpu.h"
#include "llvm/MC/MCDisassembler.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrDesc.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/MemoryObject.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/raw_ostream.h"

// #include "NewcpuGenDecoderTables.inc"
// #include "NewcpuGenRegisterNames.inc"

namespace llvm {
extern const MCInstrDesc NewcpuInsts[];
}

using namespace llvm;

const uint16_t UNSUPPORTED = -1;
#if 0
static const uint16_t mblazeBinary2Opcode[] = {
    Newcpu::ADD,   Newcpu::RSUB,   Newcpu::ADDC,   Newcpu::RSUBC,   //00,01,02,03
  Newcpu::ADDK,  Newcpu::RSUBK,  Newcpu::ADDKC,  Newcpu::RSUBKC,  //04,05,06,07
  Newcpu::ADDI,  Newcpu::RSUBI,  Newcpu::ADDIC,  Newcpu::RSUBIC,  //08,09,0A,0B
  Newcpu::ADDIK, Newcpu::RSUBIK, Newcpu::ADDIKC, Newcpu::RSUBIKC, //0C,0D,0E,0F

  Newcpu::MUL,   Newcpu::BSRL,   Newcpu::IDIV,   Newcpu::GETD,    //10,11,12,13
  UNSUPPORTED,   UNSUPPORTED,    Newcpu::FADD,   UNSUPPORTED,     //14,15,16,17
  Newcpu::MULI,  Newcpu::BSRLI,  UNSUPPORTED,    Newcpu::GET,     //18,19,1A,1B
  UNSUPPORTED,   UNSUPPORTED,    UNSUPPORTED,    UNSUPPORTED,     //1C,1D,1E,1F

  Newcpu::OR,    Newcpu::AND,    Newcpu::XOR,    Newcpu::ANDN,    //20,21,22,23
  Newcpu::SEXT8, Newcpu::MFS,    Newcpu::BR,     Newcpu::BEQ,     //24,25,26,27
  Newcpu::ORI,   Newcpu::ANDI,   Newcpu::XORI,   Newcpu::ANDNI,   //28,29,2A,2B
  Newcpu::IMM,   Newcpu::RTSD,   Newcpu::BRI,    Newcpu::BEQI,    //2C,2D,2E,2F

  Newcpu::LBU,   Newcpu::LHU,    Newcpu::LW,     UNSUPPORTED,     //30,31,32,33
  Newcpu::SB,    Newcpu::SH,     Newcpu::SW,     UNSUPPORTED,     //34,35,36,37
  Newcpu::LBUI,  Newcpu::LHUI,   Newcpu::LWI,    UNSUPPORTED,     //38,39,3A,3B
  Newcpu::SBI,   Newcpu::SHI,    Newcpu::SWI,    UNSUPPORTED,     //3C,3D,3E,3F
};
#endif
#if 0
static unsigned getRD(uint32_t insn) {
  if (!isNewcpuRegister((insn>>21)&0x1F))
    return UNSUPPORTED;
  return getNewcpuRegisterFromNumbering((insn>>21)&0x1F);
}

static unsigned getRA(uint32_t insn) {
  if (!getNewcpuRegisterFromNumbering((insn>>16)&0x1F))
    return UNSUPPORTED;
  return getNewcpuRegisterFromNumbering((insn>>16)&0x1F);
}

static unsigned getRB(uint32_t insn) {
  if (!getNewcpuRegisterFromNumbering((insn>>11)&0x1F))
    return UNSUPPORTED;
  return getNewcpuRegisterFromNumbering((insn>>11)&0x1F);
}

static int64_t getRS(uint32_t insn) {
  if (!isSpecialNewcpuRegister(insn&0x3FFF))
    return UNSUPPORTED;
  return getSpecialNewcpuRegisterFromNumbering(insn&0x3FFF);
}

static int64_t getIMM(uint32_t insn) {
    int16_t val = (insn & 0xFFFF);
    return val;
}

static int64_t getSHT(uint32_t insn) {
    int16_t val = (insn & 0x1F);
    return val;
}

static unsigned getFLAGS(int32_t insn) {
    return (insn & 0x7FF);
}

static int64_t getFSL(uint32_t insn) {
    int16_t val = (insn & 0xF);
    return val;
}

static unsigned decodeMUL(uint32_t insn) {
    switch (getFLAGS(insn)) {
    default: return UNSUPPORTED;
    case 0:  return Newcpu::MUL;
    case 1:  return Newcpu::MULH;
    case 2:  return Newcpu::MULHSU;
    case 3:  return Newcpu::MULHU;
    }
}

static unsigned decodeSEXT(uint32_t insn) {
    switch (insn&0x7FF) {
    default:   return UNSUPPORTED;
    case 0x60: return Newcpu::SEXT8;
    case 0x68: return Newcpu::WIC;
    case 0x64: return Newcpu::WDC;
    case 0x66: return Newcpu::WDCC;
    case 0x74: return Newcpu::WDCF;
    case 0x61: return Newcpu::SEXT16;
    case 0x41: return Newcpu::SRL;
    case 0x21: return Newcpu::SRC;
    case 0x01: return Newcpu::SRA;
    case 0xE0: return Newcpu::CLZ;
    }
}

static unsigned decodeBEQ(uint32_t insn) {
    switch ((insn>>21)&0x1F) {
    default:    return UNSUPPORTED;
    case 0x00:  return Newcpu::BEQ;
    case 0x10:  return Newcpu::BEQD;
    case 0x05:  return Newcpu::BGE;
    case 0x15:  return Newcpu::BGED;
    case 0x04:  return Newcpu::BGT;
    case 0x14:  return Newcpu::BGTD;
    case 0x03:  return Newcpu::BLE;
    case 0x13:  return Newcpu::BLED;
    case 0x02:  return Newcpu::BLT;
    case 0x12:  return Newcpu::BLTD;
    case 0x01:  return Newcpu::BNE;
    case 0x11:  return Newcpu::BNED;
    }
}

static unsigned decodeBEQI(uint32_t insn) {
    switch ((insn>>21)&0x1F) {
    default:    return UNSUPPORTED;
    case 0x00:  return Newcpu::BEQI;
    case 0x10:  return Newcpu::BEQID;
    case 0x05:  return Newcpu::BGEI;
    case 0x15:  return Newcpu::BGEID;
    case 0x04:  return Newcpu::BGTI;
    case 0x14:  return Newcpu::BGTID;
    case 0x03:  return Newcpu::BLEI;
    case 0x13:  return Newcpu::BLEID;
    case 0x02:  return Newcpu::BLTI;
    case 0x12:  return Newcpu::BLTID;
    case 0x01:  return Newcpu::BNEI;
    case 0x11:  return Newcpu::BNEID;
    }
}

static unsigned decodeBR(uint32_t insn) {
    switch ((insn>>16)&0x1F) {
    default:   return UNSUPPORTED;
    case 0x00: return Newcpu::BR;
    case 0x08: return Newcpu::BRA;
    case 0x0C: return Newcpu::BRK;
    case 0x10: return Newcpu::BRD;
    case 0x14: return Newcpu::BRLD;
    case 0x18: return Newcpu::BRAD;
    case 0x1C: return Newcpu::BRALD;
    }
}

static unsigned decodeBRI(uint32_t insn) {
    switch (insn&0x3FFFFFF) {
    default:        break;
    case 0x0020004: return Newcpu::IDMEMBAR;
    case 0x0220004: return Newcpu::DMEMBAR;
    case 0x0420004: return Newcpu::IMEMBAR;
    }

    switch ((insn>>16)&0x1F) {
    default:   return UNSUPPORTED;
    case 0x00: return Newcpu::BRI;
    case 0x08: return Newcpu::BRAI;
    case 0x0C: return Newcpu::BRKI;
    case 0x10: return Newcpu::BRID;
    case 0x14: return Newcpu::BRLID;
    case 0x18: return Newcpu::BRAID;
    case 0x1C: return Newcpu::BRALID;
    }
}

static unsigned decodeBSRL(uint32_t insn) {
    switch ((insn>>9)&0x3) {
    default:  return UNSUPPORTED;
    case 0x2: return Newcpu::BSLL;
    case 0x1: return Newcpu::BSRA;
    case 0x0: return Newcpu::BSRL;
    }
}

static unsigned decodeBSRLI(uint32_t insn) {
    switch ((insn>>9)&0x3) {
    default:  return UNSUPPORTED;
    case 0x2: return Newcpu::BSLLI;
    case 0x1: return Newcpu::BSRAI;
    case 0x0: return Newcpu::BSRLI;
    }
}

static unsigned decodeRSUBK(uint32_t insn) {
    switch (getFLAGS(insn)) {
    default:  return UNSUPPORTED;
    case 0x0: return Newcpu::RSUBK;
    case 0x1: return Newcpu::CMP;
    case 0x3: return Newcpu::CMPU;
    }
}

static unsigned decodeFADD(uint32_t insn) {
    switch (getFLAGS(insn)) {
    default:    return UNSUPPORTED;
    case 0x000: return Newcpu::FADD;
    case 0x080: return Newcpu::FRSUB;
    case 0x100: return Newcpu::FMUL;
    case 0x180: return Newcpu::FDIV;
    case 0x200: return Newcpu::FCMP_UN;
    case 0x210: return Newcpu::FCMP_LT;
    case 0x220: return Newcpu::FCMP_EQ;
    case 0x230: return Newcpu::FCMP_LE;
    case 0x240: return Newcpu::FCMP_GT;
    case 0x250: return Newcpu::FCMP_NE;
    case 0x260: return Newcpu::FCMP_GE;
    case 0x280: return Newcpu::FLT;
    case 0x300: return Newcpu::FINT;
    case 0x380: return Newcpu::FSQRT;
    }
}

static unsigned decodeGET(uint32_t insn) {
    switch ((insn>>10)&0x3F) {
    default:   return UNSUPPORTED;
    case 0x00: return Newcpu::GET;
    case 0x01: return Newcpu::EGET;
    case 0x02: return Newcpu::AGET;
    case 0x03: return Newcpu::EAGET;
    case 0x04: return Newcpu::TGET;
    case 0x05: return Newcpu::TEGET;
    case 0x06: return Newcpu::TAGET;
    case 0x07: return Newcpu::TEAGET;
    case 0x08: return Newcpu::CGET;
    case 0x09: return Newcpu::ECGET;
    case 0x0A: return Newcpu::CAGET;
    case 0x0B: return Newcpu::ECAGET;
    case 0x0C: return Newcpu::TCGET;
    case 0x0D: return Newcpu::TECGET;
    case 0x0E: return Newcpu::TCAGET;
    case 0x0F: return Newcpu::TECAGET;
    case 0x10: return Newcpu::NGET;
    case 0x11: return Newcpu::NEGET;
    case 0x12: return Newcpu::NAGET;
    case 0x13: return Newcpu::NEAGET;
    case 0x14: return Newcpu::TNGET;
    case 0x15: return Newcpu::TNEGET;
    case 0x16: return Newcpu::TNAGET;
    case 0x17: return Newcpu::TNEAGET;
    case 0x18: return Newcpu::NCGET;
    case 0x19: return Newcpu::NECGET;
    case 0x1A: return Newcpu::NCAGET;
    case 0x1B: return Newcpu::NECAGET;
    case 0x1C: return Newcpu::TNCGET;
    case 0x1D: return Newcpu::TNECGET;
    case 0x1E: return Newcpu::TNCAGET;
    case 0x1F: return Newcpu::TNECAGET;
    case 0x20: return Newcpu::PUT;
    case 0x22: return Newcpu::APUT;
    case 0x24: return Newcpu::TPUT;
    case 0x26: return Newcpu::TAPUT;
    case 0x28: return Newcpu::CPUT;
    case 0x2A: return Newcpu::CAPUT;
    case 0x2C: return Newcpu::TCPUT;
    case 0x2E: return Newcpu::TCAPUT;
    case 0x30: return Newcpu::NPUT;
    case 0x32: return Newcpu::NAPUT;
    case 0x34: return Newcpu::TNPUT;
    case 0x36: return Newcpu::TNAPUT;
    case 0x38: return Newcpu::NCPUT;
    case 0x3A: return Newcpu::NCAPUT;
    case 0x3C: return Newcpu::TNCPUT;
    case 0x3E: return Newcpu::TNCAPUT;
    }
}

static unsigned decodeGETD(uint32_t insn) {
    switch ((insn>>5)&0x3F) {
    default:   return UNSUPPORTED;
    case 0x00: return Newcpu::GETD;
    case 0x01: return Newcpu::EGETD;
    case 0x02: return Newcpu::AGETD;
    case 0x03: return Newcpu::EAGETD;
    case 0x04: return Newcpu::TGETD;
    case 0x05: return Newcpu::TEGETD;
    case 0x06: return Newcpu::TAGETD;
    case 0x07: return Newcpu::TEAGETD;
    case 0x08: return Newcpu::CGETD;
    case 0x09: return Newcpu::ECGETD;
    case 0x0A: return Newcpu::CAGETD;
    case 0x0B: return Newcpu::ECAGETD;
    case 0x0C: return Newcpu::TCGETD;
    case 0x0D: return Newcpu::TECGETD;
    case 0x0E: return Newcpu::TCAGETD;
    case 0x0F: return Newcpu::TECAGETD;
    case 0x10: return Newcpu::NGETD;
    case 0x11: return Newcpu::NEGETD;
    case 0x12: return Newcpu::NAGETD;
    case 0x13: return Newcpu::NEAGETD;
    case 0x14: return Newcpu::TNGETD;
    case 0x15: return Newcpu::TNEGETD;
    case 0x16: return Newcpu::TNAGETD;
    case 0x17: return Newcpu::TNEAGETD;
    case 0x18: return Newcpu::NCGETD;
    case 0x19: return Newcpu::NECGETD;
    case 0x1A: return Newcpu::NCAGETD;
    case 0x1B: return Newcpu::NECAGETD;
    case 0x1C: return Newcpu::TNCGETD;
    case 0x1D: return Newcpu::TNECGETD;
    case 0x1E: return Newcpu::TNCAGETD;
    case 0x1F: return Newcpu::TNECAGETD;
    case 0x20: return Newcpu::PUTD;
    case 0x22: return Newcpu::APUTD;
    case 0x24: return Newcpu::TPUTD;
    case 0x26: return Newcpu::TAPUTD;
    case 0x28: return Newcpu::CPUTD;
    case 0x2A: return Newcpu::CAPUTD;
    case 0x2C: return Newcpu::TCPUTD;
    case 0x2E: return Newcpu::TCAPUTD;
    case 0x30: return Newcpu::NPUTD;
    case 0x32: return Newcpu::NAPUTD;
    case 0x34: return Newcpu::TNPUTD;
    case 0x36: return Newcpu::TNAPUTD;
    case 0x38: return Newcpu::NCPUTD;
    case 0x3A: return Newcpu::NCAPUTD;
    case 0x3C: return Newcpu::TNCPUTD;
    case 0x3E: return Newcpu::TNCAPUTD;
    }
}

static unsigned decodeIDIV(uint32_t insn) {
    switch (insn&0x3) {
    default:  return UNSUPPORTED;
    case 0x0: return Newcpu::IDIV;
    case 0x2: return Newcpu::IDIVU;
    }
}

static unsigned decodeLBU(uint32_t insn) {
    switch ((insn>>9)&0x1) {
    default:  return UNSUPPORTED;
    case 0x0: return Newcpu::LBU;
    case 0x1: return Newcpu::LBUR;
    }
}

static unsigned decodeLHU(uint32_t insn) {
    switch ((insn>>9)&0x1) {
    default:  return UNSUPPORTED;
    case 0x0: return Newcpu::LHU;
    case 0x1: return Newcpu::LHUR;
    }
}

static unsigned decodeLW(uint32_t insn) {
    switch ((insn>>9)&0x3) {
    default:  return UNSUPPORTED;
    case 0x0: return Newcpu::LW;
    case 0x1: return Newcpu::LWR;
    case 0x2: return Newcpu::LWX;
    }
}

static unsigned decodeSB(uint32_t insn) {
    switch ((insn>>9)&0x1) {
    default:  return UNSUPPORTED;
    case 0x0: return Newcpu::SB;
    case 0x1: return Newcpu::SBR;
    }
}

static unsigned decodeSH(uint32_t insn) {
    switch ((insn>>9)&0x1) {
    default:  return UNSUPPORTED;
    case 0x0: return Newcpu::SH;
    case 0x1: return Newcpu::SHR;
    }
}

static unsigned decodeSW(uint32_t insn) {
    switch ((insn>>9)&0x3) {
    default:  return UNSUPPORTED;
    case 0x0: return Newcpu::SW;
    case 0x1: return Newcpu::SWR;
    case 0x2: return Newcpu::SWX;
    }
}

static unsigned decodeMFS(uint32_t insn) {
    switch ((insn>>15)&0x1) {
    default:   return UNSUPPORTED;
    case 0x0:
      switch ((insn>>16)&0x1) {
      default:   return UNSUPPORTED;
      case 0x0: return Newcpu::MSRSET;
      case 0x1: return Newcpu::MSRCLR;
      }
    case 0x1:
      switch ((insn>>14)&0x1) {
      default:   return UNSUPPORTED;
      case 0x0: return Newcpu::MFS;
      case 0x1: return Newcpu::MTS;
      }
    }
}

static unsigned decodeOR(uint32_t insn) {
    switch (getFLAGS(insn)) {
    default:    return UNSUPPORTED;
    case 0x000: return Newcpu::OR;
    case 0x400: return Newcpu::PCMPBF;
    }
}

static unsigned decodeXOR(uint32_t insn) {
    switch (getFLAGS(insn)) {
    default:    return UNSUPPORTED;
    case 0x000: return Newcpu::XOR;
    case 0x400: return Newcpu::PCMPEQ;
    }
}

static unsigned decodeANDN(uint32_t insn) {
    switch (getFLAGS(insn)) {
    default:    return UNSUPPORTED;
    case 0x000: return Newcpu::ANDN;
    case 0x400: return Newcpu::PCMPNE;
    }
}

static unsigned decodeRTSD(uint32_t insn) {
    switch ((insn>>21)&0x1F) {
    default:   return UNSUPPORTED;
    case 0x10: return Newcpu::RTSD;
    case 0x11: return Newcpu::RTID;
    case 0x12: return Newcpu::RTBD;
    case 0x14: return Newcpu::RTED;
    }
}
#endif
static unsigned getOPCODE(uint32_t insn) {
#if 0
  unsigned opcode = mblazeBinary2Opcode[ (insn>>26)&0x3F ];
  switch (opcode) {
  case Newcpu::MUL:     return decodeMUL(insn);
  case Newcpu::SEXT8:   return decodeSEXT(insn);
  case Newcpu::BEQ:     return decodeBEQ(insn);
  case Newcpu::BEQI:    return decodeBEQI(insn);
  case Newcpu::BR:      return decodeBR(insn);
  case Newcpu::BRI:     return decodeBRI(insn);
  case Newcpu::BSRL:    return decodeBSRL(insn);
  case Newcpu::BSRLI:   return decodeBSRLI(insn);
  case Newcpu::RSUBK:   return decodeRSUBK(insn);
  case Newcpu::FADD:    return decodeFADD(insn);
  case Newcpu::GET:     return decodeGET(insn);
  case Newcpu::GETD:    return decodeGETD(insn);
  case Newcpu::IDIV:    return decodeIDIV(insn);
  case Newcpu::LBU:     return decodeLBU(insn);
  case Newcpu::LHU:     return decodeLHU(insn);
  case Newcpu::LW:      return decodeLW(insn);
  case Newcpu::SB:      return decodeSB(insn);
  case Newcpu::SH:      return decodeSH(insn);
  case Newcpu::SW:      return decodeSW(insn);
  case Newcpu::MFS:     return decodeMFS(insn);
  case Newcpu::OR:      return decodeOR(insn);
  case Newcpu::XOR:     return decodeXOR(insn);
  case Newcpu::ANDN:    return decodeANDN(insn);
  case Newcpu::RTSD:    return decodeRTSD(insn);
  default:              return opcode;
  }
#endif
}

//
// Public interface for the disassembler
//

MCDisassembler::DecodeStatus NewcpuDisassembler::getInstruction(MCInst &instr,
                                        uint64_t &size,
                                        const MemoryObject &region,
                                        uint64_t address,
                                        raw_ostream &vStream,
                                        raw_ostream &cStream) const {
#if 0
    // The machine instruction.
  uint32_t insn;
  uint64_t read;
  uint8_t bytes[4];

  // By default we consume 1 byte on failure
  size = 1;

  // We want to read exactly 4 bytes of data.
  if (region.readBytes(address, 4, (uint8_t*)bytes, &read) == -1 || read < 4)
    return Fail;

  // Encoded as a big-endian 32-bit word in the stream.
  insn = (bytes[0]<<24) | (bytes[1]<<16) | (bytes[2]<< 8) | (bytes[3]<<0);

  // Get the MCInst opcode from the binary instruction and make sure
  // that it is a valid instruction.
  unsigned opcode = getOPCODE(insn);
  if (opcode == UNSUPPORTED)
    return Fail;

  instr.setOpcode(opcode);

  unsigned RD = getRD(insn);
  unsigned RA = getRA(insn);
  unsigned RB = getRB(insn);
  unsigned RS = getRS(insn);

  uint64_t tsFlags = NewcpuInsts[opcode].TSFlags;
  switch ((tsFlags & NewcpuII::FormMask)) {
  default: 
    return Fail;

  case NewcpuII::FC:
    break;

  case NewcpuII::FRRRR:
    if (RD == UNSUPPORTED || RA == UNSUPPORTED || RB == UNSUPPORTED)
      return Fail;
    instr.addOperand(MCOperand::CreateReg(RD));
    instr.addOperand(MCOperand::CreateReg(RB));
    instr.addOperand(MCOperand::CreateReg(RA));
    break;

  case NewcpuII::FRRR:
    if (RD == UNSUPPORTED || RA == UNSUPPORTED || RB == UNSUPPORTED)
      return Fail;
    instr.addOperand(MCOperand::CreateReg(RD));
    instr.addOperand(MCOperand::CreateReg(RA));
    instr.addOperand(MCOperand::CreateReg(RB));
    break;

  case NewcpuII::FRR:
    if (RD == UNSUPPORTED || RA == UNSUPPORTED)
      return Fail;
    instr.addOperand(MCOperand::CreateReg(RD));
    instr.addOperand(MCOperand::CreateReg(RA));
    break;

  case NewcpuII::FRI:
    switch (opcode) {
    default: 
      return Fail;
    case Newcpu::MFS:
      if (RD == UNSUPPORTED)
        return Fail;
      instr.addOperand(MCOperand::CreateReg(RD));
      instr.addOperand(MCOperand::CreateImm(insn&0x3FFF));
      break;
    case Newcpu::MTS:
      if (RA == UNSUPPORTED)
        return Fail;
      instr.addOperand(MCOperand::CreateImm(insn&0x3FFF));
      instr.addOperand(MCOperand::CreateReg(RA));
      break;
    case Newcpu::MSRSET:
    case Newcpu::MSRCLR:
      if (RD == UNSUPPORTED)
        return Fail;
      instr.addOperand(MCOperand::CreateReg(RD));
      instr.addOperand(MCOperand::CreateImm(insn&0x7FFF));
      break;
    }
    break;

  case NewcpuII::FRRI:
    if (RD == UNSUPPORTED || RA == UNSUPPORTED)
      return Fail;
    instr.addOperand(MCOperand::CreateReg(RD));
    instr.addOperand(MCOperand::CreateReg(RA));
    switch (opcode) {
    default:
      instr.addOperand(MCOperand::CreateImm(getIMM(insn)));
      break;
    case Newcpu::BSRLI:
    case Newcpu::BSRAI:
    case Newcpu::BSLLI:
      instr.addOperand(MCOperand::CreateImm(insn&0x1F));
      break;
    }
    break;

  case NewcpuII::FCRR:
    if (RA == UNSUPPORTED || RB == UNSUPPORTED)
      return Fail;
    instr.addOperand(MCOperand::CreateReg(RA));
    instr.addOperand(MCOperand::CreateReg(RB));
    break;

  case NewcpuII::FCRI:
    if (RA == UNSUPPORTED)
      return Fail;
    instr.addOperand(MCOperand::CreateReg(RA));
    instr.addOperand(MCOperand::CreateImm(getIMM(insn)));
    break;

  case NewcpuII::FRCR:
    if (RD == UNSUPPORTED || RB == UNSUPPORTED)
      return Fail;
    instr.addOperand(MCOperand::CreateReg(RD));
    instr.addOperand(MCOperand::CreateReg(RB));
    break;

  case NewcpuII::FRCI:
    if (RD == UNSUPPORTED)
      return Fail;
    instr.addOperand(MCOperand::CreateReg(RD));
    instr.addOperand(MCOperand::CreateImm(getIMM(insn)));
    break;

  case NewcpuII::FCCR:
    if (RB == UNSUPPORTED)
      return Fail;
    instr.addOperand(MCOperand::CreateReg(RB));
    break;

  case NewcpuII::FCCI:
    instr.addOperand(MCOperand::CreateImm(getIMM(insn)));
    break;

  case NewcpuII::FRRCI:
    if (RD == UNSUPPORTED || RA == UNSUPPORTED)
      return Fail;
    instr.addOperand(MCOperand::CreateReg(RD));
    instr.addOperand(MCOperand::CreateReg(RA));
    instr.addOperand(MCOperand::CreateImm(getSHT(insn)));
    break;

  case NewcpuII::FRRC:
    if (RD == UNSUPPORTED || RA == UNSUPPORTED)
      return Fail;
    instr.addOperand(MCOperand::CreateReg(RD));
    instr.addOperand(MCOperand::CreateReg(RA));
    break;

  case NewcpuII::FRCX:
    if (RD == UNSUPPORTED)
      return Fail;
    instr.addOperand(MCOperand::CreateReg(RD));
    instr.addOperand(MCOperand::CreateImm(getFSL(insn)));
    break;

  case NewcpuII::FRCS:
    if (RD == UNSUPPORTED || RS == UNSUPPORTED)
      return Fail;
    instr.addOperand(MCOperand::CreateReg(RD));
    instr.addOperand(MCOperand::CreateReg(RS));
    break;

  case NewcpuII::FCRCS:
    if (RS == UNSUPPORTED || RA == UNSUPPORTED)
      return Fail;
    instr.addOperand(MCOperand::CreateReg(RS));
    instr.addOperand(MCOperand::CreateReg(RA));
    break;

  case NewcpuII::FCRCX:
    if (RA == UNSUPPORTED)
      return Fail;
    instr.addOperand(MCOperand::CreateReg(RA));
    instr.addOperand(MCOperand::CreateImm(getFSL(insn)));
    break;

  case NewcpuII::FCX:
    instr.addOperand(MCOperand::CreateImm(getFSL(insn)));
    break;

  case NewcpuII::FCR:
    if (RB == UNSUPPORTED)
      return Fail;
    instr.addOperand(MCOperand::CreateReg(RB));
    break;

  case NewcpuII::FRIR:
    if (RD == UNSUPPORTED || RA == UNSUPPORTED)
      return Fail;
    instr.addOperand(MCOperand::CreateReg(RD));
    instr.addOperand(MCOperand::CreateImm(getIMM(insn)));
    instr.addOperand(MCOperand::CreateReg(RA));
    break;
  }

  // We always consume 4 bytes of data on success
  size = 4;

  return Success;
#endif
}

static MCDisassembler *createNewcpuDisassembler(const Target &T,
                                                const MCSubtargetInfo &STI) {
  return new NewcpuDisassembler(STI);
}

extern "C" void LLVMInitializeNewcpuDisassembler() {
  // Register the disassembler.
  TargetRegistry::RegisterMCDisassembler(TheNewcpuTarget,
                                         createNewcpuDisassembler);
}
