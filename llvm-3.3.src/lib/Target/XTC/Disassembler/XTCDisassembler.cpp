//===-- XTCDisassembler.cpp - Disassembler for MicroBlaze  -------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file is part of the XTC Disassembler. It contains code to translate
// the data produced by the decoder into MCInsts.
//
//===----------------------------------------------------------------------===//

#include "XTCDisassembler.h"
#include "XTC.h"
#include "llvm/MC/MCDisassembler.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrDesc.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/MemoryObject.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/raw_ostream.h"

// #include "XTCGenDecoderTables.inc"
// #include "XTCGenRegisterNames.inc"

namespace llvm {
extern const MCInstrDesc XTCInsts[];
}

using namespace llvm;

const uint16_t UNSUPPORTED = -1;
#if 0
static const uint16_t mblazeBinary2Opcode[] = {
    XTC::ADD,   XTC::RSUB,   XTC::ADDC,   XTC::RSUBC,   //00,01,02,03
  XTC::ADDK,  XTC::RSUBK,  XTC::ADDKC,  XTC::RSUBKC,  //04,05,06,07
  XTC::ADDI,  XTC::RSUBI,  XTC::ADDIC,  XTC::RSUBIC,  //08,09,0A,0B
  XTC::ADDIK, XTC::RSUBIK, XTC::ADDIKC, XTC::RSUBIKC, //0C,0D,0E,0F

  XTC::MUL,   XTC::BSRL,   XTC::IDIV,   XTC::GETD,    //10,11,12,13
  UNSUPPORTED,   UNSUPPORTED,    XTC::FADD,   UNSUPPORTED,     //14,15,16,17
  XTC::MULI,  XTC::BSRLI,  UNSUPPORTED,    XTC::GET,     //18,19,1A,1B
  UNSUPPORTED,   UNSUPPORTED,    UNSUPPORTED,    UNSUPPORTED,     //1C,1D,1E,1F

  XTC::OR,    XTC::AND,    XTC::XOR,    XTC::ANDN,    //20,21,22,23
  XTC::SEXT8, XTC::MFS,    XTC::BR,     XTC::BEQ,     //24,25,26,27
  XTC::ORI,   XTC::ANDI,   XTC::XORI,   XTC::ANDNI,   //28,29,2A,2B
  XTC::IMM,   XTC::RTSD,   XTC::BRI,    XTC::BEQI,    //2C,2D,2E,2F

  XTC::LBU,   XTC::LHU,    XTC::LW,     UNSUPPORTED,     //30,31,32,33
  XTC::SB,    XTC::SH,     XTC::SW,     UNSUPPORTED,     //34,35,36,37
  XTC::LBUI,  XTC::LHUI,   XTC::LWI,    UNSUPPORTED,     //38,39,3A,3B
  XTC::SBI,   XTC::SHI,    XTC::SWI,    UNSUPPORTED,     //3C,3D,3E,3F
};
#endif
#if 0
static unsigned getRD(uint32_t insn) {
  if (!isXTCRegister((insn>>21)&0x1F))
    return UNSUPPORTED;
  return getXTCRegisterFromNumbering((insn>>21)&0x1F);
}

static unsigned getRA(uint32_t insn) {
  if (!getXTCRegisterFromNumbering((insn>>16)&0x1F))
    return UNSUPPORTED;
  return getXTCRegisterFromNumbering((insn>>16)&0x1F);
}

static unsigned getRB(uint32_t insn) {
  if (!getXTCRegisterFromNumbering((insn>>11)&0x1F))
    return UNSUPPORTED;
  return getXTCRegisterFromNumbering((insn>>11)&0x1F);
}

static int64_t getRS(uint32_t insn) {
  if (!isSpecialXTCRegister(insn&0x3FFF))
    return UNSUPPORTED;
  return getSpecialXTCRegisterFromNumbering(insn&0x3FFF);
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
    case 0:  return XTC::MUL;
    case 1:  return XTC::MULH;
    case 2:  return XTC::MULHSU;
    case 3:  return XTC::MULHU;
    }
}

static unsigned decodeSEXT(uint32_t insn) {
    switch (insn&0x7FF) {
    default:   return UNSUPPORTED;
    case 0x60: return XTC::SEXT8;
    case 0x68: return XTC::WIC;
    case 0x64: return XTC::WDC;
    case 0x66: return XTC::WDCC;
    case 0x74: return XTC::WDCF;
    case 0x61: return XTC::SEXT16;
    case 0x41: return XTC::SRL;
    case 0x21: return XTC::SRC;
    case 0x01: return XTC::SRA;
    case 0xE0: return XTC::CLZ;
    }
}

static unsigned decodeBEQ(uint32_t insn) {
    switch ((insn>>21)&0x1F) {
    default:    return UNSUPPORTED;
    case 0x00:  return XTC::BEQ;
    case 0x10:  return XTC::BEQD;
    case 0x05:  return XTC::BGE;
    case 0x15:  return XTC::BGED;
    case 0x04:  return XTC::BGT;
    case 0x14:  return XTC::BGTD;
    case 0x03:  return XTC::BLE;
    case 0x13:  return XTC::BLED;
    case 0x02:  return XTC::BLT;
    case 0x12:  return XTC::BLTD;
    case 0x01:  return XTC::BNE;
    case 0x11:  return XTC::BNED;
    }
}

static unsigned decodeBEQI(uint32_t insn) {
    switch ((insn>>21)&0x1F) {
    default:    return UNSUPPORTED;
    case 0x00:  return XTC::BEQI;
    case 0x10:  return XTC::BEQID;
    case 0x05:  return XTC::BGEI;
    case 0x15:  return XTC::BGEID;
    case 0x04:  return XTC::BGTI;
    case 0x14:  return XTC::BGTID;
    case 0x03:  return XTC::BLEI;
    case 0x13:  return XTC::BLEID;
    case 0x02:  return XTC::BLTI;
    case 0x12:  return XTC::BLTID;
    case 0x01:  return XTC::BNEI;
    case 0x11:  return XTC::BNEID;
    }
}

static unsigned decodeBR(uint32_t insn) {
    switch ((insn>>16)&0x1F) {
    default:   return UNSUPPORTED;
    case 0x00: return XTC::BR;
    case 0x08: return XTC::BRA;
    case 0x0C: return XTC::BRK;
    case 0x10: return XTC::BRD;
    case 0x14: return XTC::BRLD;
    case 0x18: return XTC::BRAD;
    case 0x1C: return XTC::BRALD;
    }
}

static unsigned decodeBRI(uint32_t insn) {
    switch (insn&0x3FFFFFF) {
    default:        break;
    case 0x0020004: return XTC::IDMEMBAR;
    case 0x0220004: return XTC::DMEMBAR;
    case 0x0420004: return XTC::IMEMBAR;
    }

    switch ((insn>>16)&0x1F) {
    default:   return UNSUPPORTED;
    case 0x00: return XTC::BRI;
    case 0x08: return XTC::BRAI;
    case 0x0C: return XTC::BRKI;
    case 0x10: return XTC::BRID;
    case 0x14: return XTC::BRLID;
    case 0x18: return XTC::BRAID;
    case 0x1C: return XTC::BRALID;
    }
}

static unsigned decodeBSRL(uint32_t insn) {
    switch ((insn>>9)&0x3) {
    default:  return UNSUPPORTED;
    case 0x2: return XTC::BSLL;
    case 0x1: return XTC::BSRA;
    case 0x0: return XTC::BSRL;
    }
}

static unsigned decodeBSRLI(uint32_t insn) {
    switch ((insn>>9)&0x3) {
    default:  return UNSUPPORTED;
    case 0x2: return XTC::BSLLI;
    case 0x1: return XTC::BSRAI;
    case 0x0: return XTC::BSRLI;
    }
}

static unsigned decodeRSUBK(uint32_t insn) {
    switch (getFLAGS(insn)) {
    default:  return UNSUPPORTED;
    case 0x0: return XTC::RSUBK;
    case 0x1: return XTC::CMP;
    case 0x3: return XTC::CMPU;
    }
}

static unsigned decodeFADD(uint32_t insn) {
    switch (getFLAGS(insn)) {
    default:    return UNSUPPORTED;
    case 0x000: return XTC::FADD;
    case 0x080: return XTC::FRSUB;
    case 0x100: return XTC::FMUL;
    case 0x180: return XTC::FDIV;
    case 0x200: return XTC::FCMP_UN;
    case 0x210: return XTC::FCMP_LT;
    case 0x220: return XTC::FCMP_EQ;
    case 0x230: return XTC::FCMP_LE;
    case 0x240: return XTC::FCMP_GT;
    case 0x250: return XTC::FCMP_NE;
    case 0x260: return XTC::FCMP_GE;
    case 0x280: return XTC::FLT;
    case 0x300: return XTC::FINT;
    case 0x380: return XTC::FSQRT;
    }
}

static unsigned decodeGET(uint32_t insn) {
    switch ((insn>>10)&0x3F) {
    default:   return UNSUPPORTED;
    case 0x00: return XTC::GET;
    case 0x01: return XTC::EGET;
    case 0x02: return XTC::AGET;
    case 0x03: return XTC::EAGET;
    case 0x04: return XTC::TGET;
    case 0x05: return XTC::TEGET;
    case 0x06: return XTC::TAGET;
    case 0x07: return XTC::TEAGET;
    case 0x08: return XTC::CGET;
    case 0x09: return XTC::ECGET;
    case 0x0A: return XTC::CAGET;
    case 0x0B: return XTC::ECAGET;
    case 0x0C: return XTC::TCGET;
    case 0x0D: return XTC::TECGET;
    case 0x0E: return XTC::TCAGET;
    case 0x0F: return XTC::TECAGET;
    case 0x10: return XTC::NGET;
    case 0x11: return XTC::NEGET;
    case 0x12: return XTC::NAGET;
    case 0x13: return XTC::NEAGET;
    case 0x14: return XTC::TNGET;
    case 0x15: return XTC::TNEGET;
    case 0x16: return XTC::TNAGET;
    case 0x17: return XTC::TNEAGET;
    case 0x18: return XTC::NCGET;
    case 0x19: return XTC::NECGET;
    case 0x1A: return XTC::NCAGET;
    case 0x1B: return XTC::NECAGET;
    case 0x1C: return XTC::TNCGET;
    case 0x1D: return XTC::TNECGET;
    case 0x1E: return XTC::TNCAGET;
    case 0x1F: return XTC::TNECAGET;
    case 0x20: return XTC::PUT;
    case 0x22: return XTC::APUT;
    case 0x24: return XTC::TPUT;
    case 0x26: return XTC::TAPUT;
    case 0x28: return XTC::CPUT;
    case 0x2A: return XTC::CAPUT;
    case 0x2C: return XTC::TCPUT;
    case 0x2E: return XTC::TCAPUT;
    case 0x30: return XTC::NPUT;
    case 0x32: return XTC::NAPUT;
    case 0x34: return XTC::TNPUT;
    case 0x36: return XTC::TNAPUT;
    case 0x38: return XTC::NCPUT;
    case 0x3A: return XTC::NCAPUT;
    case 0x3C: return XTC::TNCPUT;
    case 0x3E: return XTC::TNCAPUT;
    }
}

static unsigned decodeGETD(uint32_t insn) {
    switch ((insn>>5)&0x3F) {
    default:   return UNSUPPORTED;
    case 0x00: return XTC::GETD;
    case 0x01: return XTC::EGETD;
    case 0x02: return XTC::AGETD;
    case 0x03: return XTC::EAGETD;
    case 0x04: return XTC::TGETD;
    case 0x05: return XTC::TEGETD;
    case 0x06: return XTC::TAGETD;
    case 0x07: return XTC::TEAGETD;
    case 0x08: return XTC::CGETD;
    case 0x09: return XTC::ECGETD;
    case 0x0A: return XTC::CAGETD;
    case 0x0B: return XTC::ECAGETD;
    case 0x0C: return XTC::TCGETD;
    case 0x0D: return XTC::TECGETD;
    case 0x0E: return XTC::TCAGETD;
    case 0x0F: return XTC::TECAGETD;
    case 0x10: return XTC::NGETD;
    case 0x11: return XTC::NEGETD;
    case 0x12: return XTC::NAGETD;
    case 0x13: return XTC::NEAGETD;
    case 0x14: return XTC::TNGETD;
    case 0x15: return XTC::TNEGETD;
    case 0x16: return XTC::TNAGETD;
    case 0x17: return XTC::TNEAGETD;
    case 0x18: return XTC::NCGETD;
    case 0x19: return XTC::NECGETD;
    case 0x1A: return XTC::NCAGETD;
    case 0x1B: return XTC::NECAGETD;
    case 0x1C: return XTC::TNCGETD;
    case 0x1D: return XTC::TNECGETD;
    case 0x1E: return XTC::TNCAGETD;
    case 0x1F: return XTC::TNECAGETD;
    case 0x20: return XTC::PUTD;
    case 0x22: return XTC::APUTD;
    case 0x24: return XTC::TPUTD;
    case 0x26: return XTC::TAPUTD;
    case 0x28: return XTC::CPUTD;
    case 0x2A: return XTC::CAPUTD;
    case 0x2C: return XTC::TCPUTD;
    case 0x2E: return XTC::TCAPUTD;
    case 0x30: return XTC::NPUTD;
    case 0x32: return XTC::NAPUTD;
    case 0x34: return XTC::TNPUTD;
    case 0x36: return XTC::TNAPUTD;
    case 0x38: return XTC::NCPUTD;
    case 0x3A: return XTC::NCAPUTD;
    case 0x3C: return XTC::TNCPUTD;
    case 0x3E: return XTC::TNCAPUTD;
    }
}

static unsigned decodeIDIV(uint32_t insn) {
    switch (insn&0x3) {
    default:  return UNSUPPORTED;
    case 0x0: return XTC::IDIV;
    case 0x2: return XTC::IDIVU;
    }
}

static unsigned decodeLBU(uint32_t insn) {
    switch ((insn>>9)&0x1) {
    default:  return UNSUPPORTED;
    case 0x0: return XTC::LBU;
    case 0x1: return XTC::LBUR;
    }
}

static unsigned decodeLHU(uint32_t insn) {
    switch ((insn>>9)&0x1) {
    default:  return UNSUPPORTED;
    case 0x0: return XTC::LHU;
    case 0x1: return XTC::LHUR;
    }
}

static unsigned decodeLW(uint32_t insn) {
    switch ((insn>>9)&0x3) {
    default:  return UNSUPPORTED;
    case 0x0: return XTC::LW;
    case 0x1: return XTC::LWR;
    case 0x2: return XTC::LWX;
    }
}

static unsigned decodeSB(uint32_t insn) {
    switch ((insn>>9)&0x1) {
    default:  return UNSUPPORTED;
    case 0x0: return XTC::SB;
    case 0x1: return XTC::SBR;
    }
}

static unsigned decodeSH(uint32_t insn) {
    switch ((insn>>9)&0x1) {
    default:  return UNSUPPORTED;
    case 0x0: return XTC::SH;
    case 0x1: return XTC::SHR;
    }
}

static unsigned decodeSW(uint32_t insn) {
    switch ((insn>>9)&0x3) {
    default:  return UNSUPPORTED;
    case 0x0: return XTC::SW;
    case 0x1: return XTC::SWR;
    case 0x2: return XTC::SWX;
    }
}

static unsigned decodeMFS(uint32_t insn) {
    switch ((insn>>15)&0x1) {
    default:   return UNSUPPORTED;
    case 0x0:
      switch ((insn>>16)&0x1) {
      default:   return UNSUPPORTED;
      case 0x0: return XTC::MSRSET;
      case 0x1: return XTC::MSRCLR;
      }
    case 0x1:
      switch ((insn>>14)&0x1) {
      default:   return UNSUPPORTED;
      case 0x0: return XTC::MFS;
      case 0x1: return XTC::MTS;
      }
    }
}

static unsigned decodeOR(uint32_t insn) {
    switch (getFLAGS(insn)) {
    default:    return UNSUPPORTED;
    case 0x000: return XTC::OR;
    case 0x400: return XTC::PCMPBF;
    }
}

static unsigned decodeXOR(uint32_t insn) {
    switch (getFLAGS(insn)) {
    default:    return UNSUPPORTED;
    case 0x000: return XTC::XOR;
    case 0x400: return XTC::PCMPEQ;
    }
}

static unsigned decodeANDN(uint32_t insn) {
    switch (getFLAGS(insn)) {
    default:    return UNSUPPORTED;
    case 0x000: return XTC::ANDN;
    case 0x400: return XTC::PCMPNE;
    }
}

static unsigned decodeRTSD(uint32_t insn) {
    switch ((insn>>21)&0x1F) {
    default:   return UNSUPPORTED;
    case 0x10: return XTC::RTSD;
    case 0x11: return XTC::RTID;
    case 0x12: return XTC::RTBD;
    case 0x14: return XTC::RTED;
    }
}
#endif
static unsigned getOPCODE(uint32_t insn) {
#if 0
  unsigned opcode = mblazeBinary2Opcode[ (insn>>26)&0x3F ];
  switch (opcode) {
  case XTC::MUL:     return decodeMUL(insn);
  case XTC::SEXT8:   return decodeSEXT(insn);
  case XTC::BEQ:     return decodeBEQ(insn);
  case XTC::BEQI:    return decodeBEQI(insn);
  case XTC::BR:      return decodeBR(insn);
  case XTC::BRI:     return decodeBRI(insn);
  case XTC::BSRL:    return decodeBSRL(insn);
  case XTC::BSRLI:   return decodeBSRLI(insn);
  case XTC::RSUBK:   return decodeRSUBK(insn);
  case XTC::FADD:    return decodeFADD(insn);
  case XTC::GET:     return decodeGET(insn);
  case XTC::GETD:    return decodeGETD(insn);
  case XTC::IDIV:    return decodeIDIV(insn);
  case XTC::LBU:     return decodeLBU(insn);
  case XTC::LHU:     return decodeLHU(insn);
  case XTC::LW:      return decodeLW(insn);
  case XTC::SB:      return decodeSB(insn);
  case XTC::SH:      return decodeSH(insn);
  case XTC::SW:      return decodeSW(insn);
  case XTC::MFS:     return decodeMFS(insn);
  case XTC::OR:      return decodeOR(insn);
  case XTC::XOR:     return decodeXOR(insn);
  case XTC::ANDN:    return decodeANDN(insn);
  case XTC::RTSD:    return decodeRTSD(insn);
  default:              return opcode;
  }
#endif
}

//
// Public interface for the disassembler
//

MCDisassembler::DecodeStatus XTCDisassembler::getInstruction(MCInst &instr,
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

  uint64_t tsFlags = XTCInsts[opcode].TSFlags;
  switch ((tsFlags & XTCII::FormMask)) {
  default: 
    return Fail;

  case XTCII::FC:
    break;

  case XTCII::FRRRR:
    if (RD == UNSUPPORTED || RA == UNSUPPORTED || RB == UNSUPPORTED)
      return Fail;
    instr.addOperand(MCOperand::CreateReg(RD));
    instr.addOperand(MCOperand::CreateReg(RB));
    instr.addOperand(MCOperand::CreateReg(RA));
    break;

  case XTCII::FRRR:
    if (RD == UNSUPPORTED || RA == UNSUPPORTED || RB == UNSUPPORTED)
      return Fail;
    instr.addOperand(MCOperand::CreateReg(RD));
    instr.addOperand(MCOperand::CreateReg(RA));
    instr.addOperand(MCOperand::CreateReg(RB));
    break;

  case XTCII::FRR:
    if (RD == UNSUPPORTED || RA == UNSUPPORTED)
      return Fail;
    instr.addOperand(MCOperand::CreateReg(RD));
    instr.addOperand(MCOperand::CreateReg(RA));
    break;

  case XTCII::FRI:
    switch (opcode) {
    default: 
      return Fail;
    case XTC::MFS:
      if (RD == UNSUPPORTED)
        return Fail;
      instr.addOperand(MCOperand::CreateReg(RD));
      instr.addOperand(MCOperand::CreateImm(insn&0x3FFF));
      break;
    case XTC::MTS:
      if (RA == UNSUPPORTED)
        return Fail;
      instr.addOperand(MCOperand::CreateImm(insn&0x3FFF));
      instr.addOperand(MCOperand::CreateReg(RA));
      break;
    case XTC::MSRSET:
    case XTC::MSRCLR:
      if (RD == UNSUPPORTED)
        return Fail;
      instr.addOperand(MCOperand::CreateReg(RD));
      instr.addOperand(MCOperand::CreateImm(insn&0x7FFF));
      break;
    }
    break;

  case XTCII::FRRI:
    if (RD == UNSUPPORTED || RA == UNSUPPORTED)
      return Fail;
    instr.addOperand(MCOperand::CreateReg(RD));
    instr.addOperand(MCOperand::CreateReg(RA));
    switch (opcode) {
    default:
      instr.addOperand(MCOperand::CreateImm(getIMM(insn)));
      break;
    case XTC::BSRLI:
    case XTC::BSRAI:
    case XTC::BSLLI:
      instr.addOperand(MCOperand::CreateImm(insn&0x1F));
      break;
    }
    break;

  case XTCII::FCRR:
    if (RA == UNSUPPORTED || RB == UNSUPPORTED)
      return Fail;
    instr.addOperand(MCOperand::CreateReg(RA));
    instr.addOperand(MCOperand::CreateReg(RB));
    break;

  case XTCII::FCRI:
    if (RA == UNSUPPORTED)
      return Fail;
    instr.addOperand(MCOperand::CreateReg(RA));
    instr.addOperand(MCOperand::CreateImm(getIMM(insn)));
    break;

  case XTCII::FRCR:
    if (RD == UNSUPPORTED || RB == UNSUPPORTED)
      return Fail;
    instr.addOperand(MCOperand::CreateReg(RD));
    instr.addOperand(MCOperand::CreateReg(RB));
    break;

  case XTCII::FRCI:
    if (RD == UNSUPPORTED)
      return Fail;
    instr.addOperand(MCOperand::CreateReg(RD));
    instr.addOperand(MCOperand::CreateImm(getIMM(insn)));
    break;

  case XTCII::FCCR:
    if (RB == UNSUPPORTED)
      return Fail;
    instr.addOperand(MCOperand::CreateReg(RB));
    break;

  case XTCII::FCCI:
    instr.addOperand(MCOperand::CreateImm(getIMM(insn)));
    break;

  case XTCII::FRRCI:
    if (RD == UNSUPPORTED || RA == UNSUPPORTED)
      return Fail;
    instr.addOperand(MCOperand::CreateReg(RD));
    instr.addOperand(MCOperand::CreateReg(RA));
    instr.addOperand(MCOperand::CreateImm(getSHT(insn)));
    break;

  case XTCII::FRRC:
    if (RD == UNSUPPORTED || RA == UNSUPPORTED)
      return Fail;
    instr.addOperand(MCOperand::CreateReg(RD));
    instr.addOperand(MCOperand::CreateReg(RA));
    break;

  case XTCII::FRCX:
    if (RD == UNSUPPORTED)
      return Fail;
    instr.addOperand(MCOperand::CreateReg(RD));
    instr.addOperand(MCOperand::CreateImm(getFSL(insn)));
    break;

  case XTCII::FRCS:
    if (RD == UNSUPPORTED || RS == UNSUPPORTED)
      return Fail;
    instr.addOperand(MCOperand::CreateReg(RD));
    instr.addOperand(MCOperand::CreateReg(RS));
    break;

  case XTCII::FCRCS:
    if (RS == UNSUPPORTED || RA == UNSUPPORTED)
      return Fail;
    instr.addOperand(MCOperand::CreateReg(RS));
    instr.addOperand(MCOperand::CreateReg(RA));
    break;

  case XTCII::FCRCX:
    if (RA == UNSUPPORTED)
      return Fail;
    instr.addOperand(MCOperand::CreateReg(RA));
    instr.addOperand(MCOperand::CreateImm(getFSL(insn)));
    break;

  case XTCII::FCX:
    instr.addOperand(MCOperand::CreateImm(getFSL(insn)));
    break;

  case XTCII::FCR:
    if (RB == UNSUPPORTED)
      return Fail;
    instr.addOperand(MCOperand::CreateReg(RB));
    break;

  case XTCII::FRIR:
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

static MCDisassembler *createXTCDisassembler(const Target &T,
                                                const MCSubtargetInfo &STI) {
  return new XTCDisassembler(STI);
}

extern "C" void LLVMInitializeXTCDisassembler() {
  // Register the disassembler.
  TargetRegistry::RegisterMCDisassembler(TheXTCTarget,
                                         createXTCDisassembler);
}
