//===-- XTCELFObjectWriter.cpp - XTC ELF Writer ---------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/XTCMCTargetDesc.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCFixup.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

namespace {
  class XTCELFObjectWriter : public MCELFObjectTargetWriter {
  public:
    XTCELFObjectWriter(uint8_t OSABI);

    virtual ~XTCELFObjectWriter();
  protected:
    virtual unsigned GetRelocType(const MCValue &Target, const MCFixup &Fixup,
                                  bool IsPCRel, bool IsRelocWithSymbol,
                                  int64_t Addend) const;
  };
}

XTCELFObjectWriter::XTCELFObjectWriter(uint8_t OSABI)
  : MCELFObjectTargetWriter(/*Is64Bit*/ false, OSABI, ELF::EM_MBLAZE,
                            /*HasRelocationAddend*/ false) {}

XTCELFObjectWriter::~XTCELFObjectWriter() {
}

unsigned XTCELFObjectWriter::GetRelocType(const MCValue &Target,
                                             const MCFixup &Fixup,
                                             bool IsPCRel,
                                             bool IsRelocWithSymbol,
                                             int64_t Addend) const {
  // determine the type of the relocation
  unsigned Type;
  if (IsPCRel) {
    switch ((unsigned)Fixup.getKind()) {
    default:
      llvm_unreachable("Unimplemented");
    case FK_PCRel_4:
      Type = ELF::R_MICROBLAZE_64_PCREL;
      break;
    case FK_PCRel_2:
      Type = ELF::R_MICROBLAZE_32_PCREL;
      break;
    }
  } else {
    switch ((unsigned)Fixup.getKind()) {
    default: llvm_unreachable("invalid fixup kind!");
    case FK_Data_4:
      Type = ((IsRelocWithSymbol || Addend !=0)
              ? ELF::R_MICROBLAZE_32
              : ELF::R_MICROBLAZE_64);
      break;
    case FK_Data_2:
      Type = ELF::R_MICROBLAZE_32;
      break;
    }
  }
  return Type;
}



MCObjectWriter *llvm::createXTCELFObjectWriter(raw_ostream &OS,
                                                  uint8_t OSABI) {
  MCELFObjectTargetWriter *MOTW = new XTCELFObjectWriter(OSABI);
  return createELFObjectWriter(MOTW, OS,  /*IsLittleEndian=*/ false);
}
