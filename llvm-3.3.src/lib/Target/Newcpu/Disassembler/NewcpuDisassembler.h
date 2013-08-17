//===-- NewcpuDisassembler.h - Disassembler for MicroBlaze  -----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file is part of the Newcpu Disassembler. It it the header for
// NewcpuDisassembler, a subclass of MCDisassembler.
//
//===----------------------------------------------------------------------===//

#ifndef MBLAZEDISASSEMBLER_H
#define MBLAZEDISASSEMBLER_H

#include "llvm/MC/MCDisassembler.h"

namespace llvm {
  
class MCInst;
class MemoryObject;
class raw_ostream;

/// NewcpuDisassembler - Disassembler for all Newcpu platforms.
class NewcpuDisassembler : public MCDisassembler {
public:
  /// Constructor     - Initializes the disassembler.
  ///
  NewcpuDisassembler(const MCSubtargetInfo &STI) :
    MCDisassembler(STI) {
  }

  ~NewcpuDisassembler() {
  }

  /// getInstruction - See MCDisassembler.
  MCDisassembler::DecodeStatus getInstruction(MCInst &instr,
                      uint64_t &size,
                      const MemoryObject &region,
                      uint64_t address,
                      raw_ostream &vStream,
                      raw_ostream &cStream) const;
};

} // namespace llvm
  
#endif
