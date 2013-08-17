//===-- NewcpuBaseInfo.h - Top level definitions for Newcpu -- --*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains small standalone helper functions and enum definitions for
// the Newcpu target useful for the compiler back-end and the MC libraries.
// As such, it deliberately does not include references to LLVM core
// code gen types, passes, etc..
//
//===----------------------------------------------------------------------===//

#ifndef NEWCPUBASEINFO_H
#define NEWCPUBASEINFO_H

#include "NewcpuMCTargetDesc.h"
#include "llvm/Support/ErrorHandling.h"

namespace llvm {

/// NewcpuII - This namespace holds all of the target specific flags that
/// instruction info tracks.
///
namespace NewcpuII {
  enum {
    // PseudoFrm - This represents an instruction that is a pseudo instruction
    // or one that has not been implemented yet.  It is illegal to code generate
    // it, but tolerated for intermediate implementation stages.
    FPseudo = 0,
    FALUg,
    FALUaz,
    FIMM,
    FormMask = 3
    //===------------------------------------------------------------------===//
    // Newcpu Specific MachineOperand flags.
    // MO_NO_FLAG,

    /// MO_GOT - Represents the offset into the global offset table at which
    /// the address the relocation entry symbol resides during execution.
    // MO_GOT,

    /// MO_GOT_CALL - Represents the offset into the global offset table at
    /// which the address of a call site relocation entry symbol resides
    /// during execution. This is different from the above since this flag
    /// can only be present in call instructions.
    // MO_GOT_CALL,

    /// MO_GPREL - Represents the offset from the current gp value to be used
    /// for the relocatable object file being produced.
    // MO_GPREL,

    /// MO_ABS_HILO - Represents the hi or low part of an absolute symbol
    /// address.
    // MO_ABS_HILO

  };
}

static inline bool isGPRNewcpuRegister(unsigned Reg) {
  return Reg <= 7;
}

static inline bool isDARNewcpuRegister(unsigned Reg) {
  switch (Reg) {
  case 0:
  case 1:
      return true;

    default:
      return false;
  }
}

/// getNewcpuRegisterNumbering - Given the enum value for some register, e.g.
/// Newcpu::R0, return the number that it corresponds to (e.g. 0).
static inline unsigned getNewcpuRegisterNumbering(unsigned RegEnum) {
  switch (RegEnum) {
    case Newcpu::R0     : return 0;
    case Newcpu::R1     : return 1;
    case Newcpu::R2     : return 2;
    case Newcpu::R3     : return 3;
    case Newcpu::R4     : return 4;
    case Newcpu::R5     : return 5;
    case Newcpu::R6     : return 6;
    case Newcpu::R7     : return 7;
    case Newcpu::A      : return 0;
//    case Newcpu::Z      : return 1;
    case Newcpu::PC     : return 0;
    default: llvm_unreachable("Unknown register number!");
  }
}

/// getRegisterFromNumbering - Given the enum value for some register, e.g.
/// Newcpu::R0, return the number that it corresponds to (e.g. 0).
static inline unsigned getGPRNewcpuRegisterFromNumbering(unsigned Reg) {
  switch (Reg) {
    case 0  : return Newcpu::R0;
    case 1  : return Newcpu::R1;
    case 2  : return Newcpu::R2;
    case 3  : return Newcpu::R3;
    case 4  : return Newcpu::R4;
    case 5  : return Newcpu::R5;
    case 6  : return Newcpu::R6;
    case 7  : return Newcpu::R7;
    default: llvm_unreachable("Unknown register number!");
  }
}

static inline unsigned getDARNewcpuRegisterFromNumbering(unsigned Reg) {
  switch (Reg) {
  case 0:
      return Newcpu::A;
    default: llvm_unreachable("Unknown register number!");
  }
}

} // end namespace llvm;

#endif
