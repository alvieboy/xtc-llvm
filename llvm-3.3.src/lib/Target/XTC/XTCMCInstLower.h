//===-- XTCMCInstLower.h - Lower MachineInstr to MCInst ------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef XTC_MCINSTLOWER_H
#define XTC_MCINSTLOWER_H

#include "llvm/Support/Compiler.h"

namespace llvm {
  class AsmPrinter;
  class MCContext;
  class MCInst;
  class MCOperand;
  class MCSymbol;
  class MachineInstr;
  class MachineModuleInfoMachO;
  class MachineOperand;

  /// XTCMCInstLower - This class is used to lower an MachineInstr
  /// into an MCInst.
class LLVM_LIBRARY_VISIBILITY XTCMCInstLower {
  MCContext &Ctx;

  AsmPrinter &Printer;
public:
  XTCMCInstLower(MCContext &ctx, AsmPrinter &printer)
    : Ctx(ctx), Printer(printer) {}
  void Lower(const MachineInstr *MI, MCInst &OutMI) const;

  MCOperand LowerSymbolOperand(const MachineOperand &MO, MCSymbol *Sym) const;

  MCSymbol *GetGlobalAddressSymbol(const MachineOperand &MO) const;
  MCSymbol *GetExternalSymbolSymbol(const MachineOperand &MO) const;
  MCSymbol *GetJumpTableSymbol(const MachineOperand &MO) const;
  MCSymbol *GetConstantPoolIndexSymbol(const MachineOperand &MO) const;
  MCSymbol *GetBlockAddressSymbol(const MachineOperand &MO) const;
protected:
  unsigned GetLowerArith(unsigned opcode) const;
};

}

#endif
