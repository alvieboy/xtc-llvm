//===-- XTCMCInstLower.cpp - Convert XTC MachineInstr to an MCInst---===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains code to lower XTC MachineInstrs to their corresponding
// MCInst records.
//
//===----------------------------------------------------------------------===//

#include "XTCMCInstLower.h"
#include "XTCInstrInfo.h"
#include "InstPrinter/XTCInstPrinter.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/IR/Constants.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/Mangler.h"
using namespace llvm;

MCSymbol *XTCMCInstLower::
GetGlobalAddressSymbol(const MachineOperand &MO) const {
  switch (MO.getTargetFlags()) {
  default: llvm_unreachable("Unknown target flag on GV operand");
  case 0:  break;
  }

  return Printer.Mang->getSymbol(MO.getGlobal());
}

MCSymbol *XTCMCInstLower::
GetExternalSymbolSymbol(const MachineOperand &MO) const {
  switch (MO.getTargetFlags()) {
  default: llvm_unreachable("Unknown target flag on GV operand");
  case 0:  break;
  }

  return Printer.GetExternalSymbolSymbol(MO.getSymbolName());
}

MCSymbol *XTCMCInstLower::
GetJumpTableSymbol(const MachineOperand &MO) const {
  SmallString<256> Name;
  raw_svector_ostream(Name) << Printer.MAI->getPrivateGlobalPrefix() << "JTI"
                            << Printer.getFunctionNumber() << '_'
                            << MO.getIndex();
  switch (MO.getTargetFlags()) {
  default: llvm_unreachable("Unknown target flag on GV operand");
  case 0:  break;
  }

  // Create a symbol for the name.
  return Ctx.GetOrCreateSymbol(Name.str());
}

MCSymbol *XTCMCInstLower::
GetConstantPoolIndexSymbol(const MachineOperand &MO) const {
  SmallString<256> Name;
  raw_svector_ostream(Name) << Printer.MAI->getPrivateGlobalPrefix() << "CPI"
                            << Printer.getFunctionNumber() << '_'
                            << MO.getIndex();

  switch (MO.getTargetFlags()) {
  default:
      llvm_unreachable("Unknown target flag on GV operand");

  case 0: break;
  }

  // Create a symbol for the name.
  return Ctx.GetOrCreateSymbol(Name.str());
}

MCSymbol *XTCMCInstLower::
GetBlockAddressSymbol(const MachineOperand &MO) const {
  switch (MO.getTargetFlags()) {
  default: llvm_unreachable("Unknown target flag on GV operand");
  case 0: break;
  }

  return Printer.GetBlockAddressSymbol(MO.getBlockAddress());
}

MCOperand XTCMCInstLower::
LowerSymbolOperand(const MachineOperand &MO, MCSymbol *Sym) const {
  // FIXME: We would like an efficient form for this, so we don't have to do a
  // lot of extra uniquing.
  const MCExpr *Expr = MCSymbolRefExpr::Create(Sym, Ctx);

  switch (MO.getTargetFlags()) {
  default:
      llvm_unreachable("Unknown target flag on GV operand");

  case 0: break;
  }

  if (!MO.isJTI() && MO.getOffset())
    Expr = MCBinaryExpr::CreateAdd(Expr,
                                   MCConstantExpr::Create(MO.getOffset(), Ctx),
                                   Ctx);
  return MCOperand::CreateExpr(Expr);
}


unsigned XTCMCInstLower::GetLowerArith(unsigned opcode) const
{
    switch(opcode) {
    case XTC::ADDD:
        return XTC::ADD;
    case XTC::ANDD:
        return XTC::AND;
    case XTC::ORD:
        return XTC::OR;
    case XTC::XORD:
        return XTC::XOR;
    case XTC::SHLD:
        return XTC::SHL;
    case XTC::SRLD:
        return XTC::SRL;
    case XTC::SRAD:
        return XTC::SRA;
    case XTC::MULD:
        return XTC::MUL;
    case XTC::ADDDC:
        return XTC::ADDC;
    case XTC::ADDDE:
        return XTC::ADDE;

    default:
        llvm_unreachable("Attempting to lower an unknown opcode");
    }
}

void XTCMCInstLower::Lower(const MachineInstr *MI, MCInst &OutMI) const {
    bool needCommutation = false;
    bool canLower = false;

  OutMI.setOpcode(MI->getOpcode());

  for (unsigned i = 0, e = MI->getNumOperands(); i != e; ++i) {
    const MachineOperand &MO = MI->getOperand(i);

    MCOperand MCOp;
    switch (MO.getType()) {
    default: llvm_unreachable("unknown operand type");
    case MachineOperand::MO_Register:
      // Ignore all implicit register operands.
      if (MO.isImplicit()) continue;
      MCOp = MCOperand::CreateReg(MO.getReg());
      break;
    case MachineOperand::MO_Immediate:
      MCOp = MCOperand::CreateImm(MO.getImm());
      break;
    case MachineOperand::MO_MachineBasicBlock:
      MCOp = MCOperand::CreateExpr(MCSymbolRefExpr::Create(
                         MO.getMBB()->getSymbol(), Ctx));
      break;
    case MachineOperand::MO_GlobalAddress:
      MCOp = LowerSymbolOperand(MO, GetGlobalAddressSymbol(MO));
      break;
    case MachineOperand::MO_ExternalSymbol:
      MCOp = LowerSymbolOperand(MO, GetExternalSymbolSymbol(MO));
      break;
    case MachineOperand::MO_JumpTableIndex:
      MCOp = LowerSymbolOperand(MO, GetJumpTableSymbol(MO));
      break;
    case MachineOperand::MO_ConstantPoolIndex:
      MCOp = LowerSymbolOperand(MO, GetConstantPoolIndexSymbol(MO));
      break;
    case MachineOperand::MO_BlockAddress:
      MCOp = LowerSymbolOperand(MO, GetBlockAddressSymbol(MO));
      break;
    case MachineOperand::MO_FPImmediate: {
      bool ignored;
      APFloat FVal = MO.getFPImm()->getValueAPF();
      FVal.convert(APFloat::IEEEsingle, APFloat::rmTowardZero, &ignored);

      APInt IVal = FVal.bitcastToAPInt();
      uint64_t Val = *IVal.getRawData();
      MCOp = MCOperand::CreateImm(Val);
      break;
    }
    case MachineOperand::MO_RegisterMask:
      continue;
    }

    OutMI.addOperand(MCOp);
  }


  switch (OutMI.getOpcode()) {
  case XTC::ADDD:
  case XTC::ANDD:
  case XTC::ORD:
  case XTC::XORD:
  case XTC::ADDDC:
  case XTC::ADDDE:
#if 0
  case XTC::SHLD:
  case XTC::SRLD:
  case XTC::SRAD:
  case XTC::MULD:
#endif
      /* */
      assert(OutMI.getNumOperands()==3);

      if (OutMI.getOperand(1).getReg() == OutMI.getOperand(0).getReg())
          canLower=true;
      if ( OutMI.getOperand(2).getReg() == OutMI.getOperand(0).getReg() && MI->getDesc().isCommutable()) {
          canLower=true;
          needCommutation=true;
      }

      if ( canLower ) {
          unsigned newOpc = GetLowerArith( OutMI.getOpcode());
          MCOperand Target = OutMI.getOperand(0);
          MCOperand LHS = OutMI.getOperand(1);
          MCOperand RHS = OutMI.getOperand(2);

          OutMI = MCInst();
          OutMI.setOpcode(newOpc);
          OutMI.addOperand( Target );
          OutMI.addOperand( needCommutation ? LHS : RHS );
      }
      break;
  case XTC::ADDEI:
      /* See if we can lower into ADDI */
      if (OutMI.getOperand(1).getReg() == OutMI.getOperand(0).getReg()) {
          MCOperand Target = OutMI.getOperand(0);
          MCOperand Immed = OutMI.getOperand(2);

          OutMI = MCInst();
          OutMI.setOpcode( XTC::ADDI );
          OutMI.addOperand( Target );
          OutMI.addOperand( Target );
          OutMI.addOperand( Immed );
      }
  }
}
