//===-- XTCInstPrinter.cpp - Convert XTC MCInst to assembly syntax --===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This class prints an XTC MCInst to a .s file.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "asm-printer"
#include "XTCInstPrinter.h"
#include "XTC.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FormattedStream.h"
#include "XTCISelLowering.h"
using namespace llvm;


// Include the auto-generated portion of the assembly writer.                 
#include "XTCGenAsmWriter.inc"

void XTCInstPrinter::printInst(const MCInst *MI, raw_ostream &O,
                                  StringRef Annot) {
  printInstruction(MI, O);
  printAnnotation(O, Annot);
}

void XTCInstPrinter::printOperand(const MCInst *MI, unsigned OpNo,
                                     raw_ostream &O, const char *Modifier) {
  assert((Modifier == 0 || Modifier[0] == 0) && "No modifiers supported");
  const MCOperand &Op = MI->getOperand(OpNo);
  if (Op.isReg()) {
    O << getRegisterName(Op.getReg());
  } else if (Op.isImm()) {
    O << (int32_t)Op.getImm();
  } else {
    assert(Op.isExpr() && "unknown operand kind in printOperand");
    O << *Op.getExpr();
  }
}

void XTCInstPrinter::printFSLImm(const MCInst *MI, int OpNo,
                                    raw_ostream &O) {
  const MCOperand &MO = MI->getOperand(OpNo);
  if (MO.isImm())
    O << "rfsl" << MO.getImm();
  else
    printOperand(MI, OpNo, O, NULL);
}

void XTCInstPrinter::printUnsignedImm(const MCInst *MI, int OpNo,
                                        raw_ostream &O) {
  const MCOperand &MO = MI->getOperand(OpNo);
  if (MO.isImm())
    O << (uint32_t)MO.getImm();
  else
    printOperand(MI, OpNo, O, NULL);
}

void XTCInstPrinter::printMemOperand(const MCInst *MI, int OpNo,
                                        raw_ostream &O, const char *Modifier) {
  printOperand(MI, OpNo, O, NULL);
  O << ", ";
  printOperand(MI, OpNo+1, O, NULL);
}

void XTCInstPrinter::printAM2PreOrOffsetIndexOp(const MCInst *MI, unsigned Op,
                                                raw_ostream &O) {
  const MCOperand &MO1 = MI->getOperand(Op);
  const MCOperand &MO2 = MI->getOperand(Op+1);
  const MCOperand &MO3 = MI->getOperand(Op+2);
#if 0
  O << markup("<mem:") << "[";
  printRegName(O, MO1.getReg());

  if (!MO2.getReg()) {
    if (ARM_AM::getAM2Offset(MO3.getImm())) { // Don't print +0.
      O << ", "
        << markup("<imm:")
        << "#"
        << ARM_AM::getAddrOpcStr(ARM_AM::getAM2Op(MO3.getImm()))
        << ARM_AM::getAM2Offset(MO3.getImm())
        << markup(">");
    }
    O << "]" << markup(">");
    return;
  }

  O << ", ";
  O << ARM_AM::getAddrOpcStr(ARM_AM::getAM2Op(MO3.getImm()));
  printRegName(O, MO2.getReg());

  printRegImmShift(O, ARM_AM::getAM2ShiftOpc(MO3.getImm()),
                   ARM_AM::getAM2Offset(MO3.getImm()), UseMarkup);
  O << "]" << markup(">");
#endif
}

void XTCInstPrinter::printAddrMode2OffsetOperand(const MCInst *MI,
                                                 unsigned OpNum,
                                                 raw_ostream &O) {
  const MCOperand &MO1 = MI->getOperand(OpNum);
  const MCOperand &MO2 = MI->getOperand(OpNum+1);
#if 0
  if (!MO1.getReg()) {
    unsigned ImmOffs = ARM_AM::getAM2Offset(MO2.getImm());
    O << markup("<imm:")
      << '#' << ARM_AM::getAddrOpcStr(ARM_AM::getAM2Op(MO2.getImm()))
      << ImmOffs
      << markup(">");
    return;
  }

  O << ARM_AM::getAddrOpcStr(ARM_AM::getAM2Op(MO2.getImm()));
  printRegName(O, MO1.getReg());

  printRegImmShift(O, ARM_AM::getAM2ShiftOpc(MO2.getImm()),
                   ARM_AM::getAM2Offset(MO2.getImm()), UseMarkup);
#endif
}

void XTCInstPrinter::printAddrMode2Operand(const MCInst *MI, unsigned Op,
                                              raw_ostream &O) {
    const MCOperand &MO1 = MI->getOperand(Op);

    if (!MO1.isReg()) {   // FIXME: This is for CP entries, but isn't right.
        printOperand(MI, Op, O);
        return;
    }

    printAM2PreOrOffsetIndexOp(MI, Op, O);
}

void XTCInstPrinter::printCCOperand(const MCInst *MI, int opNum,
                                  raw_ostream &O) {
    int CC = (int)MI->getOperand(opNum).getImm();
    O << XTCCCToString((XTCCC::CC)CC);
}
