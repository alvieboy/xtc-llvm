//===-- XTCInstrInfo.cpp - XTC Instruction Information --------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the XTC implementation of the TargetInstrInfo class.
//
//===----------------------------------------------------------------------===//

#include "XTCInstrInfo.h"
#include "XTCMachineFunction.h"
#include "XTCTargetMachine.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/ScoreboardHazardRecognizer.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/Debug.h"
#define GET_INSTRINFO_CTOR
#include "XTCGenInstrInfo.inc"

using namespace llvm;

XTCInstrInfo::XTCInstrInfo(XTCTargetMachine &tm)
  : XTCGenInstrInfo(XTC::ADJCALLSTACKDOWN, XTC::ADJCALLSTACKUP),
    TM(tm), RI(*TM.getSubtargetImpl(), *this) {}

static bool isZeroImm(const MachineOperand &op) {
  return op.isImm() && op.getImm() == 0;
}

/// isLoadFromStackSlot - If the specified machine instruction is a direct
/// load from a stack slot, return the virtual or physical register number of
/// the destination along with the FrameIndex of the loaded stack slot.  If
/// not, return 0.  This predicate must return 0 if the instruction has
/// any side effects other than loading from the stack slot.
unsigned XTCInstrInfo::
isLoadFromStackSlot(const MachineInstr *MI, int &FrameIndex) const {

    if (MI->getOpcode() == XTC::LDW) {
    if ((MI->getOperand(1).isFI()) && // is a stack slot
        (MI->getOperand(2).isImm()) &&  // the imm is zero
        (isZeroImm(MI->getOperand(2)))) {
      FrameIndex = MI->getOperand(1).getIndex();
      return MI->getOperand(0).getReg();
    }
    }

  return 0;
}

/// isStoreToStackSlot - If the specified machine instruction is a direct
/// store to a stack slot, return the virtual or physical register number of
/// the source reg along with the FrameIndex of the loaded stack slot.  If
/// not, return 0.  This predicate must return 0 if the instruction has
/// any side effects other than storing to the stack slot.
unsigned XTCInstrInfo::
isStoreToStackSlot(const MachineInstr *MI, int &FrameIndex) const {
    if (MI->getOpcode() == XTC::STW) {
    if ((MI->getOperand(1).isFI()) && // is a stack slot
        (MI->getOperand(2).isImm()) &&  // the imm is zero
        (isZeroImm(MI->getOperand(2)))) {
      FrameIndex = MI->getOperand(1).getIndex();
      return MI->getOperand(0).getReg();
    }
    }
  return 0;
}

/// insertNoop - If data hazard condition is found insert the target nop
/// instruction.
void XTCInstrInfo::
insertNoop(MachineBasicBlock &MBB, MachineBasicBlock::iterator MI) const {
    DebugLoc DL;
    BuildMI(MBB, MI, DL, get(XTC::NOP));
}

void XTCInstrInfo::
copyPhysReg(MachineBasicBlock &MBB,
            MachineBasicBlock::iterator I, DebugLoc DL,
            unsigned DestReg, unsigned SrcReg,
            bool KillSrc) const {

    llvm::BuildMI(MBB, I, DL, get(XTC::MOV), DestReg)
    .addReg(SrcReg, getKillRegState(KillSrc));

}

void XTCInstrInfo::
storeRegToStackSlot(MachineBasicBlock &MBB, MachineBasicBlock::iterator I,
                    unsigned SrcReg, bool isKill, int FI,
                    const TargetRegisterClass *RC,
                    const TargetRegisterInfo *TRI) const {

    MachineFunction *F = MBB.getParent();
    MachineRegisterInfo &MRI = F->getRegInfo();

    DebugLoc DL;
    if (RC == &XTC::GPRegsRegClass) {
        BuildMI(MBB, I, DL, get(XTC::STW)).addReg(SrcReg,getKillRegState(isKill))
            .addFrameIndex(FI).addImm(0);

    } else if (RC == &XTC::SPRegsRegClass) {
        BuildMI(MBB, I, DL, get(XTC::STSPR)).addReg(SrcReg,getKillRegState(isKill))
            .addFrameIndex(FI).addImm(0);
    } else {
        llvm_unreachable("Cannot store this register to stack slot");
    }
}

void XTCInstrInfo::
loadRegFromStackSlot(MachineBasicBlock &MBB, MachineBasicBlock::iterator I,
                     unsigned DestReg, int FI,
                     const TargetRegisterClass *RC,
                     const TargetRegisterInfo *TRI) const {

    MachineFunction *F = MBB.getParent();
    MachineRegisterInfo &MRI = F->getRegInfo();
    DebugLoc DL;

    if (RC == &XTC::GPRegsRegClass) {
        BuildMI(MBB, I, DL, get(XTC::LDW), DestReg)
            .addFrameIndex(FI).addImm(0);
    } else if (RC == &XTC::SPRegsRegClass) {
        BuildMI(MBB, I, DL, get(XTC::LDSPR), DestReg)
            .addFrameIndex(FI).addImm(0);
    } else {
        llvm_unreachable("Cannot load this register from stack slot");
    }
}

//===----------------------------------------------------------------------===//
// Branch Analysis
//===----------------------------------------------------------------------===//
bool XTCInstrInfo::AnalyzeBranch(MachineBasicBlock &MBB,
                                 MachineBasicBlock *&TBB,
                                 MachineBasicBlock *&FBB,
                                 SmallVectorImpl<MachineOperand> &Cond,
                                 bool AllowModify) const {
    // Start from the bottom of the block and work up, examining the
    // terminator instructions.
    return true;
    MachineBasicBlock::iterator I = MBB.end();
    while (I != MBB.begin()) {
        --I;
        if (I->isDebugValue())
            continue;

        // Working from the bottom, when we see a non-terminator
        // instruction, we're done.
        if (!isUnpredicatedTerminator(I))
            break;

        // A terminator that isn't a branch can't easily be handled
        // by this analysis.
        if (!I->isBranch())
            return true;

        // Handle unconditional branches.
        if (llvm::XTC::isUncondBranchOpcode(I->getOpcode())) {
            if (!AllowModify) {
                TBB = I->getOperand(0).getMBB();
                continue;
            }

            // If the block has any instructions after a JMP, delete them.
            while (llvm::next(I) != MBB.end()) {
                DEBUG(dbgs()<<"Erasing instruction after JMP\n");
                llvm::next(I)->eraseFromParent();
            }
            Cond.clear();
            FBB = 0;

            // Delete the JMP if it's equivalent to a fall-through.
            if (MBB.isLayoutSuccessor(I->getOperand(0).getMBB())) {
                TBB = 0;
                DEBUG(dbgs()<<"Fall-through jump, erasing instruction\n");
                I->dump();
                I->eraseFromParent();
                I = MBB.end();
                continue;
            }

            // TBB is used to indicate the unconditinal destination.
            TBB = I->getOperand(0).getMBB();
            continue;
        }

        // Handle conditional branches.
        assert(llvm::XTC::isCondBranchOpcode(I->getOpcode()));
#if 0
        unsigned BranchCode =
            static_cast<MSP430CC::CondCodes>(I->getOperand(1).getImm());
        if (BranchCode == MSP430CC::COND_INVALID)
            return true;  // Can't handle weird stuff.

        // Working from the bottom, handle the first conditional branch.
        if (Cond.empty()) {
            FBB = TBB;
            TBB = I->getOperand(0).getMBB();
            Cond.push_back(MachineOperand::CreateImm(BranchCode));
            continue;
        }

        // Handle subsequent conditional branches. Only handle the case where all
        // conditional branches branch to the same destination.
        assert(Cond.size() == 1);
        assert(TBB);

        // Only handle the case where all conditional branches branch to
        // the same destination.
        if (TBB != I->getOperand(0).getMBB())
            return true;

        MSP430CC::CondCodes OldBranchCode = (MSP430CC::CondCodes)Cond[0].getImm();
        // If the conditions are the same, we can leave them alone.
        if (OldBranchCode == BranchCode)
            continue;
        return true;
#endif
        return true; /* Don't handle this yet */
    }

    return false;

}

unsigned XTCInstrInfo::
InsertBranch(MachineBasicBlock &MBB, MachineBasicBlock *TBB,
             MachineBasicBlock *FBB,
             const SmallVectorImpl<MachineOperand> &Cond,
             DebugLoc DL) const {

    // Shouldn't be a fall through.
  assert(TBB && "InsertBranch must not be told to insert a fallthrough");
  assert((Cond.size() == 2 || Cond.size() == 0) &&
         "XTC branch conditions have two components!");

  unsigned Opc = XTC::BRI;
  if (!Cond.empty())
    Opc = (unsigned)Cond[0].getImm();

  if (FBB == 0) {
    if (Cond.empty()) // Unconditional branch
      BuildMI(&MBB, DL, get(Opc)).addMBB(TBB);
    else              // Conditional branch
      BuildMI(&MBB, DL, get(Opc)).addReg(Cond[1].getReg()).addMBB(TBB);
    return 1;
  }

  BuildMI(&MBB, DL, get(Opc)).addReg(Cond[1].getReg()).addMBB(TBB);
  BuildMI(&MBB, DL, get(XTC::BRI)).addMBB(FBB);

  return 2;
}

unsigned XTCInstrInfo::RemoveBranch(MachineBasicBlock &MBB) const {
  MachineBasicBlock::iterator I = MBB.end();
  if (I == MBB.begin()) return 0;
  --I;
  while (I->isDebugValue()) {
    if (I == MBB.begin())
      return 0;
    --I;
  }

  if (!XTC::isUncondBranchOpcode(I->getOpcode()) &&
      !XTC::isCondBranchOpcode(I->getOpcode()))
    return 0;

  // Remove the branch.
  I->eraseFromParent();

  I = MBB.end();

  if (I == MBB.begin()) return 1;
  --I;
  if (!XTC::isCondBranchOpcode(I->getOpcode()))
    return 1;

  // Remove the branch.
  I->eraseFromParent();
  return 2;
}

bool XTCInstrInfo::ReverseBranchCondition(SmallVectorImpl<MachineOperand>
                                               &Cond) const {
  assert(Cond.size() == 2 && "Invalid XTC branch opcode!");
  switch (Cond[0].getImm()) {
  default:            return true;
#if 0
  case XTC::BEQ:   Cond[0].setImm(XTC::BNE); return false;
  case XTC::BNE:   Cond[0].setImm(XTC::BEQ); return false;
  case XTC::BGT:   Cond[0].setImm(XTC::BLE); return false;
  case XTC::BGE:   Cond[0].setImm(XTC::BLT); return false;
  case XTC::BLT:   Cond[0].setImm(XTC::BGE); return false;
  case XTC::BLE:   Cond[0].setImm(XTC::BGT); return false;
  case XTC::BEQI:  Cond[0].setImm(XTC::BNEI); return false;
  case XTC::BNEI:  Cond[0].setImm(XTC::BEQI); return false;
  case XTC::BGTI:  Cond[0].setImm(XTC::BLEI); return false;
  case XTC::BGEI:  Cond[0].setImm(XTC::BLTI); return false;
  case XTC::BLTI:  Cond[0].setImm(XTC::BGEI); return false;
  case XTC::BLEI:  Cond[0].setImm(XTC::BGTI); return false;
  case XTC::BEQD:  Cond[0].setImm(XTC::BNED); return false;
  case XTC::BNED:  Cond[0].setImm(XTC::BEQD); return false;
  case XTC::BGTD:  Cond[0].setImm(XTC::BLED); return false;
  case XTC::BGED:  Cond[0].setImm(XTC::BLTD); return false;
  case XTC::BLTD:  Cond[0].setImm(XTC::BGED); return false;
  case XTC::BLED:  Cond[0].setImm(XTC::BGTD); return false;
  case XTC::BEQID: Cond[0].setImm(XTC::BNEID); return false;
  case XTC::BNEID: Cond[0].setImm(XTC::BEQID); return false;
  case XTC::BGTID: Cond[0].setImm(XTC::BLEID); return false;
  case XTC::BGEID: Cond[0].setImm(XTC::BLTID); return false;
  case XTC::BLTID: Cond[0].setImm(XTC::BGEID); return false;
  case XTC::BLEID: Cond[0].setImm(XTC::BGTID); return false;
#endif
  }
}

/// getGlobalBaseReg - Return a virtual register initialized with the
/// the global base register value. Output instructions required to
/// initialize the register in the function entry block, if necessary.
///
unsigned XTCInstrInfo::getGlobalBaseReg(MachineFunction *MF) const {

    XTCFunctionInfo *XTCFI = MF->getInfo<XTCFunctionInfo>();
  unsigned GlobalBaseReg = XTCFI->getGlobalBaseReg();
  if (GlobalBaseReg != 0)
    return GlobalBaseReg;

  llvm_unreachable("Cannot get Global Base Reg yet");

  // Insert the set of GlobalBaseReg into the first MBB of the function
  MachineBasicBlock &FirstMBB = MF->front();
  MachineBasicBlock::iterator MBBI = FirstMBB.begin();
  MachineRegisterInfo &RegInfo = MF->getRegInfo();
  const TargetInstrInfo *TII = MF->getTarget().getInstrInfo();

  GlobalBaseReg = RegInfo.createVirtualRegister(&XTC::GPRegsRegClass);

  BuildMI(FirstMBB, MBBI, DebugLoc(), TII->get(TargetOpcode::COPY),
          GlobalBaseReg).addReg(XTC::r6);
  RegInfo.addLiveIn(XTC::r6);

  XTCFI->setGlobalBaseReg(GlobalBaseReg);
  return GlobalBaseReg;
}
