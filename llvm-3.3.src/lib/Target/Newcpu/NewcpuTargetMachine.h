//===-- NewcpuTargetMachine.h - Define TargetMachine for Newcpu -*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares the Newcpu specific subclass of TargetMachine.
//
//===----------------------------------------------------------------------===//

#ifndef MBLAZE_TARGETMACHINE_H
#define MBLAZE_TARGETMACHINE_H

#include "NewcpuFrameLowering.h"
#include "NewcpuISelLowering.h"
#include "NewcpuInstrInfo.h"
#include "NewcpuIntrinsicInfo.h"
#include "NewcpuSelectionDAGInfo.h"
#include "NewcpuSubtarget.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/Target/TargetFrameLowering.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {
  class formatted_raw_ostream;

  class NewcpuTargetMachine : public LLVMTargetMachine {
    NewcpuSubtarget        Subtarget;
    const DataLayout       DL; // Calculates type size & alignment
    NewcpuInstrInfo        InstrInfo;
    NewcpuFrameLowering    FrameLowering;
    NewcpuTargetLowering   TLInfo;
    NewcpuSelectionDAGInfo TSInfo;
    NewcpuIntrinsicInfo    IntrinsicInfo;
    InstrItineraryData     InstrItins;

  public:
    NewcpuTargetMachine(const Target &T, StringRef TT,
                        StringRef CPU, StringRef FS,
                        const TargetOptions &Options,
                        Reloc::Model RM, CodeModel::Model CM,
                        CodeGenOpt::Level OL);

    virtual const NewcpuInstrInfo *getInstrInfo() const
    { return &InstrInfo; }

    virtual const InstrItineraryData *getInstrItineraryData() const
    {  return &InstrItins; }

    virtual const TargetFrameLowering *getFrameLowering() const
    { return &FrameLowering; }

    virtual const NewcpuSubtarget *getSubtargetImpl() const
    { return &Subtarget; }

    virtual const DataLayout *getDataLayout() const
    { return &DL;}

    virtual const NewcpuRegisterInfo *getRegisterInfo() const
    { return &InstrInfo.getRegisterInfo(); }

    virtual const NewcpuTargetLowering *getTargetLowering() const
    { return &TLInfo; }

    virtual const NewcpuSelectionDAGInfo* getSelectionDAGInfo() const
    { return &TSInfo; }

    const TargetIntrinsicInfo *getIntrinsicInfo() const
    { return &IntrinsicInfo; }

    // Pass Pipeline Configuration
    virtual TargetPassConfig *createPassConfig(PassManagerBase &PM);
  };
} // End llvm namespace

#endif
