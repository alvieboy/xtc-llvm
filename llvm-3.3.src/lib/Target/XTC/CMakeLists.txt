set(LLVM_TARGET_DEFINITIONS XTC.td)

tablegen(LLVM XTCGenRegisterInfo.inc -gen-register-info)
tablegen(LLVM XTCGenInstrInfo.inc -gen-instr-info)
tablegen(LLVM XTCGenCodeEmitter.inc -gen-emitter)
tablegen(LLVM XTCGenAsmWriter.inc -gen-asm-writer)
tablegen(LLVM XTCGenAsmMatcher.inc -gen-asm-matcher)
tablegen(LLVM XTCGenDAGISel.inc -gen-dag-isel)
tablegen(LLVM XTCGenCallingConv.inc -gen-callingconv)
tablegen(LLVM XTCGenSubtargetInfo.inc -gen-subtarget)
tablegen(LLVM XTCGenIntrinsics.inc -gen-tgt-intrinsic)
add_public_tablegen_target(XTCCommonTableGen)

add_llvm_target(XTCCodeGen
  XTCDelaySlotFiller.cpp
  XTCInstrInfo.cpp
  XTCISelDAGToDAG.cpp
  XTCISelLowering.cpp
  XTCFrameLowering.cpp
  XTCMachineFunction.cpp
  XTCRegisterInfo.cpp
  XTCSubtarget.cpp
  XTCTargetMachine.cpp
  XTCTargetObjectFile.cpp
  XTCIntrinsicInfo.cpp
  XTCSelectionDAGInfo.cpp
  XTCAsmPrinter.cpp
  XTCMCInstLower.cpp
  )

add_dependencies(LLVMXTCCodeGen intrinsics_gen)

add_subdirectory(AsmParser)
add_subdirectory(Disassembler)
add_subdirectory(InstPrinter)
add_subdirectory(TargetInfo)
add_subdirectory(MCTargetDesc)
