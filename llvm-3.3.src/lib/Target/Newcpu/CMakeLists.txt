set(LLVM_TARGET_DEFINITIONS Newcpu.td)

tablegen(LLVM NewcpuGenRegisterInfo.inc -gen-register-info)
tablegen(LLVM NewcpuGenInstrInfo.inc -gen-instr-info)
tablegen(LLVM NewcpuGenCodeEmitter.inc -gen-emitter)
tablegen(LLVM NewcpuGenAsmWriter.inc -gen-asm-writer)
tablegen(LLVM NewcpuGenAsmMatcher.inc -gen-asm-matcher)
tablegen(LLVM NewcpuGenDAGISel.inc -gen-dag-isel)
tablegen(LLVM NewcpuGenCallingConv.inc -gen-callingconv)
tablegen(LLVM NewcpuGenSubtargetInfo.inc -gen-subtarget)
tablegen(LLVM NewcpuGenIntrinsics.inc -gen-tgt-intrinsic)
add_public_tablegen_target(NewcpuCommonTableGen)

add_llvm_target(NewcpuCodeGen
  NewcpuDelaySlotFiller.cpp
  NewcpuInstrInfo.cpp
  NewcpuISelDAGToDAG.cpp
  NewcpuISelLowering.cpp
  NewcpuFrameLowering.cpp
  NewcpuMachineFunction.cpp
  NewcpuRegisterInfo.cpp
  NewcpuSubtarget.cpp
  NewcpuTargetMachine.cpp
  NewcpuTargetObjectFile.cpp
  NewcpuIntrinsicInfo.cpp
  NewcpuSelectionDAGInfo.cpp
  NewcpuAsmPrinter.cpp
  NewcpuMCInstLower.cpp
  )

add_dependencies(LLVMNewcpuCodeGen intrinsics_gen)

add_subdirectory(AsmParser)
add_subdirectory(Disassembler)
add_subdirectory(InstPrinter)
add_subdirectory(TargetInfo)
add_subdirectory(MCTargetDesc)
