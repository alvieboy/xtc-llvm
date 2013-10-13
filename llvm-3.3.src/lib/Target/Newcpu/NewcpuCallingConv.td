//===- NewcpuCallingConv.td - Calling Conventions for Newcpu -*- tablegen -*-=//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
// This describes the calling conventions for Newcpu architecture.
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Newcpu ABI Calling Convention
//===----------------------------------------------------------------------===//

def RetCC_Newcpu : CallingConv<[
  // i32 are returned in registers R1
  CCIfType<[i32], CCAssignToReg<[r1]>>
]>;

def CC_Newcpu : CallingConv<[
  CCIfType<[i32], CCCustom<"CC_Newcpu_AssignReg">>,
  CCIfType<[i32], CCAssignToStack<4, 4>>
]>;
