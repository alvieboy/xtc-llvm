//===-- NewcpuRegisterInfo.td - MBlaze Register defs -------*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
//  Declarations that describe the Newcpu register file
//===----------------------------------------------------------------------===//

// We have banks of 8 registers each.
class NewcpuReg<string n> : Register<n> {
  field bits<4> Num;
  let Namespace = "Newcpu";
}
// Special purpose registers
class NewcpuSReg<string n> : Register<n> {
  field bits<3> Num;
  let Namespace = "Newcpu";
}

// Newcpu general purpose registers
class GPR<bits<4> num, string n> : NewcpuReg<n> {
  let Num = num;
}

// Newcpu special purpose
class SPR<bits<3> num, string n> : NewcpuSReg<n> {
  let Num = num;
}

let Namespace = "Newcpu" in {
  // General Purpose Registers
  def r0  : GPR< 0,  "r0">,   DwarfRegNum<[0]>;
  def r1  : GPR< 1,  "r1">,   DwarfRegNum<[1]>;
  def r2  : GPR< 2,  "r2">,   DwarfRegNum<[2]>;
  def r3  : GPR< 3,  "r3">,   DwarfRegNum<[3]>;
  def r4  : GPR< 4,  "r4">,   DwarfRegNum<[4]>;
  def r5  : GPR< 5,  "r5">,   DwarfRegNum<[5]>;
  def r6  : GPR< 6,  "r6">,   DwarfRegNum<[6]>;
  def r7  : GPR< 7,  "r7">,   DwarfRegNum<[7]>;

  def r8  : GPR< 0,  "r8">,   DwarfRegNum<[8]>;
  def r9  : GPR< 1,  "r9">,   DwarfRegNum<[9]>;
  def r10 : GPR< 2,  "r10">,   DwarfRegNum<[10]>;
  def r11 : GPR< 3,  "r11">,   DwarfRegNum<[11]>;
  def r12 : GPR< 4,  "r12">,   DwarfRegNum<[12]>;
  def r13 : GPR< 5,  "r13">,   DwarfRegNum<[13]>;
  def r14 : GPR< 6,  "r14">,   DwarfRegNum<[14]>;
  def r15 : GPR< 7,  "r15">,   DwarfRegNum<[15]>;
  
  // Other regs
  def PC   : SPR<0, "pc">,    DwarfRegNum<[16]>;
  def BR   : SPR<1, "br">,    DwarfRegNum<[17]>;
  def Y    : SPR<2, "y">,     DwarfRegNum<[18]>;
  def FLAGS : SPR<3, "FLAGS">,     DwarfRegNum<[19]>;
  def INTPC: SPR<4, "INTPC">, DwarfRegNum<[20]>;
  def INTM : SPR<5, "INTM">,  DwarfRegNum<[21]>;
}

def GPRegs : RegisterClass<"Newcpu", [i32], 32, (sequence "r%u", 0, 15)>;
def SPRegs : RegisterClass<"Newcpu", [i32], 32, (add PC, BR, Y, FLAGS, INTPC, INTM)> { let isAllocatable = 0; }
