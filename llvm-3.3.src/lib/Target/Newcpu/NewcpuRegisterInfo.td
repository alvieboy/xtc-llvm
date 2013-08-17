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
  field bits<3> Num;
  let Namespace = "Newcpu";
}
// Special purpose registers
class NewcpuSReg<string n> : Register<n> {
  field bits<3> Num;
  let Namespace = "Newcpu";
}
// Direct access registers
class NewcpuDReg<string n> : Register<n> {
  field bits<0> Num;
  let Namespace = "Newcpu";
}

// Newcpu general purpose registers
class GPR<bits<3> num, string n> : NewcpuReg<n> {
  let Num = num;
}

// Newcpu direct access registers (Acc)
class AR<bits<0> num, string n> : NewcpuDReg<n> {
  let Num = num;
}

// Newcpu special purpose
class SPR<bits<3> num, string n> : NewcpuSReg<n> {
  let Num = num;
}

let Namespace = "Newcpu" in {
  // General Purpose Registers
  def R0  : GPR< 0,  "R0">,   DwarfRegNum<[0]>;
  def R1  : GPR< 1,  "R1">,   DwarfRegNum<[1]>;
  def R2  : GPR< 2,  "R2">,   DwarfRegNum<[2]>;
  def R3  : GPR< 3,  "R3">,   DwarfRegNum<[3]>;
  def R4  : GPR< 4,  "R4">,   DwarfRegNum<[4]>;
  def R5  : GPR< 5,  "R5">,   DwarfRegNum<[5]>;
  def R6  : GPR< 6,  "R6">,   DwarfRegNum<[6]>;
  def R7  : GPR< 7,  "R7">,   DwarfRegNum<[7]>;
  
  // Direct access Registers
  def A    : AR<0, "A">,      DwarfRegNum<[8]>;

  // Other regs
  def PC   : SPR<0, "PC">,    DwarfRegNum<[9]>;
  def BR   : SPR<1, "BR">,    DwarfRegNum<[10]>;
  def Y    : SPR<2, "Y">,     DwarfRegNum<[11]>;
  def F    : SPR<3, "F">,     DwarfRegNum<[12]>;
  def INTPC: SPR<4, "INTPC">, DwarfRegNum<[13]>;
  def INTM : SPR<5, "INTM">,  DwarfRegNum<[14]>;
}


def GPRegs : RegisterClass<"Newcpu", [i32], 32, (sequence "R%u", 0, 7)>;
def ARegs  : RegisterClass<"Newcpu", [i32], 32, (add A)>;
def SPRegs : RegisterClass<"Newcpu", [i32], 32, (add PC, BR, Y, F, INTPC, INTM)>;

def VRegs: RegisterClass<"Newcpu", [i32], 32, (add ARegs, GPRegs)>;
