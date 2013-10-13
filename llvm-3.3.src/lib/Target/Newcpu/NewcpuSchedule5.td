//===-- NewcpuSchedule5.td - Newcpu Scheduling Definitions -*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Newcpu instruction itineraries for the five stage pipeline.
//===----------------------------------------------------------------------===//
def NewcpuPipe5Itineraries : ProcessorItineraries<
  [Fetch,Decode,Execute,Memory,WriteBack], [], [

  // ALU instruction with one destination register and either two register
  // source operands or one register source operand and one immediate operand.
  // The instruction takes one cycle to execute in each of the stages. The
  // two source operands are read during the decode stage and the result is
                                                // ready after the execute stage.

  InstrItinData< IIC_ALU1,
               [ InstrStage<1,[Fetch]>    // one cycle in fetch stage
               , InstrStage<1,[Decode]>   // one cycle in decode stage
               , InstrStage<1,[Execute]>   // one cycle in execute stage
               , InstrStage<1,[WriteBack]>], // one cycle in write back stage
               [ 4                    // result ready after two cycles
               , 1                    // first operand read after one cycle
               , 1 ]>                 // second operand read after one cycle
]>;
