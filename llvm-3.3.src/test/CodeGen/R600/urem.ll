;RUN: llc < %s -march=r600 -mcpu=redwood | FileCheck %s

;The code generated by urem is long and complex and may frequently change.
;The goal of this test is to make sure the ISel doesn't fail when it gets
;a v4i32 urem
;CHECK: CF_END

define void @test(<4 x i32> addrspace(1)* %out, <4 x i32> addrspace(1)* %in) {
  %b_ptr = getelementptr <4 x i32> addrspace(1)* %in, i32 1
  %a = load <4 x i32> addrspace(1) * %in
  %b = load <4 x i32> addrspace(1) * %b_ptr
  %result = urem <4 x i32> %a, %b
  store <4 x i32> %result, <4 x i32> addrspace(1)* %out
  ret void
}
