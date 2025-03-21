	.text
	.file	"my_module"
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	3, 0x0                          # -- Begin function main
.LCPI0_0:
	.quad	0x4010000000000000              # double 4
	.text
	.globl	main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	movabsq	$4611686018427387904, %rax      # imm = 0x4000000000000000
	movq	%rax, -8(%rsp)
	movabsq	$4607182418800017408, %rax      # imm = 0x3FF0000000000000
	movq	%rax, -16(%rsp)
	movsd	.LCPI0_0(%rip), %xmm0           # xmm0 = [4.0E+0,0.0E+0]
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.section	".note.GNU-stack","",@progbits
