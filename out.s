	.build_version macos, 15, 0
	.section	__TEXT,__text,regular,pure_instructions
	.globl	"_add_i64i64->i64"              ; -- Begin function add_i64i64->i64
	.p2align	2
"_add_i64i64->i64":                     ; @"add_i64i64->i64"
	.cfi_startproc
; %bb.0:                                ; %entry
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	stp	x0, x1, [sp], #16
	add	x0, x0, x1
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	_main_i64                       ; -- Begin function main_i64
	.p2align	2
_main_i64:                              ; @main_i64
	.cfi_startproc
; %bb.0:                                ; %entry
	sub	sp, sp, #144
	stp	x20, x19, [sp, #112]            ; 16-byte Folded Spill
	stp	x29, x30, [sp, #128]            ; 16-byte Folded Spill
	.cfi_def_cfa_offset 144
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	mov	w8, #2                          ; =0x2
Lloh0:
	adrp	x9, "_anon9223372036854775807_i64i64->i64"@PAGE
Lloh1:
	add	x9, x9, "_anon9223372036854775807_i64i64->i64"@PAGEOFF
Lloh2:
	adrp	x10, "_anon0_i64i64->i64"@PAGE
Lloh3:
	add	x10, x10, "_anon0_i64i64->i64"@PAGEOFF
	mov	w0, #2                          ; =0x2
	mov	w1, #3                          ; =0x3
	stp	x8, x8, [sp, #16]
	stp	x8, x9, [sp, #40]
	str	x9, [sp, #32]
	stp	x8, x10, [sp, #72]
	stp	x8, x10, [sp, #56]
	blr	x9
	mov	x19, x0
	ldp	x0, x8, [sp, #56]
	mov	w1, #4                          ; =0x4
	str	x19, [sp, #96]
	blr	x8
	str	x0, [sp, #104]
	add	x8, x19, x0
Lloh4:
	adrp	x0, l_str@PAGE
Lloh5:
	add	x0, x0, l_str@PAGEOFF
	str	x8, [sp, #88]
	str	x8, [sp]
	bl	_printf
	ldp	x29, x30, [sp, #128]            ; 16-byte Folded Reload
	ldr	x0, [sp, #88]
	ldp	x20, x19, [sp, #112]            ; 16-byte Folded Reload
	add	sp, sp, #144
	ret
	.loh AdrpAdd	Lloh4, Lloh5
	.loh AdrpAdd	Lloh2, Lloh3
	.loh AdrpAdd	Lloh0, Lloh1
	.cfi_endproc
                                        ; -- End function
	.globl	"_anon9223372036854775807_i64i64->i64" ; -- Begin function anon9223372036854775807_i64i64->i64
	.p2align	2
"_anon9223372036854775807_i64i64->i64": ; @"anon9223372036854775807_i64i64->i64"
	.cfi_startproc
; %bb.0:                                ; %entry
	sub	sp, sp, #48
	stp	x29, x30, [sp, #32]             ; 16-byte Folded Spill
	.cfi_def_cfa_offset 48
	.cfi_offset w30, -8
	.cfi_offset w29, -16
Lloh6:
	adrp	x8, _add_ptr@PAGE
	stp	x0, x1, [sp, #8]
Lloh7:
	ldr	x8, [x8, _add_ptr@PAGEOFF]
	blr	x8
	ldp	x29, x30, [sp, #32]             ; 16-byte Folded Reload
	str	x0, [sp, #24]
	add	sp, sp, #48
	ret
	.loh AdrpLdr	Lloh6, Lloh7
	.cfi_endproc
                                        ; -- End function
	.globl	"_anon0_i64i64->i64"            ; -- Begin function anon0_i64i64->i64
	.p2align	2
"_anon0_i64i64->i64":                   ; @"anon0_i64i64->i64"
	.cfi_startproc
; %bb.0:                                ; %entry
	sub	sp, sp, #48
	stp	x29, x30, [sp, #32]             ; 16-byte Folded Spill
	.cfi_def_cfa_offset 48
	.cfi_offset w30, -8
	.cfi_offset w29, -16
Lloh8:
	adrp	x8, _add_ptr@PAGE
	stp	x0, x1, [sp, #8]
Lloh9:
	ldr	x8, [x8, _add_ptr@PAGEOFF]
	blr	x8
	ldp	x29, x30, [sp, #32]             ; 16-byte Folded Reload
	str	x0, [sp, #24]
	add	sp, sp, #48
	ret
	.loh AdrpLdr	Lloh8, Lloh9
	.cfi_endproc
                                        ; -- End function
	.globl	_main                           ; -- Begin function main
	.p2align	2
_main:                                  ; @main
	.cfi_startproc
; %bb.0:                                ; %entry
	stp	x29, x30, [sp, #-16]!           ; 16-byte Folded Spill
	.cfi_def_cfa_offset 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	bl	_main_i64
                                        ; kill: def $w0 killed $w0 killed $x0
	ldp	x29, x30, [sp], #16             ; 16-byte Folded Reload
	ret
	.cfi_endproc
                                        ; -- End function
	.section	__DATA,__data
	.globl	_add_ptr                        ; @add_ptr
	.p2align	3, 0x0
_add_ptr:
	.quad	"_add_i64i64->i64"

	.globl	_main_ptr                       ; @main_ptr
	.p2align	3, 0x0
_main_ptr:
	.quad	_main_i64

	.section	__TEXT,__cstring,cstring_literals
l_str:                                  ; @str
	.asciz	"Result: %d\n"

.subsections_via_symbols
