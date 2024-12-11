	.global _start
	.weak main
_start:
	call main
	movl %eax, %ebx
	movl $1, %eax
	int $0x80
