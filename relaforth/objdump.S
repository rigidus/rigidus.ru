
jft:     file format elf64-x86-64


Disassembly of section .gnu.hash:

00000000004000b0 <.gnu.hash>:
  4000b0:	01 00                	add    %eax,(%rax)
  4000b2:	00 00                	add    %al,(%rax)
  4000b4:	01 00                	add    %eax,(%rax)
  4000b6:	00 00                	add    %al,(%rax)
  4000b8:	01 00                	add    %eax,(%rax)
	...

Disassembly of section .dynsym:

00000000004000d0 <.dynsym>:
	...

Disassembly of section .dynstr:

00000000004000e8 <.dynstr>:
	...

Disassembly of section .text:

0000000000401000 <_start>:
  401000:	90                   	nop
  401001:	90                   	nop
  401002:	90                   	nop
  401003:	e8 38 01 00 00       	callq  401140 <forth_entry>

0000000000401008 <v0>:
	...

0000000000401010 <argc>:
	...

0000000000401018 <argv>:
	...

0000000000401020 <cold_start>:
	...

0000000000401028 <name_STATE>:
	...
  401030:	05 53 54 41 54       	add    $0x54415453,%eax
  401035:	45                   	rex.RB
  401036:	66 90                	xchg   %ax,%ax

0000000000401038 <STATE>:
  401038:	38 00                	cmp    %al,(%rax)
  40103a:	00 00                	add    %al,(%rax)
  40103c:	00 00                	add    %al,(%rax)
	...

0000000000401040 <code_STATE>:
  401040:	41 ff b7 50 00 00 00 	pushq  0x50(%r15)
  401047:	48 ad                	lods   %ds:(%rsi),%rax
  401049:	49 8d 04 07          	lea    (%r15,%rax,1),%rax
  40104d:	4c 8b 28             	mov    (%rax),%r13
  401050:	4f 8d 2c 2f          	lea    (%r15,%r13,1),%r13
  401054:	41 ff e5             	jmpq   *%r13
  401057:	90                   	nop

0000000000401058 <var_STATE>:
	...

0000000000401060 <name_HERE>:
  401060:	20 00                	and    %al,(%rax)
  401062:	00 00                	add    %al,(%rax)
  401064:	00 00                	add    %al,(%rax)
  401066:	00 00                	add    %al,(%rax)
  401068:	04 48                	add    $0x48,%al
  40106a:	45 52                	rex.RB push %r10
  40106c:	45 0f 1f 00          	rex.RB nopl (%r8)

0000000000401070 <HERE>:
  401070:	70 00                	jo     401072 <HERE+0x2>
  401072:	00 00                	add    %al,(%rax)
  401074:	00 00                	add    %al,(%rax)
	...

0000000000401078 <code_HERE>:
  401078:	41 ff b7 88 00 00 00 	pushq  0x88(%r15)
  40107f:	48 ad                	lods   %ds:(%rsi),%rax
  401081:	49 8d 04 07          	lea    (%r15,%rax,1),%rax
  401085:	4c 8b 28             	mov    (%rax),%r13
  401088:	4f 8d 2c 2f          	lea    (%r15,%r13,1),%r13
  40108c:	41 ff e5             	jmpq   *%r13
  40108f:	90                   	nop

0000000000401090 <var_HERE>:
	...

0000000000401098 <name_LATEST>:
  401098:	58                   	pop    %rax
  401099:	00 00                	add    %al,(%rax)
  40109b:	00 00                	add    %al,(%rax)
  40109d:	00 00                	add    %al,(%rax)
  40109f:	00 06                	add    %al,(%rsi)
  4010a1:	4c                   	rex.WR
  4010a2:	41 54                	push   %r12
  4010a4:	45 53                	rex.RB push %r11
  4010a6:	54                   	push   %rsp
  4010a7:	90                   	nop

00000000004010a8 <LATEST>:
  4010a8:	a8 00                	test   $0x0,%al
  4010aa:	00 00                	add    %al,(%rax)
  4010ac:	00 00                	add    %al,(%rax)
	...

00000000004010b0 <code_LATEST>:
  4010b0:	41 ff b7 c0 00 00 00 	pushq  0xc0(%r15)
  4010b7:	48 ad                	lods   %ds:(%rsi),%rax
  4010b9:	49 8d 04 07          	lea    (%r15,%rax,1),%rax
  4010bd:	4c 8b 28             	mov    (%rax),%r13
  4010c0:	4f 8d 2c 2f          	lea    (%r15,%r13,1),%r13
  4010c4:	41 ff e5             	jmpq   *%r13
  4010c7:	90                   	nop

00000000004010c8 <var_LATEST>:
  4010c8:	f8                   	clc    
  4010c9:	3f                   	(bad)  
  4010ca:	01 00                	add    %eax,(%rax)
  4010cc:	00 00                	add    %al,(%rax)
	...

00000000004010d0 <name_SZ>:
  4010d0:	90                   	nop
  4010d1:	00 00                	add    %al,(%rax)
  4010d3:	00 00                	add    %al,(%rax)
  4010d5:	00 00                	add    %al,(%rax)
  4010d7:	00 02                	add    %al,(%rdx)
  4010d9:	53                   	push   %rbx
  4010da:	30 0f                	xor    %cl,(%rdi)
  4010dc:	1f                   	(bad)  
  4010dd:	44 00 00             	add    %r8b,(%rax)

00000000004010e0 <SZ>:
  4010e0:	e0 00                	loopne 4010e2 <SZ+0x2>
  4010e2:	00 00                	add    %al,(%rax)
  4010e4:	00 00                	add    %al,(%rax)
	...

00000000004010e8 <code_SZ>:
  4010e8:	41 ff b7 f8 00 00 00 	pushq  0xf8(%r15)
  4010ef:	48 ad                	lods   %ds:(%rsi),%rax
  4010f1:	49 8d 04 07          	lea    (%r15,%rax,1),%rax
  4010f5:	4c 8b 28             	mov    (%rax),%r13
  4010f8:	4f 8d 2c 2f          	lea    (%r15,%r13,1),%r13
  4010fc:	41 ff e5             	jmpq   *%r13
  4010ff:	90                   	nop

0000000000401100 <var_S0>:
	...

0000000000401108 <name_BASE>:
  401108:	c8 00 00 00          	enterq $0x0,$0x0
  40110c:	00 00                	add    %al,(%rax)
  40110e:	00 00                	add    %al,(%rax)
  401110:	04 42                	add    $0x42,%al
  401112:	41 53                	push   %r11
  401114:	45 0f 1f 00          	rex.RB nopl (%r8)

0000000000401118 <BASE>:
  401118:	18 01                	sbb    %al,(%rcx)
  40111a:	00 00                	add    %al,(%rax)
  40111c:	00 00                	add    %al,(%rax)
	...

0000000000401120 <code_BASE>:
  401120:	41 ff b7 30 01 00 00 	pushq  0x130(%r15)
  401127:	48 ad                	lods   %ds:(%rsi),%rax
  401129:	49 8d 04 07          	lea    (%r15,%rax,1),%rax
  40112d:	4c 8b 28             	mov    (%rax),%r13
  401130:	4f 8d 2c 2f          	lea    (%r15,%r13,1),%r13
  401134:	41 ff e5             	jmpq   *%r13
  401137:	90                   	nop

0000000000401138 <var_BASE>:
  401138:	0a 00                	or     (%rax),%al
  40113a:	00 00                	add    %al,(%rax)
  40113c:	00 00                	add    %al,(%rax)
	...

0000000000401140 <forth_entry>:
  401140:	41 5f                	pop    %r15
  401142:	49 89 a7 f8 00 00 00 	mov    %rsp,0xf8(%r15)
  401149:	4c 8b 34 24          	mov    (%rsp),%r14
  40114d:	4d 89 77 08          	mov    %r14,0x8(%r15)
  401151:	4c 8b 74 24 08       	mov    0x8(%rsp),%r14
  401156:	4d 89 77 10          	mov    %r14,0x10(%r15)
  40115a:	fc                   	cld    
  40115b:	49 8d af f8 2f 00 00 	lea    0x2ff8(%r15),%rbp
  401162:	49 8d 87 f8 3f 00 00 	lea    0x3ff8(%r15),%rax
  401169:	49 89 87 88 00 00 00 	mov    %rax,0x88(%r15)
  401170:	48 c7 c0 48 07 00 00 	mov    $0x748,%rax
  401177:	49 89 47 18          	mov    %rax,0x18(%r15)
  40117b:	49 8d 77 18          	lea    0x18(%r15),%rsi
  40117f:	48 ad                	lods   %ds:(%rsi),%rax
  401181:	49 8d 04 07          	lea    (%r15,%rax,1),%rax
  401185:	4c 8b 28             	mov    (%rax),%r13
  401188:	4f 8d 2c 2f          	lea    (%r15,%r13,1),%r13
  40118c:	41 ff e5             	jmpq   *%r13

000000000040118f <exit>:
  40118f:	48 c7 c0 3c 00 00 00 	mov    $0x3c,%rax
  401196:	48 c7 c7 00 00 00 00 	mov    $0x0,%rdi
  40119d:	0f 05                	syscall 
  40119f:	90                   	nop

00000000004011a0 <name_VERSION>:
  4011a0:	00 01                	add    %al,(%rcx)
  4011a2:	00 00                	add    %al,(%rax)
  4011a4:	00 00                	add    %al,(%rax)
  4011a6:	00 00                	add    %al,(%rax)
  4011a8:	07                   	(bad)  
  4011a9:	56                   	push   %rsi
  4011aa:	45 52                	rex.RB push %r10
  4011ac:	53                   	push   %rbx
  4011ad:	49                   	rex.WB
  4011ae:	4f                   	rex.WRXB
  4011af:	4e               	rex.WRX mov $0x1,%al

00000000004011b0 <VERSION>:
  4011b0:	b0 01                	mov    $0x1,%al
  4011b2:	00 00                	add    %al,(%rax)
  4011b4:	00 00                	add    %al,(%rax)
	...

00000000004011b8 <code_VERSION>:
  4011b8:	6a 2f                	pushq  $0x2f
  4011ba:	48 ad                	lods   %ds:(%rsi),%rax
  4011bc:	49 8d 04 07          	lea    (%r15,%rax,1),%rax
  4011c0:	4c 8b 28             	mov    (%rax),%r13
  4011c3:	4f 8d 2c 2f          	lea    (%r15,%r13,1),%r13
  4011c7:	41 ff e5             	jmpq   *%r13
  4011ca:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

00000000004011d0 <name_RZ>:
  4011d0:	98                   	cwtl   
  4011d1:	01 00                	add    %eax,(%rax)
  4011d3:	00 00                	add    %al,(%rax)
  4011d5:	00 00                	add    %al,(%rax)
  4011d7:	00 02                	add    %al,(%rdx)
  4011d9:	52                   	push   %rdx
  4011da:	30 0f                	xor    %cl,(%rdi)
  4011dc:	1f                   	(bad)  
  4011dd:	44 00 00             	add    %r8b,(%rax)

00000000004011e0 <RZ>:
  4011e0:	e0 01                	loopne 4011e3 <RZ+0x3>
  4011e2:	00 00                	add    %al,(%rax)
  4011e4:	00 00                	add    %al,(%rax)
	...

00000000004011e8 <code_RZ>:
  4011e8:	41 ff b7 f8 2f 00 00 	pushq  0x2ff8(%r15)
  4011ef:	48 ad                	lods   %ds:(%rsi),%rax
  4011f1:	49 8d 04 07          	lea    (%r15,%rax,1),%rax
  4011f5:	4c 8b 28             	mov    (%rax),%r13
  4011f8:	4f 8d 2c 2f          	lea    (%r15,%r13,1),%r13
  4011fc:	41 ff e5             	jmpq   *%r13
  4011ff:	90                   	nop

0000000000401200 <DOCOL>:
  401200:	48 8d 6d f8          	lea    -0x8(%rbp),%rbp
  401204:	48 89 75 00          	mov    %rsi,0x0(%rbp)
  401208:	48 8d 70 08          	lea    0x8(%rax),%rsi
  40120c:	48 ad                	lods   %ds:(%rsi),%rax
  40120e:	49 8d 04 07          	lea    (%r15,%rax,1),%rax
  401212:	4c 8b 28             	mov    (%rax),%r13
  401215:	4f 8d 2c 2f          	lea    (%r15,%r13,1),%r13
  401219:	41 ff e5             	jmpq   *%r13
  40121c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000401220 <name_EXIT>:
  401220:	c8 01 00 00          	enterq $0x1,$0x0
  401224:	00 00                	add    %al,(%rax)
  401226:	00 00                	add    %al,(%rax)
  401228:	04 45                	add    $0x45,%al
  40122a:	58                   	pop    %rax
  40122b:	49 54                	rex.WB push %r12
  40122d:	0f 1f 00             	nopl   (%rax)

0000000000401230 <EXIT>:
  401230:	30 02                	xor    %al,(%rdx)
  401232:	00 00                	add    %al,(%rax)
  401234:	00 00                	add    %al,(%rax)
	...

0000000000401238 <code_EXIT>:
  401238:	48 8b 75 00          	mov    0x0(%rbp),%rsi
  40123c:	48 8d 6d 08          	lea    0x8(%rbp),%rbp
  401240:	48 ad                	lods   %ds:(%rsi),%rax
  401242:	49 8d 04 07          	lea    (%r15,%rax,1),%rax
  401246:	4c 8b 28             	mov    (%rax),%r13
  401249:	4f 8d 2c 2f          	lea    (%r15,%r13,1),%r13
  40124d:	41 ff e5             	jmpq   *%r13

0000000000401250 <name_RSPSTORE>:
  401250:	18 02                	sbb    %al,(%rdx)
  401252:	00 00                	add    %al,(%rax)
  401254:	00 00                	add    %al,(%rax)
  401256:	00 00                	add    %al,(%rax)
  401258:	04 52                	add    $0x52,%al
  40125a:	53                   	push   %rbx
  40125b:	50                   	push   %rax
  40125c:	21 0f                	and    %ecx,(%rdi)
  40125e:	1f                   	(bad)  
	...

0000000000401260 <RSPSTORE>:
  401260:	60                   	(bad)  
  401261:	02 00                	add    (%rax),%al
  401263:	00 00                	add    %al,(%rax)
  401265:	00 00                	add    %al,(%rax)
	...

0000000000401268 <code_RSPSTORE>:
  401268:	5d                   	pop    %rbp
  401269:	48 ad                	lods   %ds:(%rsi),%rax
  40126b:	49 8d 04 07          	lea    (%r15,%rax,1),%rax
  40126f:	4c 8b 28             	mov    (%rax),%r13
  401272:	4f 8d 2c 2f          	lea    (%r15,%r13,1),%r13
  401276:	41 ff e5             	jmpq   *%r13
  401279:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000401280 <name_LIT>:
  401280:	48 02 00             	rex.W add (%rax),%al
  401283:	00 00                	add    %al,(%rax)
  401285:	00 00                	add    %al,(%rax)
  401287:	00 03                	add    %al,(%rbx)
  401289:	4c                   	rex.WR
  40128a:	49 54                	rex.WB push %r12
  40128c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000401290 <LIT>:
  401290:	90                   	nop
  401291:	02 00                	add    (%rax),%al
  401293:	00 00                	add    %al,(%rax)
  401295:	00 00                	add    %al,(%rax)
	...

0000000000401298 <code_LIT>:
  401298:	48 ad                	lods   %ds:(%rsi),%rax
  40129a:	50                   	push   %rax
  40129b:	48 ad                	lods   %ds:(%rsi),%rax
  40129d:	49 8d 04 07          	lea    (%r15,%rax,1),%rax
  4012a1:	4c 8b 28             	mov    (%rax),%r13
  4012a4:	4f 8d 2c 2f          	lea    (%r15,%r13,1),%r13
  4012a8:	41 ff e5             	jmpq   *%r13
  4012ab:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

00000000004012b0 <name_KEY>:
  4012b0:	78 02                	js     4012b4 <name_KEY+0x4>
  4012b2:	00 00                	add    %al,(%rax)
  4012b4:	00 00                	add    %al,(%rax)
  4012b6:	00 00                	add    %al,(%rax)
  4012b8:	03 4b 45             	add    0x45(%rbx),%ecx
  4012bb:	59                   	pop    %rcx
  4012bc:	0f 1f 40 00          	nopl   0x0(%rax)

00000000004012c0 <KEY>:
  4012c0:	c0 02 00             	rolb   $0x0,(%rdx)
  4012c3:	00 00                	add    %al,(%rax)
  4012c5:	00 00                	add    %al,(%rax)
	...

00000000004012c8 <code_KEY>:
  4012c8:	e8 11 00 00 00       	callq  4012de <_KEY>
  4012cd:	50                   	push   %rax
  4012ce:	48 ad                	lods   %ds:(%rsi),%rax
  4012d0:	49 8d 04 07          	lea    (%r15,%rax,1),%rax
  4012d4:	4c 8b 28             	mov    (%rax),%r13
  4012d7:	4f 8d 2c 2f          	lea    (%r15,%r13,1),%r13
  4012db:	41 ff e5             	jmpq   *%r13

00000000004012de <_KEY>:
  4012de:	49 8b 9f 40 03 00 00 	mov    0x340(%r15),%rbx
  4012e5:	49 3b 9f 48 03 00 00 	cmp    0x348(%r15),%rbx
  4012ec:	7d 10                	jge    4012fe <_KEY+0x20>
  4012ee:	48 31 c0             	xor    %rax,%rax
  4012f1:	8a 03                	mov    (%rbx),%al
  4012f3:	48 ff c3             	inc    %rbx
  4012f6:	49 89 9f 40 03 00 00 	mov    %rbx,0x340(%r15)
  4012fd:	c3                   	retq   
  4012fe:	56                   	push   %rsi
  4012ff:	57                   	push   %rdi
  401300:	48 c7 c7 00 00 00 00 	mov    $0x0,%rdi
  401307:	49 8d b7 f8 2f 00 00 	lea    0x2ff8(%r15),%rsi
  40130e:	49 89 b7 40 03 00 00 	mov    %rsi,0x340(%r15)
  401315:	48 c7 c2 00 10 00 00 	mov    $0x1000,%rdx
  40131c:	48 c7 c0 00 00 00 00 	mov    $0x0,%rax
  401323:	0f 05                	syscall 
  401325:	48 85 c0             	test   %rax,%rax
  401328:	76 0e                	jbe    401338 <_KEY+0x5a>
  40132a:	48 01 c6             	add    %rax,%rsi
  40132d:	49 89 b7 48 03 00 00 	mov    %rsi,0x348(%r15)
  401334:	5f                   	pop    %rdi
  401335:	5e                   	pop    %rsi
  401336:	eb a6                	jmp    4012de <_KEY>
  401338:	5f                   	pop    %rdi
  401339:	5e                   	pop    %rsi
  40133a:	48 c7 c0 3c 00 00 00 	mov    $0x3c,%rax
  401341:	48 31 ff             	xor    %rdi,%rdi
  401344:	0f 05                	syscall 
  401346:	66 90                	xchg   %ax,%ax

0000000000401348 <currkey>:
  401348:	f8                   	clc    
  401349:	2f                   	(bad)  
  40134a:	00 00                	add    %al,(%rax)
  40134c:	00 00                	add    %al,(%rax)
	...

0000000000401350 <bufftop>:
  401350:	f8                   	clc    
  401351:	2f                   	(bad)  
  401352:	00 00                	add    %al,(%rax)
  401354:	00 00                	add    %al,(%rax)
	...

0000000000401358 <name_WORD>:
  401358:	a8 02                	test   $0x2,%al
  40135a:	00 00                	add    %al,(%rax)
  40135c:	00 00                	add    %al,(%rax)
  40135e:	00 00                	add    %al,(%rax)
  401360:	04 57                	add    $0x57,%al
  401362:	4f 52                	rex.WRXB push %r10
  401364:	44 0f 1f 00          	rex.R nopl (%rax)

0000000000401368 <WORD>:
  401368:	68 03 00 00 00       	pushq  $0x3
  40136d:	00 00                	add    %al,(%rax)
	...

0000000000401370 <code_WORD>:
  401370:	e8 12 00 00 00       	callq  401387 <_WORD>
  401375:	57                   	push   %rdi
  401376:	51                   	push   %rcx
  401377:	48 ad                	lods   %ds:(%rsi),%rax
  401379:	49 8d 04 07          	lea    (%r15,%rax,1),%rax
  40137d:	4c 8b 28             	mov    (%rax),%r13
  401380:	4f 8d 2c 2f          	lea    (%r15,%r13,1),%r13
  401384:	41 ff e5             	jmpq   *%r13

0000000000401387 <_WORD>:
  401387:	e8 52 ff ff ff       	callq  4012de <_KEY>
  40138c:	3c 5c                	cmp    $0x5c,%al
  40138e:	74 23                	je     4013b3 <_WORD+0x2c>
  401390:	3c 20                	cmp    $0x20,%al
  401392:	76 f3                	jbe    401387 <_WORD>
  401394:	49 8d bf b6 03 00 00 	lea    0x3b6(%r15),%rdi
  40139b:	aa                   	stos   %al,%es:(%rdi)
  40139c:	e8 3d ff ff ff       	callq  4012de <_KEY>
  4013a1:	3c 20                	cmp    $0x20,%al
  4013a3:	77 f6                	ja     40139b <_WORD+0x14>
  4013a5:	48 89 f9             	mov    %rdi,%rcx
  4013a8:	49 8d bf b6 03 00 00 	lea    0x3b6(%r15),%rdi
  4013af:	48 29 f9             	sub    %rdi,%rcx
  4013b2:	c3                   	retq   
  4013b3:	e8 26 ff ff ff       	callq  4012de <_KEY>
  4013b8:	3c 0a                	cmp    $0xa,%al
  4013ba:	75 f7                	jne    4013b3 <_WORD+0x2c>
  4013bc:	eb c9                	jmp    401387 <_WORD>

00000000004013be <word_buffer>:
	...
  4013de:	66 90                	xchg   %ax,%ax

00000000004013e0 <name_NUMBER>:
  4013e0:	50                   	push   %rax
  4013e1:	03 00                	add    (%rax),%eax
  4013e3:	00 00                	add    %al,(%rax)
  4013e5:	00 00                	add    %al,(%rax)
  4013e7:	00 06                	add    %al,(%rsi)
  4013e9:	4e 55                	rex.WRX push %rbp
  4013eb:	4d                   	rex.WRB
  4013ec:	42                   	rex.X
  4013ed:	45 52                	rex.RB push %r10
  4013ef:	90                   	nop

00000000004013f0 <NUMBER>:
  4013f0:	f0 03 00             	lock add (%rax),%eax
  4013f3:	00 00                	add    %al,(%rax)
  4013f5:	00 00                	add    %al,(%rax)
	...

00000000004013f8 <code_NUMBER>:
  4013f8:	59                   	pop    %rcx
  4013f9:	5f                   	pop    %rdi
  4013fa:	e8 12 00 00 00       	callq  401411 <_NUMBER>
  4013ff:	50                   	push   %rax
  401400:	51                   	push   %rcx
  401401:	48 ad                	lods   %ds:(%rsi),%rax
  401403:	49 8d 04 07          	lea    (%r15,%rax,1),%rax
  401407:	4c 8b 28             	mov    (%rax),%r13
  40140a:	4f 8d 2c 2f          	lea    (%r15,%r13,1),%r13
  40140e:	41 ff e5             	jmpq   *%r13

0000000000401411 <_NUMBER>:
  401411:	48 31 c0             	xor    %rax,%rax
  401414:	48 31 db             	xor    %rbx,%rbx
  401417:	48 85 c9             	test   %rcx,%rcx
  40141a:	74 52                	je     40146e <_NUMBER+0x5d>
  40141c:	49 8b 97 30 01 00 00 	mov    0x130(%r15),%rdx
  401423:	8a 1f                	mov    (%rdi),%bl
  401425:	48 ff c7             	inc    %rdi
  401428:	50                   	push   %rax
  401429:	80 fb 2d             	cmp    $0x2d,%bl
  40142c:	75 19                	jne    401447 <_NUMBER+0x36>
  40142e:	58                   	pop    %rax
  40142f:	53                   	push   %rbx
  401430:	48 ff c9             	dec    %rcx
  401433:	75 09                	jne    40143e <_NUMBER+0x2d>
  401435:	5b                   	pop    %rbx
  401436:	48 c7 c1 01 00 00 00 	mov    $0x1,%rcx
  40143d:	c3                   	retq   
  40143e:	48 0f af c2          	imul   %rdx,%rax
  401442:	8a 1f                	mov    (%rdi),%bl
  401444:	48 ff c7             	inc    %rdi
  401447:	80 eb 30             	sub    $0x30,%bl
  40144a:	72 19                	jb     401465 <_NUMBER+0x54>
  40144c:	80 fb 0a             	cmp    $0xa,%bl
  40144f:	72 08                	jb     401459 <_NUMBER+0x48>
  401451:	80 eb 11             	sub    $0x11,%bl
  401454:	72 0f                	jb     401465 <_NUMBER+0x54>
  401456:	80 c3 0a             	add    $0xa,%bl
  401459:	38 d3                	cmp    %dl,%bl
  40145b:	7d 08                	jge    401465 <_NUMBER+0x54>
  40145d:	48 01 d8             	add    %rbx,%rax
  401460:	48 ff c9             	dec    %rcx
  401463:	75 d9                	jne    40143e <_NUMBER+0x2d>
  401465:	5b                   	pop    %rbx
  401466:	48 85 db             	test   %rbx,%rbx
  401469:	74 03                	je     40146e <_NUMBER+0x5d>
  40146b:	48 f7 d8             	neg    %rax
  40146e:	c3                   	retq   
  40146f:	90                   	nop

0000000000401470 <name_FIND>:
  401470:	d8 03                	fadds  (%rbx)
  401472:	00 00                	add    %al,(%rax)
  401474:	00 00                	add    %al,(%rax)
  401476:	00 00                	add    %al,(%rax)
  401478:	04 46                	add    $0x46,%al
  40147a:	49                   	rex.WB
  40147b:	4e                   	rex.WRX
  40147c:	44 0f 1f 00          	rex.R nopl (%rax)

0000000000401480 <FIND>:
  401480:	80 04 00 00          	addb   $0x0,(%rax,%rax,1)
  401484:	00 00                	add    %al,(%rax)
	...

0000000000401488 <code_FIND>:
  401488:	59                   	pop    %rcx
  401489:	5f                   	pop    %rdi
  40148a:	e8 11 00 00 00       	callq  4014a0 <_FIND>
  40148f:	50                   	push   %rax
  401490:	48 ad                	lods   %ds:(%rsi),%rax
  401492:	49 8d 04 07          	lea    (%r15,%rax,1),%rax
  401496:	4c 8b 28             	mov    (%rax),%r13
  401499:	4f 8d 2c 2f          	lea    (%r15,%r13,1),%r13
  40149d:	41 ff e5             	jmpq   *%r13

00000000004014a0 <_FIND>:
  4014a0:	56                   	push   %rsi
  4014a1:	49 8b 97 c0 00 00 00 	mov    0xc0(%r15),%rdx
  4014a8:	48 85 d2             	test   %rdx,%rdx
  4014ab:	74 26                	je     4014d3 <_FIND+0x33>
  4014ad:	48 31 c0             	xor    %rax,%rax
  4014b0:	42 8a 44 3a 08       	mov    0x8(%rdx,%r15,1),%al
  4014b5:	24 3f                	and    $0x3f,%al
  4014b7:	38 c8                	cmp    %cl,%al
  4014b9:	75 12                	jne    4014cd <_FIND+0x2d>
  4014bb:	51                   	push   %rcx
  4014bc:	57                   	push   %rdi
  4014bd:	4a 8d 74 3a 09       	lea    0x9(%rdx,%r15,1),%rsi
  4014c2:	f3 a6                	repz cmpsb %es:(%rdi),%ds:(%rsi)
  4014c4:	5f                   	pop    %rdi
  4014c5:	59                   	pop    %rcx
  4014c6:	75 05                	jne    4014cd <_FIND+0x2d>
  4014c8:	5e                   	pop    %rsi
  4014c9:	48 89 d0             	mov    %rdx,%rax
  4014cc:	c3                   	retq   
  4014cd:	4a 8b 14 3a          	mov    (%rdx,%r15,1),%rdx
  4014d1:	eb d5                	jmp    4014a8 <_FIND+0x8>
  4014d3:	5e                   	pop    %rsi
  4014d4:	48 31 c0             	xor    %rax,%rax
  4014d7:	c3                   	retq   

00000000004014d8 <name_TCFA>:
  4014d8:	68 04 00 00 00       	pushq  $0x4
  4014dd:	00 00                	add    %al,(%rax)
  4014df:	00 04 3e             	add    %al,(%rsi,%rdi,1)
  4014e2:	43                   	rex.XB
  4014e3:	46                   	rex.RX
  4014e4:	41 0f 1f 00          	nopl   (%r8)

00000000004014e8 <TCFA>:
  4014e8:	e8 04 00 00 00       	callq  4014f1 <code_TCFA+0x1>
  4014ed:	00 00                	add    %al,(%rax)
	...

00000000004014f0 <code_TCFA>:
  4014f0:	5f                   	pop    %rdi
  4014f1:	e8 11 00 00 00       	callq  401507 <_TCFA>
  4014f6:	57                   	push   %rdi
  4014f7:	48 ad                	lods   %ds:(%rsi),%rax
  4014f9:	49 8d 04 07          	lea    (%r15,%rax,1),%rax
  4014fd:	4c 8b 28             	mov    (%rax),%r13
  401500:	4f 8d 2c 2f          	lea    (%r15,%r13,1),%r13
  401504:	41 ff e5             	jmpq   *%r13

0000000000401507 <_TCFA>:
  401507:	48 31 c0             	xor    %rax,%rax
  40150a:	48 83 c7 08          	add    $0x8,%rdi
  40150e:	42 8a 04 3f          	mov    (%rdi,%r15,1),%al
  401512:	48 ff c7             	inc    %rdi
  401515:	24 1f                	and    $0x1f,%al
  401517:	48 01 c7             	add    %rax,%rdi
  40151a:	48 83 c7 07          	add    $0x7,%rdi
  40151e:	48 83 e7 f8          	and    $0xfffffffffffffff8,%rdi
  401522:	c3                   	retq   
  401523:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000401528 <name_COMMA>:
  401528:	d0 04 00             	rolb   (%rax,%rax,1)
  40152b:	00 00                	add    %al,(%rax)
  40152d:	00 00                	add    %al,(%rax)
  40152f:	00 01                	add    %al,(%rcx)
  401531:	2c 66                	sub    $0x66,%al
  401533:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000401538 <COMMA>:
  401538:	38 05 00 00 00 00    	cmp    %al,0x0(%rip)        # 40153e <COMMA+0x6>
	...

0000000000401540 <code_COMMA>:
  401540:	58                   	pop    %rax
  401541:	e8 10 00 00 00       	callq  401556 <_COMMA>
  401546:	48 ad                	lods   %ds:(%rsi),%rax
  401548:	49 8d 04 07          	lea    (%r15,%rax,1),%rax
  40154c:	4c 8b 28             	mov    (%rax),%r13
  40154f:	4f 8d 2c 2f          	lea    (%r15,%r13,1),%r13
  401553:	41 ff e5             	jmpq   *%r13

0000000000401556 <_COMMA>:
  401556:	49 8d bf 88 00 00 00 	lea    0x88(%r15),%rdi
  40155d:	48 ab                	stos   %rax,%es:(%rdi)
  40155f:	49 89 bf 88 00 00 00 	mov    %rdi,0x88(%r15)
  401566:	c3                   	retq   
  401567:	90                   	nop

0000000000401568 <name_INTERPRET>:
  401568:	20 05 00 00 00 00    	and    %al,0x0(%rip)        # 40156e <name_INTERPRET+0x6>
  40156e:	00 00                	add    %al,(%rax)
  401570:	09 49 4e             	or     %ecx,0x4e(%rcx)
  401573:	54                   	push   %rsp
  401574:	45 52                	rex.RB push %r10
  401576:	50                   	push   %rax
  401577:	52                   	push   %rdx
  401578:	45 54                	rex.RB push %r12
  40157a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000401580 <INTERPRET>:
  401580:	80 05 00 00 00 00 00 	addb   $0x0,0x0(%rip)        # 401587 <INTERPRET+0x7>
	...

0000000000401588 <code_INTERPRET>:
  401588:	e8 fa fd ff ff       	callq  401387 <_WORD>
  40158d:	48 31 c0             	xor    %rax,%rax
  401590:	49 89 87 d0 06 00 00 	mov    %rax,0x6d0(%r15)
  401597:	e8 04 ff ff ff       	callq  4014a0 <_FIND>
  40159c:	48 85 c0             	test   %rax,%rax
  40159f:	74 18                	je     4015b9 <code_INTERPRET+0x31>
  4015a1:	48 89 c7             	mov    %rax,%rdi
  4015a4:	42 8a 44 3f 08       	mov    0x8(%rdi,%r15,1),%al
  4015a9:	50                   	push   %rax
  4015aa:	e8 58 ff ff ff       	callq  401507 <_TCFA>
  4015af:	58                   	pop    %rax
  4015b0:	24 80                	and    $0x80,%al
  4015b2:	48 89 f8             	mov    %rdi,%rax
  4015b5:	75 52                	jne    401609 <code_INTERPRET+0x81>
  4015b7:	eb 1b                	jmp    4015d4 <code_INTERPRET+0x4c>
  4015b9:	49 ff 87 d0 06 00 00 	incq   0x6d0(%r15)
  4015c0:	e8 4c fe ff ff       	callq  401411 <_NUMBER>
  4015c5:	48 85 c9             	test   %rcx,%rcx
  4015c8:	75 6a                	jne    401634 <code_INTERPRET+0xac>
  4015ca:	48 89 c3             	mov    %rax,%rbx
  4015cd:	49 8b 87 88 02 00 00 	mov    0x288(%r15),%rax
  4015d4:	49 8b 97 50 00 00 00 	mov    0x50(%r15),%rdx
  4015db:	48 85 d2             	test   %rdx,%rdx
  4015de:	74 29                	je     401609 <code_INTERPRET+0x81>
  4015e0:	e8 71 ff ff ff       	callq  401556 <_COMMA>
  4015e5:	49 8b 8f d0 06 00 00 	mov    0x6d0(%r15),%rcx
  4015ec:	48 85 c9             	test   %rcx,%rcx
  4015ef:	74 08                	je     4015f9 <code_INTERPRET+0x71>
  4015f1:	48 89 d8             	mov    %rbx,%rax
  4015f4:	e8 5d ff ff ff       	callq  401556 <_COMMA>
  4015f9:	48 ad                	lods   %ds:(%rsi),%rax
  4015fb:	49 8d 04 07          	lea    (%r15,%rax,1),%rax
  4015ff:	4c 8b 28             	mov    (%rax),%r13
  401602:	4f 8d 2c 2f          	lea    (%r15,%r13,1),%r13
  401606:	41 ff e5             	jmpq   *%r13
  401609:	49 8b 8f d0 06 00 00 	mov    0x6d0(%r15),%rcx
  401610:	48 85 c9             	test   %rcx,%rcx
  401613:	75 0e                	jne    401623 <code_INTERPRET+0x9b>
  401615:	49 8d 04 07          	lea    (%r15,%rax,1),%rax
  401619:	4c 8b 28             	mov    (%rax),%r13
  40161c:	4f 8d 2c 2f          	lea    (%r15,%r13,1),%r13
  401620:	41 ff e5             	jmpq   *%r13
  401623:	53                   	push   %rbx
  401624:	48 ad                	lods   %ds:(%rsi),%rax
  401626:	49 8d 04 07          	lea    (%r15,%rax,1),%rax
  40162a:	4c 8b 28             	mov    (%rax),%r13
  40162d:	4f 8d 2c 2f          	lea    (%r15,%r13,1),%r13
  401631:	41 ff e5             	jmpq   *%r13
  401634:	56                   	push   %rsi
  401635:	57                   	push   %rdi
  401636:	52                   	push   %rdx
  401637:	48 c7 c7 02 00 00 00 	mov    $0x2,%rdi
  40163e:	49 8d b7 bc 06 00 00 	lea    0x6bc(%r15),%rsi
  401645:	48 c7 c2 0d 00 00 00 	mov    $0xd,%rdx
  40164c:	48 c7 c0 01 00 00 00 	mov    $0x1,%rax
  401653:	0f 05                	syscall 
  401655:	5a                   	pop    %rdx
  401656:	5f                   	pop    %rdi
  401657:	5e                   	pop    %rsi
  401658:	49 8b 8f 40 03 00 00 	mov    0x340(%r15),%rcx
  40165f:	48 89 ca             	mov    %rcx,%rdx
  401662:	4d 8d a7 f8 2f 00 00 	lea    0x2ff8(%r15),%r12
  401669:	4c 29 e2             	sub    %r12,%rdx
  40166c:	48 83 fa 28          	cmp    $0x28,%rdx
  401670:	7e 07                	jle    401679 <code_INTERPRET+0xf1>
  401672:	48 c7 c2 28 00 00 00 	mov    $0x28,%rdx
  401679:	48 29 d1             	sub    %rdx,%rcx
  40167c:	56                   	push   %rsi
  40167d:	57                   	push   %rdi
  40167e:	52                   	push   %rdx
  40167f:	48 c7 c7 02 00 00 00 	mov    $0x2,%rdi
  401686:	48 89 ce             	mov    %rcx,%rsi
  401689:	48 89 d2             	mov    %rdx,%rdx
  40168c:	b8 01 00 00 00       	mov    $0x1,%eax
  401691:	0f 05                	syscall 
  401693:	5a                   	pop    %rdx
  401694:	5f                   	pop    %rdi
  401695:	5e                   	pop    %rsi
  401696:	56                   	push   %rsi
  401697:	57                   	push   %rdi
  401698:	48 c7 c7 02 00 00 00 	mov    $0x2,%rdi
  40169f:	49 8d b7 c9 06 00 00 	lea    0x6c9(%r15),%rsi
  4016a6:	ba 01 00 00 00       	mov    $0x1,%edx
  4016ab:	b8 01 00 00 00       	mov    $0x1,%eax
  4016b0:	0f 05                	syscall 
  4016b2:	5f                   	pop    %rdi
  4016b3:	5e                   	pop    %rsi
  4016b4:	48 ad                	lods   %ds:(%rsi),%rax
  4016b6:	49 8d 04 07          	lea    (%r15,%rax,1),%rax
  4016ba:	4c 8b 28             	mov    (%rax),%r13
  4016bd:	4f 8d 2c 2f          	lea    (%r15,%r13,1),%r13
  4016c1:	41 ff e5             	jmpq   *%r13

00000000004016c4 <errmsg>:
  4016c4:	50                   	push   %rax
  4016c5:	41 52                	push   %r10
  4016c7:	53                   	push   %rbx
  4016c8:	45 20 45 52          	and    %r8b,0x52(%r13)
  4016cc:	52                   	push   %rdx
  4016cd:	4f 52                	rex.WRXB push %r10
  4016cf:	3a 20                	cmp    (%rax),%ah

00000000004016d1 <errmsgend>:
  4016d1:	0a 66 0f             	or     0xf(%rsi),%ah
  4016d4:	1f                   	(bad)  
  4016d5:	44 00 00             	add    %r8b,(%rax)

00000000004016d8 <interpret_is_lit>:
	...

00000000004016e0 <name_BRANCH>:
  4016e0:	60                   	(bad)  
  4016e1:	05 00 00 00 00       	add    $0x0,%eax
  4016e6:	00 00                	add    %al,(%rax)
  4016e8:	06                   	(bad)  
  4016e9:	42 52                	rex.X push %rdx
  4016eb:	41                   	rex.B
  4016ec:	4e                   	rex.WRX
  4016ed:	43                   	rex.XB
  4016ee:	48 90                	rex.W nop

00000000004016f0 <BRANCH>:
  4016f0:	f0 06                	lock (bad) 
  4016f2:	00 00                	add    %al,(%rax)
  4016f4:	00 00                	add    %al,(%rax)
	...

00000000004016f8 <code_BRANCH>:
  4016f8:	48 03 36             	add    (%rsi),%rsi
  4016fb:	48 ad                	lods   %ds:(%rsi),%rax
  4016fd:	49 8d 04 07          	lea    (%r15,%rax,1),%rax
  401701:	4c 8b 28             	mov    (%rax),%r13
  401704:	4f 8d 2c 2f          	lea    (%r15,%r13,1),%r13
  401708:	41 ff e5             	jmpq   *%r13
  40170b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000401710 <name_ZBRANCH>:
  401710:	d8 06                	fadds  (%rsi)
  401712:	00 00                	add    %al,(%rax)
  401714:	00 00                	add    %al,(%rax)
  401716:	00 00                	add    %al,(%rax)
  401718:	07                   	(bad)  
  401719:	30 42 52             	xor    %al,0x52(%rdx)
  40171c:	41                   	rex.B
  40171d:	4e                   	rex.WRX
  40171e:	43                   	rex.XB
  40171f:	48               	rex.W and %al,(%rdi)

0000000000401720 <ZBRANCH>:
  401720:	20 07                	and    %al,(%rdi)
  401722:	00 00                	add    %al,(%rax)
  401724:	00 00                	add    %al,(%rax)
	...

0000000000401728 <code_ZBRANCH>:
  401728:	58                   	pop    %rax
  401729:	48 85 c0             	test   %rax,%rax
  40172c:	74 ca                	je     4016f8 <code_BRANCH>
  40172e:	48 ad                	lods   %ds:(%rsi),%rax
  401730:	48 ad                	lods   %ds:(%rsi),%rax
  401732:	49 8d 04 07          	lea    (%r15,%rax,1),%rax
  401736:	4c 8b 28             	mov    (%rax),%r13
  401739:	4f 8d 2c 2f          	lea    (%r15,%r13,1),%r13
  40173d:	41 ff e5             	jmpq   *%r13

0000000000401740 <name_QUIT>:
  401740:	08 07                	or     %al,(%rdi)
  401742:	00 00                	add    %al,(%rax)
  401744:	00 00                	add    %al,(%rax)
  401746:	00 00                	add    %al,(%rax)
  401748:	04 51                	add    $0x51,%al
  40174a:	55                   	push   %rbp
  40174b:	49 54                	rex.WB push %r12
  40174d:	0f 1f 00             	nopl   (%rax)

0000000000401750 <QUIT>:
  401750:	f8                   	clc    
  401751:	01 00                	add    %eax,(%rax)
  401753:	00 00                	add    %al,(%rax)
  401755:	00 00                	add    %al,(%rax)
  401757:	00 d8                	add    %bl,%al
  401759:	01 00                	add    %eax,(%rax)
  40175b:	00 00                	add    %al,(%rax)
  40175d:	00 00                	add    %al,(%rax)
  40175f:	00 58 02             	add    %bl,0x2(%rax)
  401762:	00 00                	add    %al,(%rax)
  401764:	00 00                	add    %al,(%rax)
  401766:	00 00                	add    %al,(%rax)
  401768:	78 05                	js     40176f <QUIT+0x1f>
  40176a:	00 00                	add    %al,(%rax)
  40176c:	00 00                	add    %al,(%rax)
  40176e:	00 00                	add    %al,(%rax)
  401770:	e8 06 00 00 00       	callq  40177b <QUIT+0x2b>
  401775:	00 00                	add    %al,(%rax)
  401777:	00 f0                	add    %dh,%al
  401779:	ff                   	(bad)  
  40177a:	ff                   	(bad)  
  40177b:	ff                   	(bad)  
  40177c:	ff                   	(bad)  
  40177d:	ff                   	(bad)  
  40177e:	ff                   	(bad)  
  40177f:	ff                   	(bad)  
  401780:	e9 7b 08 00 00       	jmpq   402000 <return_stack>
  401785:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  40178c:	00 00 00 00 
  401790:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401797:	00 00 00 00 
  40179b:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  4017a2:	00 00 00 00 
  4017a6:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  4017ad:	00 00 00 00 
  4017b1:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  4017b8:	00 00 00 00 
  4017bc:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  4017c3:	00 00 00 00 
  4017c7:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  4017ce:	00 00 00 00 
  4017d2:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  4017d9:	00 00 00 00 
  4017dd:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  4017e4:	00 00 00 00 
  4017e8:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  4017ef:	00 00 00 00 
  4017f3:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  4017fa:	00 00 00 00 
  4017fe:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401805:	00 00 00 00 
  401809:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401810:	00 00 00 00 
  401814:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  40181b:	00 00 00 00 
  40181f:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401826:	00 00 00 00 
  40182a:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401831:	00 00 00 00 
  401835:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  40183c:	00 00 00 00 
  401840:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401847:	00 00 00 00 
  40184b:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401852:	00 00 00 00 
  401856:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  40185d:	00 00 00 00 
  401861:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401868:	00 00 00 00 
  40186c:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401873:	00 00 00 00 
  401877:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  40187e:	00 00 00 00 
  401882:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401889:	00 00 00 00 
  40188d:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401894:	00 00 00 00 
  401898:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  40189f:	00 00 00 00 
  4018a3:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  4018aa:	00 00 00 00 
  4018ae:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  4018b5:	00 00 00 00 
  4018b9:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  4018c0:	00 00 00 00 
  4018c4:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  4018cb:	00 00 00 00 
  4018cf:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  4018d6:	00 00 00 00 
  4018da:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  4018e1:	00 00 00 00 
  4018e5:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  4018ec:	00 00 00 00 
  4018f0:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  4018f7:	00 00 00 00 
  4018fb:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401902:	00 00 00 00 
  401906:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  40190d:	00 00 00 00 
  401911:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401918:	00 00 00 00 
  40191c:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401923:	00 00 00 00 
  401927:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  40192e:	00 00 00 00 
  401932:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401939:	00 00 00 00 
  40193d:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401944:	00 00 00 00 
  401948:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  40194f:	00 00 00 00 
  401953:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  40195a:	00 00 00 00 
  40195e:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401965:	00 00 00 00 
  401969:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401970:	00 00 00 00 
  401974:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  40197b:	00 00 00 00 
  40197f:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401986:	00 00 00 00 
  40198a:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401991:	00 00 00 00 
  401995:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  40199c:	00 00 00 00 
  4019a0:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  4019a7:	00 00 00 00 
  4019ab:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  4019b2:	00 00 00 00 
  4019b6:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  4019bd:	00 00 00 00 
  4019c1:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  4019c8:	00 00 00 00 
  4019cc:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  4019d3:	00 00 00 00 
  4019d7:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  4019de:	00 00 00 00 
  4019e2:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  4019e9:	00 00 00 00 
  4019ed:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  4019f4:	00 00 00 00 
  4019f8:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  4019ff:	00 00 00 00 
  401a03:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401a0a:	00 00 00 00 
  401a0e:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401a15:	00 00 00 00 
  401a19:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401a20:	00 00 00 00 
  401a24:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401a2b:	00 00 00 00 
  401a2f:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401a36:	00 00 00 00 
  401a3a:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401a41:	00 00 00 00 
  401a45:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401a4c:	00 00 00 00 
  401a50:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401a57:	00 00 00 00 
  401a5b:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401a62:	00 00 00 00 
  401a66:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401a6d:	00 00 00 00 
  401a71:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401a78:	00 00 00 00 
  401a7c:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401a83:	00 00 00 00 
  401a87:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401a8e:	00 00 00 00 
  401a92:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401a99:	00 00 00 00 
  401a9d:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401aa4:	00 00 00 00 
  401aa8:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401aaf:	00 00 00 00 
  401ab3:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401aba:	00 00 00 00 
  401abe:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401ac5:	00 00 00 00 
  401ac9:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401ad0:	00 00 00 00 
  401ad4:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401adb:	00 00 00 00 
  401adf:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401ae6:	00 00 00 00 
  401aea:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401af1:	00 00 00 00 
  401af5:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401afc:	00 00 00 00 
  401b00:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401b07:	00 00 00 00 
  401b0b:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401b12:	00 00 00 00 
  401b16:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401b1d:	00 00 00 00 
  401b21:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401b28:	00 00 00 00 
  401b2c:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401b33:	00 00 00 00 
  401b37:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401b3e:	00 00 00 00 
  401b42:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401b49:	00 00 00 00 
  401b4d:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401b54:	00 00 00 00 
  401b58:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401b5f:	00 00 00 00 
  401b63:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401b6a:	00 00 00 00 
  401b6e:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401b75:	00 00 00 00 
  401b79:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401b80:	00 00 00 00 
  401b84:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401b8b:	00 00 00 00 
  401b8f:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401b96:	00 00 00 00 
  401b9a:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401ba1:	00 00 00 00 
  401ba5:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401bac:	00 00 00 00 
  401bb0:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401bb7:	00 00 00 00 
  401bbb:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401bc2:	00 00 00 00 
  401bc6:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401bcd:	00 00 00 00 
  401bd1:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401bd8:	00 00 00 00 
  401bdc:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401be3:	00 00 00 00 
  401be7:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401bee:	00 00 00 00 
  401bf2:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401bf9:	00 00 00 00 
  401bfd:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401c04:	00 00 00 00 
  401c08:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401c0f:	00 00 00 00 
  401c13:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401c1a:	00 00 00 00 
  401c1e:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401c25:	00 00 00 00 
  401c29:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401c30:	00 00 00 00 
  401c34:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401c3b:	00 00 00 00 
  401c3f:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401c46:	00 00 00 00 
  401c4a:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401c51:	00 00 00 00 
  401c55:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401c5c:	00 00 00 00 
  401c60:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401c67:	00 00 00 00 
  401c6b:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401c72:	00 00 00 00 
  401c76:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401c7d:	00 00 00 00 
  401c81:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401c88:	00 00 00 00 
  401c8c:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401c93:	00 00 00 00 
  401c97:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401c9e:	00 00 00 00 
  401ca2:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401ca9:	00 00 00 00 
  401cad:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401cb4:	00 00 00 00 
  401cb8:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401cbf:	00 00 00 00 
  401cc3:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401cca:	00 00 00 00 
  401cce:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401cd5:	00 00 00 00 
  401cd9:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401ce0:	00 00 00 00 
  401ce4:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401ceb:	00 00 00 00 
  401cef:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401cf6:	00 00 00 00 
  401cfa:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401d01:	00 00 00 00 
  401d05:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401d0c:	00 00 00 00 
  401d10:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401d17:	00 00 00 00 
  401d1b:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401d22:	00 00 00 00 
  401d26:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401d2d:	00 00 00 00 
  401d31:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401d38:	00 00 00 00 
  401d3c:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401d43:	00 00 00 00 
  401d47:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401d4e:	00 00 00 00 
  401d52:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401d59:	00 00 00 00 
  401d5d:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401d64:	00 00 00 00 
  401d68:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401d6f:	00 00 00 00 
  401d73:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401d7a:	00 00 00 00 
  401d7e:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401d85:	00 00 00 00 
  401d89:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401d90:	00 00 00 00 
  401d94:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401d9b:	00 00 00 00 
  401d9f:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401da6:	00 00 00 00 
  401daa:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401db1:	00 00 00 00 
  401db5:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401dbc:	00 00 00 00 
  401dc0:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401dc7:	00 00 00 00 
  401dcb:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401dd2:	00 00 00 00 
  401dd6:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401ddd:	00 00 00 00 
  401de1:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401de8:	00 00 00 00 
  401dec:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401df3:	00 00 00 00 
  401df7:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401dfe:	00 00 00 00 
  401e02:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401e09:	00 00 00 00 
  401e0d:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401e14:	00 00 00 00 
  401e18:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401e1f:	00 00 00 00 
  401e23:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401e2a:	00 00 00 00 
  401e2e:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401e35:	00 00 00 00 
  401e39:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401e40:	00 00 00 00 
  401e44:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401e4b:	00 00 00 00 
  401e4f:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401e56:	00 00 00 00 
  401e5a:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401e61:	00 00 00 00 
  401e65:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401e6c:	00 00 00 00 
  401e70:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401e77:	00 00 00 00 
  401e7b:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401e82:	00 00 00 00 
  401e86:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401e8d:	00 00 00 00 
  401e91:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401e98:	00 00 00 00 
  401e9c:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401ea3:	00 00 00 00 
  401ea7:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401eae:	00 00 00 00 
  401eb2:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401eb9:	00 00 00 00 
  401ebd:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401ec4:	00 00 00 00 
  401ec8:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401ecf:	00 00 00 00 
  401ed3:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401eda:	00 00 00 00 
  401ede:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401ee5:	00 00 00 00 
  401ee9:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401ef0:	00 00 00 00 
  401ef4:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401efb:	00 00 00 00 
  401eff:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401f06:	00 00 00 00 
  401f0a:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401f11:	00 00 00 00 
  401f15:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401f1c:	00 00 00 00 
  401f20:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401f27:	00 00 00 00 
  401f2b:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401f32:	00 00 00 00 
  401f36:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401f3d:	00 00 00 00 
  401f41:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401f48:	00 00 00 00 
  401f4c:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401f53:	00 00 00 00 
  401f57:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401f5e:	00 00 00 00 
  401f62:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401f69:	00 00 00 00 
  401f6d:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401f74:	00 00 00 00 
  401f78:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401f7f:	00 00 00 00 
  401f83:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401f8a:	00 00 00 00 
  401f8e:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401f95:	00 00 00 00 
  401f99:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401fa0:	00 00 00 00 
  401fa4:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401fab:	00 00 00 00 
  401faf:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401fb6:	00 00 00 00 
  401fba:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401fc1:	00 00 00 00 
  401fc5:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401fcc:	00 00 00 00 
  401fd0:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401fd7:	00 00 00 00 
  401fdb:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401fe2:	00 00 00 00 
  401fe6:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401fed:	00 00 00 00 
  401ff1:	66 66 2e 0f 1f 84 00 	data16 nopw %cs:0x0(%rax,%rax,1)
  401ff8:	00 00 00 00 
  401ffc:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000402000 <return_stack>:
	...

0000000000404000 <input_buffer>:
	...

0000000000405000 <data_buffer>:
	...

0000000000415000 <name_SYSCALL0>:
  415000:	38 07                	cmp    %al,(%rdi)
  415002:	00 00                	add    %al,(%rax)
  415004:	00 00                	add    %al,(%rax)
  415006:	00 00                	add    %al,(%rax)
  415008:	08 53 59             	or     %dl,0x59(%rbx)
  41500b:	53                   	push   %rbx
  41500c:	43                   	rex.XB
  41500d:	41                   	rex.B
  41500e:	4c                   	rex.WR
  41500f:	4c 30 0f             	rex.WR xor %r9b,(%rdi)
  415012:	1f                   	(bad)  
  415013:	80 00 00             	addb   $0x0,(%rax)
	...

0000000000415018 <SYSCALL0>:
  415018:	18 40 01             	sbb    %al,0x1(%rax)
  41501b:	00 00                	add    %al,(%rax)
  41501d:	00 00                	add    %al,(%rax)
	...

0000000000415020 <code_SYSCALL0>:
  415020:	58                   	pop    %rax
  415021:	0f 05                	syscall 
  415023:	50                   	push   %rax
  415024:	48 ad                	lods   %ds:(%rsi),%rax
  415026:	49 8d 04 07          	lea    (%r15,%rax,1),%rax
  41502a:	4c 8b 28             	mov    (%rax),%r13
  41502d:	4f 8d 2c 2f          	lea    (%r15,%r13,1),%r13
  415031:	41 ff e5             	jmpq   *%r13

Disassembly of section .dynamic:

0000000000415038 <_DYNAMIC>:
  415038:	f5                   	cmc    
  415039:	fe                   	(bad)  
  41503a:	ff 6f 00             	ljmp   *0x0(%rdi)
  41503d:	00 00                	add    %al,(%rax)
  41503f:	00 b0 00 40 00 00    	add    %dh,0x4000(%rax)
  415045:	00 00                	add    %al,(%rax)
  415047:	00 05 00 00 00 00    	add    %al,0x0(%rip)        # 41504d <_DYNAMIC+0x15>
  41504d:	00 00                	add    %al,(%rax)
  41504f:	00 e8                	add    %ch,%al
  415051:	00 40 00             	add    %al,0x0(%rax)
  415054:	00 00                	add    %al,(%rax)
  415056:	00 00                	add    %al,(%rax)
  415058:	06                   	(bad)  
  415059:	00 00                	add    %al,(%rax)
  41505b:	00 00                	add    %al,(%rax)
  41505d:	00 00                	add    %al,(%rax)
  41505f:	00 d0                	add    %dl,%al
  415061:	00 40 00             	add    %al,0x0(%rax)
  415064:	00 00                	add    %al,(%rax)
  415066:	00 00                	add    %al,(%rax)
  415068:	0a 00                	or     (%rax),%al
  41506a:	00 00                	add    %al,(%rax)
  41506c:	00 00                	add    %al,(%rax)
  41506e:	00 00                	add    %al,(%rax)
  415070:	01 00                	add    %eax,(%rax)
  415072:	00 00                	add    %al,(%rax)
  415074:	00 00                	add    %al,(%rax)
  415076:	00 00                	add    %al,(%rax)
  415078:	0b 00                	or     (%rax),%eax
  41507a:	00 00                	add    %al,(%rax)
  41507c:	00 00                	add    %al,(%rax)
  41507e:	00 00                	add    %al,(%rax)
  415080:	18 00                	sbb    %al,(%rax)
  415082:	00 00                	add    %al,(%rax)
  415084:	00 00                	add    %al,(%rax)
  415086:	00 00                	add    %al,(%rax)
  415088:	15 00 00 00 00       	adc    $0x0,%eax
	...
  415095:	00 00                	add    %al,(%rax)
  415097:	00 fb                	add    %bh,%bl
  415099:	ff                   	(bad)  
  41509a:	ff 6f 00             	ljmp   *0x0(%rdi)
  41509d:	00 00                	add    %al,(%rax)
  41509f:	00 00                	add    %al,(%rax)
  4150a1:	00 00                	add    %al,(%rax)
  4150a3:	08 00                	or     %al,(%rax)
	...

Disassembly of section .debug_aranges:

0000000000000000 <.debug_aranges>:
   0:	2c 00                	sub    $0x0,%al
   2:	00 00                	add    %al,(%rax)
   4:	02 00                	add    (%rax),%al
   6:	00 00                	add    %al,(%rax)
   8:	00 00                	add    %al,(%rax)
   a:	08 00                	or     %al,(%rax)
   c:	00 00                	add    %al,(%rax)
   e:	00 00                	add    %al,(%rax)
  10:	00 10                	add    %dl,(%rax)
  12:	40 00 00             	add    %al,(%rax)
  15:	00 00                	add    %al,(%rax)
  17:	00 34 40             	add    %dh,(%rax,%rax,2)
  1a:	01 00                	add    %eax,(%rax)
	...

Disassembly of section .debug_info:

0000000000000000 <.debug_info>:
   0:	2a 00                	sub    (%rax),%al
   2:	00 00                	add    %al,(%rax)
   4:	02 00                	add    (%rax),%al
   6:	00 00                	add    %al,(%rax)
   8:	00 00                	add    %al,(%rax)
   a:	08 01                	or     %al,(%rcx)
   c:	00 00                	add    %al,(%rax)
   e:	00 00                	add    %al,(%rax)
  10:	00 10                	add    %dl,(%rax)
  12:	40 00 00             	add    %al,(%rax)
  15:	00 00                	add    %al,(%rax)
  17:	00 34 50             	add    %dh,(%rax,%rdx,2)
  1a:	41 00 00             	add    %al,(%r8)
  1d:	00 00                	add    %al,(%rax)
  1f:	00 00                	add    %al,(%rax)
  21:	00 00                	add    %al,(%rax)
  23:	00 06                	add    %al,(%rsi)
  25:	00 00                	add    %al,(%rax)
  27:	00 3c 00             	add    %bh,(%rax,%rax,1)
  2a:	00 00                	add    %al,(%rax)
  2c:	01                   	.byte 0x1
  2d:	80                   	.byte 0x80

Disassembly of section .debug_abbrev:

0000000000000000 <.debug_abbrev>:
   0:	01 11                	add    %edx,(%rcx)
   2:	00 10                	add    %dl,(%rax)
   4:	06                   	(bad)  
   5:	11 01                	adc    %eax,(%rcx)
   7:	12 01                	adc    (%rcx),%al
   9:	03 0e                	add    (%rsi),%ecx
   b:	1b 0e                	sbb    (%rsi),%ecx
   d:	25 0e 13 05 00       	and    $0x5130e,%eax
	...

Disassembly of section .debug_line:

0000000000000000 <.debug_line>:
   0:	80 01 00             	addb   $0x0,(%rcx)
   3:	00 03                	add    %al,(%rbx)
   5:	00 1c 00             	add    %bl,(%rax,%rax,1)
   8:	00 00                	add    %al,(%rax)
   a:	01 01                	add    %eax,(%rcx)
   c:	fb                   	sti    
   d:	0e                   	(bad)  
   e:	0d 00 01 01 01       	or     $0x1010100,%eax
  13:	01 00                	add    %eax,(%rax)
  15:	00 00                	add    %al,(%rax)
  17:	01 00                	add    %eax,(%rax)
  19:	00 01                	add    %al,(%rcx)
  1b:	00 6a 66             	add    %ch,0x66(%rdx)
  1e:	74 2e                	je     4e <sys_exit+0x12>
  20:	53                   	push   %rbx
  21:	00 00                	add    %al,(%rax)
  23:	00 00                	add    %al,(%rax)
  25:	00 00                	add    %al,(%rax)
  27:	09 02                	or     %eax,(%rdx)
  29:	00 10                	add    %dl,(%rax)
  2b:	40 00 00             	add    %al,(%rax)
  2e:	00 00                	add    %al,(%rax)
  30:	00 03                	add    %al,(%rbx)
  32:	df 00                	filds  (%rax)
  34:	01 21                	add    %esp,(%rcx)
  36:	21 21                	and    %esp,(%rcx)
  38:	03 0c 02             	add    (%rdx,%rax,1),%ecx
  3b:	3d 01 02 38 13       	cmp    $0x13380201,%eax
  40:	02 38                	add    (%rax),%bh
  42:	13 02                	adc    (%rdx),%eax
  44:	38 13                	cmp    %dl,(%rbx)
  46:	02 38                	add    (%rax),%bh
  48:	13 08                	adc    (%rax),%ecx
  4a:	e9 30 76 4b 4b       	jmpq   4b4b767f <__bss_start+0x4b0a2577>
  4f:	59                   	pop    %rcx
  50:	4b 22 75 75          	rex.WXB and 0x75(%r13),%sil
  54:	76 75                	jbe    cb <F_IMMED+0x4b>
  56:	4c                   	rex.WR
  57:	4d f4                	rex.WRB hlt 
  59:	75 75                	jne    d0 <F_IMMED+0x50>
  5b:	08 a4 02 30 13 03 31 	or     %ah,0x31031330(%rdx,%rax,1)
  62:	08 74 83 4b          	or     %dh,0x4b(%rbx,%rax,4)
  66:	02 2c 16             	add    (%rsi,%rdx,1),%ch
  69:	83 03 13             	addl   $0x13,(%rbx)
  6c:	02 28                	add    (%rax),%ch
  6e:	01 21                	add    %esp,(%rcx)
  70:	03 0b                	add    (%rbx),%ecx
  72:	02 2f                	add    (%rdi),%ch
  74:	01 30                	add    %esi,(%rax)
  76:	21 02                	and    %eax,(%rdx)
  78:	2d 15 59 21 f4       	sub    $0xf4215915,%eax
  7d:	75 75                	jne    f4 <F_IMMED+0x74>
  7f:	2f                   	(bad)  
  80:	3d 2f 3d 75 23       	cmp    $0x23753d2f,%eax
  85:	21 21                	and    %esp,(%rcx)
  87:	75 75                	jne    fe <F_IMMED+0x7e>
  89:	75 75                	jne    100 <F_IMMED+0x80>
  8b:	75 30                	jne    bd <F_IMMED+0x3d>
  8d:	3d 2f 3d 75 21       	cmp    $0x21753d2f,%eax
  92:	21 31                	and    %esi,(%rcx)
  94:	21 21                	and    %esp,(%rcx)
  96:	75 3d                	jne    d5 <F_IMMED+0x55>
  98:	03 0b                	add    (%rbx),%ecx
  9a:	02 2c 01             	add    (%rcx,%rax,1),%ch
  9d:	59                   	pop    %rcx
  9e:	21 21                	and    %esp,(%rcx)
  a0:	f6 59 2f             	negb   0x2f(%rcx)
  a3:	2f                   	(bad)  
  a4:	2f                   	(bad)  
  a5:	31 76 21             	xor    %esi,0x21(%rsi)
  a8:	59                   	pop    %rcx
  a9:	2f                   	(bad)  
  aa:	31 3d 75 3d 24 59    	xor    %edi,0x59243d75(%rip)        # 59243e25 <__bss_start+0x58e2ed1d>
  b0:	2f                   	(bad)  
  b1:	2f                   	(bad)  
  b2:	03 09                	add    (%rcx),%ecx
  b4:	02 3c 01             	add    (%rcx,%rax,1),%bh
  b7:	21 21                	and    %esp,(%rcx)
  b9:	59                   	pop    %rcx
  ba:	21 21                	and    %esp,(%rcx)
  bc:	f4                   	hlt    
  bd:	3d 3e 3d 30 76       	cmp    $0x76303d3e,%eax
  c2:	2f                   	(bad)  
  c3:	3d 21 3d 2f 21       	cmp    $0x212f3d21,%eax
  c8:	21 3d 2f 21 75 24    	and    %edi,0x2475212f(%rip)        # 247521fd <__bss_start+0x2433d0f5>
  ce:	4b 2f                	rex.WXB (bad) 
  d0:	3f                   	(bad)  
  d1:	3d 2f 3d 2f 3d       	cmp    $0x3d2f3d2f,%eax
  d6:	2f                   	(bad)  
  d7:	3e 2f                	ds (bad) 
  d9:	2f                   	(bad)  
  da:	3d 3d 33 21 3d       	cmp    $0x3d21333d,%eax
  df:	2f                   	(bad)  
  e0:	3e 08 93 21 21 59 21 	or     %dl,%ds:0x21592121(%rbx)
  e7:	f4                   	hlt    
  e8:	23 76 3d             	and    0x3d(%rsi),%esi
  eb:	32 3d 59 2f 2f 31    	xor    0x312f2f59(%rip),%bh        # 312f304a <__bss_start+0x30eddf42>
  f1:	21 21                	and    %esp,(%rcx)
  f3:	59                   	pop    %rcx
  f4:	2f                   	(bad)  
  f5:	21 21                	and    %esp,(%rcx)
  f7:	31 21                	xor    %esp,(%rcx)
  f9:	3d 23 4b 32 21       	cmp    $0x21324b23,%eax
  fe:	3d 08 85 21 59       	cmp    $0x59218508,%eax
 103:	21 f4                	and    %esi,%esp
 105:	3d 4b 4b 3d 2f       	cmp    $0x2f3d4b4b,%eax
 10a:	3d 4b 4b 08 cb       	cmp    $0xcb084b4b,%eax
 10f:	21 59 f4             	and    %ebx,-0xc(%rcx)
 112:	75 2f                	jne    143 <F_IMMED+0xc3>
 114:	75 02                	jne    118 <F_IMMED+0x98>
 116:	22 15 5a 3d 75 59    	and    0x59753d5a(%rip),%dl        # 59753e76 <__bss_start+0x5933ed6e>
 11c:	3d 31 3d 59 21       	cmp    $0x21593d31,%eax
 121:	59                   	pop    %rcx
 122:	21 2f                	and    %ebp,(%rdi)
 124:	3d 2f 32 75 59       	cmp    $0x5975322f,%eax
 129:	3d 2f 3d 77 75       	cmp    $0x75773d2f,%eax
 12e:	3d 2f 59 75 3d       	cmp    $0x3d75592f,%eax
 133:	2f                   	(bad)  
 134:	3d 5a f6 75 3d       	cmp    $0x3d75f65a,%eax
 139:	31 4b 3d             	xor    %ecx,0x3d(%rbx)
 13c:	4c                   	rex.WR
 13d:	40 21 f6             	rex and %esi,%esi
 140:	21 21                	and    %esp,(%rcx)
 142:	21 75 75             	and    %esi,0x75(%rbp)
 145:	75 75                	jne    1bc <F_IMMED+0x13c>
 147:	2f                   	(bad)  
 148:	21 21                	and    %esp,(%rcx)
 14a:	22 75 3d             	and    0x3d(%rbp),%dh
 14d:	75 3d                	jne    18c <F_IMMED+0x10c>
 14f:	4b 2f                	rex.WXB (bad) 
 151:	76 3d                	jbe    190 <F_IMMED+0x110>
 153:	21 21                	and    %esp,(%rcx)
 155:	21 75 3d             	and    %esi,0x3d(%rbp)
 158:	3d 59 2f 21 21       	cmp    $0x21212f59,%eax
 15d:	22 21                	and    (%rcx),%ah
 15f:	21 75 75             	and    %esi,0x75(%rbp)
 162:	59                   	pop    %rcx
 163:	59                   	pop    %rcx
 164:	2f                   	(bad)  
 165:	21 21                	and    %esp,(%rcx)
 167:	03 0c 02             	add    (%rdx,%rax,1),%ecx
 16a:	44 01 3d 02 2d 15 21 	add    %r15d,0x21152d02(%rip)        # 21152e73 <__bss_start+0x20d3dd6b>
 171:	3d 2f 2f 03 90       	cmp    $0x90032f2f,%eax
 176:	01 02                	add    %eax,(%rdx)
 178:	f0 f1                	lock icebp 
 17a:	04 01                	add    $0x1,%al
 17c:	21 2f                	and    %ebp,(%rdi)
 17e:	21 02                	and    %eax,(%rdx)
 180:	10 00                	adc    %al,(%rax)
 182:	01 01                	add    %eax,(%rcx)

Disassembly of section .debug_str:

0000000000000000 <.debug_str>:
   0:	6a 66                	pushq  $0x66
   2:	74 2e                	je     32 <JONES_VERSION+0x3>
   4:	53                   	push   %rbx
   5:	00 2f                	add    %ch,(%rdi)
   7:	68 6f 6d 65 2f       	pushq  $0x2f656d6f
   c:	72 69                	jb     77 <sys_exit+0x3b>
   e:	67 69 64 75 73 2f 73 	imul   $0x6372732f,0x73(%ebp,%esi,2),%esp
  15:	72 63 
  17:	2f                   	(bad)  
  18:	72 69                	jb     83 <F_IMMED+0x3>
  1a:	67 69 64 75 73 2e 72 	imul   $0x2f75722e,0x73(%ebp,%esi,2),%esp
  21:	75 2f 
  23:	6f                   	outsl  %ds:(%rsi),(%dx)
  24:	72 67                	jb     8d <F_IMMED+0xd>
  26:	2f                   	(bad)  
  27:	6c                   	insb   (%dx),%es:(%rdi)
  28:	72 6e                	jb     98 <F_IMMED+0x18>
  2a:	2f                   	(bad)  
  2b:	66 6f                	outsw  %ds:(%rsi),(%dx)
  2d:	72 74                	jb     a3 <F_IMMED+0x23>
  2f:	68 2f 70 6c 61       	pushq  $0x616c702f
  34:	69 6e 66 6f 72 74 68 	imul   $0x6874726f,0x66(%rsi),%ebp
  3b:	00 47 4e             	add    %al,0x4e(%rdi)
  3e:	55                   	push   %rbp
  3f:	20 41 53             	and    %al,0x53(%rcx)
  42:	20 32                	and    %dh,(%rdx)
  44:	2e 33 34 00          	xor    %cs:(%rax,%rax,1),%esi
