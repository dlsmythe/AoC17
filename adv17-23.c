// gcc -g -Wall -o adv17-23 adv17-23.c -lpthread
#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <time.h>
#include <getopt.h>
#include <ctype.h>
#include <err.h>
#include <assert.h>

int part = 1;

typedef enum {
    OPC_SET,
    OPC_SUB,
    OPC_MUL,
    OPC_JNZ,

    OPC_LAST
} opcode_t;

char *opnames[OPC_LAST] = {
    "set",
    "sub",
    "mul",
    "jnz"
};

typedef struct insn_s {
    opcode_t opcode;
    int reg;
    int reg_is_constant;
    int arg;
    int arg_is_constant;
} insn_t;

insn_t *prog;
int proglen, maxproglen;
int trace = 0;

int64_t regs[26];
int regs_set[26];

int only_print_valid_regs = 1;
void print_regs(void)
{
    int i;
    for (i = 0; i < 26; i++) {
	if (only_print_valid_regs  &&  !regs_set[i]) {
	    continue;
	}
	printf("   reg %c: %ld%s\n", 'a'+i, regs[i], regs_set[i] ? "":" (never set)");
    }
}

void print_insn(insn_t *i)
{
    printf("%s ", opnames[i->opcode]);
    if (i->reg_is_constant) {
	printf("%d", i->reg);
    } else {
	printf("%c", 'a'+i->reg);
    }
    if (i->opcode == OPC_SET ||
	i->opcode == OPC_SUB ||
	i->opcode == OPC_MUL ||
	i->opcode == OPC_JNZ) {
	if (i->arg_is_constant) {
	    printf(" %d", i->arg);
	} else {
	    printf(" %c", 'a'+i->arg);
	}
    }
    puts("");
}

void print_program(void)
{
    int pc;
    for (pc = 0; pc < proglen; pc++) {
	printf("%d: ", pc);
	print_insn(&prog[pc]);
    }
    printf("======================\n");
}

int64_t get_reg(int t, int pc, int reg)
{
#if 0
    if (!regs_set[reg]) {
	errx(1, "read of uninitialized value at time %d pc=%d", t, pc);
    }
#endif
    return regs[reg];
}

void set_reg(int reg, int64_t val)
{
    regs[reg] = val;
    regs_set[reg] = 1;
}
    
void add_insn(int opcode, int reg, int reg_is_constant, int arg, int arg_is_constant)
{
    if (proglen >= maxproglen) {
	maxproglen += 1000;
	prog = realloc(prog, maxproglen * sizeof(insn_t));
	assert(prog);
    }
    if (2 == part) {
	if (OPC_SET == opcode  &&  !reg_is_constant  &&  reg == ('f'-'a')  &&  arg_is_constant  &&  0 == arg) {
	    // patch in a jump to h++
	    opcode = OPC_JNZ;
	    reg_is_constant = 1;
	    reg = 1;
	    arg_is_constant = 1;
	    arg = 10;
	}
    }
    prog[proglen].opcode = opcode;
    prog[proglen].reg = reg;
    prog[proglen].reg_is_constant = reg_is_constant;
    prog[proglen].arg = arg;
    prog[proglen].arg_is_constant = arg_is_constant;
    proglen++;
}

char *read_val(char *buf, int *val, int *is_const)
{
    while (isspace(*buf)) {
	buf++;
    }
    if (*buf >= 'a'  &&  *buf <= 'z') {
	*is_const = 0;
	*val = *buf - 'a';
	return &buf[1];
    }
    *is_const = 1;
    char *p;
    *val = strtol(buf, &p, 0);
    return p;
}

void read_program(char *filename)
{
    char buf[80];
    FILE *fp;
    fp = fopen(filename, "r");
    if (!fp) {
	err(1, "fopen(%s)", filename);
    }
    while (fgets(buf, sizeof buf, fp) != NULL) {
	char *p;
	p = strchr(buf, '\n');
	if (p) {
	    *p = '\0';
	}
	opcode_t op = OPC_LAST;
	int reg = 0;
	int reg_is_constant = 0;
	p = read_val(&buf[4], &reg, &reg_is_constant);
	int arg = 0;
	int arg_is_constant = 0;
	if (!strncmp(buf, "set", 3)) {
	    op = OPC_SET;
	    read_val(p, &arg, &arg_is_constant);
	} else if (!strncmp(buf, "sub", 3)) {
	    op = OPC_SUB;
	    read_val(p, &arg, &arg_is_constant);
	} else if (!strncmp(buf, "mul", 3)) {
	    op = OPC_MUL;
	    read_val(p, &arg, &arg_is_constant);
	} else if (!strncmp(buf, "jnz", 3)) {
	    op = OPC_JNZ;
	    read_val(p, &arg, &arg_is_constant);
	} else {
	    errx(1, "bogus instruction: %s", buf);
	}
	add_insn(op, reg, reg_is_constant, arg, arg_is_constant);
    }
}

#define R(X) ((X)-'a')

void run_program(void)
{
    int pc = 0, t;
    int mulcnt = 0;
    int64_t reg, val;
    printf("program running\n");
    for (t = 0; ; t++) {
	if (pc < 0 || pc >= proglen) {
	    printf("Program terminated at time %d (pc=%d)\n", t, pc);
	    if (1 == part) {
		printf("MUL count: %d\n", mulcnt);
	    } else {
		printf("Final value of reg h: %ld\n", get_reg(t, pc, R('h')));
	    }
	    return;
	}
	/* if ((t%1000000) == 0) { */
	/*     printf("%d\n", t); */
	/* } */
	if (8 == pc) {
	    printf("OUTER: b: %ld  c: %ld  h: %ld\n", get_reg(t, pc, R('b')), get_reg(t, pc, R('c')), get_reg(t, pc, R('h')));
	}
	if (10 == pc) {
	    printf("  loop1: d: %ld  b: %ld\n", get_reg(t, pc, R('d')), get_reg(t, pc, R('b')));
	}
	if (10 == pc) {
	    printf("  loop1: d: %ld  b: %ld\n", get_reg(t, pc, R('d')), get_reg(t, pc, R('b')));
	}
	if (11 == pc) {
	    printf("    loop2: e: %ld  b: %ld\n", get_reg(t, pc, R('e')), get_reg(t, pc, R('b')));
	}
	if (trace) {
	    printf(" => [%d] %d: ", t, pc);
	    print_insn(&prog[pc]);
	    print_regs();
	}
	if (t > 100000) exit(1);
	switch (prog[pc].opcode) {
	default:
	    break;
	case OPC_SET:
	    assert(!prog[pc].reg_is_constant);
	    val = prog[pc].arg_is_constant ? prog[pc].arg : get_reg(t, pc, prog[pc].arg);
	    set_reg(prog[pc].reg, val);
	    pc++;
	    break;
	case OPC_SUB:
	    assert(!prog[pc].reg_is_constant);
	    val = prog[pc].arg_is_constant ? prog[pc].arg : get_reg(t, pc, prog[pc].arg);
	    set_reg(prog[pc].reg, get_reg(t, pc, prog[pc].reg) - val);
	    pc++;
	    break;
	case OPC_MUL:
	    assert(!prog[pc].reg_is_constant);
	    val = prog[pc].arg_is_constant ? prog[pc].arg : get_reg(t, pc, prog[pc].arg);
	    set_reg(prog[pc].reg, get_reg(t, pc, prog[pc].reg) * val);
	    pc++;
	    mulcnt++;
	    break;
	case OPC_JNZ:
	    reg = prog[pc].reg_is_constant ? prog[pc].reg : get_reg(t, pc, prog[pc].reg);
	    val = prog[pc].arg_is_constant ? prog[pc].arg : get_reg(t, pc, prog[pc].arg);
	    if (reg != 0) {
		pc += val;
	    } else {
		pc++;
	    }
	    break;
	}
    }
}

int main(int argc, char **argv)
{
    int c;
    char *filename = "adv17-23.input";
    int do_print = 0;
    
    while ((c = getopt(argc, argv, "lf:tp:")) != EOF) {
	switch (c) {
	case 'f':
	    filename = strdup(optarg);
	    break;
	case 't':
	    trace = 1;
	    break;
	case 'l':
	    do_print = 1;
	    break;
	case 'p':
	    part = strtol(optarg, NULL, 0);
	    break;
	}
    }
    
    read_program(filename);
    if (do_print) {
	print_program();
    }
    printf("proglen %d\n", proglen);

    if (2 == part) {
	set_reg(0, 1);
    }
    run_program();
    
    return 0;
}
