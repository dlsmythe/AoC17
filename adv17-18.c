// gcc -g -Wall -o adv17-18 adv17-18.c -lpthread
#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/signal.h>
#include <sys/wait.h>
#include <sys/socket.h>
#include <time.h>
#include <semaphore.h>
#include <getopt.h>
#include <ctype.h>
#include <err.h>
#include <assert.h>

#define PART 2
#define USE_PIPES 0
#define USE_LOCAL_SOCKETS 1

#if USE_PIPES
# error pipes mysteriously block on write early on. No idea why
#endif

#define LENGTHOF(X) (sizeof(X)/sizeof((X)[0]))

typedef enum {
    OPC_SET,
    OPC_ADD,
    OPC_MUL,
    OPC_MOD,
    OPC_SND,
    OPC_RCV,
    OPC_JGZ,

    OPC_LAST
} opcode_t;

char *opnames[OPC_LAST] = {
    "set",
    "add",
    "mul",
    "mod",
    "snd",
    "rcv",
    "jgz"
};

typedef struct insn_s {
    opcode_t opcode;
    int reg;
    int reg_is_constant;
    int arg;
    int arg_is_constant;
} insn_t;

int program_id;
insn_t *prog;
int proglen, maxproglen;
int trace = 0;

int64_t regs[26];
int regs_set[26];
int64_t freqval;
int freqval_set;

int pfds[2][2];
int pids[2];
char *sem_name = "/adv17-18";
sem_t *child_counter_semaphore;

int only_print_valid_regs = 1;
void print_regs(void)
{
    int i;
    if (!only_print_valid_regs  ||  freqval_set) {
	printf("p%d:   freqval: %ld%s\n", program_id, freqval, freqval_set ? "":" (never set)");
    }
    for (i = 0; i < 26; i++) {
	if (only_print_valid_regs  &&  !regs_set[i]) {
	    continue;
	}
	printf("%d:   reg %c: %ld%s\n", program_id, 'a'+i, regs[i], regs_set[i] ? "":" (never set)");
    }
}

void print_insn(insn_t *i)
{
    printf("%d: %s ", program_id, opnames[i->opcode]);
    if (i->reg_is_constant) {
	printf("%d", i->reg);
    } else {
	printf("%c", 'a'+i->reg);
    }
    if (i->opcode == OPC_SET ||
	i->opcode == OPC_ADD ||
	i->opcode == OPC_MUL ||
	i->opcode == OPC_MOD ||
	i->opcode == OPC_JGZ) {
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
	printf("%d: %d: ", program_id, pc);
	print_insn(&prog[pc]);
    }
    printf("%d: ======================\n", program_id);
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
	} else if (!strncmp(buf, "add", 3)) {
	    op = OPC_ADD;
	    read_val(p, &arg, &arg_is_constant);
	} else if (!strncmp(buf, "mul", 3)) {
	    op = OPC_MUL;
	    read_val(p, &arg, &arg_is_constant);
	} else if (!strncmp(buf, "mod", 3)) {
	    op = OPC_MOD;
	    read_val(p, &arg, &arg_is_constant);
	} else if (!strncmp(buf, "snd", 3)) {
	    op = OPC_SND;
	} else if (!strncmp(buf, "rcv", 3)) {
	    op = OPC_RCV;
	} else if (!strncmp(buf, "jgz", 3)) {
	    op = OPC_JGZ;
	    read_val(p, &arg, &arg_is_constant);
	} else {
	    errx(1, "bogus instruction: %s", buf);
	}
	add_insn(op, reg, reg_is_constant, arg, arg_is_constant);
    }
}

void run_program(void)
{
    int pc = 0, t;
    int64_t reg, val;
    printf("program %d (%ld) running\n", program_id, regs['p'-'a']);
    for (t = 0; ; t++) {
	if (pc < 0 || pc >= proglen) {
	    printf("%d: Program terminated at time %d (pc=%d)\n", program_id, t, pc);
	    return;
	}
	if (trace) {
	    printf("%d: => [%d] %d: ", program_id, t, pc);
	    print_insn(&prog[pc]);
	    print_regs();
	}
	switch (prog[pc].opcode) {
	default:
	    break;
	case OPC_SET:
	    assert(!prog[pc].reg_is_constant);
	    val = prog[pc].arg_is_constant ? prog[pc].arg : get_reg(t, pc, prog[pc].arg);
	    set_reg(prog[pc].reg, val);
	    pc++;
	    break;
	case OPC_ADD:
	    assert(!prog[pc].reg_is_constant);
	    val = prog[pc].arg_is_constant ? prog[pc].arg : get_reg(t, pc, prog[pc].arg);
	    set_reg(prog[pc].reg, get_reg(t, pc, prog[pc].reg) + val);
	    pc++;
	    break;
	case OPC_MUL:
	    assert(!prog[pc].reg_is_constant);
	    val = prog[pc].arg_is_constant ? prog[pc].arg : get_reg(t, pc, prog[pc].arg);
	    set_reg(prog[pc].reg, get_reg(t, pc, prog[pc].reg) * val);
	    pc++;
	    break;
	case OPC_MOD:
	    assert(!prog[pc].reg_is_constant);
	    val = prog[pc].arg_is_constant ? prog[pc].arg : get_reg(t, pc, prog[pc].arg);
	    set_reg(prog[pc].reg, get_reg(t, pc, prog[pc].reg) % val);
	    pc++;
	    break;
	case OPC_SND:
	    reg = prog[pc].reg_is_constant ? prog[pc].reg : get_reg(t, pc, prog[pc].reg);
#if PART == 1
	    freqval = reg;
	    freqval_set = 1;
#else
	    {
		char buf[80];
		int len = sprintf(buf, "%ld\n", reg), wlen;
		static int written_so_far = 0;
		written_so_far++;
		printf("%d: SND[%d] %ld (%d)\n", program_id, pfds[1-program_id][1], reg, written_so_far);
#if USE_PIPES
		if (-1 == (wlen = write(pfds[1-program_id][1], buf, len))) {
		    err(1, "write");
		}
#endif
#if USE_LOCAL_SOCKETS
		struct msghdr mhdr;
		struct iovec iov;
		memset(&mhdr, 0, sizeof mhdr);
		iov.iov_base = buf;
		iov.iov_len = len;
		mhdr.msg_iov = &iov;
		mhdr.msg_iovlen = 1;
		ssize_t ret;
		ret = sendmsg(pfds[1-program_id][1], &mhdr, 0);
		if (-1 == ret) {
		    err(1, "sendmsg");
		}
		wlen = ret;
#endif
		if (wlen != len) {
		    err(1, "short write");
		}
	    }
#endif
	    pc++;
	    break;
	case OPC_RCV:
#if PART == 1
	    reg = prog[pc].reg_is_constant ? prog[pc].reg : get_reg(t, pc, prog[pc].reg);
	    if (reg != 0) {
		printf("%d: Recovered frequency: %ld%s\n", program_id, freqval, freqval_set ? "":" (never set)");
		return;
	    }
#else
	    {
		char buf[80], *p;
		int len;
		static int read_so_far = 0;

		if (-1 == sem_wait(child_counter_semaphore)) {
		    err(1, "p%d: sem_wait()", program_id);
		}
#if USE_PIPES
		if (-1 == (len = read(pfds[program_id][0], buf, sizeof buf))) {
		    err(1, "read");
		}
#endif
#if USE_LOCAL_SOCKETS
		struct msghdr mhdr;
		struct iovec iov;
		memset(&mhdr, 0, sizeof mhdr);
		iov.iov_base = buf;
		iov.iov_len = sizeof buf;
		mhdr.msg_iov = &iov;
		mhdr.msg_iovlen = 1;
		ssize_t ret;
		ret = recvmsg(pfds[program_id][0], &mhdr, 0);
		if (-1 == ret) {
		    err(1, "recvmsg");
		}
		len = ret;
#endif
		if (-1 == sem_post(child_counter_semaphore)) {
		    err(1, "p%d: sem_post()", program_id);
		}
		val = strtoll(buf, NULL, 0);
		p = strchr(buf, '\n');
		if (p) {
		    *p = '\0';
		    len--;
		}
		read_so_far++;
		printf("%d: RCV[%d] %d bytes (%d) = %ld\n", program_id, pfds[program_id][0], len, read_so_far, val);
		set_reg(prog[pc].reg, val);
	    }
#endif
	    pc++;
	    break;
	case OPC_JGZ:
	    reg = prog[pc].reg_is_constant ? prog[pc].reg : get_reg(t, pc, prog[pc].reg);
	    val = prog[pc].arg_is_constant ? prog[pc].arg : get_reg(t, pc, prog[pc].arg);
	    if (reg > 0) {
		pc += val;
	    } else {
		pc++;
	    }
	    break;
	}
    }
}

void cleanup(void)
{
    if (child_counter_semaphore) {
	sem_close(child_counter_semaphore);
	sem_unlink(sem_name);
    }
    kill(pids[0], SIGTERM);
    kill(pids[1], SIGTERM);
}

int main(int argc, char **argv)
{
    int c;
    char *filename = "adv17-18.input";
    int do_print = 0;
    
    while ((c = getopt(argc, argv, "lf:t")) != EOF) {
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
	}
    }
    
    read_program(filename);
    if (do_print) {
	print_program();
    }
    printf("proglen %d\n", proglen);

    atexit(cleanup);
    child_counter_semaphore = sem_open(sem_name, O_CREAT|O_RDWR, 0777, 2);
    if (SEM_FAILED == child_counter_semaphore) {
	err(1, "sem_open(%s)", sem_name);
    }

#if USE_PIPES
    if (-1 == pipe2(pfds[0], O_DIRECT)) {
	err(1, "pipe(0)");
    }
    if (-1 == pipe2(pfds[1], O_DIRECT)) {
	err(1, "pipe(1)");
    }
#endif
#if USE_LOCAL_SOCKETS
    if (-1 == socketpair(AF_LOCAL, SOCK_DGRAM, 0, pfds[0])) {
	err(1, "socketpair(0)");
    }
    if (-1 == socketpair(AF_LOCAL, SOCK_DGRAM, 0, pfds[1])) {
	err(1, "socketpair(1)");
    }
#endif
    printf("pipe fds: (%d,%d) (%d,%d)\n", pfds[0][0], pfds[0][1], pfds[1][0], pfds[1][1]);
#if USE_PIPES
    {
	int val;
	printf("IN PARENT\n");
	if (-1 == fcntl(pfds[1][1], F_GETPIPE_SZ, &val)) {
	    err(1, "fcntl(F_GETPIPE_SZ)");
	}
	printf("Pipe capacity is %d bytes\n", val);
	if (-1 == fcntl(pfds[1][1], F_GETFD, &val)) {
	    err(1, "fcntl(F_GETFD)");
	}
	printf("Descriptor flags: %#x\n", val);
	if (-1 == fcntl(pfds[1][1], F_GETFL, &val)) {
	    err(1, "fcntl(F_GETFL)");
	}
	printf("File-status flags: %#x\n", val);
	//exit(0);
    }
#endif
    
    pids[0] = fork();
    if (-1 == pids[0]) {
	err(1, "fork(0)");
    }
    if (0 == pids[0]) {
	close(pfds[0][1]);
	close(pfds[1][0]);
#if USE_PIPES
    {
	int val;
	printf("IN CHILD\n");
	if (-1 == fcntl(pfds[1][1], F_GETPIPE_SZ, &val)) {
	    err(1, "fcntl(F_GETPIPE_SZ)");
	}
	printf("Pipe capacity is %d bytes\n", val);
	if (-1 == fcntl(pfds[1][1], F_GETFD, &val)) {
	    err(1, "fcntl(F_GETFD)");
	}
	printf("Descriptor flags: %#x\n", val);
	if (-1 == fcntl(pfds[1][1], F_GETFL, &val)) {
	    err(1, "fcntl(F_GETFL)");
	}
	printf("File-status flags: %#x\n", val);
	//exit(0);
    }
#endif
	set_reg('p'-'a', 0);
	program_id = 0;
	run_program();
	exit(0);
    }

    pids[1] = fork();
    if (-1 == pids[1]) {
	err(1, "fork(1)");
    }
    if (0 == pids[1]) {
	close(pfds[0][0]);
	close(pfds[1][1]);
	set_reg('p'-'a', 1);
	program_id = 1;
	run_program();
	exit(0);
    }

    close(pfds[0][0]);
    close(pfds[0][1]);
    close(pfds[1][0]);
    close(pfds[1][1]);

    int dead_children = 0;
    while (dead_children < 2) {
	int sts, pid;
	if (-1 == (pid = waitpid(-1, &sts, WNOHANG))) {
	    err(1, "wait");
	}
	if (0 == pid) {
	    int semval;
	    // check for deadlock
	    if (-1 == sem_getvalue(child_counter_semaphore, &semval)) {
		err(1, "sem_getvalue");
	    }
	    if (0 == semval) {
		static time_t when_first_noticed;
		if (0 == when_first_noticed) {
		    when_first_noticed = time(0);
		} else if (time(0) - when_first_noticed > 5) {
		    errx(0, "Deadlock detected");
		}
	    }
	    continue;
	}
	dead_children++;
	if (WIFEXITED(sts)) {
	    printf("Child %d exited with status %d\n", pid, WEXITSTATUS(sts));
	} else if (WIFSIGNALED(sts)) {
	    printf("Child %d exited from signal %d\n", pid, WTERMSIG(sts));
	} else {
	    printf("Child %d exited for an unknown reason\n", pid);
	}
	sleep(1);
    }
    
    return 0;
}
