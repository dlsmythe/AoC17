#!/usr/bin/env python

import sys
import re

regvals = {}
def regval(regname):
    if regname in regvals:
        return regvals[regname]
    regvals[regname] = 0
    return 0

regmaxval = 0
def update_reg(lineno, regname, op, arg):
    global regmaxval
    val = regval(regname)
    if op == 'inc':
        val += int(arg)
    elif op == 'dec':
        val -= int(arg)
    else:
        print 'Error at line {0}: unknown operation "{1}"'.format(lineno, op)
        sys.exit(1)
    regvals[regname] = val
    if val > regmaxval:
        regmaxval = val

def pred_val(lineno, regname, op, arg):
    val = regval(regname)
    if op == '==':
        return val == int(arg)
    if op == '!=':
        return val != int(arg)
    if op == '<':
        return val < int(arg)
    if op == '<=':
        return val <= int(arg)
    if op == '>':
        return val > int(arg)
    if op == '>=':
        return val >= int(arg)
    print 'Error at line {0}: unknown relational operator "{1}"'.format(lineno, op)
    sys.exit(1)

def read_insns():
    prog = []
    pat = re.compile(r'^(\S+)\s+(\S+)\s+(\S+)\s+if\s+(\S+)\s+(\S+)\s+(\S+)')
    linecount=0
    for line in sys.stdin:
        linecount += 1
        m = re.match(pat,line.strip())
        if not m:
            print 'garbled line at {0}: {1}'.format(linecount, line)
            sys.exit(0)
        insn = {}
        insn['line'] = linecount
        insn['destreg'] = m.group(1)
        insn['destop'] = m.group(2)
        insn['destarg'] = m.group(3)
        insn['predreg'] = m.group(4)
        insn['predop'] = m.group(5)
        insn['predarg'] = m.group(6)
        prog.append(insn)
    return prog

prog = read_insns()
for i in prog:
    if pred_val(i['line'], i['predreg'], i['predop'], i['predarg']):
        update_reg(i['line'], i['destreg'], i['destop'], i['destarg'])

print 'Regs at end:'
rvs = []
for k in regvals:
    rvs.append(regvals[k])
print 'largest val: {0}'.format(sorted(rvs)[-1])
print 'largest ever: {0}'.format(regmaxval)
print 'Regs at end: {0}'.format(rvs)
