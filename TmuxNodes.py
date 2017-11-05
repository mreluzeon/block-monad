#!/usr/bin/env python3
import sys
import subprocess


NAMES = ['Sarah', 'Jacob', 'John']


subprocess.run('stack build', shell=True)


try:
    n_of_nodes = int(sys.argv[1])
except Exception:
    n_of_nodes = 3


def iter_commands(n_of_nodes):
    for i, name in zip(range(n_of_nodes), NAMES):
        yield '"stack exec block-monad-exe {}"'.format(name)


commands = list(iter_commands(n_of_nodes))
fst_line = commands[0]
tmux = ['new-session ' + fst_line]
for line in commands[1:]:
    tmux.append('split-window -h ' + line)

command = 'tmux ' + '\; '.join(tmux)
print(command)
subprocess.run(command, shell=True)
