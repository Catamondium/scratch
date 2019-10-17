#!/usr/bin/env python3
import tarfile as tf
from cmd import Cmd
from pathlib import Path
import sys

"""
TODO

lazy TemporaryDirectory
tokenize by spaces, escaped by '\'

globbing support:
	tar context
		fnmatch
	'real' context (during extractions, appends etc.)
		glob.glob
'real' effects:
	cd
	shell runs

Path-based trie
cd & cd environ
multiple files, append?
columnize ls (reparse stringio redirect?)
perms: chown, chmod, chgrp(?)
editing: edit, mv, rm, add
    might get complicated tracking changes
        heterogenous trie values?
    
    add:
        create temporary; either duplicate name or extension preserving
        load tmp into $EDITOR, overridable?
        keep tmp until making changes, or commit to BytesIO/TarInfo?
"""
class Tarcmd(Cmd):
    intro = "Tar explorer shell"
    postfix = "T> "
    prompt = postfix
    file = None
    members = None

    def do_mount(self, target):
        """Mount a new tar archive"""
        self.file = target
        with tf.open(target) as f:
            self.members = f.getmembers()
    
    def do_ls(self, verbose=False):
        """List members"""
        with tf.open(self.file) as f:
            f.list(verbose=verbose)

if __name__ == "__main__":
    from argparse import ArgumentParser, FileType
    parser = ArgumentParser()
    parser.add_argument("tar", type=Path)
    args = parser.parse_args()
    cmd = Tarcmd()
    cmd.do_mount(args.tar)
    try:
        cmd.cmdloop()
    except KeyboardInterrupt:
        pass
