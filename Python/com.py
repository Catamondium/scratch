#!/usr/bin/env python3
import cmd
import sys


class Shell(cmd.Cmd):
    intro = "Crappy shell"
    prompt = "!Sh> "
    file = None

    def do_reverse(self, arg):
        "Prints arguments in reverse"
        print(f"{arg[::-1]}")

    def do_echo(self, arg):
        "Prints own arguments"
        print(arg)

    def do_quit(self, arg):
        "Exit shell"
        print("Byeeeee")
        self.close()
        return True

    # -- record & playback, misc --
    def do_record(self, arg):
        "Save future commands to file"
        self.file = open(arg, 'a')

    def do_playback(self, arg):
        "Playback commands from file"
        self.close()
        with open(arg) as f:
            self.cmdqueue.extend(f.read().splitlines())

    def precmd(self, line):
        line = line.lower()
        if self.file and 'playback' not in line:
            print(line, file=self.file)
        return line

    def close(self):
        if self.file:
            self.file.close()
            self.file = None


if __name__ == '__main__':
    Shell().cmdloop()
