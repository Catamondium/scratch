#!/usr/bin/env python3
import sys
import gi
gi.require_version("Gtk", "3.0")
from gi.repository import Gtk

# gtk setup stuff
builder = Gtk.Builder()
builder.add_from_file("gui.glade")
primary = builder.get_object("primary")
#test = builder.get_object("test")
stream = builder.get_object("stream") # text buffer

handlers = {} 
def register(handler):
    def decorator(func):
        handlers[handler] = func
        return func
    return decorator

def printGtk(output):
    print(output)
    text = "{}\n".format(output)
    stream.insert(stream.get_end_iter(), text)

@register("onButtonPressed")
def pressed(button):
    printGtk("Button pressed")

@register("onDelete")
def kill(*args):
    printGtk("Window killed")
    Gtk.main_quit
    sys.exit()

printGtk(handlers)

builder.connect_signals(handlers)
primary.show_all()
#test.show_all()

Gtk.main()
