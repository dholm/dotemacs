#!/bin/sh
# Emacs please open this in -*-Tcl-*- mode
# do not remove this backslash! -> \
    exec tclsh $1 ${1+"$@"}
#
# loosely based on Python code from
# http://disgruntled-programmer.com/notes/emacs-with-outlook.html
#
package require tcom
#
# find Outlook:-
if { [catch {::tcom::ref getactiveobject Outlook.Application} o] } {
    puts "Couldn't find Outlook via COM: $o"
    exit 1
}
# get the current item:-
if { [catch {$o -get ActiveInspector} a] } {
    puts "Couldn't get the ActiveInspector for Outlook via COM: $a"
    exit 2
}
if { [catch {$a -get CurrentItem} i] } {
    puts "Couldn't get the CurrentItem for Outlook via COM: $i"
    exit 3
}
# and finally slurp its body
if { [catch {$i -get Body} body] } {
    puts "Couldn't retrieve body from CurrentItem via COM: $body"
    exit 4
}
#
# spew it out to stdout
#
puts $body
# that's it