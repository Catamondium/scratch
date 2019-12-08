#!/usr/bin/env rakudo

sub onA(Channel:D $ca, Channel:D $cb) {
    return {
        react {
            whenever $ca.Supply -> $v {
                say $v;
                $cb.send("B");
                LAST {say "A finishing"}
            }
        }
    }
}

sub onB(Channel:D $ca, Channel:D $cb) {
    return {
        react {
            whenever $cb.Supply -> $v {
                say $v;
                $ca.send("A");
                LAST {say "B finishing"}
            }
        }
    }
}

my $ca = Channel.new;
my $cb = Channel.new;

my $ta = Thread.new(:code(onA($ca, $cb)), :app_lifetime(True));
my $tb = Thread.new(:code(onB($ca, $cb)), :app_lifetime(True));

$ca.send("A");
$ta.run();
$tb.run();
sleep(3);