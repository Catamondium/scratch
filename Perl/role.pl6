#!/usr/bin/perl6
use strict;
use worries;

# quantity capabilities
role Unitish[$unit= fail ("SI quantifier must be provided")] {
	has $.SI-unit = $unit;
	method gist {
		# Scale prefixes, optimally defined in conversion table, but off topic
		given self {
			# matches down from here, hence unidirectional boolexpr
			when $_ < 1e0 { return self * 1e+3 ~ 'm' ~ $.SI-unit }
			when $_ < 1e3 { return self ~ $.SI-unit }
			when $_ < 1e6 { return self * 1e-3 ~ 'k' ~ $.SI-unit }
			when $_ < 1e9 { return self * 1e-6 ~ 'M' ~ $.SI-unit }
		}
	}
}

# quantity suffixes
role SI-second   does Unitish[<s>] {}
role SI-metre    does Unitish[<m>] {}
# technically scaled SI quantity, but prefixes remain generalised
role SI-gram 	 does Unitish[<g>] {}

# 'conversion' operators

sub postfix:<s>(Numeric $n){($n) does SI-second}
sub postfix:<m>(Numeric $n){($n) does SI-metre}

# 'g' unprefixed quantifier
sub postfix:<g>(Numeric $n){($n) does SI-gram}
# 'kg' requires scaling
sub postfix:<kg>(Numeric $n){($n * 1000) does SI-gram}

constant g = 9.81;

role SI-Newton does Unitish[<N>] {} # derived quantifier
multi sub N(SI-gram $mass, SI-metre $dist, SI-second $s) returns SI-Newton {
	($mass * ($dist / ($s**2))) does SI-Newton }
multi sub N(SI-gram $mass) returns SI-Newton {
	($mass * g) does SI-Newton }

say 5000g.gist;
say 5000g.^name; # SI-gram twice applied?
say N(50kg).^name;
say N(50g);
