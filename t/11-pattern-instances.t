#! /usr/bin/env perl
use strict;
use warnings;
use Test::More;

use_ok 'Path::Match'
	or BAIL_OUT;

{ package PatternObject;
	sub new { bless $_[1], $_[0] }
	sub pattern { $_[0]{pattern} }
}

my @patterns= (
	'a',
	[ 'b' ],
	{ pattern => 'c' },
	PatternObject->new({ pattern => 'd' })
);
my $matcher= Path::Match->new(\@patterns);
my @tests= (
	[ 'a' => [ $patterns[0], [] ] ],
	[ 'b' => [ $patterns[1], [] ] ],
	[ 'c' => [ $patterns[2], [] ] ],
	[ 'd' => [ $patterns[3], [] ] ],
);
#local $Web::ConServe::PathMatch::DEBUG= sub { warn "$_[0]\n" };
for (@tests) {
	my ($path, @expected)= @$_;
	my @actual;
	ok( $matcher->search($path, sub { push @actual, [@_]; 1 }) );
	is_deeply( \@actual, \@expected, "path $path" );
}

done_testing;
