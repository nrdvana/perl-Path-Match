#! /usr/bin/env perl
use strict;
use warnings;
use Test::More;

use_ok 'Path::Match'
	or BAIL_OUT;

my @patterns= qw(
	t/**
	**/a/**
	**/README
	a/**/c
);
my $matcher= Path::Match->new(\@patterns);
my @tests= (
	[ 't/lib/Test.pm' => [ $patterns[0], ['lib/Test.pm'] ] ],
	[ 'x/a/b'         => [ $patterns[1], ['x','b'] ] ],
	[ 't/README'      => [ $patterns[0], ['README'] ], [ $patterns[2], ['t'] ] ],
	# Shell globstar is allowed to match "zero directories", meaning the '/' is more like
	# a boundary than like a character to be matched.
	[ 'a/b/c'         => [ $patterns[3], ['b'] ], [ $patterns[1], ['', 'b/c'] ] ],
	[ 'README'        => [ $patterns[2], [''] ] ],
);
#diag explain ($matcher->_compile);
#local $Path::Match::DEBUG= sub { warn "@_\n" };
for (@tests) {
	my ($path, @expected)= @$_;
	my @actual;
	ok( !$matcher->search($path, sub { push @actual, [@_]; 0 }), 'search = false' );
	is_deeply( \@actual, \@expected, "path $path" )
		or diag explain \@actual;
}

done_testing;
