#! /usr/bin/env perl
use strict;
use warnings;
use Test::More;

use_ok 'Path::Match'
	or BAIL_OUT;

my @patterns= (
	# common prefix
	'/a/1',
	'/a/2',
	'/a/3',
	# superset/subset
	'a',
	'aa',
	'aaa',
	'aaaa',
	'aaaaa',
	# duplicates
	'x',
	'x',
);
my $matcher= Path::Match->new(patterns => \@patterns);
#use DDP; &p($matcher->_compile);
# Test that each pattern matches itself and only itself
for my $literal (@patterns) {
	my @actual;
	my @expected= $literal eq 'x'? ( ['x',[]], ['x',[]] ) : ( [$literal,[]] );
	# callback returns false to trigger all backtracking
	ok( !$matcher->search($literal, sub { push @actual, [@_]; 0 }), 'search returns false' );
	is_deeply( \@actual, \@expected, "pattern '$literal'" )
		or diag explain \@actual;
}

done_testing;
