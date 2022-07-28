#! /usr/bin/env perl
use strict;
use warnings;
use Test::More;

use_ok 'Path::Match'
	or BAIL_OUT;

# normal hashref
my $m= Path::Match->new([ '*' ]);
is_deeply( $m->patterns, [ '*' ], 'arrayref' );

$m= Path::Match->new({ patterns => [ '*' ] });
is_deeply( $m->patterns, [ '*' ], 'hashref' );

$m= Path::Match->new(patterns => ['*']);
is_deeply( $m->patterns, [ '*' ], 'k/v list' );

$m= Path::Match->new([ { pattern => '*' } ]);
is_deeply( $m->patterns, [ { pattern => '*' } ], 'arrayref of hashref' );

done_testing;
