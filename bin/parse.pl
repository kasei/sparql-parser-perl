#!/usr/bin/perl

use strict;
use warnings;
use SPARQL::Parser;

use Time::HiRes qw(gettimeofday tv_interval);
use Data::Dumper;
use File::Spec;
use TryCatch;

$|				= 1;
my $verbose		= 0;
my $limit		= 0;
if ($ARGV[0] eq '-v') {
	shift;
	$verbose	= 1;
}

while (@ARGV) {
	my $filename	= shift;
	print "parsing $filename ...\n";
	open(my $fh, '<:encoding(UTF-8)', $filename) or die $!;
	my $count	= 0;
	my $t0		= [gettimeofday];
	
	my $p		= SPARQL::Parser->new();
	try {
		my $q	= $p->parse_file($fh);
		warn $q->as_string;
	} catch (RDF::Trine::Error::ParserError::Explainable $e) {
		$e->explain( $fh );
		next;
	}
}

