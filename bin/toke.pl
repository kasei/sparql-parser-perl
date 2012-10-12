#!/usr/bin/perl

use strict;
use warnings;
use SPARQL::Lexer;
use SPARQL::Constants;

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
	
	my $l		= SPARQL::Lexer->new( file => $fh );
	try {
		while (my $t = $l->get_token) {
			$count++;
			die if ($limit and $count >= $limit);
			if ($verbose) {
				my $string	= join(' ', @{ $t->args });
				printf("%3d:%-3d %3d:%-3d %-20s %s\n", $t->start_line, $t->start_column, $t->line, $t->column, decrypt_constant($t->type), $string);
			}
		}
		
		if ($verbose) {
			my $elapsed	= tv_interval( $t0, [gettimeofday]);
			print STDERR sprintf("\n%d tokens in %.3fs (%.1f T/s)\n", $count, $elapsed, ($count/$elapsed));
		}
	} catch (Error $e) {
		$e->explain( $fh );
		next;
	}
}

