#!/usr/bin/perl

use v5.10;
use strict;
use warnings;
use SPARQL::Lexer;
use SPARQL::Constants;
use Scalar::Util qw(blessed);

use Data::Dumper;
use File::Spec;
use TryCatch;

my $filename	= shift;
# print "parsing $filename ...\n";
open(my $fh, '<:encoding(UTF-8)', $filename) or die $!;
my $l		= SPARQL::Lexer->new( file => $fh, comments => 1 );
try {
	pretty_print($l);
} catch (Error $e) {
	$e->explain( $fh );
	next;
}




sub pretty_print {
	my $l		= shift;
	my $state	= { count => 0, newline => 1, bracelevel => 0, semicolon => 0 };
	while (my $t = $l->get_token) {
# 		warn decrypt_constant($t->type) . "\n";
		if ($t->type == RBRACE) {
			$state->{bracelevel}--;
		}
		unless ($state->{'newline'} or $state->{'space'}) {
			if ($t->type == LBRACE) {
				print " ";
			} elsif ($t->type == KEYWORD and ($t->value eq 'PREFIX' or $t->value eq 'BASE' or $t->value eq 'SELECT' or $t->value eq 'ASK' or $t->value eq 'DESCRIBE' or $t->value eq 'CONSTRUCT')) {
				print "\n";
				$state->{'newline'}	= 1;
			} elsif ($t->type == RBRACE and blessed($state->{'last'}) and $state->{'last'}->type != LBRACE) {
				print "\n";
				$state->{'newline'}	= 1;
			} elsif ($t->type != RBRACE and blessed($state->{'last'}) and $state->{'last'}->type == LBRACE) {
				print "\n";
				$state->{'newline'}	= 1;
			} elsif (not($t->type == KEYWORD and $t->value eq 'UNION') and blessed($state->{'last'}) and $state->{'last'}->type == RBRACE) {
				print "\n";
				$state->{'newline'}	= 1;
			} elsif ($t->type == KEYWORD and ($t->value eq 'OPTIONAL' or $t->value eq 'MINUS')) {
				print "\n";
				$state->{'newline'}	= 1;
			} elsif ($t->type == KEYWORD and blessed($state->{'last'}) and $state->{'last'}->type == LPAREN) {
				# no-op
			} elsif ($t->type == PREFIXNAME and blessed($state->{'last'}) and $state->{'last'}->type != HATHAT) {
				print " ";
			} elsif ($t->type == KEYWORD or $t->type == DOT) {
				print " ";
			} elsif ($t->type == SEMICOLON) {
				print " ";
			} elsif ($t->type == COMMENT) {
				print " ";
			} elsif ($t->type == VAR or $t->type == IRI or $t->is_number or $t->is_string) {
				if (not($state->{parenlevel})) {
					print " ";
				}
			} elsif (blessed($state->{'last'}) and $state->{'last'}->type == KEYWORD and $state->{'last'}->value eq 'SELECT') {
				print " ";
			} elsif ($t->type != LPAREN and blessed($state->{'last'}) and $state->{'last'}->type == KEYWORD) {
				print " ";
			} elsif ($t->type != COMMA and blessed($state->{'last'}) and $state->{'last'}->type == RPAREN) {
				print " ";
			}
		}
		
		print_indent($state) if ($state->{'newline'});
		delete $state->{'newline'};
		delete $state->{'space'};
		
		given ($t->type) {
			when (KEYWORD) {
				my $k	= $t->value;
				if ($k eq 'A') {
					print 'a';
				} else {
					print $k;
				}
			}
			when (VAR) {
				print '?' . $t->value;
			}
			when (LPAREN) {
				print $t->value;
				$state->{parenlevel}++;
			}
			when (RPAREN) {
				print $t->value;
				$state->{parenlevel}--;
			}
			when (LBRACE) {
				print $t->value;
				$state->{bracelevel}++;
			}
			when (RBRACE) {
				print $t->value;
			}
			when (PREFIXNAME) {
				my ($ns, $local)	= @{ $t->args };
				print join(':', $ns, $local);
			}
			when (STRING1D) {
				my $v	= $t->value;
				for ($v) {
					s/\\/\\\\/g;
					s/"/\\"/g;
					s/\n/\\n/g;
				}
				print qq["$v"];
			}
			when (STRING1S) {
				my $v	= $t->value;
				for ($v) {
					s/'/\\'/g;
					s/\\/\\\\/g;
					s/\n/\\n/g;
				}
				print qq['$v'];
			}
			when (STRING3S) {
				my $v	= $t->value;
				for ($v) {
					s/\\/\\\\/g;
					s/\n/\\n/g;
				}
				print qq['''$v'''];
			}
			when (STRING3D) {
				my $v	= $t->value;
				for ($v) {
					s/\\/\\\\/g;
					s/\n/\\n/g;
				}
				print qq["""$v"""];
			}
			print $t->value when (COMMA);
			when (SEMICOLON) {
				print $t->value;
				$state->{'semicolon'}	= 1;
			}
			when (DOT) {
				print $t->value;
				$state->{'semicolon'}	= 0;
			}
			when (IRI) {
				print '<' . $t->value . '>';
			}
			print $t->value when (PLUS);
			print '||' when (OROR);
			print '&&' when (ANDAND);
			print '<' when (LT);
			print '<=' when (LE);
			print '>' when (GT);
			print '>=' when (GE);
			print '=' when (EQUALS);
			print '!=' when (NOTEQUALS);
			print '^^' when (HATHAT);
			print $t->value when (INTEGER);
			print $t->value when (DECIMAL);
			print $t->value when (DOUBLE);
			when (LANG) {
				my $lang	= $t->value;
				print "@" . $lang;
			}
			when (COMMENT) {
				my $comment	= $t->value;
				$comment	=~ s/^\s+//;
				print "# $comment\n";
				$state->{'newline'}	= 1;
			}
			die 'pretty-printing not implemented for ' . decrypt_constant($t->type);
		}

		# $t->type == LBRACE or $t->type == RPAREN or 
		if ($t->type == COMMA or $t->type == SEMICOLON or $t->type == DOT or $t->is_relop) {
			if ($t->type == SEMICOLON or $t->type == DOT) {
				print "\n";
				$state->{'newline'}	= 1;
			} else {
				print " ";
				$state->{'space'}	= 1;
			}
		} elsif ($t->type == KEYWORD and $t->value eq 'AS') {
			print " ";
			$state->{'space'}	= 1;
		}
		
		$state->{'count'}++;
		$state->{'last'}	= $t;
		my $string	= join(' ', @{ $t->args });
	}
	print "\n";
}

sub print_indent {
	my $state	= shift;
	my $level	= $state->{bracelevel};
	$level++ if ($state->{semicolon});
# 	printf("[%4d] ", $level);
	print '  ' x $level;
}
