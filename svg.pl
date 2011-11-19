#!/usr/bin/env perl
use strict;
use warnings;

use SVG;

my $svg = SVG->new(width=>1500, height=>1500);
my $layer = $svg->group(id => 'layer');
my $board = $layer->group(id => 'board');

my $square_size = 100;
my $margin = 0;

my %colours = (
        TW => "#FB4C08",
        DL => "#2E89CD",
        DW => "#E32745",
        TL => "#579B0B",
    );

sub makesquare
{
    my ($board, %args) = @_;
    my ($x, $y, $style, $colour, $text) = @args{qw(x y style colour text)};
    $style ||= "";
    $text ||= "";

    my $xc = $x * $square_size + $margin;
    my $yc = $y * $square_size + $margin;

    my $square = $board->rect(
            id     => "rect_${x}_${y}",
            width  => $square_size,
            height => $square_size,
            x      => $xc,
            y      => $yc,
            style  => "fill:$colour",
        );

    $board->text(
            x     => 20 + $xc,
            y     => 67 + $yc,
            style => "font-size:2em;fill:white;font-family:monospace;$style",
        )->tspan->cdata($text);
}

my %specials = (
        TW => [ [ 0, 0], [ 7, 0], [14, 0],
                [ 0, 7],          [14, 7],
                [ 0,14], [ 7,14], [14,14] ],
        DL => [ [ 3, 0],          [11, 0],
                [ 6, 2],          [ 8, 2],
                [ 0, 3], [ 7, 3], [14, 3],
                [ 2, 6], [ 6, 6], [ 8, 6], [12, 6],
                [ 3, 7],          [11, 7],
                [ 2, 8], [ 6, 8], [ 8, 8], [12, 8],
                [ 0,11], [ 7,11], [14,11],
                [ 6,12],          [ 8,12],
                [ 3,14],          [11,14], ],
        DW => [ [ 1, 1],          [13, 1],
                [ 2, 2],          [12, 2],
                [ 3, 3],          [11, 3],
                [ 4, 4],          [10, 4],
                [ 4,10],          [10,10],
                [ 3,11],          [11,11],
                [ 2,12],          [12,12],
                [ 1,13],          [13,13], ],
        TL => [ [ 5, 1],          [ 9, 1],
                [ 1, 5], [ 5, 5], [ 9, 5], [13, 5],
                [ 1, 9], [ 5, 9], [ 9, 9], [13, 9],
                [ 5,13],          [ 9,13], ],

    );
for my $type (keys %specials) {
    for my $c (@{ $specials{$type} }) {
        makesquare($board, x => $c->[0], y => $c->[1], colour => $colours{$type}, text => $type);
    }
}

print $svg->xmlify;

