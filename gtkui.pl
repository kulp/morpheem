#!/usr/bin/env perl

package Furries;
use base qw( Gtk2::GladeXML::Simple );

use strict;
use warnings;

use charnames ':full';
use utf8;

use Glib qw/TRUE FALSE/;
use Gtk2::Ex::Simple::List;
use Gtk2 '-init';
use Gtk2::SimpleList;
use List::Util qw(min);

use Gnome2::Rsvg;
use SVG;

my $glade_file = "board.glade";

my $square_size = 25;
my $margin      = 1;
my $board_size  = 15 * $square_size + 16 * $margin;
my $font_size   = 0.39 * $square_size;

my %colours = (
        TW => "#FB4C08",
        DL => "#2E89CD",
        DW => "#E32745",
        TL => "#579B0B",

        board  => "#E3F6FF",
        border => "#20111C",
        center => "#E32745",
        tile   => "#DFBC95",
    );

my $svg = SVG->new(width => $board_size, height => $board_size);
my $layer = $svg->group(id => 'layer');
my $board = $layer->group(id => 'board');
my $rect = $board->rect(
        id     => "rect_board",
        style  => "fill:$colours{board}",
        x      => 0,
        y      => 0,
        width  => $board_size,
        height => $board_size,
    );

sub makesquare
{
    my ($board, %args) = @_;
    my ($x, $y, $style, $colour, $text) = @args{qw(x y style colour text)};
    $style ||= "";
    $text ||= "";

    my $xc = $x * ($square_size + $margin) + $margin;
    my $yc = $y * ($square_size + $margin) + $margin;

    my $square = $board->rect(
            id     => "rect_${x}_${y}",
            width  => $square_size,
            height => $square_size,
            x      => $xc,
            y      => $yc,
            style  => "fill:$colour",
        );

    $board->text(
            x     => 0.20 * $square_size + $xc,
            y     => 0.67 * $square_size + $yc,
            style => "font-size:${font_size}pt;fill:white;font-family:monospace;$style",
        )->tspan->cdata($text);
}

sub drawmargins
{
    my ($board) = @_;
    for my $i (0 .. 15) {
        my $c = $i * ($square_size + $margin);
        $board->rect(
                id     => "margin_row_$i",
                y      => $c,
                height => $margin,
                width  => $board_size,
                style  => "fill:$colours{border}",
            );
        $board->rect(
                id     => "margin_col_$i",
                x      => $c,
                height => $board_size,
                width  => $margin,
                style  => "fill:$colours{border}",
            );
    }
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

sub new
{
    my $class = shift;
    my $self;
    if (-f $glade_file) {
        $self = $class->SUPER::new($glade_file);
    } else {
        die "Glade file '$glade_file' does not exist";
    }

    drawmargins($board);

    makesquare($board, x => 7, y => 7, colour => $colours{center}, text => "\N{BLACK STAR}", style => "font-size:@{[1.1*$font_size]}pt");
    for my $type (keys %specials) {
        for my $c (@{ $specials{$type} }) {
            makesquare($board, x => $c->[0], y => $c->[1], colour => $colours{$type}, text => $type);
        }
    }

    return $self;
}

sub board_button_press_event_cb
{
    die "halp";
}

sub boardimage_expose_event_cb
{
    my ($self) = @_;
    warn "exposed";
    #makesquare($board, x => 1, y => 2, colour => $colours{tile}, text => "D", style => "fill:black;text-align:center");

    # TODO cache pixbufs
    my $b = $self->get_widget('boardimage');
    my $r = Gnome2::Rsvg::Handle->new;

    $r->set_size_callback( sub { return (min($b->allocation->width, $b->allocation->height)) x 2 });
    $r->write($svg->xmlify) or die "Failed to write SVG to RSVG handle";
    $r->close or die "Failed to parse SVG";
    my $pixbuf = $r->get_pixbuf;

    $b->set_from_pixbuf($pixbuf);

    #binmode STDOUT, ":utf8";
    #print $svg->xmlify;

    return FALSE; # propagate
}

sub gtk_main_quit { Gtk2->main_quit }

package main;
use strict;
use warnings;

Furries->new->run;

