#!/usr/bin/env perl

package Furries;
use base qw( Gtk2::GladeXML::Simple );

use strict;
use warnings;

use charnames ':full';
use utf8;

use Data::Dumper;

use Glib qw/TRUE FALSE/;
use Gtk2::Ex::Simple::List;
use Gtk2 '-init';
use Gtk2::SimpleList;

use Gnome2::Rsvg;
use List::Util qw(min);
use SVG;
use YAML qw(LoadFile);

my $glade_file = "board.glade";

my $square_size = 25;
my $margin      = 1;
my $board_size  = 15 * $square_size + 16 * $margin;
my $font_size   = 0.39 * $square_size;

# colours lifted from Google Images pictures of real scrabble boards (tile
# colour lightened)
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

# TODO other languages
# From Games::Literati
# XXX these are not the WordFeud point values
my %values = (
    a =>  1,
    b =>  3,
    c =>  3,
    d =>  2,
    e =>  1,
    f =>  4,
    g =>  2,
    h =>  4,
    i =>  1,
    j =>  8,
    k =>  5,
    l =>  1,
    m =>  3,
    n =>  1,
    o =>  1,
    p =>  3,
    q => 10,
    r =>  1,
    s =>  1,
    t =>  1,
    u =>  1,
    v =>  4,
    w =>  4,
    x =>  8,
    y =>  4,
    z => 10,
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
            #id     => "rect_${x}_${y}",
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

sub makeletter
{
    my ($board, %args) = @_;
    my ($x, $y, $letter) = @args{qw(x y letter)};
    makesquare($board, %args, colour => $colours{tile}, text => uc $letter, style => "fill:black");

    my $xc = $x * ($square_size + $margin) + $margin;
    my $yc = $y * ($square_size + $margin) + $margin;
    my $font_size = $font_size * 0.5;

    $board->text(
            x     => 0.50 * $square_size + $xc,
            y     => 0.77 * $square_size + $yc,
            style => "font-size:${font_size}pt;fill:black;font-family:monospace",
        )->tspan->cdata($values{lc $letter});
}

sub makeword
{
    my ($board, %args) = @_;
    my ($x, $y, $dir, $word) = @args{qw(x y dir word)};
    my $incs = $dir eq "down" ? [ 0, 1 ] : [ 1, 0 ];

    for my $letter (split //, $word) {
        makeletter($board, %args, letter => $letter, x => $x, y => $y);
        $x += $incs->[0];
        $y += $incs->[1];
    }
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

sub makedefaultboard
{
    my ($board) = @_;

    makesquare($board, x => 7, y => 7, colour => $colours{center}, text => "\N{BLACK STAR}", style => "font-size:@{[1.1*$font_size]}pt");
    for my $type (keys %specials) {
        for my $c (@{ $specials{$type} }) {
            makesquare($board, x => $c->[0], y => $c->[1], colour => $colours{$type}, text => $type);
        }
    }
}

sub loadboard
{
    my ($board, $game) = @_;
    for my $tile (@{ $game->{game}{tiles} }) {
        # TODO handle blanks
        makeletter($board, x => $tile->[0], y => $tile->[1], letter => $tile->[2], blank => $tile->[3]);
    }
}

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
    makedefaultboard($board);
    #makeword($board, x => 4, y => 7, dir => "right", word => "DARREN");
    my $dump = LoadFile("board.yaml") or die "Failed to load board";
    loadboard($board, $dump->{content} || die "Bad board");

    return $self;
}

sub boardarea_draw_cb
{
    my ($self, $b, $event) = @_;
    warn "drawing $b";

    # TODO cache pixbufs
    my $r = Gnome2::Rsvg::Handle->new;

    my $size = min($b->allocation->width, $b->allocation->height);
    $r->set_size_callback( sub { ($size, $size) });
    $r->write($svg->xmlify) or die "Failed to write SVG to RSVG handle";
    $r->close or die "Failed to parse SVG";
    # XXX deprecated, use Cairo
    $r->get_pixbuf->render_to_drawable($b->window, $b->style->fg_gc($b->state),
            0, 0, 0, 0, $size, $size, "GDK_RGB_DITHER_NONE", 0, 0);

    return FALSE; # propagate
}

sub boardclick_cb
{
    my ($self, $ebox, $event) = @_;
    #warn Dumper \@_;
    my ($x, $y) = ($event->x, $event->y);
    warn "$x, $y";
    #my $b = $self->get_widget(
    return FALSE;
}

sub gtk_main_quit { Gtk2->main_quit }

package main;
use strict;
use warnings;

Furries->new->run;

