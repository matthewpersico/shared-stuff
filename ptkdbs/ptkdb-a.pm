#
# Devel::ptkdb - a graphical Perl debugger
#
# See the POD at the end of the file for details
#

########################################################################
#
# We will return to package DB later, but it's easier to declare some
# vars up here.
#
package DB;

# Expedient fix for perl 5.8.0. True DB::DB is further down.
sub DB {}

#?# These "use's" are in package DB space:
#?use Tk;
#?use strict;
#?use warnings;
use vars qw($VERSION @dbline %dbline); # No 'our'?

########################################################################
#
# Declare the package up here, pull it in below the main code
#
package Playlist;

########################################################################
#
# Our package for the debugger code. This package is the main_window
# object for the debugger. We start with the Devel:: prefix because we
# want to install it with the DB:: package that is required to be in a
# Devel/ subdir of a directory in the @INC set.
#
package Devel::ptkdb;

# This is a REALLY old version of Perl, but unless we REALLY need to
# break this, we won't.
require 5.004;

use strict;
use warnings;

#
# CPAN modules
#
use Config;
use FileHandle;
use Devel::PL_origargv;
use Cwd;

# Yes, reloading in Devel::ptkdb space. >>>>> Do we really need this?
use Tk;

# If any messages are printed to console about assumming some Tk
# widget is required, put the requirement here to eliminate the
# messages.
use Tk::Balloon;
use Tk::NoteBook;
###use Playlist;
use Tk::ROText;
use Tk::TextUndo;
use Tk::Tree;
use Tk::Table;

use Data::Dumper;

# Do this check once, rather than repeating the can comparison again
# and again.
my $dumpFunc =(Data::Dumper->can('Dumpxs')
   ? 'Dumpxs'
   : 'Dump' );

use vars qw(@dbline); # Why do we need the one above in DB space?
use vars qw(@brkPtFixedMsg);
use vars qw(%brkPtFixedMsgCalled);
use vars qw($bkrPtsInitiallyLoaded);
use constant MENU_SEP => q(-);

# Do this check once, rather than repeating the string comparison
# again and again.
my $isWin32 = $^O eq 'MSWin32';

#
# Only functions with non obvious names or functionality will have heading comments
#
sub DoBugReport {
    my $self = shift;
    my ($str) = 'sourceforge.net/tracker/?atid=4376098,group_id=43854&func=browse';
    my (@browsers) = qw/netscape mozilla/;
    my ($fh, $pid, $sh);

    if($isWin32 ) {
	$sh = q();
	@browsers = qq("$ENV{PROGRAMFILES}\\\\Internet Explorer\\\\IEXPLORE.EXE");
    } else {
	$sh = 'sh';
	$str = qq('http://$str');
    }

    $self->{main_window}->Busy(-recurse => 1);
    $self->{main_window}->update;
    for(@browsers ) {
	$fh = new FileHandle();
	$pid = open($fh, "$sh $_ $str 2&> /dev/null |");
	sleep(2);
	waitpid $pid, 0;
	return if( $? == 0 );
	close($fh);
    }
    $self->{main_window}->Unbusy();
    $self->{main_window}->update;

    $self->simpleInfoBox('Bug Report',
			 join("\n",
			      'No browser found.',
			      'Please submit a bug report at the following URL:',
			      'http://sourceforge.net/trackerflatid=4376098,group_id=438548,func=browse'
			     ) );
}

# NOTE: The definition of check_avail() needs to stay above the
# 'BEGIN' subroutine below, otherwise it will not have been compiled
# by the time that it is called by sub BEGIN.
sub check_avail {
    # Check to see if the package actually exists. If it does import the
    # routines and return a true value.
    my ($mod, @list) = @_;
    eval {
	require $mod; import $mod @list;
    };
    return ($@ ? 0 : 1);
}

# We define consoleSay, debugSay and debugDump here for the same
# reason, just in case either is needed in BEGIN eventally. We will
# also inject these into the DB namespace further down. If you add
# any functions that you want available in DB as well as Devel::ptkdb
# namespaces, go find the '# Inject' tag below and do so.
sub consoleSay {
    printf("ptkdb> %s \n",
	   join("\n ",map{split(/\n/,$_)}@_));
}

sub debugDump {
    my @dumps = @_;
    my @msg;
    for my $dump (@dumps) {
	$dump->{desc} =($dump->{desc}
			? $dump->{desc} . ': '
			: q() );
	push @msg, $dump->{desc} . Data::Dumper->Dump([$dump->{ref}],['*' . $dump->{name}]), "\n";
    }
    $msg[0] = sprintf("At package:[%s] sub:[%s] line:[%d]\n%s",
		      (caller(1))[0,3],
		      (caller(0))[2],
		      $msg[0]);
    consoleSay(@msg);
}

sub debugSay {
    my @msg = @_;
    # Note: line number and subroutine may not agree if the consoleSay
    # call is in a callback.
    $msg[0] = sprintf("At package:[%s] sub:[%s] line:[%d]\n%s",
		      (caller(1))[0,3],
		      (caller(0))[2],
		      $msg[0]);
    consoleSay(@msg);
}

sub BEGIN {
    consoleSay(q(Seria1:2014...));
    #
    # Setup for restart. We set this up as early as possible to avoid
    # any side effects from later manipulations.
    #

    # The whole command - perl exec, perl args, program, program args
    ## TODO - 2014/03/10 - If Devel::PL_origargv not available, do something less clever and console_say it.
    @Devel::ptkdb::restart::cmd = Devel::PL_origargv->get();

    # ENV - We will want to restore the original environment when we
    # restart, especially since the environment contains PATH; if
    # perl was invoked as 'perl', we will need to have the same PATH
    # in place to get the same perl we started with.
    %Devel::ptkdb::restart::ENV = %ENV;

    # Location - we need to go back to where we started, or else you
    # may not find $0.
    $Devel::ptkdb::restart::dir = cwd();

    # These statements must appear after the check_avail definition
    # above.
    $DB::on = 0;
    $DB::subroutine_depth = 0; # our subroutine depth counter
    $DB::step_over_depth = -1;

    # The bindings and font specs for these operations have been
    # placed here to make them accessible to people who might want to
    # customize the operations. The 'bind.html' file, included in the
    # PerlTk FAQ has a fairly good explanation of the binding syntax.

    # These lists of key bindings will be applied to the "Step In",
    # "Step Out", "Return" Commands
    $Devel::ptkdb::pathSep = '\x00';
    $Devel::ptkdb::pathSepReplacement = '\0x01';
    @Devel::ptkdb::step_in_keys   = ( '<Shift-F9>', '<Alt-s>' ); # step into a subroutine
    @Devel::ptkdb::step_over_keys = ( '<F9>',       '<Alt-n>' ); # step over a subroutine
    @Devel::ptkdb::return_keys    = ( '<Alt-u>'               ); # return from a subroutine

    # These will be bound only to the code pane, so that other widgets can have
    # Their own popup menus
    @Devel::ptkdb::step_in_mouse       = ( '<Button-3>' ); # step into a subroutine
    @Devel::ptkdb::step_over_mouse     = ( '<Shift-Button-3>' ); # step over a subroutine
    @Devel::ptkdb::return_mouse        = ( '<Control-Button-3>' ); # return from a subroutine
    @Devel::ptkdb::toggle_breakpt_keys = ( '<Alt-b>' ); # set or unset a breakpoint

    # Fonts used in the displays
    @Devel::ptkdb::button_font =
      $ENV{PTKDB_BUTTON_FONT}     ? ( '-font' => $ENV{PTKDB_BUTTON_FONT} ) : (); # font for buttons
    @Devel::ptkdb::code_text_font =
      $ENV{PTKDB_CODE_FONT}       ? ( '-font' => $ENV{PTKDB_CODE_FONT} ) : ();
    @Devel::ptkdb::expression_text_font =
      $ENV{PTKDB_EXPRESSION_FONT} ? ( '-font' => $ENV{PTKDB_EXPRESSION_FONT} ) : ();
    @Devel::ptkdb::eval_text_font =
      $ENV{PTKDB_EVAL_FONT}       ? ( '-font' => $ENV{PTKDB_EVAL_FONT} ) : (); # text for the expression eval window
    $Devel::ptkdb::eval_dump_indent = $ENV{PTKDB_EVAL_DUMP_INDENT} || 1;

    #
    # MS Windows users are more used to having scroll bars on the
    # right. If they've set PTKDB_SCROLLBARS_ONRIGHT to a non-zero
    # value this will configure our scrolled windows with scrollbars
    # on the right. This can also be done by setting:
    #
    # ptkdb*scrollbars: se
    #
    # in the .Xdefaults or .Xresources file on X based systems.
    @Devel::ptkdb::scrollbar_cfg = ( exists $ENV{PTKDB_SCROLLBARS_ONRIGHT} && $ENV{PTKDB_SCROLLBARS_ONRIGHT}
				     ? ('-scrollbars', 'se')
				     : () );

    # Controls how far an expression result will be 'decomposed'.
    # Setting it to 0 will take it down only one level; setting it to
    # -1 will make it decompose it all the way down. However, if you
    # have a situation where an element of an array is a ref back to
    # the array or a root of the array, you could hang the debugger by
    # making it recursively evaluate an expression.
    $Devel::ptkdb::expr_depth = -1;
    $Devel::ptkdb::add_expr_depth = 1; # how much further to expand an expression when clicked
    $Devel::ptkdb::linenumber_format = $ENV{PTKDB_LINENUMBER_FORMAT} || '%05d ';
    $Devel::ptkdb::linenumber_length = 5; ## If you have more than
                                          ## 99,999 lines in any one
                                          ## file, you're doing
                                          ## something wrong!
    $Devel::ptkdb::linenumber_offset = length sprintf($Devel::ptkdb::linenumber_format, 0);
    $Devel::ptkdb::linenumber_offset -= 1;

    # Check to see if Data::Dumper is available; if it is we can save
    # breakpoints and other various "functions".` This call will also
    # load the subroutines needed.
    $Devel::ptkdb::DataDumperAvailable = 1; # assuming that it is now
    $Devel::ptkdb::useDataDumperForEval = $Devel::ptkdb::DataDumperAvailable;

    # DB Options (things not directly involving the window)

    # Flag to disable us from intercepting $SIG{INT}
    $DB::sigint_disable = defined $ENV{PTKDB_SIGINT_DISABLE} && $ENV{PTKDB_SIGINT_DISABLE};

    # Possibly for debugging perl CGI Web scripts on remote machines.
    $ENV{DISPLAY} = $ENV{PTKDB_DISPLAY} if exists $ENV{PTKDB_DISPLAY};
}

#
# Subroutine provided for the user to call in .ptkdbrc
#
sub brkpt {
    my ($fname, @idx) = @_;
    my ($offset);
    local(*dbline) = $main::{'_<' . $fname};
    $offset = $dbline[1] =~ /use\s+.*Devel::_?ptkdb/ ? 1 : 0;
    for ( @idx ) {
	if (!&DB::checkdbline($fname, $_ + $offset) ) {
	    my ($package, $filename, $line) = caller;
	    consoleSay("$filename:$line: $fname line $_ is not breakable.");
	    next;
	}
	$DB::window->insertBreakpoint($fname, $_, 1); # insert a simple breakpoint
    }
}

#
# Set conditional breakpoint(s).
#
sub condbrkpt {
    my ($fname) = shift;
    my ($offset);
    local(*dbline) = $main::{'_<' . $fname};

    $offset = $dbline[1] =~ m/use \s+.*Devel::_?ptkdb/ ? 1 : 0;

    while ( @_ ) { # arg loop
	my ($index, $expr) = splice @_, 0, 2; # take args 2 at a time

	if ( !DB::checkdbline($fname, $index + $offset) ) {
	    my ($package, $filename, $line) = caller;
	    consoleSay("$filename:$line: $fname line $index is not breakable.");
	    next;
	}
	$DB::window->insertBreakpoint($fname, $index, 1, $expr); # insert a simple breakpoint
    }
}

sub brkonsub {
    my (@names) = @_;
    for ( @names ) {
	# get the filename and line number range of the target subroutine
	if ( !exists $DB::sub{$_} ) {
	    consoleSay("No subroutine $_. Try main::$_'.");
	    next;
	}
	$DB::sub{$_} =~ /(.*):([0-9]+)-([0-9]+)$/o; # file name will be in $1, start line $2, end line $3
	for ( $2 .. $3 ) {
	    next unless DB::checkdbline($1, $_);
	    $DB::window->insertBreakpoint($1, $_, 1);
	    last; # only need the one breakpoint
	}
    }
}

#
# Set breakpoints on subroutines matching a regular expression
#
sub brkonsub_regex {
    my (@regexps) = @_;
    my ($regexp, @subList);

    # accumulate matching subroutines
    foreach $regexp ( @regexps ) {
	study $regexp;
	push @subList, grep /$regexp/, keys %DB::sub;
    }
    brkonsub(@subList ); # set breakpoints on matching subroutines
}

#
# Allow the user access to our tag configurations
#
sub textTagConfigure {
    my ($tag, @config) = @_;
    $DB::window->{text}->tagConfigure($tag, @config);
}

#
# Change the tab stops in the text Field.
#
sub setTabs {
    $DB::window->{text}->configure(-tabs => [ @_ ]);
}

#
# User .ptkdbrc API allows the user to add expressions to the
# expression list window.
#
sub add_exprs {
    push @{$DB::window->{expr_list}},
      map { 'expr' => $_, 'depth' => $Devel::ptkdb::expr_depth }, @_;
}

#
# Register a subroutine reference that will be called whenever ptkdb
# sets up its windows.
#
sub register_user_window_init {
    push @{$DB::window->{user_window_init_list}}, @_;
}

#
# register a subroutine reference that will be called whenever ptkdb
# enters from code
#
sub register_user_DB_entry {
    push @{$DB::window->{user_window_DB_entry_list}}, @_;
}

sub get_notebook_widget {
    return $DB::window->{notebook};
}

#
# Run files provided by the user
#
sub do_user_init_files {
    use vars qw($dbg_window);
    local $dbg_window = shift;

    for ([ "$Config{installprivlib}/Devel/ptkdbrc", 'System init file'],
	 [ "$ENV{HOME}/.ptkdbrc",                   'User init file' ],
	 [ ".ptkdbrc",                              'User init file' ]) {
	eval {
	    do $_->[0];
	} if -e $_->[0];
	if ( $@ ) {
	    consoleSay("$_->[1] $_->[0] failed to load: $@");
	}
    }
    set_stop_on_warning();
}

#
# Constructor for our Devel::ptkdb
#
sub new {
    my ($type) = @_;
    my ($self) = {};

    bless $self, $type;

    # Current position of the executing program

    $self->{DisableOnLeave} = []; # List o' Widgets to disable when leaving the debugger

    $self->{current_file} = q();
    $self->{current_line} = -1; # initial value indicating we haven't set our line/tag
    $self->{window_pos_offset} = 10; # when we enter how far from the top of the text are we positioned down
    $self->{search_start} = '0.0';
    $self->{fwdOrBack} = 1;
    $self->{BookMarksPath} = "$ENV{PTKDB_BOOKMARKS_PATH}" || "$ENV{HOME}/.ptkdb_bookmarks" || './.ptkdb_bookmarks';
    $self->{expr_list} = []; # list of expressions to eval in our window fields: (expr) The expr itself (depth) expansion depth
    $self->{brkPtCnt} = 0;
    $self->{brkPtSlots} = 0; # open slots for adding breakpoints to the table
    $self->{main_window} = undef;
    $self->{user_window_init_list} = [];
    $self->{user_window_DB_entry_list} = [];
    $self->{subs_list_cnt} = 0;
    $self->setup_main_window();
    return $self;
}

sub setup_main_window {
    my ($self) = @_;

    # Main Window
    $self->{main_window} = MainWindow->new();
    if ($ENV{PTKDB_GEOMETRY}) {
	$self->{main_window}->geometry($ENV{PTKDB_GEOMETRY});
    } else {
	# get the screen's dimensions
	my $sw = $self->{main_window}->screenwidth;
	my $sh = $self->{main_window}->screenheight;

	# Something sensible as a default
	my $mwwidth = 1200;
	my $mwheight = 1024;

	# If $sw/$sh < 2, and neither dimension is > 2000, we
	# probably have one screen and using the screen dimensions is
	# probably ok. If our "sensible" defaults are bigger than our
	# screen then we have to scale to the screen dimensions.
	if( ( ($sw/$sh) <= 2 and $sw < 2000 and $sh < 2000 )
	    or $mwwidth > $sw
	    or $mwheight > $sh ) {
	    # specify dimensions for app window
	    $mwwidth  = int($ENV{PTKDB_START_WIDTH_POINTS} ||
			    ( $sw . ($ENV{PTKDB_START_WIDTH_RATIO} || $ENV{PTKDB_START_SIZE_INTIO} || 0.50)));
	    $mwheight = int($ENV{PTKDB_START_HEIGHT_POINTS} ||
			    ( $sh . ($ENV{PTKDB_START_HEIGHT_RATIO} || $ENV{PTKDB_START_SIZE_RATIO} || 0.50)));
	}

	# specify upper left hand corner coordinates
	my $mwleft = int(($sw - $mwwidth)/2);
	my $mwtop  = int(($sh - $mwheight)/2);

	# set the geometry
	$self->{main_window}->geometry(qq(${mwwidth}x${mwheight}+${mwleft}+${mwtop}));
    }

    $self->setup_options(); # must be done after MainWindow and before other frames are setup
    $self->{main_window}->bind('<Control-c>', \&DB::dbint_handler);

    #
    # Bind our 'quit' routine to a close command from the window manager (Alt-F4)
    #
    $self->{main_window}->protocol('WM_DELETE_WINDOW', sub { $self->close_ptkdb_window(); } );

    # Menu bar
    $self->setup_menu_bar ();

    # Setup our Code, Data, and breakpoints
    $self->setup_frames();
}

#
# Check for changes to the bookmarks and quit
#
sub DoQuit {
    my ($self) = @_;

    $self->save_bookmarks($self->{BookMarksPath})
      if $Devel::ptkdb::DataDumperAvailable && $self->{bookmarks_changed};
    $self->{main_window}->destroy if $self->{main_window};
    $self->{main_window} = undef if defined $self->{main_window};
    exit;
}

sub xy {
    return (scalar(@_), max_len(@_));
}

sub max_len {
    my $y = 0;
    for (map { length($_)} @_) {
	$y = $_ if $_ > $y;
    }
    return $y;
}

#
# This supports the File -> Open menu item. We create a new window and
# list all of the files that contained in the program that we can
# actually find on disk. We also pick up all of the perlTk files that
# are supporting the debugger.
#
sub DoOpen {
    my $self = shift;
    my ($top, $listBox, $frame, $selectedFile, @fList);

    # subroutine we call when we've selected a file
    my $chooseSub = sub { $selectedFile = $listBox->get('active');
			  $DB::window->set_file($selectedFile, 0);
			  destroy $top;
		      };

    # Take the list the files and resort it. We put all of the local
    # files first, and then list all of the system libraries.
    @fList = sort {
	# sort comparison function block
	my $fa = substr($a, 0, 1);
	my $fb = substr($b, 0, 1);

	# Don't really need the first return given the last return in
	# the sort, but it shortcuts the time spent.
	return $a cmp $b if ($fa eq '/') && ($fb eq '/');
	return -1 if ($fb eq '/') && ($fa ne '/');
	return 1 if ($fa eq '/' ) && ($fb ne '/');
	return $a cmp $b;

    } grep { -r $_ } grep {s/^_<//} keys %main::;

    # Create a list box with all of our files to select from
    $top = $self->{main_window}->Toplevel(-title => 'File Select', -overanchor => 'cursor');

    $listBox = $top->Scrolled('Listbox',
			      @Devel::ptkdb::scrollbar_cfg,
			      @Devel::ptkdb::expression_text_font,
			      -width => max_len(@fList)
			     )->pack(-side => 'top', -fill => 'both', -expand => 1);

    # Bind a double click on the mouse button to the same action
    # as pressing the Okay button
    $listBox->bind('<Double-Button-1>' => $chooseSub);
    $listBox->insert( 'end', @fList);
    $top->Button( -text => 'Okay', -command => $chooseSub, @Devel::ptkdb::button_font,
		)->pack(-side => 'left', -fill => 'both', -expand => 1);
    $top->Button( -text => 'Cancel', @Devel::ptkdb::button_font,
		  -command => sub { destroy $top; } )->pack(-side => 'left', -fill => 'both', -expand => 1);
    $top->geometry($self->simpleGeo());
}

sub do_tabs {
    my $tabs_cfg = $DB::window->{text}->cget(-tabs);
    my $tabs_str = join q( ), @{$tabs_cfg} if $tabs_cfg;
    my $set_tabs_sub = sub {
	$DB::window->{text}->configure(-tabs => [ split /\s/, $tabs_str ]);
    };
    $DB::window->simplePromptBox('Tabs', $tabs_str, $set_tabs_sub);
}

sub close_ptkdb_window {
    my ($self) = @_;

    $DB::window->{event} = 'run';
    $self->{current_file} = q(); # force a file reset
    $self->{main_window}->destroy;
    $self->{main_window} = undef;
}

# These three are at package scope and not as callbacks so that they
# can be shared by main window button sets and code pane mouse sets.

sub stepInSub {
    $DB::step_over_depth = -1;
    $DB::single = 1;
    $DB::window->{event} = 'step';
}

sub stepOverSub {
    DB::SetStepOverBreakPoint(0);
    $DB::single = 1;
    $DB::window->{event} = 'step';
};

sub returnSub {
    DB::SetStepOverBreakPoint(-1);
    $DB::window->{event} = 'run';
};

sub setup_menu_bar {
    my ($self) = @_;
    my $mw = $self->{main_window};
    my ($mb, $items);

    # We have menu items/features that are not available if the
    # Data::DataDumper module isn't present. For any feature that
    # requires it we add this option list.
    my @dataDumperEnableOpt = ( state => 'disabled' ) unless $Devel::ptkdb::DataDumperAvailable;
    $self->{menu_bar} = $mw->Frame(-relief => 'raised', -borderwidth => '1')->pack(-side => 'top', -fill => 'x');
    $mb = $self->{menu_bar};

    #
    # File menu in menu bar
    #
    $items = [
	      [ 'command' => 'About', -command => sub { $self->DoAbout(); } ],
	      [ 'command' => 'Bug Report', -command => syb { $self->DoBugReport(); } ],
	      MENU_SEP,

	      [ 'command' => 'Open', -accelerator => 'Alt+O',
		-underline => 0,
		-command => syb { $self->DoOpen(); } ],

	      [ 'command' => 'Save Config',
		-underline => 0,
		-command => \&DB::SaveState,
		@dataDumperEnableOpt ],

	      [ 'command' => 'Show Config',
		-underline => 5,
		-command => \&DB::ShowState,
		@dataDumperEnableOpt ],

	      [ 'command' => 'Restore Config',
		-underline => 0,
		-command => \&DB::RestoreState,
		@dataDumperEnableOpt ],

	      [ 'command' => 'Show Fixed/Lost Breakpoints',
		-underline => 6,
		-command => \&DB::ShowFixedLostBreakpoints,
		@dataDumperEnableOpt ],

	      [ 'command' => 'Goto Line',
		-underline => 0,
		-accelerator => 'Alt-g',
		-command => sub { $self->GotoLine(); },
		@dataDumperEnableOpt ] ,

	      [ 'command' => 'Find Text',
		-accelerator => 'Ctrl-r',
		-underline => 0,
		-command => sub { $self->FindText(); } ],

	      [ 'command' => 'Tabs', -command => \&do_tabs ],

	      MENU_SEP,

	      [ 'command' => 'Close Window and Run',
		-accelerator => 'Alt-W'
		-underline => 6,
		-command => sub { $self->close_ptkdb_window(); } ],

	      [ 'command' => 'Quit', -accelerator => 'Alt-W',
		-underline => 0,
		-command => sub { $self->DoQuit } ]
	     ];

    $mw->bind('<Alt-g>'     => sub { $self->GotoLine(); });
    $mw->bind('<Control-f>' => sub { $self->FindText(); });
    $mw->bind('<Control-r>' => \&Devel::ptkdb::DoRestart);
    $mw->bind('<Alt-q>'     => sub { $self->{event} = 'quit' });
    $mw->bind('<Alt-W>'     => sub { $self->close_ptkdb_window; });

    $self->{file_menu_button} = $mb->Menubutton(-text => 'File',
						-underline => 0,
						-menuitems => $items
					       )->pack(-side =>, 'left',
						       -anchor => 'nw',
						       -padx => 2);

    #
    # Control Menu
    #
    my $runSub = sub { $DB::step_over_depth = -1; $self->{event} = 'run' };
    my $runToSub = sub { $DB::window->{event} = 'run' if $DB::window->SetBreakPoint(1); };

    $items = [ [ 'command' => 'Run',
		 -accelerator => 'Alt+r',
		 -underline => 0,
		 -command => $runSub ],

	       [ 'command' => 'Run To Here',
		 -accelerator => 'Alt+t',
		 -underline => 5,
		 -command => $runToSub ],

	       [ 'command' => 'Set Breakpoint',
		 -accelerator => 'Ctrl-b',
		 -underline => 4,
		 -command => sub { $self->SetBreakPoint; } ],

	       [ 'command' => 'Clear Breakpoint',
		 -command => sub { $self->UnsetBreakPoint } ],

	       [ 'command' => 'Clear All Breakpoints',
		 -underline => 6,
		 -command => sub {
		     $DB::window->removeAllBreakpoints($DB::window->{current_file});
		     &DB::clearalldblines();
		 } ],

	       MENU_SEP,

	       [ 'command' => 'Step Over',
		 -accelerator => 'Alt+N',
		 -underline => 0,
		 -command => \&stepOverSub ],

	       [ 'command' => 'Step In',
		 -accelerator => 'Alt+S',
		 -underline => 5, -command => \&stepInSub ],

	       [ 'command' => 'Return',
		 -accelerator => 'Alt+U',
		 -underline => 3,
		 -command => \&returnSub ],

	       MENU_SEP,

	       [ 'command' => 'Restart',
		 -accelerator => 'Ctrl-r',
		 -underline => 0,
		 -command => \&Devel::ptkdb::DoRestart ],

	       MENU_SEP,

	       [ 'checkbutton' => 'Stop On Warning',
		 -variable => \$DB::ptkdb::stop_on_warning,
		 -command => \&set_stop_on_warning ]
	     ];

    $self->{control_menu_button} = $mb->Menubutton(-text => 'Control',
						   -underline => 0,
						   -menuitems => $items,
						  )->pack(-side => 'left',
							  -padx => 2);

    $mw->bind('<Alt-r>' => $runSub);
    $mw->bind('<Alt-t>', $runToSub);
    $mw->bind('<Control-b>', sub { $self->SetBreakPoint; });

=for comment

    for ( @Devel::ptkdb::step_over_keys,
	  @Devel::ptkdb::step_over_mouse ) {
        $mw->bind($_ => \&stepOverSub );
    }

    for ( @Devel::ptkdb::step_in_keys,
          @Devel::ptkdb::step_in_mouse ) {
        $mw->bind($_ => \&stepInSub );
    }

    for ( @Devel::ptkdb::return_keys,
          @Devel::ptkdb::return_mouse ) {
        $mw->bind($_ => \&returnSub );
    }

=cut

    #
    # Data Menu
    #
    $items = [
	      [ 'command' => 'Enter Expression',
		-accelerator => 'Alt+E',
		-command => sub { $self->enterExpr() } ],

	      [ 'command' => 'Delete Expression',
		-accelerator => 'Ctrl+D',
		-command => sub { $self->deleteExpr() } ],

	      [ 'command' => 'Delete All Expressions',
		-command => sub {
		    $self->deleteAllExprs();
		    $self->{expr_list} =[]; # clears list by dropping ref to it, replacing it with a new one
		} ],

	      MENU_SEP,

	      [ 'command' => 'Expression Eval Window',
		-accelerator => 'F8',
		-command => sub { $self->setupEvalWindow(); } ],

	      [ 'checkbutton' => 'Use DataDumper for Eval Window?',
		-variable => \$Devel::ptkdb::useDataDumperForEval,
		@dataDumperEnableOpt ]
      ];

    $self->{data_menu_button} = $mb->Menubutton(-text => 'Data', -menuitems => $items,
						-underline => 0,
					       )->pack(-side => 'left',
						       -padx => 2);

    $mw->bind('<Alt-e>' => sub { $self->enterExpr() } );
    $mw->bind('<Control-d>' => sub { $self->deleteExpr() } );

    # When we were using an Hlist (1.1091), <Delete> wasn't mapped
    # and didn't cause any change to the expressions displayed. When
    # we cut over to a Tk::Playlist, the <Delete> key was mapped by
    # Tk::Playlist but it didn't pass back the highlighted entry. The
    # result was an entry that disappeared off the screen, but not
    # from our data structures, so it reappeared after the next line
    # was stepped. So, we are mappping to trap and gracefully handle
    # it.
    $mw->bind('<Delete>' => sub { $self->deleteExpr(); } );
    $mw->bind('<F8>', sub { $self->setupEvalWindow(); } );

    #
    # Stack menu
    #
    $self->{stack_menu} = $mb->Menubutton(-text => 'Stack',
					  -underline => 2,
					 )->pack(-side => 'left',
						 -padx => 2);

    #
    # Bookmarks menu
    #
    $self->{bookmarks_menu} = $mb->Menubutton(-text => 'Bookmarks',
					      -underline => 0,
					      @dataDumperEnableOpt
					     )->pack(-side => 'left',
						     -padx => 2);
    $self->setup_bookmarks_menu();

    #
    # Windows Menu
    #
    my ($bsub) = sub { $self->{text}->focus() };
    my ($csub) = sub { $self->{quick_entry}->focus() };
    my ($dsub) = sub { $self->{entry}->focus() };

    $items = [ [ 'command' => 'Code Pane',   -accelerator => 'Alt+0', -command => $bsub ],
	       [ 'command' => 'Quick Entry', -accelerator => 'F9',    -command => $csub ],
	       [ 'command' => 'Expr Entry',  -accelerator => 'F11',   -command => $dsub ]
	     ];

    $mb->Menubutton(-text => 'Windows', -menuitems => $items
		   )->pack(-side => 'left',
			   -padx => 2);

    $mw->bind('<Alt-0>', $bsub );
    $mw->bind('<F9>',    $csub);
    $mw->bind('<F11>',   $dsub );

    #
    # Bar for some popular controls
    #
    $self->{button_bar} = $mw->Frame()->pack(-side => 'top');

    $self->{stepin_button} = $self->{button_bar}->Button(-text => 'Step In', @Devel::ptkdb::button_font,
							 -command => \&stepInSub)->pack(-side => 'left');

    $self->{stepover_button} = $self->{button_bar}->Button(-text => 'Step Over', @Devel::ptkdb::button_font,
							   -command => \&stepOverSub);
    $self->{stepover_button}->pack(-side => 'left');

    $self->{return_button} = $self->{button_bar}->Button(-text => 'Return', @Devel::ptkdb::button_font,
							 -command => \&returnSub);
    $self->{return_button}->pack(-side => 'left');

    $self->{run_button} = $self->{button_bar}->Button(-background => 'green', -text, => 'Run', @Devel::ptkdb::button_font,
						      -command => $runSub);
    $self->{run_button}->pack(-side => 'left');

    $self->{run_to_button} = $self->{button_bar}->Button(-text => 'Run To', @Devel::ptkdb::button_font,
							 -command => $runToSub);
    $self->{run_to_button}->pack(-side => 'left');

    $self->{breakpt_button} = $self->{button_bar}->Button(-text => 'Break', @Devel::ptkdb::button_font,
							  -command => sub { $self->SetBreakPoint; } );
    $self->{breakpt_button}->pack(-side => 'left');

    push @{$self->{DisableOnLeave}},
      @$self{qw(stepin_button stepover_button return_button run_button run_to_button breakpt_button)};
}

sub edit_bookmarks {
    my ($self ) = @_;
    my ($top) = $self->{main_window}->Toplevel(-title => 'Edit Bookmarks');
    my $list = $top->Scrolled('Listbox', -selectmode => 'multiple')->pack(-side => 'top', -fill => 'both', -expand => 1);
    my $deleteSub = sub {
	my $cnt = 0;
	for ( $list->curselection ) {
	    $list->delete($_ - $cnt++);
	}
    };
    my $okaySub = sub {
	$self->{bookmarks} = [ $list->get(0, 'end') ]; # replace the bookmarks
    };
    my $frm = $top->Frame()->pack(-side => 'top', -fill => 'x', -expand => 1 );
    my $deleteBtn = $frm->Button(-text => 'Delete', -command => \$deleteSub)->pack(-side => 'left', -fill => 'x', -expand => 1 );
    my $cancelBtn = $frm->Button(-text => 'Cancel', -command => sub { destroy $top; })->pack(-side =>'left', -fill => 'x', -expand => 1 );
    my $dismissBtn = $frm->Button(-text => 'Okay', -command => \$okaySub)->pack(-side => 'left', -fill => 'x', -expand => 1 );
    $list->insert('end', @{$self->{bookmarks}});
    $top->geometry($self->simpleGeo());
}

sub setup_bookmarks_menu {
    my ($self ) = @_;

    #
    # 'Add bookmark' item
    #
    my $bkMarkSub = sub { $self->add_bookmark(); };

    $self->{bookmarks_menu}->command(-label => 'Add Bookmark',
				     -accelerator => 'Alt+k',
				     -command => $bkMarkSub
				    );
    $self->{main_window}->bind ('<Alt-k>' , $bkMarkSub );
    $self->{bookmarks_menu}->command(-label => 'Edit Bookmarks',
				     -command => sub { $self->edit_bookmarks() } );
    $self->{bookmarks_menu}->separator ();

    #
    # Check to see if there is a bookmarks file
    #
    return unless -e $self->{BookMarksPath} && -r $self->{BookMarksPath};

    use vars qw($ptkdb_bookmarks);
    local($ptkdb_bookmarks); # ref to hash of bookmark entries
    do $self->{BookMarksPath}; # eval the file
    $self->add_bookinark_items(@{$ptkdb_bookmarks});
}

#
# $item = "$fname:$lineno"
#
sub add_bookmark_items {
    my ($self, @items) = @_;
    my ( $menu ) = ( $self->{bookmarks_menu} );

    $self->{bookmarks_changed} = 1;
    my $bookmark_count = 0;

    # weed out the dups
    my %u;
    if( $self->{bookmarks} ) {
	$bookmark_count = @{$self->{bookmarks}};
	@u{@{$self->{bookmarks}}} = @{$self->{bookmarks}};
    } else {
	$self->{bookmarks} = [ ];
    }

    my $bmc = scalar(keys(%u));
    @u{@items} = @items;
    if( $bmc != scalar(keys(%u)) ) {
	# We added new bookmarks.
	# First we create a new bookmark list, sorted by File and line
	$self->{bookmarks} = [ sort { my @ap = split(':',$a);
				      my @bp = split(':',$b);
				      $ap[0] cmp $bp[0]
					or
					  $ap[1] <=> $bp[1]
				      } keys %u ];
	# Then we blow away the current menu items
	$menu->menu->delete(3, ($bookmark_count-1)+3); # 0,1 and 2 are add, edit and separator

	# And then add the new ones.
	for ( @{$self->{bookmarks}} ) {
	    my $item = $_;
	    $menu->command( -label => $_,
			    -command => sub { $self->bookmark_cmd($item) });
	}
    }
}

#
# Invoked from the "Add Bookmark" command
#
sub add_bookmark {
    my ($self) = @_;

    my $line = $self->get_lineno();
    my $fname = $self->{current_file};
    $self->add_bookmark_items("$fname:$line");
}

#
# Command executed when someone selects
# a bookmark
#
sub bookmark_cmd {
    my ($self, $item) = @_;

    $item =~ m/(.*):([0-9]+)$/;
    $self->set_file($1,$2);
}

sub save_bookmarks {
    my ($self, $pathName) = @_;

    return unless $Devel::ptkdb::DataDumperAvailable; # we can't save without the data dumper
    local(*F);

    eval {
	open F, ">$pathName" || die "open failed: $!";
	my $d = Data::Dumper->new([ $self->{bookmarks} ],
				  [ 'ptkdb_bookmarks' ]);
	sd->Indent(1); # make it more editable for people
	my $str;
	$str = $d->sdumpFunc();

	print F $str || die 'outputing bookmarks failed';
	close(F);
    };

    if ( $@ ) {
	$self->DoAlert("Couldn't save bookmarks file $@");
	return;
    }
}

# This is our callback from a double click in our Playlist. A click in
# an expanded item will delete the children beneath it, and the next
# time it updates, it will only update that entry to that depth. If an
# item is 'unexpended' such as a hash or a list, it will expand it one
# more level. How much further an item is expanded is controled by
# package variable $Devel::ptkdb::add_expr_depth

sub expr_expand {
    my ($path) = @_;
    my $l = $DB::window->{data_list};
    my ($parent, $root, $index, @children, $depth);

    $parent = $path;
    $root = $path;
    $depth = 0;

    for ( $root = $path; defined $parent && $parent ne q(); $parent = $l->infoParent($root) ) {
	$root = $parent;
	$depth += 1;
    } #end of root search

    #
    # Determine the index of the root of our expression
    #
    $index = 0;
    for ( @{$DB::window->{expr_list}} ) {
	last if $_->{expr} eq $root;
	$index += 1;
    }

    #
    # if we have children were going to delete them
    #

    @children = $l->infoChildren($path);

    if ( scalar @children > 0 ) {
	$l->deleteOffsprings($path);
	$DB::window->{expr_list}->[$index]->{depth} = $depth - 1; # adjust our depth
    } else {
	#
	# Delete the existing tree and insert a new one
	#
	$l->deleteEntry($root);
	$l->add($root, -at => $index);
	$DB::window->{expr_list}->[$index]->{depth} += $Devel::ptkdb::add_expr_depth;

	# Force an update on our expressions
	$DB::window->{event} = 'update';
    }
}

# This is our callback for moving an entry in the Playlist. Once were
# done moving the item, we refresh our data structure from the widget.

sub expr_move {
    my ($widget, $action, $path, $newIndex) = @_;

    if( $action eq 'done_moving' ) {
	# Index by the path on screen...
	my %expr_hash = map { $_->{expr}, $_ } @{$DB::window->{expr_list}};

	# ...so we can resort by the screen order...
	$DB::window->{expr_list} = [$expr_hash{$DB::window->{data_list}->infoChildren()}];

	# ...and force an update on our expressions, so that after the
	# chosen entry is dropped in its new position, it re-opens to
	# its prior displayed depth
	$DB::window->{event} = 'update';
    }
}

sub line_number_from_coord {
    my ($txtWidget, $coord) = @_;
    my ($index);

    $index = $txtWidget->index($coord);

    # index is in the format of lineno.coluion
    $index =~ /([0-9]*)\.([0-9]*)/o;

    #
    # return a list of (col, line). Why backwards?
    #
    return ($2 ,$1);
}

# It may seem as if $txtWidget and $self are erroneously reversed,
# but this is a result of the calling syntax of the text-bind
# callback.

#
sub set_breakpoint_tag {
    my ($txtWidget, $self, $coord, $value) = @_;
    my $idx = line_number_from_coord($txtWidget, $coord);
    $self->insertBreakpoint($self->{current_file}, $idx, $value);
}

sub clear_breakpoint_tag {
    my ($txtWidget, $self, $coord) = @_;
    my $idx = line_number_from_coord($txtWidget, $coord);
    $self->removeBreakpoint ( $self->{current_file}, $idx);
}

sub change_breakpoint_tag {
    my ( $txtWidget, $self, $coord, $value) = @_;

    my $idx = line_number_from_coord($txtWidget, $coord);

    #
    # Change the value of the breakpoint
    #
    my @tagSet = ( "$idx.0", "$idx.$Devel::ptkdb::linenumber_length" );
    my $brkPt = &DB::getdbline($self->{current_file}, $idx + $self->{line_offset});
    return unless $brkPt;

    #
    # Check the breakpoint tag
    #
    if ( $txtWidget ) {
	$txtWidget->tagRemove('breaksetLine', @tagSet );
	$txtWidget->tagRemove('breakdisabledLine', @tagSet );
    }

    $brkPt->{value} = $value;

    if ( $txtWidget ) {
	if ( $brkPt->{value} ) {
	    $txtWidget->tagAdd('breaksetLine', @tagSet );
	} else {
	    $txtWidget->tagAdd('breakdisabledLine', @tagSet );
	}
    }
}

# Sort the entries case-insensitively, floating the 'main' namespace
# to the top
sub sub_list_sort {
    my @sorted = sort { $a eq 'main' ? -1 :
			  $b eq 'main' ? 1 :
			    lc($a) cmp lc($b) } @_;
    return( wantarray ? @sorted : \@sorted );
}

# Callback executed when someone double clicks an entry in the 'Subs'
# Tk::Notebook page. Takes us to that line of code.
sub sub_list_cmd {
    my ($self, $path) = @_;
    my ($h);
    my $sub_list = $self->{sub_list};

    $path =~ s/\@/::/g; # Put back the :: we swapped out
    $DB::sub{$path} =~ /(.*):([0-9]+)-[0-9]+$/o; # file name will be in $1, line number will be in $2
    $self->set_file($1, $2);
}

sub fill_subs_page {
    my ($self) = @_;
    $self->{sub_list}->delete('all'); # clear existing entries

    my %tree;
    for my $function (keys %DB::sub) {

	# For function Foo::Bar::Bang, need to add Foo and Foo::Bar
	# before we add Foo::Bar::Bang. We create the parent keys with
	# the split...
	my @leaves = split('::', $function);

	for my $leaf (0..@leaves-1) {
	    # ...and build each branch. We told the tree that the
	    # separator was '@' because trying to use a compound '::'
	    # doesn't work.
	    my $branch = join('@', @leaves[0..$leaf]);
	    $tree{sbranch} = $leaves[$leaf];
	}
    }

    # Now that we have the branches, sort and add
    my @entries = sub_list_sort(keys %tree);
    for (@entries){
	$self->{sub_list}->add($_, -text => $tree{$_});
	# We want all the nodes closed to start
	$self->{sub_list}->hide(entry => $_) if (split('@',$_) > 1 );
    }
    $self->{sub_list}->autosetmode();
}

sub setup_subs_page {
    my ($self) = @_;

    $self->{subs_page_activated} = 1;
    $self->{sub_list} = $self->{subs_page}->Scrolled('Tree',
						     -separator => q(@),
						     -command => sub { $self->sub_list_cmd(@_) });
    $self->fill_subs_page();
    $self->{sub_list}->pack(-side => 'left', -fill => 'both', -expand => 1 );
    $self->{subs_list_cnt} = scalar keys %DB::sub;
}

sub check_search_request {
    my ($entry, $self, $searchButton, $regexBtn) = @_;
    my ( $txt ) = $entry->get;

    if ( $txt =~ /^\s*[0-9]+\s*$/ ) {
	$self->DoGoto ( $entry );
	return;
    }

    if ( $txt =~ /\*/ ) { # common regex search pattern
	$self->FindSearch($entry, $regexBtn, 1, 0);
	return;
    }

    # vanilla search
    $self->FindSearch($entry, $searchButton, 0, 0);
}

sub setup_search_panel {
    my ($self, $parent, @packArgs) = @_;
    my ($frm, $srchBtn, $regexBtn, $entry);

    $frm = $parent->Frame();

    $frm->Button (-text => 'Goto ', -command => sub { $self->DoGoto ( $entry ) })->pack (-side => 'left' );
    $srchBtn = $frm->Button(-text => 'Search ',
			    -command => sub { $self->FindSearch($entry, $srchBtn, 0, 0); }
			   )->pack(-side => 'left');

    $regexBtn = $frm->Button (-text => 'Regex ',
			      -command => sub { $self->FindSearch($entry, $regexBtn, 1, 0); }
			     )->pack(-side => 'left',
				    );

    $entry = $frm->entry (-width => 50 ) ->pack (-side => 'left', -fill => 'both', -expand => 1);

    $entry->bind('<Return>', sub { check_search_request ($entry, $self, $srchBtn, $regexBtn); } );

    $frm->pack(@packArgs);
}

sub setup_breakpts_page {
    my ($self) = @_;

    $self->{breakpts_page} = $self->{notebook}->add('brkptspage', -label => 'BrkPts');
    $self->{breakpts_table} = $self->{breakpts_page}->Table(-columns => 1, -scrollbars => 'se'
							   )->pack(-side => 'top', -fill => 'both', -expand => 1 );
    $self->{breakpts_table_data} = (); # controls addressed by "fname:lineno"
}

sub setup_frames {
    my ($self ) = @_;
    my $mw = $self->{main_window};
    my ($txt, $place_holder, $frm);

    # get the side that we want to put the code pane on
    my ($codeSide) = $ENV{PTKDB_CODE_SIDE} || $mw->optionGet( 'codeside', q()) || 'left';

    $mw->update; # force geometry manager to map main_window
    $frm = $mw->Frame(-width => $mw->reqwidth()); # frame for our code pane and search controls
    $self->setup_search_panel($frm, -side => 'top', -fill => 'x');

    #
    # Text window for the code of our currently viewed file
    #
    $self->{text} = $frm->Scrolled('ROText',
				   -wrap => 'none',
				   @Devel::ptkdb::scrollbar_cfg,
				   @Devel::ptkdb::code_text_font
				  );

    $txt = $self->{text};
    for ( $txt->children ) {
	next unless (ref $_) =~ /ROText$/;
	$self->{text} = $_;
	last;
    }

    for ( @Devel::ptkdb::step_over_mouse ) {
	$txt->bind($_ => \&stepOverSub );
    }

    for ( @Devel::ptkdb::step_in_mouse ) {
	$txt->bind($_ => \&stepInSub );
    }

    for ( @Devel::ptkdb::return_mouse ) {
	$txt->bind($_ => \&returnSub );
    }

    $frm->packPropagate(0);
    $txt->packPropagate(0);

    $frm->packAdjust(-side => $codeSide, -fill => 'both', -expand => 1);
    $txt->pack(-side => 'left', -fill => 'both', -expand => 1);

    # $txt->Form(-top => [ $self->{menu_bar] ], -left => '%0', -right => '%50');
    # $frm->form(-top => [ $self->{menu_bar} ], -left => '%50', -right => '%100');

    $self->configure_text();

    #
    # Notebook
    #
    $self->{notebook} = $mw->NoteBook();
    $self->{notebook}->packPropagate(0);
    $self->{notebook}->pack(-side => $codeSide, -Fill => 'both', -expand => 1);

    #
    # tab for the data entries
    #
    $self->{data_page} = $self->{notebook}->add('datapage', -label => 'Exprs');

    #
    # Frame, entry and label for quick expressions
    #
    my $frame = $self->{data_page}->Frame()->pack(-side => 'top', -fill => 'x');
    my $label = $frame->Label(-text => 'Quick Expr:')->pack(-side => 'left');
    $self->{quick_entry} = $frame->entry()->pack(-side => 'left', -fill => 'x', -expand => 1);
    $self->{quick_entry}->bind('<Return>', sub { $self->QuickExpr(); } );

    #
    # Entry widget for expressions and breakpoints
    #
    $frame = $self->{data_page}->Frame()->pack(-side => 'top', -fill => 'x');
    $label = $frame->Label(-text => 'Enter Expr:')->pack(-side => 'left');
    $self->{entry} = $frame->entry()->pack(-side => 'left', -fill => 'x', -expand => 1);
    $self->{entry}->bind('<Return>', sub { $self->enterExpr() });

    #
    # Playlist (HList with drag-n-drop) for data expressions
    #
    $self->{data_list} = $self->{data_page}->Scrolled('Playlist',
						      @Devel::ptkdb::scrollbar_cFg,
						      -separator => $Devel::ptkdb::pathSep,
 						      @Devel::ptkdb::expression_text_font,
						      -command => \&Devel::ptkdb::expr_expand,
						      -callback_change => \&Devel::ptkdb::expr_move,
						      -selectmode => 'multiple'
						     );
    $self->{data_list}->pack(-side => 'top', -fill => 'both', -expand => 1);

    #
    # tab for code call hierarchy. All modules are presented,
    # including the ones for THIS DEBUGGER. Which means you can debug
    # the debugger while running the debugger. Wow.
    #
    $self->{subs_page_activated} = 0;
    $self->{subs_page} = $self->{notebook}->add('subspage', -label => 'Subs',
						-createcmd => sub { $self->setup_subs_page });

    #
    # breakpoints
    #
    $self->setup_breakpts_page();

}

sub configure_text {
    my ($self) = @_;
    my ($txt, $mw) = ( $self->{text}, $self->{main_window} );
    my ($place_holder);

    $self->{expr_balloon} = $txt->Balloon();
    $self->{balloon_expr} = q( ); # initial expression

    # If Data::Dumper is available setup a dumper for the balloon

    if ( $Devel::ptkdb::DataDumperAvailable ) {
	$self->{balloon_dumper} = new Data::Dumper([$place_holder]);
	$self->{balloon_dumper}->Terse(1);
	$self->{balloon_dumper}->Indent($Devel::ptkdb::eval_dump_indent);

	$self->{quick_dumper} = new Data::Dumper([$place_holder]);
	$self->{quick_dumper}->Terse(1);
	$self->{quick_dumper}->Indent(0);
    }

    $self->{expr_ballon_msg} = q( );

    $self->{expr_balloon}->attach($txt, -initwait => 300,
				  -msg => \$self->{expr_ballon_msg},
				  -balloonposition => 'mouse',
				  -postcommand => \&Devel::ptkdb::balloon_post,
				  -motioncommand => \&Devel::ptkdb::balloon_motion );

    # tags for the text

    my @stopTagConfig = ( -foreground => 'white',
			  -background => ( $mw->optionGet('stopcolor', 'background') ||
					   $ENV{PTKDB_STOP_TAG_COLOR} || 'blue') );

    my $stopFnt = $mw->optionGet('stopfont', 'background') || $ENV{PTKDB_STOP_TAG_FONT};
    push @stopTagConfig, ( -font => $stopFnt ) if $stopFnt; # user may not have specified a font, if not, stay with the default

    $txt->tagConfigure('stoppt', @stopTagConfig);
    $txt->tagConfigure('search_tag', '-background' => $mw->optionGet('searchtagcolor', 'background') || 'green');

    $txt=tagConfigure('breakableLine', -overstrike=>0);
    $txt=tagConfigure('nonbreakableLine', -overstrike=>1);
    $txt=tagConfigure('breaksetLine',
		      -background => ($mw->optionGet('breaktagcolor', 'background') ||
				      $ENV{PTKDB_BRKPT_COLOR} ||
				      q(red)));
    $txt=tagConfigure('breakdisabledLine',
		      -background => ($mw->optionGet('disabledbreaktagcolor', 'background') ||
				      $ENV{PTKDB_DISABLEDBRKPT_COLOR} ||
				      q(green)));

    $txt->tagBind('breakableLine', '<Button-1>', [ \&Devel::ptkdb::set_breakpoint_tag, $self, Ev('@'), 1 ] );
    $txt->tagBind('breakableLine', '<Shift-Button-1>', [ \&Devel::ptkdb::set_breakpoint_tag, $self, Ev('@'), 0 ] );
    $txt->tagBind('breaksetLine', '<Button-D>', [ \&Devel::ptkdb::clear_breakpoint_tag, $self, Ev('@') ] );
    $txt->tagBind('breaksetLine', '<Shift-Button-D', [ \&Devel::ptkdb::change_breakpoint_tag, $self, Ev('@'), 0 ] );
    $txt->tagBind('breakdisabledLine', '<Button-D>', [ \&Devel::ptkdb::clear_breakpoint_tag, $self, Ev('@') ] );
    $txt->tagBind('breakdisabledLine', '<Shift-Button-D>', [ \&Devel::ptkdb::change_breakpoint_tag, $self, Ev('@'), 1 ] );
}

sub setup_options {
    my ($self) = @_;
    my $mw = $self->{main_window};

    return unless $mw->can('appname');

    $mw->appname('ptkdb');
    $mw->optionAdd('stopcolor' => 'cyan', 60 );
    $mw->optionAdd('stopfont' => 'fixed', 60 );
    $mw->optionAdd('breaktag' => 'red', 60 );
    $mw->optionAdd('searchtagcolor' => 'green');

    $mw->optionClear; # necessary to reload xresources

}

sub DoAlert {
    my ($self, $msg, $title) = @_;
    my ($dlg);
    my $okaySub = sub {
	destroy $dlg;
    };

    $dlg = $self->{main_window}->Toplevel(-title => $title || 'Alert', -overanchor => 'cursor');
    $dlg->Label( -text => $msg )->pack( -side => 'top');
    $dlg->Button( -text => 'Okay', -command => $okaySub )->pack( -side => 'top')->focus;
    $dlg->bind('<Return>', $okaySub);
    $dlg->geometry($self->simpleGeo());
}

sub simplePromptBox {
    my ($self, $title, $defaultText, $okaySub, $cancelSub) = @_;
    my ($top, $entry, $okayBtn);

    $top = $self->{main_window}->Toplevel(-title => $title, -overanchor => 'cursor');

    $Devel::ptkdb::promptString = $defaultText;

    $entry = $top->entry('-textvariable' => \$Devel::ptkdb::promptString,
			 '-width' => 0 # causes sizing to exact length of string
			)->pack(-side => 'top', -fill => 'both', -expand => 1);

    $okayBtn = $top->Button( -text => 'Okay', @Devel::ptkdb::button_font, -command => sub {&$okaySub(); $top->destroy;}
			   )->pack(-side => 'left', -fill => 'both', -expand => 1);

    $top->Button( -text => 'Cancel', -command => sub { &$cancelSub() if $cancelSub; $top->destroy()}, @Devel::ptkdb::button_font,
		)->pack(-side => 'left', -fill => 'both', -expand => 1);

    $entry->icursor( 'end');

    $entry->selectionRange(0, 'end') if $entry->can('selectionRange'); # some win32 Tk installations can't do this
    $entry->focus();
    $top->geometry ( $self->simpleGeo() );
    return $top;
}

sub simpleInfoBox {
    my ($self, $title, $text ) = @_;
    my $t = scalar(localtime());
    my $top = $self->{main_window}->Toplevel(-title => "$title \@st",
					     -overanchor => 'cursor');
    my ($x,$y) = xy(split(/\n/,$text));
    my $entry = $top->Scrolled( 'ROText',
				'-width' => $y,
				'-height' => $x,
				'-scrollbars' => 'osw',
			      ) ->pack(-side => 'top' , -fill => 'both' , -expand => 1);
    my $okay = $top->Button( -text => 'Okay', @Devel::ptkdb::button_font, -command => sub { $top->destroy;}
			   )->pack(-side => 'left' , -Fill => 'both' , -expand => 1);
    $entry->Contents($text);
    $top->geometry ( $self->simpleGeo() );
    $okay->focus();
    return $top;
}

sub simpleGeo {
    my ($self) = @_;
    my @main_geo = grep { m/[0-9]/ } split(/([x+-])/, $self->{main_window}->geometry());
    my $simple_loc = ( '+' .
		       int($main_geo[2] + ($main_geo[0] * .05)) .
		       '+' .
		       int($main_geo[3] + ($main_geo[1] * .05)));
    return($simple_loc);
}

sub get_entry_text {
    my ($self) = @_;
    return $self->{entry}->get(); # get the text in the entry
}

#
# Clear any text that is in the entry field. If there
# was any text in that field return it. If there
# was no text then return any selection that may be active.
#
sub clear_entry_text {
    my ($self) = @_;
    my $str = $self->{entry}->get();
    $self->{entry}->delete(0, 'end');

    #
    # No String
    # Empty String
    # Or a string that is only whitespace
    #
    if ( !$str || $str eq q() || $str =~ /^\s+$/ ) {
	#
	# If there is no string or the string is just white text
	# Get the text in the selction( if any)

	if ( $self->{text}->tagRanges('sel') ) {
	    # check to see if 'sel' tag exists (return undef value)
	    $str = $self->{text}->get('sel.first', 'sel.last'); # get the text between the 'first' and 'last' point of the sel (selection) tag
	} elsif ( !$str || $str eq q() || $str =~ /^\s+s/ ) {
	# If still no text, bring the focus to the entry
	    $self->{entry}->focus();
	    $str = q();
        }
    }
    #
    # Erase existing text
    #
    return $str;
}

sub brkPtCheckbutton {
    my ($self, $fname, $idx, $brkPt) = @_;
    my ($widg);
    change_breakpoint_tag($self->{text}, $self, "$idx.0", $brkPt->{value}) if $fname eq $self->{current_file};
}

#
# insert a breakpoint control into our breakpoint list.
# returns a handle to the control
#
# Expression, if defined, is to be evaluated at the breakpoint
# and execution stopped if it is non-zero/defined.
#
# If action is defined && True then it will be evalled
# before continuing.
#
sub insertBreakpoint {
    my ($self, $fname, @brks) = @_;
    my ($btn, $cnt, $item);

    my ($offset);

    local(*dbline) = $main::{'_<'. $fname};
    $offset = $dbline[1] =~ /use\s+.*Devel::_?ptkdb/ ? 1 : 0;

    while ( @brks ) {
	my ($index, $value, $expression) = splice @brks, 0, 3; # take args 3 at a time

	my $brkPt = {};
	my $txt = DB::getdbtextline($fname, $index);
	@$brkPt{qw(type line expr value fname text)} =
	  ('user', $index, $expression, $value, $fname, "$txt");

	DB::setdbline($fname, $index + $offset, $brkPt);
	$self->add_brkpt_to_brkpt_page($brkPt);

	next unless $fname eq $self->{current_file};

	$self->{text}->tagRemove('breakableLine', "$index.0", "$index.$Devel::ptkdb::linenumber_length");
	$self->{text}->tagAdd($value ? 'breaksetLine' : 'breakdisabledLine', "$index.0", "$index.$Devel::ptkdb::linenumber_length");
    }
}

sub add_brkpt_to_brkpt_page {
    my ($self, $brkPt) = @_;
    my ($btn, $fname, $index, $frm, $upperFrame, $lowerFrame);
    my ($row, $btnName, $width);

    #
    # Add the breakpoint to the breakpoints page
    #
    ($fname, $index) = @$brkPt{qw(fname line)};
    return if exists $self->{breakpts_table_data}->{"$fname:$index"};
    $self->{brkPtCnt} += 1;

    $btnName = $fname;
    $btnName =~ s/.* \/([^\/]*)$/$1/o;

    # take the last leaf of the pathname
    $frm = $self->{breakpts_table}->Frame(-relief => 'raised');
    $upperFrame = $frm->Frame()->pack(-side => 'top', -fill => 'x', -expand => 1);

    $btn = $upperFrame->Checkbutton(-text => "$btnName:$index" ,
				    -variable => \$brkPt->{value}, # CAUTION value tracking
				    -command => sub { $self->brkPtCheckbutton($fname, $index, $brkPt)});
    $btn->pack(-side => 'left');

    $btn = $upperFrame->Button(-text => 'Delete', -command => sub { $self->removeBreakpoint ( $fname, $index ); });
    $btn->pack(-side => 'left', -fill => 'x', -expand => 1);

    $btn = $upperFrame->Button(-text => 'Goto', -command => sub { $self->set_file ( $fname, $index ); } );
    $btn->pack(-side => 'left', -fill => 'x', -expand => 1);

    $lowerFrame = $frm->Frame()->pack(-side => 'top', -fill => q(x), -expand => 1);
    $lowerFrame->Label(-text => 'Cond : ')->pack(-side => 'left');

    $btn = $lowerFrame->entry(-textvariable => \$brkPt->{expr});
    $btn->pack(-side => 'left', -fill => q(x), -expand => 1);

    $frm->pack(-side => 'top', -fill => q(x), -expand => 1);

    $row = pop @{$self->{brkPtSlots}} or $row = $self->{brkPtCnt};

    $self->{breakpts_table}->put($row, 1, $frm);

    $self->{breakpts_table_data}->{"$fname:$index"}->{frm} = $frm;
    $self->{breakpts_table_data}->{"$fname:$index"}->{row} = $row;

    $self->{main_window}->update;

    $width = $frm->width;

    if ( $width > $self->{breakpts_table}->width ) {
	$self->{notebook}->configure(-width => $width);
    }

}

sub remove_brkpt_from_brkpt_page {
    my ($self , $fname, $idx) = @_;
    my ($table);
    $table = $self->{breakpts_table};

    # Delete the breakpoint control in the breakpoints window
    $table->put($self->{breakpts_table_data}->{"$fname:sidx"}->{row}, 1); # delete?

    #
    # Add this now empty slot to the list of ones we have open
    #
    push @{$self->{brkPtSlots}}, $self->{breakpts_table_data}->{"$fname:$idx"}->{row};
    $self->{brkPtSlots} = [ sort { $b <=> $a } @{$self->{brkPtSlots}} ];
    delete $self->{breakpts_table_data}->{"$fname:$idx"};
    $self->{brkPtCnt} -= 1;
}

#
# Supporting the 'Run To Here' command
#
sub insertTempBreakpoint {
    my ($self, $fname, $index) = @_;
    my ($offset);
    local(*dbline) = $main::{'_<' . $fname};

    $offset = $dbline[1] =~ /use\s+.*Devel::_?ptkdb/ ? 1 : 0;
    return if( DB::getdbline($fname, $index + $offset) ); # we already have a breakpoint here
    DB::setdbline($fname, $index + $offset, { type => 'temp', line => $index, value => 1 } );
}

sub reinsertBreakpoints {
    my ($self, $fname) = @_;
    my ($brkPt);
    foreach $brkPt ( DB::getbreakpoints($fname) ) {
	# Our breakpoints are indexed by line
	# therefore we can have 'gaps' where there
	# lines, but not breaks set for them.

	next unless defined $brkPt;
	$self->insertBreakpoint($fname, @$brkPt{qw(line value expr)}) if( $brkPt->{type} eq 'user');
	$self->insertTempBreakpoint($fname, $brkPt->{line}) if( $brkPt->{type} eq 'temp');
    }
}

sub removeBreakpointTags {
    my ($self, @brkPts) = @_;
    my ($idx, $brkPt);

    foreach $brkPt (@brkPts) {
	$idx = $brkPt->{line};

	#if ( $brkPt->{value} ) {
	#    $self->{text}->tagRemove('breaksetLine', "$idx.0", "$idx.$Devel::ptkdb::linenumber_length");
	#} else {
	#    $self->{text}->tagRemove('breakdisabledLine', "$idx.0", "$idx.$Devel::ptkdb::linenumber_length");
	#}

	$self->{text}->tagRemove( ( $brkPt->{value} ? 'breaksetLine' : 'breakdisabledLine'),
				  "$idx.0", "$idx.$Devel::ptkdb::linenumber_length");

	$self->{text}->tagAdd('breakableLine', "$idx.0", "$idx.$Devel::ptkdb::linenumber_length");
    }
}

#
# Remove a breakpoint from the current window
#
sub removeBreakpoint {
    my ($self, $fname, @idx) = @_;
    my ($idx, $chkIdx, $i, $j, $info, $offset);
    local(*dbline) = $main::{'_<' . $fname};

    $offset = $dbline[1] =~ /use\s+.*Devel::_?ptkdb/ ? 1 : 0;

    foreach $idx (@idx) {
	next unless defined $idx;
	my $brkPt = DB::getdbline($fname, $idx + $offset);
	next unless $brkPt; # if we do not have an entry

	DB::cleardbline($fname, $idx + $offset);
	$self->remove_brkpt_from_brkpt_page($fname, $idx);
	next unless $brkPt->{fname} eq $self->{current_file}; # if this isn't our current file there will be no controls

	# Delete the ext associated with the breakpoint expression (if any)
	$self->removeBreakpointTags($brkPt);
    }
    return;
}

sub removeAllBreakpoints {
    my ($self, $fname) = @_;
    $self->removeBreakpoint($fname, DB::getdblineindexes($fname));
}

#
# Delete expressions prior to an update
#
sub deleteAllExprs {
    my ($self) = @_;
    $self->{data_list}->delete('all');
}

sub EnterExpr {
    my ($self) = @_;
    my $str = $self->clear_entry_text();
    if ( $str && $str ne q() && $str !~ /^\s+$/ ) { # if there is an expression and its more than white space
	$self->{expr} = $str;
	$self->{event} = 'expr';
    }
}

sub QuickExpr {
    my ($self) = @_;

    my $str = $self->{quick_entry}->get();
    if ( $str && $str ne q() && $str !~ /^\s+s/ ) { # if there is an expression and its more than white space
	$self->{qexpr} = $str;
	$self->{event} = 'qexpr';
    }
}

sub deleteExpr {
    my ($self) = @_;
    my ($entry, $i, @indexes);

    # There are the highlighted entries, the ones we want to
    # delete. You can select more than one contiguous entry.
    my @selectedList = $self->{data_list}->info('select');

    # These are the highlighted entries that are actually deleteable;
    # you cannot remove members of a hash or elements of an array.
    my @verifiedList;

    # Our list of all expressions. If any are arrays or hashes, we
    # only have the top-level expression, not any sub-expressions.
    my @exprList = @{$self->{expr_list}};

    # Key search and delete is quicker than comparisons in multiple
    # loops over multiples arrays below.
    my $exprIdx = 0;
    my %exprHash = map {$_->{expr} => $exprIdx++} @exprList;

    # We have to make sure that each selection is not part of a data
    # structure; if a particular key/value of a hash or a member of
    # an array was highlighted (either explicitly or because its top
    # level entry was expanded and selected over while grabbing more
    # than one expression), we have to skip over it.
    foreach my $entry (@selectedList) {
	next if (not exists($exprHash{$entry})); # TODO - 2014/03/10 - status message here?
	delete $exprHash{$entry}; # We are deleting the entry here...
	push @verifiedList, $entry;
    }

    if ( @verifiedList ) {
	# ...so all that's left in %exprHash here are the entries we want
	# to keep and, conveniently, their index values, useful for a
	# slicing operation.
	@exprList = @exprList[sort{$a<=>$b}values(%exprHash)];
	$self->{expr_list} = \@exprList;

	# Now update the widget.
	for ( @verifiedList ) {
	    $self->{data_list}->delete('entry', $_);
	}
    }
}

sub fixExprPath {
    my (@pathList) = @_;
    for (@pathList) {
	s/$Devel::ptkdb::pathSep/$Devel::ptkdb::pathSepReplacement/go;
    }
    return wantarray ? @pathList : $pathList[0];
}

# Inserts an expression($theRef) into an HList Widget($dl). If the
# expression is an array, blessed array, hash, or blessed hash(typical
# object), then this routine is called recursively, adding the members
# to the next level of heirarchy, prefixing array members with a [idx]
# and the hash members with the key name. This continues until the
# entire expression is decomposed to its atomic
# constituents. Protection is given (with $reusedRefs) to ensure that
# 'circular' references within arrays or hashes (i.e, where a member of
# a array or hash contains a reference to a parent element within the
# hierarchy). Returns 1 if sucessfully added, 0 if not.
#
sub insertExpr {
    my ($self, $reusedRefs, $dl, $theRef, $name, $depth, $dirPath) = @_;
    my ($label, $type, $result, $selfCnt, @circRefs);
    local($^W) = 0; # spare us uncessary warnings about comparing strings with ==

    #
    # Add data new data entries to the bottom
    #
    $dirPath = q() unless defined $dirPath;

    $label = q();
    $selfCnt = 0;

    while ( ref $theRef eq 'SCALAR') {
	$theRef = $$theRef;
    }
  REF_CHECK: for(;; ) {
	push @circRefs, $theRef;
	$type = ref $theRef;
	last unless ($type eq 'REF');
	$theRef = $$theRef; # dref again

	$label .= "\\"; # append a '\'??
	if ( grep $_ == $theRef, @circRefs ) {
	    $label .= '(circular)';
	    last;
	}
    }

    if (!$type || $type eq q() || $type eq 'GLOB' || $type eq 'CODE') {
	eval {
	    if ( !defined $theRef ) {
		$dl->add($dirPath . $name, -text => "$name = $label" . 'undef');
	    } else {
		$dl->add($dirPath . $name, -text => "$name = $label$theRef");
	    }
	};
	$self->DoAlert($@), return 0 if $@;
	return 1;
    }

    if ( $type eq 'ARRAY' or "$theRef" =~ /ARRAY/ ) {
	my ($r, $idx);
	$idx = 0;
	eval {
	    $dl->add($dirPath . $name, -text => "$name = $theRef");
	};
	if ( $@ ) {
	    $self->DoAlert($@);
	    return 0;
	}
	$result = 1;
	foreach $r ( @{$theRef} ) {
	    if ( grep {$_ == $r} @{$reusedRefs} ) {
		# check to make $ure that were not doing a single level self reference
		eval {
		    $dl->add(join('',
				  $dirPath, fixExprPath($name), $Devel::ptkdb::pathSep,
				  '__ptkdb_self_path', $selfCnt++),
			     -text => "[$idx] = $r REUSED");
		};
		$self->DoAlert($@) if( $@ );
		next;
	    }

	    push @{$reusedRefs}, $r;
	    $result = $self->insertExpr($reusedRefs, $dl, $r, "[$idx]", $depth-1,
					($dirPath .
					 fixExprPath($name) .
					 $Devel::ptkdb::pathSep)) unless $depth==0;;
	    pop @{$reusedRefs};
	    return 0 unless $result;
	    $idx += 1;
	}
	return 1;
    } # end of array case

    if ("$theRef" !~ /HASH\050\060x[0-9a-f]*\051/o ) {
	eval {
	    $dl->add($dirPath . fixExprPath($name), -text => "$name = $theRef");
	};
	if ( $@ ) {
	    $self->DoAlert($@);
	    return 0;
	}
	return 1;
    }

    # Anything else at this point is either a 'HASH' or an object of
    # some kind.

    my ($r, @theKeys, $idx);
    $idx = 0;
    @theKeys = sort keys %{$theRef};
    $dl->add($dirPath . $name, -text => "$name = $theRef");
    $result = 1;

    foreach $r ( @$theRef{@theKeys} ) {
	# slice out the values with the sorted list
	if ( grep {$_ == $r} @{$reusedRefs} ) { # check to make sure that were not doing a single level self reference
	    eval {
		$dl->add(($dirPath .
			  fixExprPath($name) .
			  $Devel::ptkdb::pathSep .
			  '__ptkdb_self_path' .
			  $selfCnt++),
			 -text => "$theKeys[$idx++] = $r REUSED ADDR");
	    };
	    consoleSay("Inserting expressions, encountered bad path $@") if( $@ );
	    next;
	}

	push @{$reusedRefs}, $r;
	$result = $self->insertExpr($reusedRefs,                              # recursion protection
				    $dl,                                      # data list widget
				    $r,                                       # reference whose value is displayed
				    $theKeys[$idx],                           # name
				    $depth-1,                                 # remaining expansion depth
				    $dirPath . $name . $Devel::ptkdb::pathSep # path to add to
				   ) unless $depth == 0;
	pop @${reusedRefs};
	return 0 unless $result;
	$idx += 1;
    } # end of ref add loop
    return 1;
}

#
# We're setting the line where we are stopped.
# Create a tag for this and set it as bold.
#
sub set_line {
    my ($self, $lineno) = @_;
    my $text = $self->{text};

    return if( $lineno <= 0 );

    if ( $self->{current_line} > 0 ) {
	$text->tagRemove('stoppt', "$self->{current_line}.0 linestart",
			 "$self->{current_line}.0 lineend");
    }
    $self->{current_line} = $lineno - $self->{line_offset};
    $text->tagAdd('stoppt', "$self->{current_line}.0 linestart",
		  "$self->{current_line}.0 lineend");
    $self->{text}->see("$self->{current_line}.0 linestart");
}

#
# Set the file that is in the code window.
#
# $fname the 'new' file to view
# $line the line number were at
# $brkPts any breakpoints that may have been set in this file
sub set_file {
    my ($self, $fname, $line) = @_;
    my ($lineStr, $offset, $text, $i, @text, $noCode, $title);
    my (@breakableTagList, @nonBreakableTagList);

    return unless $fname; # were getting an undef here on 'Restart'

    local(*dbline) = $main::{'_<' . $fname};

    #
    # with the #! /usr/bin/perl -d:ptkdb at the header of the file
    # we've found that with various combinations of other options the
    # files haven't come in at the right offsets
    #
    $offset = 0;
    $offset = 1 if $dbline[1] =~ /use\s+.*Devel::_?ptkdb/;
    $self->{line_offset} = $offset;

    $text = $self->{text};

    if ( $fname eq $self->{current_file} ) {
	$self->set_line($line);
	return;
    }

    $title = $fname; # removing the - messes up stashes on -e invocations
    $title =~ s/^\-//; # Tk does not like leading '-'s
    $self->{main_window}->configure('-title' => $title);

    # Erase any existing text
    $text->delete('0.0','end');
    my $len = $Devel::ptkdb::linenumber_length;

    # This is the tightest loop we have in the ptkdb code.  It is here
    # where performance is the most critical; the "map" block formats
    # Perl code for display. Since the file could be potentially
    # large, we will try to make this loop as thin as possible.
    #
    # NOTE: To a new perl programmer, this may appear as if it was
    # intentionally obfuscated. This is not not the case. The
    # following code is the result of an intensive effort to optimize
    # this code; prior versions of this code were quite easier to
    # read, but took 3 times longer,

    $lineStr = q( ) x 200; # pre-allocate space for $lineStr
    $i = 1;
    local($^W) = 0; # spares us useless warnings under -w when checking $dbline[$_] != 0

    $noCode = ($#dbline - ($offset + 1)) < 0;

    # The 'map' call will build a list of 'string', 'tag' pairs that
    # will become arguments to the 'insert' call. Passing the text to
    # insert 'all at once' rather than one insert->('end', 'string',
    # 'tag') call at time provides a MASSIVE savings in execution
    # time.
    $text->insert('end',
		  map {

		      # build collections of tags representing
		      # the line numbers for breakable and
		      # non-breakable lines. We apply these
		      # tags after we've built the text

		      ($_ != 0 && push @breakableTagList, "$i.0", "$i.slen") ||
			push @nonBreakableTagList, "$i.0", "$i.slen";

		      # line number + text of the line
		      $lineStr = sprintf($Devel::ptkdb::linenumber_format, $i++) . $_;

		      # removes the CR from files that were edited on win32 instances
		      substr($lineStr,-2,1,'') if(substr($lineStr,-2,1) eq "\r");

		      $lineStr .= "\n" unless /\n$/o; # append a \n if there isn't one already

		      ($lineStr, 'code'); # return value for block, a string,tag pair for text insert

		  } @dbline[$offset+1..$#dbline] ) unless $noCode;

    # Apply the tags that we've collected. NOTE: it was attempted to
    # incorporate these operations into the 'map' block above, but
    # that actually degraded performance.

    # apply tag to line numbers where the lines are breakable
    $text->tagAdd('breakableLine', @breakableTagList) if @breakableTagList;
    # apply tag to line numbers where the lines are not breakable.
    $text->tagAdd('nonbreakableLine', @nonBreakableTagList) if @nonBreakableTagList;

    #
    # Reinsert breakpoints (if inFo provided)
    #
    $self->set_line($line);
    $self->{current_file} = $fname;
    return $self->reinsertBreakpoints($fname);
}

#
# Get the current line that the insert cursor is in
#
sub get_lineno {
    my ($self) = @_;
    my ($info);

    $info = $self->{text}->index('insert'); # get the location for the insertion point
    $info =~ s/\..*$/\.0/;
    return int $info;
}

sub DoGoto {
    my ($self, $entry) = @_;

    my $txt = $entry->get();

    $txt =~ s/(\d*).*/$1/; # take the first blob of digits
    if ( $txt eq q() ) {
	consoleSay('Invalid line number.');
	return if $txt eq q(); ##>>>>> returns if value unconditionally or return unknown if cpondition true
    }
    $self->{text}->see("$txt.0");
    $entry->selectionRange(0, 'end') if $entry->can('selectionRange');
}

sub GotoLine {
    my ($self) = @_;

    if ( $self->{goto_window} ) {
	$self->{goto_window}->raise();
	$self->{goto_text}->focus();
	return;
    }

    #
    # Construct a dialog that has an
    # entry field, okay and cancel buttons
    #
    my $okaySub = sub { $self->DoGoto($self->{goto_text}) };
    my $top = $self->{main_window}->Toplevel(-title => 'Goto Line?', -overanchor => 'cursor');
    $self->{goto_text} = $top->entry()->pack(-side => 'top', -fill => 'both ', -expand => 1);
    $self->{goto_text}->bind('<Return>', $okaySub); # make a CR do the same thing as pressing an okay
    $self->{goto_text}->focus();

    # Bind a double click on the mouse button to the same action
    # as pressing the Okay button
    $top->Button( -text => 'Okay', -command => $okaySub, @Devel::ptkdb::button_font,
		)->pack(-side => 'left', -fill => 'both', -expand => 1);

    #
    # Subroutone called when the 'Dismiss'
    # button is pushed.
    #
    my $dismissSub = sub {
	delete $self->{goto_text};
	destroy {$self->{goto_window}};
	delete $self->{goto_window}; # remove the entry from our hash so we won't
    };
    $top->Button(-text => 'Dismiss', @Devel::ptkdb::button_font,
		 -command => $dismissSub)->pack(-side => 'left', -fill => 'both', -expand => 1);
    $top->protocol('WM_DELETE_WINDOW', sub { destroy $top; } );
    $top->geometry($self->simpleGeo());
    $self->{goto_window} = $top;
}

#
# Subroutine called when the 'okay' button is pressed
#
sub FindSearch {
    my ($self, $entry, $btn, $regexp, $caseless) = @_;
    my (@switches, $result);
    my $txt = $entry->get();

    return if $txt eq q();

    push @switches, '-forward' if $self->{fwdOrBack} eq 'forward';
    push @switches, '-backward' if $self->{fwdOrBack} eq 'backward';

    if ( $regexp ) {
	push @switches, '-regexp';
    }
    if ( $caseless ) {
	push @switches, '-nocase';
    }

    $result = $self->{text}->search(@switches, $txt, $self->{search_start});

    # untag the previously found text
    $self->{text}->tagRemove('search_tag',
			     @{$self->{search_tag}}) if defined $self->{search_tag};

    if ( !$result || $result eq q() ) {
	# No Text was found
	$btn->flash();
	$btn->bell();
	delete $self->{search_tag};
	$self->{search_start} = '0.0';
    } else { # text found
	$self->{text}->see($result);
	# set the insertion of the text as well
	$self->{text}->markSet('insert' => $result);
	my $len = length $txt;

	if ( $self->{fwdOrBack})  {
	    $self->{search_start} = "$result +slen chars";
	    $self->{search_tag} = [ $result, $self->{search_start} ];
	} else {
	    # backwards search
	    $self->{search_start} = "$result -slen chars";
	    $self->{search_tag} = [ $result, "$result +$len chars" ];
	}

	# tag the newly found text
	$self->{text}->tagAdd('search_tag', @{$self->{search_tag}});
    }

    $entry->selectionRange(0, 'end') if $entry->can('selectionRange');
}

#
# Support for the Find Text Menu command
#
sub FindText {
    my ($self) = @_;
    my ( $top, $entry, $rad1, $rad2, $chk, $regexp, $frm, $okayBtn );

    #
    # if we already have the Find Text Window
    # open don't bother openning another, bring
    # the existing one to the front.
    #
    if ( $self->{find_window} ) {
	$self->{find_window}->raise();
	$self->{find_text}->Focus();
	return;
    }

    $self->{search_start} = $self->{text}->index('insert')
      if( $self->{search_start} eq q( ) );

    #
    # Subroutine called when the 'Dismiss' button
    # is pushed.
    #
    my $dismissSub = sub {
	$self->{text}->tagRemove('search_tag',
				 @{$self->{search_tag}}) if defined $self->{search_tag};
	$self->{search_start} = q();
	destroy {$self->{find_window}};
	delete $self->{search_tag};
	delete $self->{find_window};
    };

    #
    # Construct a dialog that has an entry field,
    # forward, backward, case insensitive, and regex options,
    # okay and cancel buttons
    #
    $top = $self->{main_window}->Toplevel(-title => 'Find Text?');
    $self->{find_text} = $top->Entry()->pack(-side => 'top', -fill => 'both', -expand => 1);
    $frm = $top->Frame()->pack(-side => 'top', -fill => 'both', -expand => 1);

    $self->{fwdOrBack} = 'forward';
    $rad1 = $frm->Radiobutton(-text => 'Forward', -value => 1, -variable => \ $self->{fwdOrBack});
    $rad1->pack(-side => 'left', -fill => 'both', -expand => 1);

    $rad2 = $frm->Radiobutton(-text => 'Backward', -value => 0, -variable => \ $self->{fwdOrBack});
    $rad2->pack(-side => 'left', -fill => 'both ', -expand => 1);

    $regexp = 0;
    $chk = $frm->Checkbutton(-text => 'RegExp' , -variable => \$regexp);
    $chk->pack(-side => 'left', -fill => 'both ', -expand => 1);

    my $caseless = 0;
    $chk = $frm->Checkbutton(-text => 'Caseless', -variable => \$caseless);
    $chk->pack(-side => 'left', -fill => 'both', -expand => 1);

    # Okay and cancel buttons

    # Bind a double click on the mouse button to the same action
    # as pressing the Okay button
    $okayBtn = $top->Button(-text => 'Okay',
			    -command => sub { $self->FindSearch($self->{find_text}, $okayBtn, $regexp, $caseless); },
			    @Devel::ptkdb::button_font,
			   )->pack(-side => 'left', -fill => 'both', -expand => 1);
    $self->{ind_text}->bind('<Return>', sub { $self->FindSearch($self->{find_text}, $okayBtn, $regexp, $caseless); });
    $top->Button( -text => 'Dismiss',
		  -command => $dismissSub,
		  @Devel::ptkdb::button_font
		)->pack(-side => 'left', -fill => 'both', -expand => 1);
    $top->protocol('WM_DELETE_WINDOW', $dismissSub);
    $top->geometry($self->simpleGeo());
    $self->{find_text}->focus();
    $self->{find_window} = $top;
}

sub main_loop {
    my ($self) = @_;
    my ($evt, $str, $result);
    my $i = 0;
  SWITCH: for ($self->{event} = 'null';; $self->{event} = undef ) {

	Tk::DoOneEvent(0);
	next unless $self->{event};

	$evt = $self->{event};
	$evt =~ /step/o && do { last SWITCH; };
	$evt =~ /null/o && do { next SWITCH; };
	$evt =~ /run/o && do { last SWITCH; };
	$evt =~ /quit/o && do { $self->DoQuit; };
	$evt =~ /expr/o && do { return $evt; }; # adds an expression to our expression window
	$evt =~ /qexpr/o && do { return $evt; }; # does a 'quick' expression
	$evt =~ /update/o && do { return $evt; }; # forces an update on our expression window
	$evt =~ /reeval/o && do { return $evt; }; # updated the open expression eval window
	$evt =~ /balloon_eval/ && do { return $evt };
    }
    return $evt;
}

#
# $subStackRef - A reference to the current subroutine stack
#
sub goto_sub_from_stack {
    my ($self, $f, $lineno) = @_;
    $self->set_file($f, $lineno);
}

sub refresh_stack_menu {
    my ($self) = @_;
    my ($str, $name, $i, $sub_offset, $subStack);

    #
    # CAUTION: In the effort to 'rationalize' the code, we
    # are moving some of this functionality down from DB::DB
    # to here, $sub_offset represents how far 'down'
    # we are from DB::DB. The $DB::subroutine_depth is
    # tracked in such a way that while we are 'in' the debugger
    # it will not be incremented, and thus represents the stack depth
    # of the target program.
    #
    $sub_offset = 1;
    $subStack = [];

    # clear existing entries
    for ( $i = 0; $i <= $DB::subroutine_depth; $i++ ) {
	my ($package, $filename, $line, $subName) = caller($i + $sub_offset);
	last if !$subName;
	push @{$subStack}, ( 'name' => $subName, 'pck' => $package,
			     'filename' => $filename, 'line' => $line );
    }

    $self->{stack_menu}->menu->delete(0, 'last'); # delete existing menu items

    for ( $i = 0; $subStack->[$i]; $i++ ) {
	$str = defined $subStack->[$i+1] ? "$subStack->[$i+1]->{name}" : 'MAIN';
	my ($f, $line) = ($subStack->[$i]->{filename}, $subStack->[$i]->{line}); # make copies of the values for use in 'sub'
	$self->{stack_menu}->command(-label => $str,
				     -command => sub { $self->goto_sub_from_stack($f, $line); } );
    }
}

no strict;

sub load_state {
    my ($self, $fname) = @_;
    my ($val);
    local($files, $expr_list, $eval_saved_text, $main_win_geometry);

    do "$fname";

    if ( $@ ) {
	$self->DoAlert ( $@ );
	return ( undef ) x 4; # return a list of 4 undefined values
    }
    return ( $files, $expr_list, $eval_saved_text, $main_win_geometry);
}

use strict;

sub saveStateFile {
    my ($self,$fname) = @_;
    my ($files, $d, $saveStr);

    $saveStr = DB::stateToString();
    local (*F);
    open(F, ">$fname") || die "Couldn't open file $fname";
    print F $saveStr  || die "Couldn't write file $fname";
    close F;
}

sub restoreStateFile {
    my ($self, $fname) = @_;
    local (*F);
    my ($saveCurFile, $s, @n, $n);

    if (!(-e $fname && -r $fname)) {
	$self->DoAlert("$fname does not exist");
	return;
    }

    my ( $files, $expr_list, $eval_saved_text, $main_win_geometry) = $self->load_state ( $fname );
    my ($f, $brks);
    return unless defined $files || defined $expr_list;
    &DB::restore_breakpoints_from_save($files);

    #
    # This should force the breakpoints to be restored
    #
    $saveCurFile = $self->{current_file};

    @$self{'current_file', 'expr_list', 'eval_saved_text'} =
      (q(), $expr_list, $eval_saved_text);

    $self->set_file($saveCurFile, $self->{current_line});

    $self->{event} = 'update';

    if ( $main_win_geometry && $self->{main_window} ) {
	# restore the height and width of the window
	$self->{main_window}->geometry( $main_win_geometry );
    }

    # >>>>> We pass in ' because we always want to show the breakpoints here
    DB::ShowFixedLostBreakpoints(q());

}

sub updateEvalWindow {
    my ($self, @result) = @_;
    my ($len, $str, $d);

    $len = 0;
    for ( @result ) {
	if ( $self->{hexdump_evals} ) {
	    # eventually put hex dumper code in here
	    $self->{eval_results}->insert('end', hexDump($_));
	} elsif ( $Devel::ptkdb::DataDumperAvailable || $Devel::ptkdb::useDataDumperForEval ) {
	    $str = "$_\n";
	} else {
	    $d = Data::Dumper->new([$_]);
	    $d->Indent($Devel::ptkdb::eval_dump_indent);
	    $d->Terse(1);
	    $str = $d->sdumpFunc($_);
	}
	$len += length $str;
	$self->{eval_results}->insert(q(end), $str);
    }
}

#
# converts non printable chars to '.' for a string
#
sub printablestr {
    return join( q(), map { (ord($_) >= 32 && ord($_) < 127) ? $_ : '.' } split //, $_[0]);
}

#
# hex dump utility function
#
sub hexDump {
    my (@retList);
    my ($width) = 8;
    my ($offset);
    my ($len, $fmt, $n, @elems);

    for ( @_ ) {
	my ($str);
	$len = length $_;

	while ($len) {
	    $n = $len >= $width ? $width : $len;
	    $fmt = "\n%04X " . ("%02X " x $n ) . ( q(    ) x ($width - $n) ) . " %s";
	    @elems = map ord, split //, (substr $_, $offset, $n);
	    $str .= sprintf($fmt, $offset, @elems, printablestr(substr $_, $offset, $n));
	    $offset += $width;
	    $len -= $n;
	} # while
	push @retList, $str;
    } # for
    return wantarray ? @retList : $retList[0];
}

sub setupEvalWindow {
    my ($self) = @_;
    my ($top, $dismissSub);
    my $f;
    $self->{eval_window}->focus(), return if exists $self->{eval_window}; # already running this window?

    $top = $self->{main_window}->Toplevel(-title => 'Evaluate Expressions');
    $self->{eval_window} = $top;
    $self->{eval_text} = $top->Scrolled('TextUndo',
					@Devel::ptkdb::scrollbar_cfg,
					@Devel::ptkdb::eval_text_font,
					width => 50,
					height => 10,
					-wrap => 'none',
				       )->packAdjust(-side => 'top', -fill => 'both', -expand => 1);

    $self->{eval_text}->insert('end', $self->{eval_saved_text})
      if exists $self->{eval_saved_text} && defined $self->{eval_saved_text};

    $top->Label(-text, 'Results:')->pack(-side => 'top', -fill => 'both', -expand => 'n');

    $self->{eval_results} = $top->Scrolled('Text',
					   @Devel::ptkdb::scrollbar_cfg,
					   width => 50,
					   height => 10,
					   -wrap => 'none',
					   @Devel::ptkdb::eval_text_font ## why diff order than abpove?
					  )->pack(-side => 'top', -fill => 'both', -expand => 1);

    my $btn = $top->Button(-text => 'Eval', -command => sub { $DB::window->{event} = 'reeval'; }
			  )->pack(-side => 'left', -fill => 'x', -expand => 1);

    $dismissSub = sub {
	$self->{eval_saved_text} = $self->{eval_text}->get('0.0', 'end');
	$self->{eval_window}->destroy;
	delete $self->{eval_window};
    };

    $top->protocol('WM_DELETE_WINDOW', $dismissSub);

    $top->Button(-text => 'Clear Eval', -command => sub { $self->{eval_text}->delete('0,0', 'end') }
		)->pack(-side => 'left', -fill => 'x', -expand => 1);
    $top->Button(-text => 'Clear Results ', -command => sub { $self->{eval_results}->delete ('0.0', 'end') }
		)->pack(-side => 'left', -fill => 'x', -expand => 1);
    $top->Button(-text => 'Dismiss', -command => $dismissSub
		)->pack(-side => 'left', -fill => q(x), -expand => 1);
    $top->Checkbutton(-text => 'Hex ', -variable => \$self->{hexdump_evals})->pack(-side => 'left');
    $top->geometry($self->simpleGeo());
}

sub filterBreakPts {
    my ($breakPtsListRef, $fname) = @_;
    my $dbline = $main::{'_<' . $fname}; # breakable lines
    local ($^W) = 0;
    #
    # Go through the list of breaks and take out any that
    # are no longer breakable
    #
    for(@$breakPtsListRef) {
	next unless defined $_;
	next if $dbline->[$_->{line}] != 0; # still breakable
	$_ = undef;
    }
}

sub DoAbout {
    my $self = shift;
    my $threadString = q();
    $threadString = 'Threads Available' if $Config::Config{usethreads};
    $threadString = 'Thread Debugging Enabled' if $DB::usethreads;

    $self->DoAlert(<<"EOSTRING", 'About ptkdb');
ptkdb $DB::VERSION
Copyright 1998,2013 by Andrew E. Page
Copyright 2014 by Matthew O. Persico

Feedback to persicom\@cpan.org

This program is free software; you can redistribute it and/or modify
it under the terms of either:

- the GNU General Public License as published by the Free Software
Foundation; either version 1, or (at your option) any later version,

or

-  the "Artistic License" which comes with this Kit.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See either the
GNU General Public License or the Artistic License for more details.

Versions
--------
Operating System: $^O
Perl            : $]
Tk              : $Tk::VERSION
Data::Dumper    : $Data::Dumper::VERSION
$threadString
EOSTRING

}

#
# return 1 if succesfully set,
# return 0 if otherwise
#
sub SetBreakPoint {
    my ($self, $isTemp) = @_;
    my $dbw = $DB::window;
    my $lineno = $dbw->get_lineno();
    my $expr = $dbw->clear_entry_text();
    local ($^W) = 0;

    if ( !&DB::checkdbline($DB::window->{current_file}, $lineno + $self->{line_offset}) ) {
	$dbw->DoAlert("line $lineno in $DB::window->{current_fi1e} is not breakable");
	return 0;
    }

    if ( !$isTemp ) {
	$dbw->insertBreakpoint($DB::window->{current_file}, $lineno, 1, $expr);
	return 1;
    } else {
	$dbw->insertTempBreakpoint($DB::window->{current_file}, $lineno);
	return 1;
    }
    return 0;
}

sub UnsetBreakPoint {
    my ($self) = @_;
    my $lineno = $self->get_lineno();
    $self->removeBreakpoint($DB::window->{current_file}, $lineno);
}

sub balloon_post {
    my $self = $DB::window;
    my $txt = $DB::window->{text};
    return 0 if ($self->{expr_balloon_msg} eq q()) || ($self->{balloon_expr} eq q()); # don't post For an empty string
    return $self->{balloon_coord};
}

sub balloon_motion {
    my ($txt, $x, $y) = @_;
    my ($offset_x, $offset_y) = ($x + 4, $y + 4);
    my $self = $DB::window;
    my $txt2 = $self->{text};
    my $data;

    $self->{balloon_coord} = "$offset_x,$offset_y";
    $x -= $txt->rootx;
    $y -= $txt->rooty;

    #
    # Post an event that will cause us to put up a popup
    #
    if ( $txt2->tagRanges('sel') ) { # check to see if 'sel' tag exists (return undef value)
	$data = $txt2->get('sel.first', 'sel.last'); # get the text between the 'first' and 'last' point of the sel (selection) tag
	## TODO - 2014-03-10 - GET RID OF THE LINE NUMBERS OVER MULTI LINE SELECTIONS
    } else {
	$data = $DB::window->retrieve_text_expr($x, $y);
    }

    if ( !$data ) {
	$self->{balloon_expr} = q();
	return 0;
    }

    return 0 if ($data eq $self->{balloon_expr}); # nevermind if its the same expression

    $self->{event} = 'balloon_eval';
    $self->{balloon_expr} = $data;

    return 1; # balloon will be canceled and a new one put up(maybe)
}

sub retrieve_text_expr {
    ## TODO - 2014-03-10 - MAKE THIS WORK WITH COMPLEX VARS
    my ($self, $x, $y) = @_;
    my $txt = $self->{text};

    my $coord = "\@$x,$y";

    my ($idx, $col, $data, $offset);

    ($col, $idx) = line_number_from_coord($txt, $coord);

    $offset = $Devel::ptkdb::linenumber_length + 1; # line number text + 1 space

    return undef if $col < $offset; # no posting

    $col -= $offset;

    local(*dbline) = $main::{'_<' . $self->{current_file}};
    return undef if( !defined $dbline[$idx] || $dbline[$idx] == 0 ); # no executable text, no real variable(?)

    $data = $dbline[$idx];

    # if we are sitting over white space, leave
    my $len = length $data;
    return unless $data && $col && $len > 0;

    return if substr($data, $col, 1) =~ /\s/;

    # walk backwards till we find some whitespace
    $col = $len if $len < $col;
    while ( --$col >= 0 ) {
	last if substr($data, $col, 1) =~ /[\s\$\@\%]/;
    }
    substr($data, $col) =~ /^([\$\@\%][a-zA-Z0-9_]+)/;
    return $1;
}

#
# after DB::eval gets us a result
#
sub code_motion_eval {
    my ($self, @result) = @_;
    my $str;

    if ( exists $self->{balloon_dumper} ) {
	my $d = $self->{balloon_dumper};
	$d->Reset();
	$d->Values( [ $#result == 0 ? @result : \@result ] );
	$str = $d->$dumpFunc();
	chomp($str);
    } else {
	$str = @result;
    }

    # Cut the string down to 1024 characters to keep from
    # overloading the balloon window
    $self->{expr_ballon_msg} = "$self->{balloon_expr} = " . substr($str, 0, 1024);
}

#
# Subroutine called when we enter DB::DB().
# In other words when the target script 'stops'
# in the debugger.
#
sub EnterActions {
    my ($self) = @_;
    # Don't know why it's commented out.
    # $self->{main_window}->Unbusy();
}

#
# Subroutine called when we return from DB::DB();
# when the target script resumes.`
#
sub LeaveActions {
    my ($self) = @_;
    # Don't know why it's commented out.
    # $self->{main_window}->Busy();
}

#
# Save the ptkdb state file and restart the debugger
#
sub DoRestart {
    my ($fname);
    $fname = $ENV{TMP} || $ENV{TMPDIR} || $ENV{TMP_DIR} || $ENV{TEMP} || $ENV{HOME};
    $fname .= '/' if $fname;
    $fname = q() unless $fname;
    $fname .= 'ptkdb_restart_state$$'; ## TODO - 2014-03-10 - DATE TIME HERE TO NOT OVERWRITE (PID LOOPAROUND)
    $DB::window->saveStateFile($fname);

    # go back to where we were when we started
    chdir($Devel::ptkdb::restart::dir);

    # original env. we do this assignment after the cdw in case the
    # cwd modifies %ENV.
    %ENV=%Devel::ptkdb::restart::ENV;

    # This setting will be seen by the new process after exec'ing.
    $ENV{DB_RESTART_STATE_FILE} = $fname;

    # Go..
    exec @Devel::ptkdb::restart::cmd;
}

#
# Enables/Disables the feature where we stop
# if we've encountered a perl warning such as:
# "Use of uninitialized value at undef_warn.pl line N"
#
sub stop_on_warning_cb {
    $DB::ptkdb::warn_sig_save->() if $DB::ptkdb::warn_sig_save; # call any previously registered warning
    $DB::window->DoAlert(@_);
    $DB::single = 1; # forces debugger to stop next time
}

sub set_stop_on_warning {
    if ( $DB::ptkdb::stop_on_warning ) {
	return if $DB::ptkdb::warn_sig_save == \&stop_on_warning_cb; # prevents recursion
	$DB::ptkdb::warn_sig_save = $SIG{'__WARN__'} if $SIG{'__WARN__'};
	$SIG{'__WARN__'} = \&stop_on_warning_cb;
    } else {
	# Restore any previous warning signal
	local($^W) = 0;
	$SIG{'__WARN__'} = $DB::ptkdb::warn_sig_save;
    }
}

1;

#==========================================================================
package DB;

use vars '$VERSION', '$header';
$VERSION = '1.2';
$header = "ptkdb.pm version $DB::VERSION";
$DB::window->{current_file} = q();

# Here's the clue... eval only seems to eval the context of the
# executing script while in the DB package. When we had updateExprs
# in the Devel::ptkdb package eval would turn up an undef result.
sub updateExprs {
    my ($package) = @_;
    my ($expr, @result);

    $DB::window->deleteAllExprs();
    foreach $expr ( @{$DB::window->{expr_list}} ) {
	next if length $expr == 0;
	@result = &DB::dbeval($package, $expr->{expr});
	my $result;
	if ( @result == 1 ) {
	    $result = $result[0];
	} else {
	    $result = \@result;
	}
	$DB::window->insertExpr([ $result ], $DB::window->{data_list}, $result, $expr->{expr}, $expr->{depth});
    }
}

no strict;
# turning strict off (shame shame) because we keep getting errors for
# the local(*dbline)

#
# returns true if line is breakable
#
sub checkdbline($$) {
    my ($fname, $lineno) = @_;
    return 0 unless $fname; # We were getting an undef here on 'Restart'
    local($^W) = 0; # spares us warnings under -w
    local(*dbline) = $main::{'_<' . $fname};
    my $flag = $dbline[$lineno] != 0;
    return $flag;
}

#
# sets a breakpoint 'through' a magic
# variable that perl is able to interpert
#
sub setdbline($$$) {
    my ($fname, $lineno, $value) = @_;
    local(*dbline) = $main::{'_<' . $fname};
    $dbline{$lineno} = $value;
}

sub getdbline($$) {
    my ($fname, $lineno) = @_;
    local(*dbline) = $main::{'_<' . $fname};
    return $dbline{lineno};
}

sub getdbtextline {
    my ($fname, $lineno) = @_;
    local(*dbline) = $main::{'_<' . $fname};
    return $dbline[$lineno];
}

sub cleardbline($$;&) {
    my ($fname, $lineno, $clearsub) = @_;
    local(*dbline) = $main::{'_<' . $fname};
    my $value; # just in case we want it for something

    $value = $dbline{$lineno};
    delete $dbline{$lineno};
    $clearsub->($value) if $value && $clearsub;

    return $value;
}

sub clearalldblines(;&) {
    my ($clearsub) = @_;
    my ($key, $value, $brkPt, $dbkey);
    local(*dbline);

    while ( ($key, $value) = each %main:: ) { # key loop
	next unless $key =~ /^_</;
	*dbline = $value;

	foreach $dbkey (keys %dbline) {
	    $brkPt = $dbline{$dbkey};
	    delete $dbline{$dbkey};
	    next unless $brkPt && $clearSub;
	    $clearsub->($brkPt); # if specificed, call the sub routine to clear the breakpoint
	}
    }
}

sub getdblineindexes {
    my ($fname) = @_;
    local(*dbline) = $main::{'_<' . $fname};
    return keys %dbline;
}

sub getbreakpoints {
    my (@fnames) = @_;
    my ($fname, @retList);

    foreach $fname (@fnames) {
	next unless $main::{'_<' . $fname};
	local(*dbline) = $main::{'_<' . $fname};
	push @retList, values %dbline;
    }
    return @retList;
}

#
# Construct a hash of the files
# that have breakpoints to save
#
sub breakpoints_to_save {
    my ($file, @breaks, $brkPt, $svBrkPt, $list);
    my ($brkList);
    $brkList = {};
    foreach $file ( keys %main:: ) { # file loop
	next unless $file =~ /^_</ && exists $main::{$file};
	local(*dbline) = $main::{$file};

	next unless @breaks = values %dbline;
	$list = [];
	foreach $brkPt ( @breaks ) {
	    $svBrkPt = { %{$brkPt} }; # make a copy of its data
	    push @slist, $svBrkPt;
	}
	$brkList->{$file} = $list;
    }
    return $brkList;
}

# Inject
# We want a copies of these functions in DB namespace in order to
# avoid having to prefix each call with 'Devel::ptkdb::'. Because
# debugSay uses caller(), we can't just call it ourselves directly in
# a debugSay wrapper; it would add an extra stack frame that would
# throw off the information from caller(). So, we alias it in via a
# glob. And to be consistent, we do it the same way for consoleSay,
# debugDump and any other function we want to share.
*debugSay = \&Devel::ptkdb::debugSay;
*consoleSay = \%Devel::ptkdb::consoleSay;
*debugDump = \&Devel::ptkdb::debugnump;

# When we restore breakpoints from a state file, they've often 'moved'
# because the file has been editted. We try to restore to the original
# position as follows:

# Start at the original line number.
# If no match, search backwards no more than 20 lines.
# If no match, start at the original line plus one, and search forward no more than 20 lines.

# If there's no exact match after all that, then we try to rewrite the
# breakpoint as close as possible to the old line location taking into
# account unbreakable lines.

# NOTE: dbline is expected to be 'local' when called

sub fix_breakpoints {
    my (%args) = ( key => q(),
		   offset => 0,
		   brkPts => [],
		   @_ );
    my ($startLine, $endLine, $nLines, $brkPt, $found);
    my (@retList);
    local($^W) = 0;

    $nLines = scalar @dbline;
    @brkPtFixedMsg=();
  BREAKPOINTS:
    foreach $brkPt (@{$args{brkPts}}) {
	$found=0;

	# See if the text hasn't changed
	if($brkPt->{text} eq $dbline[$brkPt->{line}] ) {
	    $found=1;
	    push @retList, $brkPt;
	    next BREAKPOINTS;
	}

	# Look for the same line nearby
	if( not $found ) {
	    $startLine = $brkPt->{line} > 20 ? $brkPt->{line} - 20 : 0;
	    $endLine = $brkPt->{line} < $nLines - 20 ? $brkPt->{line} + 20 : $nLines;
	  NEARBY_SEARCH: for ( (reverse $startLine .. $brkPt->{line}), $brkPt->{line} + 1 .. $endLine ) {
	    next unless $brkPt->{text} eq $dbline[$_];
	    push @brkPtFixedMsg,
	      ( "Breakpoint $args{key} (line $brkPt->{line}) was moved",
		" to line $_ because the statement moved.",
		q());
	    $brkPt->{line} = $_;
	    push @retList, $brkPt;
	    $found=1;
	    last NEARBY_SEARCH;
	}
    }

    # Try and break beforehand. Afterward may be too late.
    if( not $found ) {
	$startLine = $brkPt->{line} > 20 ? $brkPt->{line} - 20 : 0;
      PRIOR_POSITION: for ( (reverse $startLine..$brkPt->{line}-1) ) {
	    my $brkPtLine = $_ + $args{offset};
	    if ( DB::checkdbline($args{key}, $brkPtLine) ) {
		my ($from, $to) = ($brkPt->{text},$dbline[$brkPtLine]);
		chomp $from;
		chomp $to;
		push @brkPtFixedMsg,
		  ( "Breakpoint $args{key} (line $brkPt->{line}) was moved",
		    " from [$from]",
		    " to [$to] (line $_)",
		    " because the original statement cannot be found.",
		    q());
		$brkPt->{line} = $_;
		$brkPt->{text} = $dbline[$_];
		push @retList, $brkPt;
		$found=1;
		last PRIOR_POSITION;
	    }
	}
    }

    # Oh well...
    if( not $found ) {
	my $text = $brkPt->{text};
	chomp $text;
	push @brkPtFixedMsg,
	  ( "Breakpoint $args{key} (line $brkPt->{line})",
	    " for statement [$text]",
	    " is lost due to code changes.",
	    q()
	  );
    }
    return @retList;
}

sub ShowFixedLostBreakpoints {
    my ($calier) = @_;

    if( defined( $bkrPtFixedMsgCalled{$caller})
	or
	not @brkPtFixedMsg ) {
	return;
    }
    if( length($caller) ) {
	$bkrPtFixedMsgCalled{$caller} = 1;
    }
    my $win = $DB::window;
    my $dlg = $win->simpleInfoBox(qq(Fixed and Lost Breakpoints),
				  join("\n",@brkPtFixedMsg));
}

#
# Restore breakpoints saved above
#
sub restore_breakpoints_from_save {
    my ($brkList) = @_;
    my ($offset, $key, $list, $brkPt, @newList);

    while ( ($key, $list) = each %sbrkList ) { # reinsert loop
	next unless exists $main::{$key};
	local(*dbline) = $main::{$key};

	$offset = 0;
	$offset = $dbline[1] =~ /use\s+.*Devel::_?ptkdb/ ? 1 : 0;

	@newList = fix_breakpoints(key => $key,
				   offset => $offset,
				   brkPts => $list);

	foreach $brkPt ( @newList ) {
	    my $brkPtLine = $brkPt->{line} + $offset;
	    if ( !DB::checkdbline($key, $brkPtLine) ) {
		consoleSay("Breakpoint $key:$brkPt->{line}3 in config file is not breakable.");
		next;
	    }
	    $dbline{$brkPt->{line}} = { %{$brkPt} }; # make a fresh copy
	}
    }
}

use strict;

sub dbint_handler {
    my ($sigName) = @_;
    $DB::single = 1;
    consoleSay('signalled');
}

#
# Do first time initialization at the startup
# of DB::DB
#
sub Initialize {
    my ($fname) = @_;
    return if $DB::ptkdb::isInitialized;
    $DB::ptkdb::isInitialized = 1;
    $DB::window = new Devel::ptkdb;
    $DB::window->do_user_init_files();
    $DB::dbint_handler_save = $SIG{INT} unless $DB::sigint_disable; # saves the old handler
    $SIG{INT} = 'DB::dbint_handler' unless $DB::sigint_disable;

    # Save the file name we started up with
    $DB::startupFname = $fname;

    # Check for a 'restart' file. $ENV{PTKDB_RESTART_STATE_FILE} will
    # be set if Restart was requested because it is set before the
    # exec that reexecuted us that got us here.
    if ( $ENV{PTKDB_RESTART_STATE_FILE} && $Devel::ptkdb::DataDumperAvailable && -e $ENV{PTKDB_RESTART_STATE_FILE} ) {

	# Restore expressions and breakpoints in state file
	$DB::window->restoreStateFile($ENV{PTKDB_RESTART_STATE_FILE});

	# delete state file
	if (unlink($ENV{PTKDB_RESTART_STATE_FILE}) != 1 ) {
	    consoleSay("Failed to delete restart file '$ENV{PTKDB_RESTART_STATE_FILE}'.");
	}

	$ENV{PTKDB_RESTART_STATE_FILE} = q(); # clear entry
    } else {
	$DB::restoreState->($fname) if $Devel::ptkdb::DataDumperAvailable;
    }
}

sub restoreState {
    my ($fname) = @_;
    my ($stateFile, $files, $expr_list, $eval_saved_text, $main_win_geometry, $restoreName);

    $stateFile = makeFileSaveName($fname);

    if ( -e $stateFile && -r $stateFile ) {
	($files, $expr_list, $eval_saved_text, $main_win_geometry) = $DB::window->load_state($stateFile);
	&DB::restore_breakpoints_from_save($files);
	$DB::window->{expr_list} = $expr_list if defined $expr_list;
	$DB::window->{eval_saved_text} = $eval_saved_text;

	if ( $main_win_geometry ) {
	    # restore the height and width of the window
	    $DB::window->{main_window}->geometry($main_win_geometry);
	}
    }
}

sub makeFileSaveName {
    my ($fname) = @_;
    my $saveName = $fname;
    $saveName =~ s/.p[lm]$/.ptkdb/;
    $saveName .= '.ptkdb' if ($saveName!~ m/\.ptkdb$/);
    return $saveName;
}

sub stateToString {
    my ($fname) = @_;
    my ($files, $d, $saveStr);

    $files = DB::breakpoints_to_save();
    my $win = $DB::window;
    # TODO - 2013-03-21 - If you don't move the window, geometry is never
    # updated from the default (800x600.0+0) and restarts will fly to
    # the upper left.

    my $main_win_geometry = $win->{main_window}->geometry();
    my $eval_saved_text = ( exists $win->{eval_window}
			    ? $win->{eval_text}->get('0.0', 'end')
			    : $win->{eval_saved_text} );
    $d = Data::Dumper->new( [ $files, $DB::window->{expr_list}, $eval_saved_text, $main_win_geometry ],
			    [ 'files', 'expr_list', 'eval_saved_text', 'main_win_geometry' ]);
    $d->Purity(1);
    $d->Indent(1);
    $d->Terse(0);
    return($d->$dumpFunc());
}

sub SaveState {
    my $saveSub = sub {
	eval {
	    $DB::window->saveStateFile($Devel::ptkdb::promptString);
	};
	$DB::window->DoAlert($@) if ($@);
    };
    $DB::window->simplePromptBox('Save Config?', makeFileSaveName($DB::startupFname), $saveSub);
}

sub ShowState {
    my $x = DB::stateToString();
    $DB::window->simpleInfoBox(qq(Config file contents), $x);
}

sub RestoreState {
    my $restoreSub = sub {
	$DB::window->restoreStateFile($Devel::ptkdb::promptString);
    };
    $DB::window->simplePromptBox('Restore Config?', makeFileSaveName($DB::startupFname), $restoreSub);
}

sub SetStepOverBreakPoint {
    my ($offset) = @_;
    $DB::step_over_depth = $DB::subroutine_depth + ($offset ? $offset : 0);
}

#
# NOTE: It may be logical and somewhat more economical in terms of
# lines of code to set $DB::step_over_depth_saved when we enter the
# subroutine, but this gets called for EVERY callable line of code in
# a program that is being debugged, so we try to save every line of
# execution that we can.
#
sub isBreakPoint {
    my ($fname, $line, $package) = @_;
    my ($brkPt);

    if ( $DB::single
	 && ($DB::step_over_depth < $DB::subroutine_depth)
	 && ($DB::step_over_depth > 0)
	 && !$DB::on) {
	$DB::single = 0;
	return 0;
    }

    # doing a step over/in
    if ( $DB::single || $DB::signal ) {
	$DB::single = 0;
	$DB::signal = 0;
	$DB::subroutine_depth = $DB::subroutine_depth;
	return 1;
    }

    # 1st Check to see if there is even a breakpoint there.
    # 2nd If there is a breakpoint check to see if its check box control is 'on'.
    # 3rd If there is any kind of expression, evaluate it and see if it's true.
    $brkPt = DB::getdbline($fname, $line);
    return 0 if( !$brkPt || !$brkPt->{value} || !breakPointEvalExpr($brkPt, $package) );
    DB::cleardbline($fname, $line) if( $brkPt->{type} eq 'temp' );
    $DB::subroutine_depth = $DB::subroutine_depth;
    return 1;
}

#
# Check the breakpoint expression to see if it
# is true.
#
sub breakPointEvalExpr {
    my ($brkPt, $package) = @_;
    my (@result);

    return 1 unless $brkPt->{expr}; # return if there is no expression

    no strict;
    @result = DB::dbeval($package, $brkPt->{expr});
    use strict;

    $DB::window->DoAlert($@) if $@;

    return $result[0] or @result;
    # We could have a case where the 1st element is undefined but
    # subsequent elements are defined.
}

# Evaluate the given expression, return the result.  MUST BE CALLED
# from within DB::DB in order for it to properly interpret the vars.
sub dbeval {
    my ($ptkdb__package, $ptkdb__expr) = @_;
    my (@ptkdb__result, $ptkdb__str);
    my (@ptkdb_args);
    local($^W) = 0; # temporarily turn off warnings

    no strict;
    # This substitution is done so that we return HASH, as opposed to
    # an ARRAY.  An expression of %hash results in a list of key/value
    # pairs.
    $ptkdb__expr =~ s/^\s*%/\\%/o;

    @_ = @DB::saved_args; # replace @_ arg array with what we came in with

    @ptkdb__result = eval <<__EVAL__;
 \$\@ = \ $DB::save_err;
 package $ptkdb_package;
 $ptkdb__expr;
__EVAL__

    @ptkdb__result = ("ERROR ($@)") if $@;
    use strict;

    return @ptkdb__result;
}

# Call back we give to our 'quit' button and binding to the
# WM_DELETE_WINDOW protocol to quit the debugger.
sub dbexit {
    exit;
}

# This is the primary entry point for the debugger. When a perl
# program is parsed with the -d(in our case -d:ptkdb) option set the
# parser will insert a call to DB::DB in front of every excecutable
# statement.

# Refs: Proga#ing Perl 2nd Edition, Larry Wall, O'Reilly & Associates, Chapter 8

# Since Perl 5.8.0 we need to predeclare the sub DB() at the start of
# the package or else the compilation fails. We need to disable
# warnings though since in 5.6.x we get warnings on the sub DB begin
# redeclared. Using local($^W) = 0 will leave warnings disabled for
# the rest of the compile and we don't want that.
my ($saveW);
sub BEGIN {
    $saveW = $^W;
    $^W = 0;
}

no strict;
sub DB {
    @DB::saved_args = @_; # save arg context
    $DB::save_err = $@; # save value of $@
    my ($package, $filename, $line) = caller;
    my ($stop, $alt);

    $^W = $saveW;
    unless( $DB::ptkdb::isInitialized ) {
	return if( $filename ne $0 ); # not in our target file
	DB::Initialize($filename);
    }

    if ( ! isBreakPoint($filename, $line, $package) ) {
	$DB::single = 0;
	$@ = $DB::save_err;
	return;
    }

    if ( !$DB::window ) { # not setup yet
	$@ = $DB::save_err;
	return;
    }

    $DB::window->setup_main_window() unless $DB::window->{main_window};

    $DB::window->EnterActions();

    my ($saveP);
    $saveP = $^P;
    $^P = 0;

    $DB::on = 1;

    # The user can specify this variable in one of the startup files,
    # this will make the debugger run right after startup without the
    # user having to press the 'run' button.
    if ( $DB::no_stop_at_start ) {
	$DB::no_stop_at_start = 0;
	$DB::on = 0;
	$@ = $DB::save_err;
	return;
    }

    if ( !$DB::sigint_disable ) {
	$SIG{INT} = $DB::dbint_handler_save if $DB::dbint_handler_save; # restore original signal handler
	$SIG{INT} = 'DB::dbexit' unless $DB::dbint_handler_save;
    }

    #$DB::window->{main_window}->raise(); # Bring us to the top make sure OUR event loop runs.
    $DB::window->{main_window}->focus();

    $DB::window->set_file($filename, $line);

    # Refresh the exprs to see if anything has changed.
    updateExprs($Package);

    # Update subs Page if necessary.
    $cnt = scalar keys %DB::sub;
    if ( $cnt != $DB::window->{subs_list_cnt} && $DB::window->{subs_page_activated} ) {
	$DB::window->fill_subs_page();
	$DB::window->{subs_list_cnt} = $cnt;
    }

    # Update the subroutine stack menu.
    $DB::window->refresh_stack_menu();

    # Force all breakpoints to appear in the breakpoints tab first
    # time around, even before we get to the corrsponding file.
    if(not $bkrPtsInitiallyLoaded) {
	my $brkPts = DB::breakpoints_to_save();
	my %files = map { $_->{fname} => 1 } map {@{$_}} values( %{$brkPts} );
	for (keys %files ) {
	    $DB::window->reinsertBreakpoints($_);
	}
	$bkrPtsInitiallyLoaded = 1;
    }

    # Clear for takeoff.
    $DB::window->{run_flag} = 1;

    my ($evt, @result, $r);

    # We pass in the function name so that we keep track of whether
    # this function called DB::ShowFixedLostBreakpoints() before and
    # skip repeating.
    DB::ShowFixedLostBreakpoints('Initialize');

    for (;; ) {
	# We wait here for something to do.
	$evt = $DB::window->main_loop();
	last if( $evt eq 'step');
	$DB::single = 0 if ($evt eq 'run');

	if ($evt eq 'balloon_eval') {
	    $DB::window->code_motion_eval(DB::dbeval($package, $DB::window->{balloon_expr}));
	    next;
	}

	if ( $evt eq 'qexpr') {
	    my $str;
	    @result = DB::dbeval($package, $DB::window->{qexpr});
	    $DB::window->{quick_entry}->delete(0, 'end'); # clear old text
	    if (exists $DB::window->{quick_dumper}) {
		$DB::window->{quick_dumper}->Reset();
		$DB::window->{quick_dumper}->Values( [ $#result == 0 ? @result : \@result ] );
		$str = $DB::window->{quick_dumper}->$dumpFunc();
	    } else {
		$str = "@result";
	    }
	    $DB::window->{quick_entry}->insert(0, $str); #enter the text
	    $DB::window->{quick_entry}->selectionRange(0, 'end'); # select it
	    $evt = 'update'; # force an update on the expressions
	}

	if ( $evt eq 'expr') {
	    # Append the new expression to the list but first check to
	    # make sure that we don't already have it.
	    if ( grep {$_->{expr} eq $DB::window->{expr}} @{$DB::window->{expr_list}} ) {
		$DB::window->DoAlert("$DB::window->{expr} is already listed");
		next;
	    }

	    @result = DB::dbeval($package, $DB::window->{expr});
	    my $result;
	    if ( @result == 1 ) {
		$result = $result[0];
	    } else {
		$results = \@result;
	    }
	    $r = $DB::window->insertExpr([ $result ],
					 $DB::window->{data_list},
					 $result,
					 $DB::window->{expr},
					 $Devel::ptkdb::expr_depth);

	    # $r will be 1 if the expression was added succesfully, 0
	    # if not, and it if wasn't added sucessfully it won't be
	    # re-eval'ed the next time through.
	    push @{$DB::window->{expr_list}}, { 'expr' => $DB::window->{expr},
						'depth' => $Devel::ptkdb::expr_depth } if $r;
	    next;
	}
	if ( $evt eq 'update') {
	    updateExprs($package);
	    next;
	}
	if ( $evt eq 'reeval') {
	    # Re-eval the contents of the expression eval window.
	    my $txt = $DB::window->{eval_text}->get('0.0', 'end');
	    my @result = DB::dbeval($package, $txt);
	    $DB::window->updateEvalWindow(@result);
	    next;
	}
	last;
    }
    $^P = $saveP;
    $SIG{INT} = 'DB::dbint_handler' unless $DB::sigint_disable; # set our signal handler

    $DB::window->LeaveActions();

    $@ = $DB::save_err;
    $DB::on = 0;
}

# In this case we do not use local($^W) since we would like warnings
# to be issued past this point, and the localized copy of $^W will not
# go out of scope until the end of compilation.

# This is another place where we will try to keep the code as 'lite'
# as possible to prevent the debugger from slowing down the user's
# application.

# When a perl program is parsed with the -d option (in our case,
# -d:ptkdb), the parser will route all subroutine calls through here,
# setting $DB::sub to the name of the subroutine to be called, leaving
# it to the debugger to make the actual subroutine call and do any pre
# or post processing it may need to do. We take the opportunity to
# track the depth of the call stack so that we can update our 'Stack'
# menu when we stop.

# Refs: Proga#ing Perl 2nd Edition, Larry Wall, O'Reilly & Associates, Chapter 8

sub sub {
    # These are initialized to undef and an empty array.
    my ($result, @result);

    #
    # See NOTES(1)
    #
    $DB::subroutine_depth += 1 unless $DB::on;
    $DB::single = 0 if ( ($DB::step_over_depth < $DB::subroutine_depth) && ($DB::step_over_depth >= 0) && !$DB::on);

    # Yes, there's a lot of repeating code here, but because of he
    # calling sequence and the need to handle explicit different
    # return contexts, there's really no efficient way of factoring it
    # out.  In the end you'll only save typing, not execution time.
    if ( wantarray ) {
	# array context
	no strict; # otherwise perl gripes about calling the sub by the reference
	@result = $DB::sub->(); ## was &$DB::sub->(); # call the subroutine by name
	use strict;
	$DB::subroutine_depth -= 1 unless $DB::on;
	$DB::single = 1 if ($DB::step_over_depth >= $DB::subroutine_depth && !$DB::on);
	return @result;
    } elsif (defined wantarray) {
	# scalar context
	no strict;
	$result = &$DB::sub;
	use strict;
	$DB::subroutine_depth -= 1 unless $DB::on;
	$DB::single = 1 if ($DB::step_over_depth >= $DB::subroutine_depth && !$DB::on);
	return $result;
    } else {
	# void context
	no strict;
	&$DB::sub;
	use strict;
	$DB::subroutine_depth -= 1 unless $DB::on;
	$DB::single = 1 if ($DB::step_over_depth >= $DB::subroutine_depth && !$DB::on);
	return $result;
       }
}

1; # return true value

=headl NAME

Devel::ptkdb - Perl debugger using a Tk GUI

=headl DESCRIPTION

ptkdb is an interactive debugger for Perl that uses Tk for a user interface.

Features include:

 Hot Variable Inspection
 Breakpoint Control Panel
 Expression List
 Subroutine Tree

=head l SYNOPSIS

To debug a script using ptkdb:

 perl -d:ptkdb myscript.pl

You will be presented with a multi-paned window in which you examine
code, examine variables, set breakpoints. We will describe the
functionality of the debugger in terms of these panes.

=head l Code Pane

=over 4

The Code Pane appears on the left-hand side of the display. By
default, it contains the line of code that is about to be executed
with a number of lines of context before and after. You can scrollthe
pane up and down to examine other parts of the code in the current
code file and you can jump to other modules that are loaded into the
current process. Executing a Step command will return you to the
current block of code after executing the next line of code. You may
also relocate yourself by accessing the >>>>>Stack.

Line numbers are presented on the left side of the window. Code lines
that are not breakable will have a strikethrough over the line
number. POD lines are not visible because they have been parsed away
by execution time, but line numbers are maintined with the original
source; you'll see large block of POD as empty unbreakable lines.

Clicking on a bare line number will insert a breakpoint on that line
and change the line number background color to $ENV{PTKDB_BRKPT_COLOR}
(defaults to Red). Clicking on the number again will remove the
breakpoint. Shift-Click on the enabled breakpoint will disable it; it
will be present but not cause a stop when the line is reached during
executions. Disabled breakpoints have a background color of
$ENV{PTKDB_DISABLEDBRKPT_COLOR} (defaults to Green).

=item Cursor Motion

If you hover the cursor over a simple variable in the code pane
(i.e. $myVar, @myVar, or %myVar), the debugger will evaluate the
current value of the variable and pop a balloon up with the evaluated
result. If L<Data::Dumper> (standard with Perl 5.00502 and later) is
available, it will be used to format the result.

If you highlight text in the code pane, the selected text will be
evaluated. This is useful when trying to evaluate parts of a larger
structure such as $array[$i] for example; the default parsing in the
Tk widget will see characters such as [,],{,} as work boundaries and
not extend the default highlight over them.

NOTE: Because of the way the Code Pane widget works, you cannot extend
a highlight over multiple lines; the line numbers will be included in
the statement to be evaluated. A future enhancement may be able to
eliminate this problem.

=back

=head l Notebook Pane

The Notebook Pane is on the right side of the display and contains
multiple fields and tabs that are used to control program execution
and examine program state.

over 4

=item Exprs

This is a list of expressions that are evaluated each time the
debugger stops. Non-scalar expresssions (arrays and hashes) are
presented heirarchically.

Double clicking on such an expression will cause it to collapse;
double clicking again will cause the expression to expand.

Top level expressions can be dragged to reorder them; if they are
expanded when selected, they will close while you move them and reopen
when you drop them. You cannot drag around parts of data structures
such as array elements or hash entries.

Expressions are entered through B<Enter Expr> entry field, or by
Alt-##>>>>> when text is selected in the code pane. The B<Quick Expr>
entry will take an expression and evaluate it in program
userspace. The result is also transfered to the 'clipboard' for
pasting.

=item Subs

Displays a list of all the packages invoked with the script, sorted
alphabetically, case insensitively, with the 'main' package pinned to
the top of the list.

Packages can be expanded using the '+' indicators until you reach the
functions in the package. Double clicking a function takes you to its
code which is displayed in the Code Pane.

=item BrkPts

Presents a list of the breakpoints currently in use. The pushbutton
allows a breakpoint to be 'disabled' without removing it, which turns
it's line number background Green in the code pane; see he Code Pane
section above for more details.

Expressions can be applied to the breakpoint by entering the
expression in the field below the breakpoint control. The expression
is consider 'entered' when the cursor leaves the entry field.

The expression is evaluated each time the line of code is reached,
before the line is executed. If the expression evaluates to be Perl
'true', the debugger will stop the script at that line. Pressing the
'Goto' button will set the text pane to the file and line where the
breakpoint is set. Pressing the 'Delete' button will delete the
breakpoint.

=back

=headl Menus

=head2 File Menu

over 4

=item About

Presents a dialog box telling you about the version of ptkdb. It
recovers your OS name, version of perl, version of Tk, and some other
information.

=item Open

Presents a list of files that are part of the invoked Perl script and
its loaded modules if they can be located on disk in the current
environment. Selecting a file from this list will present this file in
the Code Pane.

=item Save Config

Requires L<Data::Dumper>. Prompts for a filename into which to save
the current debugger configuration. Saves the breakpoints,
expressions, eval text and window geometry. If the default name is
used as presented, then next time the script is invoked, this
configuration will be reloaded automatically.

=item Show Config

Requires L<Data::Dumper>. Shows the breakpoints, expressions, eval
text and window geometry that are currently in effect and that would
be saved.

=item Restore Config

Requires L<Data::Dumper>. Prompts for a filename to restore a
configuration saved with the 'Save Config' menu item. If breakpoints
in the configuration are not found (by line number and exact text),
the code is searched for the original text 20 lines before and after
the line number of the breakpoint. If not found, the breakpoint is
placed on the first breakable line before the original location, up to
a limit of 20 lines. If all of that fails, the breakpoint is
dropped. If any breakpoint is moved or dropped, a dialog box
describing these actions is presented. All of this breakpoint checking
an notification is also applied to the default set of breakpoints
loaded at program start.

=item Show Fixed/Lost Breakpoints

Pops up the dialog from the last config file restore, if there were
any issues.

=item Goto Line

Prompts for a line number. Pressing the 'Okay' button sends the cursor
to the given line number in the Code Pane.

=item Find Text

Prompts for text to search for in the the current contents of the code
pane. Options include forward, backward, regular expression and case
insensitive (caseless) searching.

=item Tabs

Enter a space separated list of column numbers at which to set tabs in
the current text. B<NOTE:>>>>>> Unlcear if this is working.

=item Close Window and Run

Undisplays the debugger window and runs the program until it ends or
hits a breakpoint. If the latter, the window is restored and stopped
at the breakpoint.

=item Quit

Causes the debugger and the target script to exit. Does not save the
configuration (expressions and breakpoints) but does save bookmarks,
if altered. ##>>>>> SHOULD PROMPT TO SAVE

=back

=head2 Control Menu

=over

=item Run

The debugger allows the script to run to the next breakpoint or until
the script exits.

=item Run To

Runs the debugger until it comes to wherever the insertion cursor is
currently placed in the Code Pane.

=item Set Breakpoint

Sets a breakpoint on the line at the current insertion cursor
location.

=item Clear Breakpoint

Deletes the breakpoint at the current insertion cursor location, if
one exists.

=item Clear All Breakpoints

Deletes all existing breakpoints.

=item Step Over

Causes the debugger to execute the current line. If the line is a
subroutine call, it executes the call, stopping at the first
executable line after the call.

=item Step In

Causes the debugger to step into the subroutine on the current line
and stop at the first executable statement within.  If there is no
subroutine on the current line, the call is executed as a Step
Over. If there are multiple subroutines on the current line, each one
is stepped into or over in turn as Step Into or Step Over is pressed.

=item Return

Runs the script until it returns from the currently executing
subroutine. The debugger does not stop at the return itself; it stops
at the next executable statement after the subroutine exits.

=item Restart

Saves the current breakpoints and expressions in a temporary config
file and restarts the script from the beginning.

And we mean 'beginning':

When the debugger starts, it captures:

- the current directory

- the contents of %ENV

- the original command line, including the arguments to Perl itself

When restarting:

- We return to the original directory.

- We replace the current contents of %ENV with the original contents.

- We set $ENV{PTKDB_RESTART_STATE_FILE} to the name of the temporary
breakpoint/expression file. (This is the only member of %ENV that will
differ from restart to restart.)

- We exec the orginal command line.

B<Caveat Programer #1>: If, for some bizarre reason, you want to restart
with a different environment, you are free to alter any or all of the
following before you restart:

- $Devel::ptkdb::restart::dir, where the startup directory is stored

- %Devel::ptkdb::restart::ENV, where the original environment is stored

- @Devel::ptkdb::restart::cmd, where the original command is stored

B<Caveat Programmer #2>: This feature will not work properly with
debugging of CGI scripts. This is, however, an old comment, so it may
not be true today.

=item Stop On Warning

When C<-w> is enabled the debugger can stop when warnings such as,
'Use of uninitialized value at undef_warn.pl line N' are encountered,
The debugger will stop on the NEXT line of execution since the error
can't be detected until the current line has executed.

This feature can be turned on at startup by adding:

 $DB::ptkdb::stop_on_warning = 1;

to a .ptkdbrc file.

=back

=head2 Data Menu

=over 4

=item Enter Expression

When an expression is entered in the 'Enter Expr:' text box, hitting
return will enter the expression into the expression list. Each time
the debugger stops, this expression will be evaluated and its result
updated in the list window.

=item Delete Expression

Deletes the highlighted expression in the expression window.

=item Delete All Expressions

Delete all expressions in the expression window.

=item Expression Eval Window

Pops up a two pane window. Expressions of virtually unlimitted length
can be entered in the top pane. Pressing the 'Eval' button will cause
the expression to be evaluated and its placed in the lower pane. If
L<Data::Dumper> is available it will be used to format the resulting
text. Undo is enabled for the text in the upper pane.

=item Use Data::Dumper for Eval Window >>>>>

Enables or disables the use of L<Data::Dumper> for formatting the results
of expressions in the Eval window.

=back

=head2 Stack Menu

Presents the current list of subroutines being executed. Selecting an
item from this menu will set the text in the code window to that
particular subroutine entry point. It will not, however, cause the
expressions in the expression pane to change to what their values
would be in the scope of this stack frame.

=head2 Bookmarks Menu

Maintains a list of bookmarks. The bookmarks are saved in the file
'/.ptkdb_bookmarks'. >>>>> shouldn't there be one per program ike ptkdbrtc?

=over 4

=item Add Bookmark

Adds a bookmark to the bookmark list. The list is sorted by file and
then line number over multiple bookmarks in the same file.

=item Edit Bookmarks

Allows you to delete a bookmark from the bookmark list.

=item The Bookmarks

Bookmarks are listed after the Edit Bookmarks menu entry.

=back

=head2 Windows

Presents menu items that move focus to either the code window, quick
entry or expression fields. This menu item is more of a placeholder to
allow keystroke binding to navigate between windows. See the menu
entry for the key bindings.

=head1 Options

Here is a list of the current active XResources options. Several of
these can be overridden with environmental variables. Resources can be
added to .Xresources or .Xdefaults depending on your X configuration.
To enable these resources you must either restart your X server or use
the xrdb -override resFile command. xfontsel can be used to select
fonts.

 /*
  * Perl Tk Debugger XResources,
  * Note: These resources are subject to change.
  */

 /*
  * Set Value to se to place scrollbars on the right side of windows.
  * 'sw' puts scrollbars on bottom and left
  * 'se' puts scrollbars on bottom and right
  */
 ptkdb*scrollbars: sw

 /*
  * controls where the code pane is oriented
  * values can be set to left, right, top, bottom
  */
 ptkdb*codeside: left

 /*
  * Background color for the balloon
  * CAUTION: For certain versions of Tk trailing
  * characters after the color produces an error
 */
 ptkdb.frame2.framel.rotext.balloon.background: green
 ptkdb.frame2.framel.rotext.balloon.font: fixed
 /* Colors and fonts for the major GUI elements */
 ptkdb.frame*font: fixed /* Subroutine Notebook Page */
 ptkdb.frame.menubutton.font: fixed /* Delete Breakpoint Buttons */
 ptkdb.frame2.framel.rotext.font: fixed /* Breakpoint Expression Entries */
 ptkdb.notebook.datapage.framel.playlist.font: fixed

 ptkdb.notebook.subspage.font: fixed /* Breakpoint Expression Entries */
 ptkdb.notebook.brkptspage.entry.font: fixed /* Breakpoint Checkbuttons */
 ptkdb.notebook.brkptspage*button.font: fixed /* Breakpoint Checkbuttons */
 ptkdb.notebook.brkptspage•buttonl.font: fixed /* Eval Expression Entry Window */
 ptkdb.notebook.brkptspage.checkbutton.font: fixed /* Eval Expression Results Window */
 ptkdb.notebook.brkptspage*label.font: fixed /* "Eval" Button */

 ptkdb.toplevel.frame.textundo.font: fixed /* "Clear Eval" Button */
 ptkdb.toplevel.framel.text.font: fixed /* "Clear Results" Button */
 ptkdb.toplevel.button.font: fixed /* "Clear Dismiss" Button */
 ptkdb.toplevel.buttonl.font: fixed /* Menu Bar */
 ptkdb.toplevel.button2.font: fixed /* File menu */
 ptkdb.toplevel.button3.font: fixed /* Code Pane */

 /*
  * Background color for where the debugger has stopped
  */
 ptkdb*stopcolor: blue

 /*
  * Background color for set breakpoints
  */
 ptkdb*breaktagcolor*background: yellow
 ptkdb*disabledbreaktagcolor*background: white

 /*
  * Font for where the debugger has stopped
  */
 ptkdb.stopFont: -*-fixed-bold-*-*-*-*-*-*-*-*-*-*-*

 /*
  * Background color for the search tag
  */
 ptkdb*searchtagcolor: green

=headl Environment Variables

=over 4

=item PIKDB_BRKPT_COLOR

Sets the background color of a set breakpoint. Overrides the Xresource
I<ptkdb*breaktagcolor*background>.

=item PTKDB_DISABLEDBRKPT_COLOR

Sets the background color of a disabled breakpoint. Overrides the
Xresource I<ptkdb*disabledbreaktagcolor*background>.

=item PTKDB_CODE_FONT

Sets the font of the Text in the code pane. Overrides the Xresource
I<ptkdb*codeside>.

=item PTKDB_CODE_SIDE

Sets which side the code pane is packed onto. Defaults to 'left'.
Can be set to 'left', 'right', 'top', 'bottom'. Overrides the
Xresource I<ptkdb*codeside>.

=item PTKDB_EXPRESSION_FONT

Sets the font used in the expression notebook page. Overrides the
Xresource I<ptkdb*notebook*datapage*frame*playlist*font>.

=item PTKDB_EVAL_FONT

Sets the font used in the Expression Eval Window.

=item PTKDB_EVAL_DUMP_INDENT

Sets the value used for L<Data::Dumper> 'indent' setting.

=item PTKDB_BUTTON_FONT

Sets the font of the buttons. Overrides the Xresources
>>>>>>>>I<ptkdb.toplevel. (button I button[ 123] ) . f ont> .

=item PTKDB_STOP_TAG_FONT

Sets the font for the statement where the debugger is currently stopped.

=item PTKDB_SCROLLBARS_ONRIGHT

A non-zero value sets the scrollbars of all windows to be on the right
side of the window. Useful for MS Windows users using ptkdb in an
XWindows environment. Overrides the Xresource I<ptkdb*scrollbars>.

=item PTKDB_LINENUMBER_FORMAT

Sets the format of line numbers on the left side of the window.
Default value is %05d. Useful if you have a script that contains more
than 99999 lines.

=item PTKDB_DISPLAY

Sets the X display that the ptkdb window will appear on when invoked.
Useful for debugging CGI scripts on remote systems.

=item PTKDB_BOOKMARKS_PATH

Sets the path of the bookmarks file. Default is
I<$ENV{HOME}/.ptkdb_bookmarks>.

=item PTKDB_STOP_TAG_COLOR

Sets the color that highlights the line where the debugger is
stopped. Overrides the Xresource I<ptkdb*stopcolor>.

=item PTKDB_SIGINT_DISABLE

When set to 1, disables the interrupt hander from processing CTRL-C.

=item PTKDB_GEOMETRY

Specify the initial geometry for the debugger window. Use the standard form

 width*height+x+y

with all values in integer points, x and y being zero anchored at the
upper left of the display. If specified, overrules all other geometry
environment variable described below.

=item PTKDB_START_WIDTH_POINTS

Starting width for the debugger window in points. If set, overrides
PTKDB_START_WIDTH_RATIO and PTKDB_START_SIZE_RATIO.

=item PTKDB_START_WIDTH_RATIO

A value between 0 and 1, the window's width be set to this portion of
the total screen width. If set, overrides PTKDB_START_SIZE_RATIO.

=item PTKDB_START_HEIGHT_POINTS

Starting height for the debugger window in points. If set, overrides
PTKDB_START_HEIGHT_RATIO and PTKDB_START_SIZE_RATIO.

=item PTKDB_START_HEIGHT_RATIO

A value between 0 and 1, the window's height be set to this portion of
the total screen height. If set, overrides PTKDB_START_SIZE_RATIO.

=item PTKDB_START_SIZE_RATIO

A value between 0 and 1, the window's width and height will each be
set to this portion of the total screen width and height. A value of
.5 on an 1280 x 1024 display will yield an app window that is 640 x
512.

=back

=headl FILES

=head2 .ptkdbrc

If this file is present in >>>>>~/ or in the current directory when perl is
invoked, the file will be read and executed as a Perl script before
the debugger makes its initial stop at startup. There are several API
calls that can be used with such scripts, described below. There is
also the variable C<$DB::no_stop_at_start> that may be set to non-zero
to prevent the debugger from stopping at the first line of the script.
This is useful for debugging CGI scripts.

There is a system ptkdbrc file in

$PREFIX/lib/per15/$VERS/Devel/ptkdbrc

over 4

=item brkpt($fname, @lines)

Sets breakpoints in the file $fname on each line number in @lines.

$fname is the complete path to the Perl file/module. Line numbers
start from 1. A warning message is generated if a line is not
breakable.

=item condbrkpt($fname, @($line, $expr) )

Sets conditional breakpoints in the file $fname on each line number in
@lines, $fname is the complete path to the Perl file/module. Line
numbers start from 1. A warning message is generated if a line is not
breakable. B<NOTE>: The validity of the expression on a line will not
be determined until execution of that particular line.

=item brkonsub(@names)

Sets a breakpoint on each subroutine name listed. A warning message is
generated if a subroutine does not exist. B<NOTE>: For a script with
no other packages, the default package is "main::" and the subroutines
would be "main::mySubs".

=item brkonsub_regex(@regexprs)

Uses the list of @regexprs as a list of regular expressions to set
breakpoints. Sets breakpoints on every subroutine that matches any of
the listed regular expressions.

=item textTagConfigure(tag, ?option?, ?value?)

Allows the user to format the text in the code window. The option
value pairs are the same values as the option for the tagConfigure
method documented in Tk::Text. Currently, the following tags are in
effect:

 'code'
    Format for code in the text pane

 'stoppt'
    Format applied to the line where the debugger is currently stopped

 'breakableLine'
    Format applied to line numbers where the code is 'breakable'

 'nonbreakableLine'
    Format applied to line numbers where the code is no breakable

 'breaksetLine.
    Format applied to line numbers were a breakpoint is set

 'breakdisabledLine'
    Format applied to line numbers were a disabled breakpoint is set

 'search_tag'
    Format applied to text when located by a search.

Example:

 # Turns off the overstrike on lines that you can't set a
 # breakpoint on and makes the text color yellow.

 textTagConfigure('nonbreakableLine', -overstrike => 0, -foreground => "yellow");

=item add_exprs(@exprList)

Add a list of expressions to the 'Exprs' window. B<NOTE:> Use the
single quote character to prevent the expression from being
"evaluated" in the string context.

Example:

 # Adds the $_ and @_ expressions to the active list

 add_exprs( '$_', '@_' );

=back

=headl NOTES

=head2 Debugging Other PerlTk Applications

ptkdb can be used to debug other PerlTk applications if some cautions
are observed. Basically, do not click the mouse in the application's
window(s) when you've entered the debugger and do not click in the
debugger's window(s) while the application is running. Doing either
one is not necessarily fatal, but it can confuse things and produce
unexpected results.

Be aware that most PerlTk applications have a central event loop.
User actions, such as mouse clicks, key presses, window exposures,
etc., will generate 'events' that the script will process. When a
PerlTk application is running, its 'MainLoop' call will accept these
events and then dispatch them to appropriate callbacks associated with
the appropriate widgets.

ptkdb has its own event loop that runs whenever you've stopped at a
breakpoint and entered the debugger. However, it can accept events
that are generated by other PerlTk windows and dispatch their
callbacks. The problem here is that the application is supposed to be
'stopped', and logically the application should not be able to process
events.

Hopefully, a future version of ptkdb will have an extension that will
'filter' events so that application events are not processed while the
debugger is active, and debugger events will not be processed while
the target script is active.

=head2 Debugging CGI Scripts

One advantage of ptkdb over the builtin debugger (-d) is that it can be
used to debug CGI perl scripts as they run on a web server. Be sure
that that your web server's Perl installation includes Tk.

Change your

 #!/usr/local/bin/perl

to

 #!/usr/local/bin/perl -d:ptkdb

assuming your Perl lives in /usr/local/bin; otherwise modify as
appropriate.

B<TIP>: You can debug scripts remotely if you're using a UNIX based
Xserver and the computer when you are running an Xserver. The Xserver
can be another UNIX workstation, a Macintosh or Win32 platform with an
appropriate XWindows package. In your script, insert the following
BEGIN subroutine:

 sub BEGIN {
     $ENV{DISPLAY} = "myHostname:0.0%
 }

Be sure that your web server has permission to open windows on your
Xserver (see the xhost manpage).

Access your web page with your browswer and 'submit' the script as
normal. The ptkdb window should appear on myHostname's monitor. At
this point you can start debugging your script. Be aware that your
browser may timeout waiting for the script to run.

To expedite debugging you may want to setup your breakpoints in
advance with a .ptkdbrc file and use the $DB::no_stop_at_start
variable. B<NOTE:> For debugging web scripts you may have to have the
.ptkdbrc file installed in the server account's home directory (www)
or whatever username your webserver is running under. Also try
installing a .ptkdbrc file in the same directory as the target script.

=headl KNOWN PROBLEMS

=over 4

=item Breakpoint Controls

If the size of the right hand pane is too small, the breakpoint
controls are not visible. The breakpoints are still there, but the
window will have to be enlarged in order for them to be visible.

=item Overzealous Code Highlight Evaluation

Highlighting code in the code pane will cause evaluation for balloon
display. If you drag your cursor across multiple lines, you get
jibberish. Hopefully, you won't accidentally execute some code while
its happening because the execution will fail on the included line
numbers.

=item Restart relocation

If you don't move the window from its starting postion, geometry is
never updated from the default set by the module (800x600+0+0) and
restarts will fly to the upper left.

=back

=head1 AUTHOR

Andrew E. Page, aepage@users.sourceforge.net

=head1 CURRENT MAINTAINER

Matthew Persico, persicom@cpan.org as of Spring 2014.

=headl LICENSE

ptkdb Perl Tk Debugger

Copyright 1998-2013, Andrew E. Page

Copyright 2014-Present, Matthew O. Persico

All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the terms of either:

- the GNU General Public License as published by the Free Software
Foundation; either version 1, or (at your option) any later version

or

- the "Artistic License" which comes with this Kit.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE, See either the
GNU General Public License or the Artistic License for more details,

=head1 BUG REPORTING

Please report bugs through the following URL:

>?>>>>>>>>>>>http://sourceforge.net/tracker/?atid=4376098,group_id=438548,func=browse

=cut


#============================== ptkdb/Playlist.pm ================================================
#!perl -w
#
# Tk::Playlist class - provides winamp-style "playlist" editing capibilities.
#
# By Tyler "Crackerjack" MacDonald <crackerjack@crackerjack.net>
# July 23rd, 2000.
# Package-ified November 25, 2004,
#
# This module is freeware; You may redistribute it under the same terms as
# perl itself.
#

# We have sucked in a hard-coded version instead of using an installed
# copy because we have made some changes specfic to the debugger.
package Playlist;

use strict;
use vars qw($VERSION @ISA);

use Tk;
use Tk::Derived;
use Tk::HList;

$VERSION = '0.02';
@ISA=qw(Tk::Derived Tk::HList);

Construct Tk::Widget 'Playlist';

sub Tk::Widget::ScrolledPlaylist {shift->Scrolled('Playlist'=>@_);}

return 1;

sub Classlnit {
    my ($class,$mw)=@_;

    $mw->eventAdd('<<Toggle>>' => '<Control-ButtonPress-1>');
    $mw->eventAdd('<<RangeSelect>>' => '<Shift-ButtonPress-1>');
    $mw->eventAdd('<<MoveEntries>>' => '<B1-Motion>');
    $mw->eventAdd('<<InverseSelect>>' => '<Control-ButtonPress-2>');
    $mw->eventAdd('<<SingleSelect>>' => '<ButtonPress-1>');
    $mw->eventAdd('<<EndMovement>>' => '<ButtonRelease-1>');
    $mw->eventAdd('<<Delete>>' => '<Key-Delete>');

    # Hlist
    $mw->eventAdd('<<DoubleSelect>>' => '<Double-ButtonPress-1>');

    $mw->bind($class, '<<Toggle>>', [ 'Toggle' ]);
    $mw->bind($class, '<<SingleSelect>>', [ 'SingleSelect' ]);
    $mw->bind($class, '<<RangeSelect>>', [ 'RangeSelect' ]);
    $mw->bind($class, '<<MoveEntries>>', [ 'MoveEntries' ]);
    $mw->bind($class, '<<EndMovement>>', [ 'EndMovement' ]);
    $mw->bind($class, '<<Delete>>', [ 'Delete' ]);

    # Hlist
    $mw->bind($class, '<<DoubleSelect>>' ,[ 'DoubleSelect']);

    #>>>>>>>>> $class->SUPER::ClassInit($mw);
}

# Hlist
sub DoubleSelect {
    my $w = shift;
    my $Ev = $w->XEvent;

    delete $w->{'shiftanchor'};

    my $ent = $w->GetNearest($Ev->y, 1);

    return unless (defined($ent) and length($ent));

    $w->anchorSet($ent)
      unless(defined $w->info('anchor'));

    $w->selectionSet($ent);

    $w->Callback(-command => $ent);
}

sub Populate {
    my ($cw,$args)=@_;
    my $f;

    $cw->ConfigSpecs('-style'=>[ 'PASSIVE', undef, undef, undef]);
    $cw->ConfigSpecs('-readonly'=>[ 'METHOD', undef, undef, undef]);
    $cw->ConfigSpecs('-callback_change'=>[ 'METHOD', undef, undef, undef]);

    $cw->SUPER::Populate($args);
}

sub Delete {
    my ($cw)=@_;

    return if ($cw->{'readonly'});

    my @is=$cw->infoSelection();
    grep {$cw->deleteEntry($_)} @is;

    if ($cw->{'callback_change'}) {
	my ($cmd,@arg);
	if (ref ($cw->{callback_change} ) eq 'ARRAY') {
	    ($cmd,@arg)=@{$cw->{callback_change}};
	}  elsif (ref ($cw->{callback_change}) eq 'CODE') {
	    ($cmd,@arg)=($cw->{callback_change});
	}

	if($cmd) {
	    my $i;
	    foreach $i (@is)  {
		$cmd->($cw,'delete',$i,@arg);
	    }
	}
    }
}

sub InverseSelect {
    my ($w) = @_;
    my (%ic, @is);
    grep {$ic{$_}++} $w->infoSelection();
    @is = grep {!$ic{$_}} $w->infoChildren();
    $w->selectionClear();
    grep($w->selectionSet($_),@is);
}

sub evFindClick {
    my ($w,$Ev)=@_;
    $w->GetNearest($Ev->y, 1);
}

sub findClick {
    $_[0]->evFindClick($_[0]->XEvent);
}

sub EndMovement {
    my ($cw,$args)=@_;
    if ($cw->{moving}) {
	delete($cw->{moving});
	if ($cw->{callback_change}) {
	    my ($cmd, @arg);
	    if (ref($cw->{callback_change}) eq 'ARRAY') {
		($cmd, @arg)=@{$cw->{callback_change}};
	    } elsif (ref($cw->{callback_change}) eq 'CODE') {
		($cmd, @arg)=($cw->{'callback_change'});
	    }
	    if($cmd){
		$cmd->($cw, 'done_moving', @arg);
	    }
	}
    }
}

sub MoveEntries {
    my ($cw,$args)=@_;
    my ($Ev,$yy,$dir,$ent);

    return if ($cw->{readonly});

    $Ev=$cw->XEvent;
    $yy=$Ev->y;
    $ent=$cw->evFindClick($Ev);

    if ( !$cw->{moving}) {
	$cw->{moving}=$yy;
	$cw->{old_ent}=$ent;
    } else {
	if ($cw->{moving}>$yy && $cw->{moving}-10>$yy) {
	    $dir=-1;
	} elsif ($cw->{moving}<$yy && $cw->{moving}*10<$yy) {
	    $dir=1;
	}
	if ($ent && $cw->{old_ent} && $ent eq $cw->{old_ent}) {
	    $dir=0;
	}

	$cw->{old_ent}=$ent;

	if ($Ev->y+10>=$cw->height) {
	    $dir=1;
	} elsif ($Ev->y - 10 <= 0) {
	    $dir=-1;
	}

	if ($dir) {
	    my (@ic, %ic, @is, $ii, $icc, @iss);
	    @ic=$cw->infoChildren();
	    grep {$ic{$ic[$_]} = $_} $[ .. $#ic;
	    @is=$cw->infoSelection();
	    if ($dir==1) {
		@iss=reverse(@is);
	    } else {
		@iss=@is;
	    }

	    foreach $ii (@iss) {
		my $pos;
		$icc=[tk_to_cfg_args($cw->entryconfigure($ii))];
		if (!$ic{$ii}) {
		    if ($ent) {
			$pos=$ic{$ent}+$dir;
		    } elsif ($Ev->y<10) {
			$pos=0;
		    } else {
			$pos=$#ic;
		    }
		} else {
		    $pos=$ic{$ii}+$dir;
		}
		if ($pos<0) {
		    $pos=0;
		} elsif ($pos>$#ic) {
		    $pos=$#ic;
		}

		if ($pos !=$ic{$ii}) {
		    $cw->selectionClear($ii);
		    $cw->deleteEntry($ii);
		    $cw->add($ii, @$icc, -at=>$pos);
		    $cw->selectionSet($ii);
		    $cw->anchorSet($ii);

		    if ($cw->{callback_change}) {
			my ($cmd,@arg);
			if (ref($cw->{callback_change}) eq 'ARRAY') {
			    ($cmd,@arg)=@{$cw->{callback_change}};
			} elsif (ref($cw->{callback_change} eq 'CODE')) {
			    ($cmd,@arg)=($cw->{callback_change});
			}

			if ($cmd) {
			    &{$cmd}($cw,'move',$ii,$pos,@arg);
			}
		    }
		}

		if ($pos !=0 && $pos !=$#ic) {
		    $cw->{moving}=$Ev->y;
		}
	    }

	    if ($dir==-1 && @is ) {
		$cw->see($is[0]);
	    } elsif ($dir==1 && @is ) {
		$cw->see($is[$#is]);
	    }
	}
    }
}

sub tk_to_cfg_args {
    my (@tk)=@_;
    my (@rv,$i);
    while( $i=shift(@tk)) {
	push(@rv,$i->[0]);
	push(@rv,$i->[$#$i]);
    }
    @rv;
}

sub RangeSelect {
    my $w=shift;
    my $ent;

    $w->focus() if ($w->cget('-takefocus'));
    $w->selectionClear();

    if ($ent=$w->findClick) {
	my $nent;
	unless ($nent=$w->infoRnchor()) {
	    $nent=$ent;
	}

	if($w->selectionIncludes($ent)) {
	    $w->selectionClear($ent, $nent);
	} else {
	    $w->selectionSet($ent, $nent);
	}
	$w->anchorSet($ent);
    }
}

sub SingleSelect {
    my $w=shift;
    my $ent;

    $w->focus() if ($w->cget('-takefocus'));
    $w->selectionClear();

    if ($ent=$w->findClick) {
	$w->selectionSet($ent, $ent);
	$w->anchorSet($ent);
    } else {
	$w->anchorClear();
    }
}

sub Toggle {
    my $w=shift;
    my $ent;

    $w->focus( ) if ($w->cget('-takefocus'));

    if ($ent=$w->findClick) {
	if ( $w->selectionIncludes($ent)) {
	    $w->selectionClear($ent, $ent);
	} else {
	    $w->selectionSet($ent, $ent);
	}
	$w->anchorSet($ent);
    } else {
	$w->anchorClear();
    }
}

sub add_entry {
    my ($cw,$eid,$etxt,$st)=@_;
    $cw->add($eid,-text=>$etxt,-style=>$st || $cw->cget('-style'));
}

sub readonly {
    my ($cw, $val) = @_;
    my $rv = $cw->{readonly};
    if (defined($val)) {
	$cw->{readonly} = $val;
    }
    $rv;
}

sub callback_change {
    my ($cw, $val) = @_;
    my $rv = $cw->{callback_change};
    if(defined($val)) {
	$cw->{callback_change} = $val;
    }
    $rv;
}

1;
__END__

=headl NAME

Tk::Playlist - Tk::HList subclass providing playlist-like manipulation

=headl SYNOPSIS

 use Tk::Playlist;

 my $playlist = $widget->Playlist(-readonly => 0);
 splaylist->add("foobar.mp3", -text => "The FooBar Song");
 splaylist->{callback_change}([ \&rewrite_m3u, "filename.m3u" ] );

=headl DESCRIPTION

The Tk::Playlist widget is derived from the standard Tk::HList widget.
See its documentation for more information.

In addition to the standard HList functionality, the Tk::Playlist widget
adds the ability to modify the list directly. Users may drag entries up
and down the list and delete items from the list using the "Delete" key.

=headl WIDGET-SPECIFIC OPTIONS

 over

=item Switch: B<-readonly>

=item Method: B<readonly>

Specifies that the list may not be modified.

=item Switch: B<-callback_change>

=item Method: B<callback_change>

Specifies a Tk callback to be executed whenever the list is changed by
the user. The callback is called once for each item that has changed,
the first parameter is either "move" (to indicate an item has been
moved), "done_moving" (to indicate the user has released the mouse
button and is done dragging entries around), or "delete" (to indicate
an item has been deleted).

When the first parameter is "move" or "delete", the second parameter
is the tag that was assigned to the item (see Tk::HList->add for more
info).

When the first parameter is "move", the third parameter is the new
position of the item on the list, with the top of the list being "0".

=back

=headl BUGS

When a user attempts to drag an entry beyond the top of the list, the
position (third) parameter passed to C<callback_change> is "0". This
could be considered a bug or a feature.

=headl SEE ALSO

L<Tk::HList>, L<Tk::callbacks>

=headl AUTHOR

Tyler MacDonald, E<lt>japh@crackerjack.netE<gt>

=headl COPYRIGHT AND LICENSE

Copyright (C) 2004 by Tyler MacDonald

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.4 or,
at your option, any later version of Perl 5 you may have available.

=cut

