# ASCII

$ scala

\>:load src/Ascii.scala

## Commands

**Render** - Displays the current state of the grid
ASCII RENDER

**Height** - sets the height of the grid (# of characters)
ASCII HEIGHT

**Width** - sets the width of the grid (# of characters
ASCII WIDTH

**Set** - Cursor marks the grid when moved
ASCII SET

**Unset** - Cursor does not mark the grid when moved
ASCII UNSET

**Change_color** - changes the color of the marker
ASCII CHANGE_COLOR <GREEN, BLUE, RED, YELLOW, MAGENTA, BLACK, CYAN, WHITE>

**Marker** - changes the characters of the marker
ASCII MARKER <'@', '|', '+', '-', '0'>

**Rotate** - rotates the entire grid
ASCII ROTATE <NINETY, HUNDRED_EIGHTY, TWO_HUNDRED_SEVENTY>

**Directions**
LEFT, RIGHT, UP, DOWN

**Move** - moves the cursor in the given direction
ASCII MOVE <Direction>

**Then** - allows chaining of MOVEs
ASCII MOVE <Direction< THEN <Direction>

**Do** - takes in an integer X and executes the MOVE X times
ASCII MOVE DOWN THEN RIGHT DO 5 TIMES

**Do if** - takes in an IF and executes the statement if it evaluates to true
ASCII MOVE DOWN THEN RIGHT DO IF EXISTS DOWN END

**Do while** - takes in a WHILE and executes the statement in a loop
ASCII MOVE DOWN THEN RIGHT DO WHILE EXISTS DOWN END

**And/Or** - Allows for compound conditions
ASCII MOVE DOWN THEN RIGHT DO WHILE EXISTS DOWN AND RIGHT END
ASCII MOVE DOWN THEN RIGHT DO WHILE EXISTS DOWN OR RIGHT END

**Undo** - Undo the previous cursor move
ASCII UNDO

**Reset** - resets the entire grid
ASCII RESET

**Reset_cursor** - moves the cursor to (0, 0)
ASCII RESET_CURSOR

**Jump** - moves the cursor to the given location
ASCII JUMP (X, Y)

**Rect** - Creates a rectangle with the given size at the location of the cursor

**Fill** - Fills a selected area with a color or character
ASCII FILL (<Color> | <Char>)

**Select_rect** - selects an area
ASCII SELECT_RECT (WIDTH, HEIGHT) <MOVE/FILL>
ASCII SELECT_RECT (5,5) MOVE RIGHT THEN DOWN
ASCII SELECT_RECT (5,5) FILL 'x'


## Example Artwork
$ scala

\> :load examples/wave.scala

\> :load examples/pride.scala

\> :load examples/smiley.scala

\> :load examples/TheEnd.scala
