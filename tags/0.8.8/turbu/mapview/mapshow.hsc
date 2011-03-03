HelpScribble project file.
15
...
0
2




FALSE


1
BrowseButtons()
0
FALSE

FALSE
TRUE
16777215
0
16711680
8388736
255
FALSE
FALSE
FALSE
FALSE
150
50
600
500
TRUE
1
FALSE
FALSE
Contents
%s Contents
Index
%s Index
Previous
Next
FALSE

142
10
Scribble10
About TURBU




Writing



FALSE
6
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}{\f2\fnil Courier New;}}
{\colortbl ;\red0\green0\blue255;\red128\green0\blue0;\red0\green128\blue0;}
\viewkind4\uc1\pard\tx580\cf1\b\fs32 About TURBU
\par \cf0\b0\f1\fs20 
\par \f0\tab TURBU is an RPG engine and game editor currently under development by Mason Wheeler.  You can download the latest version and find news about the project at \cf1\strike\f2 www.turbu-rpg.com\cf2\strike0\{link=*! ExecFile("http://www.turbu-rpg.com")\}\cf3\strike\f0 
\par }
11
Scribble11
Controls




Writing



FALSE
7
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;}
\viewkind4\uc1\pard\cf1\b\fs32 Controls\cf0\b0\f1\fs20 
\par 
\par \pard\tx500\f0\tab The current version of the TURBU Map Viewer is for viewing maps created in RPG Maker 2000.  You can click on the screen to scroll the camera to center it on a new point.  Right-clicking will place Hero #1 from the database on the screen, if the tile clicked on is a tile that the hero can walk on.
\par \tab Once a hero has been placed on the map, you can use the arrow keys to walk around.  Use the \b action button\b0  to activate an event, and the \b cancel button\b0  to enter the menu.  The \b action button\b0  is currently defined as the <ENTER> key, and the \b cancel button\b0  is defined as <ESC>.  In a later version, the buttons will be configurable by the user.\f1 
\par }
12
Scribble12
System Menu




Writing



FALSE
9
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 System Menu\cf0\b0\f1\fs20 
\par 
\par \pard\tx500\f0\tab The System Menu displays certain information about the current party and allows the player to access the characters' equipment and skills and the party's inventory.  You can enter the System Menu by pressing the \b cancel button\b0  (currently <ESC>).
\par \tab The System Menu is currently only for viewing.  None of the options do anything yet; their functionality is currently under development and will be added in the next few updates.  You can leave the menu by pressing the \b cancel button\b0  again.
\par \pard\tx540\tab NOTE: Don't change the composition of the current party from the \cf2\strike console\cf3\strike0\{linkID=20\}\cf0  while the menu is open.  This can cause system errors.
\par \pard\tx500\f1 
\par }
20
Scribble20
Console help




Writing



FALSE
6
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\tx560\cf1\b\fs32 Console help\cf0\b0\f1\fs20 
\par 
\par \f0\tab The Console window is a debugging feature that allows you to examine the current state of the program and run script commands.  It contains \cf2\strike data grids\cf3\strike0\{linkID=30\}\cf0  and a \cf2\strike script input line\cf3\strike0\{linkID=25\}\cf0 .  The data grids can be used to view the current switches and variables, and the script input line can enter RPG Script code for the script engine to run.
\par }
25
Scribble25
Script input line




Writing



FALSE
8
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\tx580\cf1\b\fs32 Script input line\cf0\b0\f1\fs20 
\par 
\par \f0\tab The input box in the \cf2\strike Console window\cf3\strike0\{linkID=20\}\cf0  allows you to enter an \cf2\strike RPG Script\cf3\strike0\{linkID=40\}\cf0  command and have the script engine run it.  Simply type your command in and press the "Execute Script" button or hit <ENTER>.
\par 
\par \tab Your command should end with a semicolon.  This isn't 100% required, as the script engine will compensate for it, but it's good to get in the habit of doing it if you plan to write scripts in RPG Script, where they \b will\b0  be required.\f1 
\par }
30
Scribble30
Console Data Grids




Writing



FALSE
8
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\tx560\cf1\b\fs32 Console Data Grids\cf0\b0\f1\fs20 
\par 
\par \f0\tab At the top of the \cf2\strike console window\cf3\strike0\{linkID=20\}\cf0  is a tabbed panel containing two grids.  The first grid shows the state of all switches, which can be true (on) or false (off).  The grid in the second tab shows the project's variables, which each hold an integer.
\par \tab The grids do not automatically update themselves with new values any time one of the switches or variables is changed.  You can update the values for either grid by clicking the "Rescan" button below that grid.  Also, loading the console by pressing <SPACE> from the main window while the console window is not open will immediately rescan both grids.
\par \tab The grids only display the values of the current switches and variables; they cannot be used to input new values.  Use the script input line for that.\f1 
\par }
40
Scribble40
RPG Script




Writing



FALSE
10
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}{\f2\fnil Courier New;}{\f3\fnil\fcharset0 Courier New;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\tx560\cf1\b\fs32 RPG Script\cf0\b0\f1\fs20 
\par 
\par \f0\tab RPG Script is the scripting language for \cf2\strike TURBU\cf3\strike0\{linkID=10\}\cf0  projects.  It is built upon \cf1\strike\f2 RemObjects's PascalScript\cf3\strike0\{link=*! ExecFile("http://www.remobjects.com/\f3 ?ps\f2\}\f3  \cf0\f0 framework, which in turn is based on the Object Pascal programming language.  It's designed to be powerful, yet easy to use, and its capabilities are constantly being developed and added to, just as TURBU is.
\par 
\par \tab\cf2\strike Object Pascal basics\cf3\strike0\{linkID=50\}\cf0 
\par 
\par \tab\cf2\strike RPG Script reference\cf3\strike0\{linkID=60\}\cf0\f1 
\par }
50
Scribble50
Object Pascal basics




Writing



FALSE
46
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}{\f2\fnil\fcharset2 Symbol;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\tx560\cf1\b\fs32 Object Pascal basics
\par \cf0\b0\f1\fs20 
\par \f0\tab DISCLAIMER:  This is not meant to be a comprehensive tutorial on writing Object Pascal code.  There are plenty of those available on the Internet.  What it does do is give you a bit of information to get you up to speed on the fundamentals.
\par 
\par \tab The scripting system in TURBU is based on Object Pascal.  Object Pascal is an all-purpose programming language that provides a good balance between ease of use and raw power.  Writing programs or scripts in Object Pascal is based on using instructions to manipulate data.  The data is set up as \i variables\i0 , which are a lot like variables in algebra: a letter or word that refers to a bit of information whose value is not always the same.  In algebra, variables are used in place of numbers.  In programming, however, a variable can represent any kind of information: a number, a sentence, an image, a sound, and so forth.  There are six basic types of variables used in TURBU's script engine:
\par 
\par \pard{\pntext\f2\'B7\tab}{\*\pn\pnlvlblt\pnf2\pnindent0{\pntxtb\'B7}}\tx560 Integer:  A whole number, with nothing after the decimal point.\f1 
\par \f0{\pntext\f2\'B7\tab}Real: Any real number, including decimals.\f1 
\par \f0{\pntext\f2\'B7\tab}Boolean: A "switch" value which can be either \cf2 true\cf0  or \cf2 false\cf0 .\f1 
\par \f0{\pntext\f2\'B7\tab}String:  A group of letters.  This can represent a word, a sentence, a paragraph, or almost any amount of text.  If you use text in your code, to assign to a string variable or send to a routine that needs a string, (there'll be more on routines later,) you have to put the text inside 'single quotes' (not "double quotes").  So:  \i 'Hello everybody!'\i0  is good. \i "Hello everybody!"\i0  is bad, and so is \i Hello everybody!\i0  without quotes of any kind.  \b NOTE:\b0  Make sure to only use ordinary, straight quotes, not the curly "smart quotes" that word processing programs like to create.\f1 
\par \f0{\pntext\f2\'B7\tab}Array:  A group of variables of the same kind that are all grouped together.  Picture a row of mailboxes, with each one only big enough to hold one envelope.  Every mailbox is exactly like every other mailbox except for two things: their location, and what they contain.  This is how arrays work; such a group of mailboxes would be referred to by programmers as an "array of envelopes", since it's not the boxes themselves that are important, but what they hold.\f1 
\par \f0{\pntext\f2\'B7\tab}Object:  An object is a group of variables that are not all of the same kind but are all grouped together.  Objects can be made up of integers, booleans, strings, arrays, other objects, or other data types that are beyond the scope of this tutorial.  Objects also contain their own code for managing their data members.\f1 
\par \pard\tx560 
\par \b\f0\fs24 Referring to variables
\par \b0\fs20 
\par \tab Variables are referenced by a name.  A variable name is a single word or letter.  It can also contain numbers and the underscore character, if the programmer wishes to use them, but the first character in the name must be a letter.  It's best to use descriptive names that are easy to read and understand.  For example, the variables that make up a Hero object have names such as "name", "level", "exp", "hp", "maxHp" and so on.  How you refer to a variable depends on where the variable is located.
\par \tab A variable that stands on its own (that is not part of an array or an object) is referred to simply by its name.
\par \tab A variable that is part of an array is referred to by the array's name, plus the specific variable's index in square brackets.  For example, Switch #21 would be \i switch[21]\i0 , or hero #5 would be \i hero[5]\i0 .
\par \tab A variable that is part of an object is referred to by the object's name, plus a dot (period), plus the variable's name.  So the amount of EXP that hero #5 has would be found at \i hero[5].exp\i0  and the amount of money the party currently has is \i party.money\i0 .
\par 
\par \b\fs24 Manipulating variables
\par \b0\f1\fs20 
\par \f0\tab Now that you know how to refer to variables by name, all that's left is to understand how to do things with them.  There are two basic things that can be done with a variable:  It can be manipulated directly with \b operators\b0 , or passed to a \b routine\b0  as input.
\par 
\par \b Operators:\b0 
\par \tab Most of the operators should be familiar to anyone who took math classes in school.  \b\fs24 +\b0\fs20  adds two things together.  \fs28 -\fs20  subtracts one value from another.  \b\fs28 *\b0\fs20  multiplies two values, and \b\fs24 /\b0\fs20  divides. The word \b\fs24 mod\b0\fs20  is also used as an operator.  It produces the remainder when one number is divided by another.  And parenthesis \b\fs24 ( )\b0\fs20  can be used to enforce a certain order of operations.
\par 
\par \tab There are two other important operators: \b\fs24 =\b0\fs20  and \b\fs24 :=\b0\fs20 .  An equals sign is used to test variables to see if one is equal to the other.  It produces a boolean (true or false) value, and is commonly used in \b IF..THEN\b0  statements, such as \i if switch[7] = true then\i0 .  Equals does not set a variable equal to a certain value.  Let me repeat that.  \i The = operator DOES NOT set a variable to a certain value in Object Pascal.\i0   That's a very easy mistake for new Object Pascal programmers to make, especially if they've used BASIC or C/C++ in the past.  To set a variable to a value, you use the assignment operator, colon-equals.  For example:  \i variable[10] := party.money;\i0 
\par 
\par \tab There are five other comparison operators, in addition to \b\fs24 =\b0\fs20 .  They are: \b\fs24 <\b0\fs20  (less than), \b\fs24 <=\b0\fs20  (less than or equal to), \b\fs24 >\b0\fs20  (greater than), \b\fs24 >=\b0\fs20  (greater than or equal to), and  \b\fs24 <>\b0\fs20  (not equal to).  These all give boolean results.  For example, if \b x\b0  is an integer variable whose value is currently 5:  x > 4: true.  x > 6: false.  x > 5: false.  x >= 5: true. x < 6: true. x <= 4: false. x = 5: true. x = 6: false. x <> 6: true.
\par 
\par \b Routines:\b0 
\par \tab A routine is a block of code that does a specific thing and runs when its name is "called" by the program code.  Routines often require variables as input, and then do work on those variables.  For example, the \i showMessage\i0  routine requires a string variable.  It takes the text in that string and displays it in the in-game message box.  So, \i showMessage(hero[3].name);\i0  would open the message box and output hero #3's name in it.
\par \tab There are two basic types of routines, \b procedures\b0  and \b functions\b0 .  A procedure simply takes its inputs, if any, and executes its code, while a function takes its inputs, executes its code, and produces a result, which is a variable of a certain type.  The \i showMessage\i0  routine mentioned above is a procedure.  There's a similar routine, however, called \i inputNumber\i0 , which takes an integer as input, describing how many digits the number to be input should have, then opens a message box with a number selecter in it.  When the user inputs a number, the \i inputNumber\i0  routine sends this number back to whatever code called it as its return value.
\par \tab Function return values are handled as if the entire function was a variable.  So, to call \i inputNumber\i0  to have it produce a 3-digit number and store that number in variable #38, you would write: \i variable[38] := inputNumber(3);\i0   Likewise, a routine that belongs to an object is referred to just like a variable that belongs to an object: by the object's name, a dot, and the routine's name.  So, to equip hero #1 with item #28, (assuming that item #28 is a type of weapon or armor,) you would use: \i hero[1].equip(28);\i0 
\par   
\par \b\fs24 End of line semicolon\b0\fs20 
\par 
\par \tab You may have noticed that the examples of actual lines of code used above all end with a semicolon \b\fs24 ; \b0\fs20 character.  This is an essential part of Object Pascal syntax.  The semicolon is necessary to tell the computer where each command ends and the one after it begins.  There are a few very specific exceptions which we won't get into here, but as a general rule, it's essential to end every line of code with a semicolon.
\par 
\par \b\fs24 That's it... for now\b0 
\par \fs20 
\par \tab This tutorial has barely scratched the surface of Object Pascal or RPG Script.  It will be expanded somewhat as the scripting feature becomes more important.  If you really want to learn more about the language, run a web search for "Pascal tutorial" or "Delphi tutorial".  More information about the variables, objects and routines available in RPG Script and the TURBU game engine can be found in the \cf3\strike RPG Script Reference\cf4\strike0\{linkID=60\}\cf0  section.\i\f1 
\par }
51
Scribble51
Explanation of Object Pascal types




Writing



FALSE
24
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\tx1140\cf1\b\fs32 Explanation of Object Pascal types\cf0\b0\f1\fs20 
\par 
\par \pard\cf2\strike\f0 Back to RPG Script Reference\cf3\strike0\{linkID=60\}\cf0\f1 
\par \pard\tx1140\b\f0 
\par Integer types:\b0   Several different types of integers are used in Object Pascal.  They differ in two factors: how many bytes are used to store the data, and whether the number is \i signed\i0  (allowed to be either negative or positive) or \i unsigned\i0  (only zero and positive numbers).  Types with more bytes can represent a wider range of numbers, but take up more memory.  Likewise, unsigned types can represent numbers twice as large as signed types of the same byte size, by sacrificing the ability to represent negative numbers.
\par 
\par \b Byte:\b0  \tab Unsigned, 1 byte.  Range: 0..255
\par \b Shortint:\b0\tab Signed, 1 byte.  Range: -127..128
\par \b Word:\b0\tab Unsigned, 2 bytes.  Range: 0..65,535
\par \b Smallint\b0 :\tab Signed, 2 bytes.  Range: -32,768..32,767
\par \b Cardinal:\b0\tab Unsigned, 4 bytes.  Range: 0..4,294,967,295
\par \b Integer:\b0\tab Signed, 4 bytes.  Range: -2,147,483,648..2,147,483,647
\par 
\par \b Enumerated types:\b0   A set of names that each represent a number, but which is generally restricted to a far smaller set size than integer types provide.  They are used by the programmer as a mnemonic device to make certain sets of data easier to remember and refer to.  For example, instead of numbering a hero's slots as 1..5, they are represented by an enumerated type using the names of the slots.  Enumerated types are declared using a list of names separated by commas, within parenthesis.
\par 
\par \b String:\b0   A sequence of letters, used to represent text.  A \i literal string\i0  (actual string data, not a string variable) used in a script must be enclosed in 'single quotes', so the computer knows it's looking at a string and not a variable name or enumerated type identifier.  "Double quotes" and curly "smart quotes" generated by word processors will not work.  Only straight 'single quotes' are accepted by the script compiler.
\par 
\par \b Array:\b0   A group of variables of the same type, arranged in an ordered sequence.  In Object Pascal, arrays are declared with the word "array," then an optional range declaration in square brackets, then "of <variable type>".  So an array of ten \b word\b0  variables would be defined as: \i array [1..10] of word\i0 , and an array of \b integer\b0  variables with no predetermined length would simply be: \i array of integer\i0 .
\par 
\par \b Object:\b0   A group of variables which are not all of the same type, grouped together to form a larger, composite variable.  Objects are used to represent a single item with many different attributes.  Objects also contain routines for managing the variables that they contain.  An object's routines are known as \b methods\b0 , and its variables are usually hidden from direct access by the object's programmer.  Instead, the variables are accessed through \b properties\b0 , an interface that looks like variables to the person using the object, but which may actually contain routines to process information behind the scenes when reading or changing it.
\par \pard\tx400\tab For example, the Hero object's "hp" property reads directly from the variable, but when a script changes its value, that value is first checked to make sure that it's valid (not negative and not higher than the hero's maximum HP), corrects the value if necessary, then assigns it to the real variable.  Then it checks if the value it assigned is 0, and if so, it sets the "dead" condition on the hero.  But all this happens behind the scenes; all the RPG Script writer has to do is change the property as if it were a normal variable.\f1 
\par }
52
Scribble52
Default Property




Writing



FALSE
11
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\tx580\cf1\b\fs32 Default Property\cf0\b0\f1\fs20 
\par 
\par \f0\tab If an object contains at least one array property, and the objects in that array are the main focus of the parent object, the programmer may set that array as the \i default property\i0  for the object.  This means that the script writer can treat the parent as the array itself in scripts.  For example, the \i hero\i0  array property is the default property for the \cf2\strike TRpgParty\cf3\strike0\{linkID=80\}\cf0  class.  This means that, for a TRpgParty object named "party," \i party.hero[2]\i0  and \i party[2]\i0  can be used interchangeably.
\par 
\par \b Related topics\b0 
\par \tab\cf2\strike RPG Script Reference\cf3\strike0\{linkID=60\}\cf0 
\par \tab\cf2\strike Object Pascal basics\cf3\strike0\{linkID=50\}\cf0 
\par \tab\cf2\strike TRpgParty Properties\cf3\strike0\{linkID=81\}\cf0\f1 
\par }
53
Scribble53
Inheritance




Writing



FALSE
6
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 Inheritance\cf0\b0\f1\fs20 
\par 
\par \pard\tx560\f0\tab Certain object classes have enough things in common that implementing the same properties and methods for each different one would be redundant.  Instead, it's possible to declare all of the common elements in a \i base class\i0  and define the other classes as \i descendant classes\i0  (also called \i subclasses\i0  or \i child classes\i0 ) of the base class.  A descendant class inherits all of the elements of its parent class, plus any new properties or methods that are defined for it.  In essence, a descendant class is a more specialized type of its base class.  For example, the \cf2\strike TRpgCharacter\cf3\strike0\{linkID=220\}\cf0  class contains a \cf2\strike Flash\cf3\strike0\{linkID=223\}\cf0  method which can be used with any object that descends from TRpgCharacter.\f1 
\par }
60
Scribble60
RPG Script reference




Writing



FALSE
126
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\tx520\cf1\b\fs32 RPG Script reference\cf0\b0\f1\fs20 
\par 
\par \f0\tab This section describes the variables, object types, and routines that have been written thus far for the \cf2\strike RPG Script\cf3\strike0\{linkID=40\}\cf0  engine.
\par \tab\b DISCLAIMER:\b0   RPG Script and the TURBU engine are both under development.  Nothing here is guaranteed to remain the same in future releases as it is now, and several properties and routines described here have not yet been fully implemented.\b 
\par 
\par Global variables:\b0 
\par \tab The following variables are directly available from the scripting system:
\par 
\par \cf3\{html=<table border="1"><tbody>
\par <tr><td><b>Name</b></td>
\par <td><b>Type</b></td>
\par <td><b>Description</b></td></tr>
\par <tr><td>Switch</td>
\par <td>array of boolean</td>
\par <td>This array contains a group of boolean values which can be
\par set to true or false in scripts throughout the game.</td></tr>
\par <tr><td>variable</td>
\par <td>array of integer</td>
\par <td>This array contains a group of integer values which can be
\par set to any valid number in scripts throughout the game.</td></tr>
\par <tr><td>party</td>
\par <td>\}\cf2\strike TRpgParty\cf3\strike0\{linkID=80\}\{html=</td>
\par <td>This object refers to the currently active party.</td></tr>
\par <tr><td>hero</td>
\par <td>Array of \}\cf2\strike TRpgHero\cf3\strike0\{linkID=70\}\{html=</td>
\par <td>This array contains a group of Hero objects, one for each
\par Hero entry in the project's database.</td></tr>
\par <tr><td>event</td>
\par <td>Array of \}\cf2\strike TRpgEvent\cf3\strike0\{linkID=90\}\{html=</td>
\par <td>This array refers to the various events on the map.</td></tr>
\par <tr><td>thisEvent</td>
\par <td>\}\cf2\strike TRpgEvent\cf3\strike0\{linkID=90\}\{html=</td>
\par <td>Refers to to the TRpgEvent object whose scipt is currently running.</td></tr>
\par <tr><td>timer</td>
\par <td>\}\cf2\strike TRpgTimer\cf3\strike0\{linkID=100\}\{html=</td>
\par <td>This object refers to the in-game timer.</td></tr>
\par <tr><td>menuEnabled</td>
\par <td>boolean</td>
\par <td>Flag controlling whether or not the user can access the system menu.</td></tr>
\par <tr><td>image</td>
\par <td>Array of \}\cf2\strike TRpgImage\cf3\strike0\{linkID=200\}\{html=</td>
\par <td>This array contains a group of Image objects.  Its length is fixed at 20 slots</td></tr>
\par </tbody></table>
\par \}
\par 
\par \cf0\b Object types:\b0 
\par \tab The following classes (types of objects) are used in the scripting system.  Following the established Object Pascal conventions, all object types begin with a T.  Objects contain variables, known as "properties," and routines, known as "methods."  The page for each object below will show a list of all the properties and methods that each type of object in the RPG Script scripting system contains.
\par 
\par \cf2\b\strike TRpgHero:\cf3\strike0\{linkID=70\}\cf0\b0  Object representing a hero.
\par \cf2\b\strike TRpgParty:\cf3\strike0\{linkID=80\}\cf0\b0  Object representing the active party.
\par \cf2\b\strike TRpgEvent:\cf3\strike0\{linkID=90\}\cf0\b0  Object representing an event on the map.
\par \cf2\b\strike TRpgTimer:\cf3\strike0\{linkID=100\}\cf0\b0  Object representing the in-game timer.
\par \cf2\b\strike TRpgInventory:\cf3\strike0\{linkID=140\}\cf0\b0  \tab Object representing the party's inventory.
\par \cf2\b\strike TRpgVehicle:\cf3\strike0\{linkID=170\}\cf0\b0  \tab Object representing an in-game vehicle.
\par \pard\tx540\cf2\b\strike TRpgImage:\cf3\strike0\{linkID=200\}\cf0\b0  \tab Object representing an on-screen image.
\par \pard\tx520 
\par \b Global routines:\b0 
\par \tab The following routines are available directly from the scripting system, without being members of any class.
\par 
\par \cf2\b\strike random:\cf3\strike0\{linkID=120\}\cf0\b0  Generates a random number.
\par \cf2\b\strike showMessage:\cf3\strike0\{linkID=121\}\cf0\b0  Displays an in-game message box
\par \cf2\b\strike messageOptions\cf3\strike0\{linkID=122\}\cf0 :\b0  Sets various options for the in-game message box.
\par \cf2\b\strike setPortrait\cf3\strike0\{linkID=123\}\cf0 :\b0  Sets the current portrait (face graphic) to be displayed in the message box.
\par \cf2\b\strike clearPortrait\cf3\strike0\{linkID=124\}\cf0 :\b0  Sets the message box to not display a portrait.
\par \cf2\b\strike showChoice:\cf3\strike0\{linkID=125\}\cf0\b0  Shows a choice in the message box.
\par \cf2\b\strike inputNumber:\cf3\strike0\{linkID=126\}\cf0\b0  Uses the message box to input a number from the user.
\par \cf2\b\strike heldItems:\cf3\strike0\{linkID=127\}\cf0\b0  Checks to see how many of a certain item the party has.
\par \cf2\b\strike heroJoin:\cf3\strike0\{linkID=128\}\cf0\b0  Adds a hero to the party.
\par \cf2\b\strike heroLeave:\cf3\strike0\{linkID=129\}\cf0\b0  Removes a hero from the party.
\par \cf2\b\strike addItem:\cf3\strike0\{linkID=130\}\cf0\b0  Adds items to the inventory.
\par \cf2\b\strike removeItem:\cf3\strike0\{linkID=131\}\cf0\b0  Removes items from the inventory.
\par \cf2\b\strike addExp:\cf3\strike0\{linkID=132\}\cf0\b0  Adds experience to heroes.
\par \cf2\b\strike removeExp:\cf3\strike0\{linkID=133\}\cf0\b0  Removes experience from heroes.
\par \cf2\b\strike addLevels:\cf3\strike0\{linkID=134\}\cf0\b0  Adds levels to heroes.
\par \cf2\b\strike removeLevels:\cf3\strike0\{linkID=135\}\cf0\b0  Removes levels from heroes.
\par \cf2\b\strike setSystemMusic:\cf3\strike0\{linkID=136\}\b0  \cf0 Changes the system background music for various events.
\par \cf2\b\strike setSystemSound:\cf3\strike0\{linkID=161\}\cf0\b0  Changes the preset system SFX for various events.
\par \cf2\b\strike setSkin:\cf3\strike0\{linkID=162\}\cf0\b0  Changes the current system "skin" graphics set.
\par \cf2\b\strike battle:\cf3\strike0\{linkID=137\}\cf0\b0  Begins a battle.  (Not yet implemented)
\par \cf2\b\strike prepareStore:\cf3\strike0\{linkID=164\}\cf0\b0  Sets up a store inventory for the shopping system to use.\cf2\b\strike 
\par shop:\cf3\strike0\{linkID=138\}\cf0\b0  Opens the shopping system.
\par \cf2\b\strike inn:\cf3\strike0\{linkID=139\}\cf0  \b0 Calls the inn. (Partially implemented)
\par \cf2\b\strike buttonStart:\cf3\strike0\{linkID=160\}\cf0\b0  Checks to see if the current event script was started by pressing the action button.
\par \cf2\b\strike openMenu:\cf3\strike0\{linkID=163\}\cf0\b0  Opens the system menu.
\par \cf2\b\strike inputText:\cf3\strike0\{linkID=165\}\cf0\b0  Allows the user to enter a text string. Commonly used for entering a custom hero name.
\par \cf2\b\strike rideVehicle:\cf3\strike0\{linkID=182\}\cf0\b0  Causes the party to board/leave a vehicle.
\par \cf2\b\strike teleport:\cf3\strike0\{linkID=168\}\cf0\b0  Teleports the current party to another location. (Partially implemented.)
\par \cf2\b\strike memorizeLocation:\cf3\strike0\{linkID=167\}\cf0\b0  Stores the current party's location in three variables. (Slightly bugged.)
\par \cf2\b\strike teleportVehicle:\cf3\strike0\{linkID=169\}\cf0\b0  Teleports a specified vehicle to another location.
\par \cf2\b\strike teleportEvent:\cf3\strike0\{linkID=180\}\cf0\b0  Teleports a specified event to another location on the map.
\par \cf2\b\strike swapEvents:\cf3\strike0\{linkID=181\}\cf0\b0  Exchanges the position of two events on the map.
\par \cf2\b\strike getTerrainID:\cf3\strike0\{linkID=183\}\cf0\b0  Returns the ID # of the map terrain at a certain square.
\par \cf2\b\strike getEventID:\cf3\strike0\{linkID=184\}\cf0\b0  Returns the ID # of the event at a certain square.
\par \cf2\b\strike eraseScreen:\cf3\strike0\{linkID=185\}\cf0\b0  Clears the screen, optionally using a system transition.
\par \cf2\b\strike showScreen:\cf3\strike0\{linkID=186\}\cf0\b0  Restores the screen after an erase, optionally using a system transition.
\par \cf2\b\strike setTransition:\cf3\strike0\{linkID=187\}\cf0\b0  Changes the default transition settings.
\par \cf2\b\strike setScreenTone:\cf3\strike0\{linkID=188\}\cf0\b0  Changes the overall color of the screen. (Partially implemented.)
\par \cf2\b\strike flashScreen:\cf3\strike0\{linkID=189\}\cf0\b0  Flashes a certain color over the screen.
\par \cf2\b\strike lockScreen:\cf3\strike0\{linkID=190\}\cf0\b0  Locks the current screen position.
\par \cf2\b\strike unlockScreen:\cf3\strike0\{linkID=191\}\cf0\b0  Unlocks the current screen position.
\par \cf2\b\strike panScreen:\cf3\strike0\{linkID=192\}\cf0\b0  Pans the screen from the current camera position.
\par \cf2\b\strike panScreenTo:\cf3\strike0\{linkID=193\}\cf0\b0  Pans the screen to a certain position.
\par \cf2\b\strike returnScreen:\cf3\strike0\{linkID=194\}\cf0\b0  Causes the screen to return to the current party.
\par \cf2\b\strike setWeather:\cf3\strike0\{linkID=195\}\cf0\b0  Sets the current weather effect.
\par \cf2\b\strike increaseWeather:\cf3\strike0\{linkID=196\}\cf0\b0  Increases the severity of the current weather effect.
\par \cf2\b\strike decreaseWeather:\cf3\strike0\{linkID=197\}\cf0\b0  Decreases the severity of the current weather effect.
\par \cf2\b\strike newImage:\cf3\strike0\{linkID=199\}\cf0\b0  Sets up a new on-screen image.
\par \cf2\b\strike showBattleAnim:\cf3\strike0\{linkID=213\}\cf0\b0  Displays a battle animation on-screen.
\par \cf2\b\strike playMusic:\cf3\strike0\{linkID=211\}\cf0\b0  Changes the background music.
\par \cf2\b\strike fadeOutMusic:\cf3\strike0\{linkID=212\}\cf0\b0  Causes the background music to gradually fade out.
\par \cf2\b\strike memorizeBgm:\cf3\strike0\{linkID=214\}\cf0\b0  Causes the system to remember the current background music.
\par \cf2\b\strike playMemorizedBgm:\cf3\strike0\{linkID=215\}\cf0\b0  Causes the system to play previously memorized background music.
\par \cf2\b\strike playSound:\cf3\strike0\{linkID=216\}\cf0\b0  Plays a sound effect.
\par \cf2\b\strike prepareRoute:\cf3\strike0\{linkID=219\}\cf0\b0  Sets up a new move script for characters to use.\cf2\b\strike 
\par waitUntilMoved:\cf3\strike0\{linkID=217\}\cf0\b0  Pauses the script until all characters have finished their move scripts.
\par \cf2\b\strike stopMoveScripts:\cf3\strike0\{linkID=218\}\cf0\b0  Cancels all active move scripts.\cf2\b\strike 
\par wait:\cf3\strike0\{linkID=198\}\cf0\b0  Pauses the script execution.
\par \cf2\b\strike keyScan:\cf3\strike0\{linkID=230\}\cf0\b0  Checks to see if any commands are being entered from the keyboard.
\par \cf2\b\strike setBGImage:\cf3\strike0\{linkID=210\}\cf0\b0  Changes the current background image.
\par \cf2\b\strike deleteCharacter:\cf3\strike0\{linkID=231\}\cf0\b0  Removes the character the current script is attached to from the map entirely.
\par \cf2\b\strike callGlobalEvent:\cf3\strike0\{linkID=232\}\cf0\b0  Causes a global event to run.
\par \cf2\b\strike callEvent:\cf3\strike0\{linkID=233\}\cf0\b0  Causes an event from the current map to run.
\par 
\par }
61
Scribble61
Array Element Zero




Writing



FALSE
9
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\tx600\cf1\b\fs32 Array Element Zero\cf0\b0\f1\fs20 
\par 
\par \f0\tab Due to the nature of Object Pascal, all dynamic arrays (\cf2\strike arrays\cf3\strike0\{linkID=51\}\cf0  whose range is not determined before the program runs) start at element #0, not element #1.  Actual objects used in-game start at #1, in keeping with the RPG Maker database arrangement.  The RPG Script system reserves element 0 of each array as a "junkyard," redirecting invalid values to it.  For example, if there were only seven heroes defined in the database, the line \i hero[9].equip(10);\i0  would instead equip hero[0].  This helps keep the script engine from bailing out or causing a system error.
\par \tab\b Warning:\b0   Unlike objects defined from the database, element 0 objects are not guaranteed to be stable, and the values of their variables are not clearly defined at any given moment.  Clever programmers could use element 0 for their own purposes, but they do so at their own risk.  Accessing element 0 has the potential to cause unforseen consequences, especially if it is done unintentionally.  In the example given above, the \cf2\strike equip\cf3\strike0\{linkID=73\}\cf0  routine would still work properly, \i including removing the equipment item from the inventory\i0 , but the item would be equipped to hero # 0, not to anyone defined in the database.  This could make the item appear to have vanished if the programmer is not aware of this behavior.
\par 
\par \cf2\strike RPG Script Reference\cf3\strike0\{linkID=60\}\cf0\f1 
\par }
62
Scribble62
Read-only Properties




Writing



FALSE
8
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\tx540\cf1\b\fs32 Read-only Properties\cf0\b0\f1\fs20 
\par 
\par \f0\tab Certain object \cf2\strike properties\cf3\strike0\{linkID=51\}\cf0  are designated as read-only in RPG Script.  This means that the script writer is allowed to examine their value, but attempting to change them will cause the RPG Script compiler to reject the script.
\par 
\par \cf2\strike RPG Script Reference\cf3\strike0\{linkID=60\}\cf0\f1 
\par }
63
Scribble63
MIDI Lead-in Issue




Writing



FALSE
6
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}{\f2\fnil Courier New;}}
{\colortbl ;\red0\green0\blue255;\red128\green0\blue0;\red0\green128\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 MIDI Lead-in Issue\cf0\b0\f1\fs20 
\par 
\par \f0 Certain MIDI files, including many of the files distributed in the RPG Maker Runtime Package, begin with a certain amount of silence.  RPG Maker's MIDI player skips the silence at the front of the file and begins playing at the first note immediately.  Unfortunately, the \cf1\strike\f2 SDL_Mixer\cf2\strike0\{link=*! ExecFile("http://www.libsdl.org/projects/SDL_mixer/")\}\cf0\f0  library used by TURBU does not currently support this feature.  This can cause timing problems.  Most notably, such songs will not work properly with the \cf3\strike Inn routine.\cf2\strike0\{linkID=139\}\cf0   This issue has been reported to the SDL developers, and I've requested that the option to skip lead-in silence be added to SDL_Mixer.  Until then, it will be necessary to work around this issue by either choosing MIDIs with no lead-in, using the \cf3\strike Wait routine\cf2\strike0\{linkID=198\}\cf0  in scripts to compensate if music/action synchronization is important, or using other file formats.\f1 
\par }
64
Scribble64
XYZ Transparency Issue




Writing



FALSE
8
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;}
\viewkind4\uc1\pard\cf1\b\fs32 XYZ Transparency Issue\cf0\b0\f1\fs20 
\par 
\par \f0 TURBU can import the XYZ image format used in many RPG Maker projects.  However, due to technical limitations in the current graphics library, it imports slowly, and transparency is not preserved correctly.  The current version of the Map Viewer uses a hack to fake proper transparency for Chipset and Charset graphics in XYZ format, but not other graphic types. Therefore, it is recommended to convert XYZ images to PNG format in RPG Maker.
\par 
\par This will be fixed in a later version, after the Map Viewer has been migrated to a more powerful graphics library.\f1 
\par }
70
Scribble70
TRpgHero Class




Writing



FALSE
11
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red128\green0\blue0;\red0\green128\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgHero Class\cf0\b0\f1\fs20 
\par 
\par \f0\tab TRpgHero is a class that represents a hero.  One TRpgHero object is automatically created per Hero entry in the database.
\par 
\par \cf2\f1\{button Properties,71\}\f0   \{button Methods,72\}
\par 
\par \cf3\strike Back to RPG Script Reference\cf2\strike0\{linkID=60\}\cf0\f1 
\par \cf3\strike\f0 Explanation of Object Pascal types\cf2\strike0\{linkID=51\}\cf0\f1 
\par }
71
Scribble71
TRpgHero Properties




Writing



FALSE
65
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgHero Properties\cf0\b0\f1\fs20 
\par 
\par \cf2\strike\f0 Back to TRpgHero\cf3\strike0\{linkID=70\}\cf0 
\par \cf2\strike Back to RPG Script Reference\cf3\strike0\{linkID=60\}\cf0\f1 
\par \cf2\strike\f0 Explanation of Object Pascal types\cf3\strike0\{linkID=51\}\cf0 
\par 
\par 
\par \{html=<table border="1">
\par <tbody>
\par <tr>
\par <td><b>Name</b></td>
\par <td><b>Type</b></td>
\par <td><b>Description</b></td></tr>
\par <tr><td>name</td>
\par <td>string </td>
\par <td>The hero's name</td></tr>
\par <tr><td>charClass</td>
\par <td>string</td>
\par <td>The hero's class (listed as "degree" in RPG Maker)</td></tr>
\par <tr><td>level</td>
\par <td>byte</td>
\par <td>The hero's current level</td></tr>
\par <tr><td>exp</td>
\par <td>integer</td>
\par <td>The hero's total EXP</td></tr>
\par <tr><td>hp</td>
\par <td>integer</td>
\par <td>The hero's current HP</td></tr>
\par <tr><td>mp</td>
\par <td>integer</td>
\par <td>The hero's current MP</td></tr>
\par <tr><td style="vertical-align: top;">maxHp</td>
\par <td style="vertical-align: top;">integer</td>
\par <td style="vertical-align: top;">The hero's maximum HP</td></tr>
\par <tr><td style="vertical-align: top;">maxMp</td>
\par <td style="vertical-align: top;">integer</td>
\par <td style="vertical-align: top;">The hero's maximum MP</td></tr>
\par <tr><td style="vertical-align: middle;">stat</td>
\par <td style="vertical-align: middle;">array[1..4] of smallint</td>
\par <td style="vertical-align: top;">Array containing the hero's primary statistics.&nbsp; They can be accessed through this array, or individually by name: attack, defense, mind, agility</td></tr>
\par <tr><td>attack</td>
\par <td>smallint</td>
\par <td style="vertical-align: top;">The hero's total attackpower.&nbsp; Corresponds to stat[1]</td></tr>
\par <tr><td>defense</td>
\par <td>smallint</td>
\par <td style="vertical-align: top;">The hero's total defensive power.&nbsp; Corresponds to stat[2]</td></tr>
\par <tr><td>mind</td>
\par <td>smallint</td>
\par <td style="vertical-align: top;">The hero's mental strength.&nbsp;&nbsp; Affects magical attack and defense. Corresponds to stat[3]</td></tr>
\par <tr><td>agility</td>
\par <td>smallint</td>
\par <td style="vertical-align: top;">The hero's quickness and dexterity.&nbsp; Affects the chance to hit and to dodge attacks.&nbsp; Corresponds to stat[4]</td></tr>
\par <tr><td>equipment</td>
\par <td>array[1..5] of word</td>
\par <td style="vertical-align: top;">Array of numbers referring to the ID of the items the hero currently has equipped.&nbsp; A 0 means an unequipped slot.&nbsp; Slots 1-5 correspond to: weapon, shield or second weapon, armor, helmet, miscellaneous item.</td></tr>
\par <tr><td>skill</td>
\par <td>array of boolean</td>
\par <td style="vertical-align: top;">Array of boolean values, one for each skill defined in the database, signifying whether the hero knows that skill or not.</td></tr>
\par <tr><td>condition</td>
\par <td>array of boolean</td>
\par <td style="vertical-align: top;">Array of boolean values, one for each condition defined in the database, signifying whether the hero is afflicted by that condition or not.</td></tr>
\par </tbody></table>\}\f1 
\par }
72
Scribble72
TRpgHero Methods




Writing



FALSE
16
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\tx1440\cf1\b\fs32 TRpgHero Methods\cf0\b0\f1\fs20 
\par 
\par \cf2\strike\f0 Back to TRpgHero\cf3\strike0\{linkID=70\}\cf0 
\par \cf2\strike Back to RPG Script Reference\cf3\strike0\{linkID=60\}\cf0\f1 
\par 
\par \cf2\b\strike\f0 equip:\cf3\strike0\{linkID=73\}\cf0\b0\tab Sets a hero's equipment.
\par \cf2\b\strike unequip:\cf3\strike0\{linkID=74\}\cf0\b0\tab Unequips whatever item is in a certain slot.
\par \cf2\b\strike fullheal:\cf3\strike0\{linkID=75\}\cf0\b0\tab Fully restores the hero.
\par \cf2\b\strike takeDamage:\cf3\strike0\{linkID=76\}\cf0\tab\b0 Damages the hero.  Used to calculate damge taken in combat.
\par \cf2\b\strike setSprite:\cf3\strike0\{linkID=77\}\cf0\b0\tab Changes the hero's current sprite (character graphics set).
\par \cf2\b\strike setPortrait:\cf3\strike0\{linkID=78\}\cf0\b0\tab Changes the hero's current portrait (face graphic).
\par \cf2\b\strike inParty:\cf3\strike0\{linkID=79\}\cf0\b0  \tab Checks to see if the hero is in the current party.
\par \cf2\b\strike equipped:\cf3\strike0\{linkID=150\}\cf0\b0\tab Checks to see if the hero has a certain item equipped.\f1 
\par }
73
Scribble73
TRpgHero.equip




Writing



FALSE
17
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue128;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgHero.equip\cf0\b0\f1\fs20 
\par 
\par \b\f0 Class
\par \pard\tx420\tab\cf2\b0\strike TRpgHero\cf3\strike0\{linkID=70\}\cf0\b 
\par \pard 
\par Syntax\b0 
\par \pard\tx440\tab\cf4 procedure equip(id: word);
\par \pard\cf0 
\par \b Description
\par \pard\tx420\b0\tab The equip method attempts to equip a hero with an item of the type represented by \b id\b0  in the database.  If that item does not exist, is not a piece of equipment, or can't be equipped by the current hero, nothing happens.  Otherwise, the hero's currently equipped item for that slot (if any) is \cf2\strike unequipped\cf3\strike0\{linkID=74\}\cf0 , one of that item is removed from the inventory if it is present, or created if it is not found in the inventory, and equipped on the hero.
\par 
\par \b Related topics\b0 
\par \tab\cf2\strike TRpgHero.unequip\cf3\strike0\{linkID=74\}\cf0\b 
\par \b0\f1 
\par }
74
Scribble74
TRpgHero.unequip




Writing



FALSE
17
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue128;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgHero.unequip\cf0\b0\f1\fs20 
\par 
\par \b\f0 Class
\par \pard\tx420\tab\cf2\b0\strike TRpgHero\cf3\strike0\{linkID=70\}\cf0\b 
\par \pard 
\par Syntax\b0 
\par \pard\tx440\tab\cf4 procedure unequip(id: TSlotList);
\par \pard\cf0 
\par \b Description
\par \pard\tx420\b0\tab The unequip method removes the item that the hero has equipped in the slot specified by \b id\b0 , if any, and returns it to the inventory.  Using the \cf2\strike "all"\cf3\strike0\{linkID=110\}\cf0  value causes all currently equipped items to be unequipped.
\par 
\par \pard\b Related topics
\par \pard\tx400\b0\tab\cf2\strike THero.equip\cf3\strike0\{linkID=73\}
\par \tab\cf2\strike TSlotList\cf3\strike0\{linkID=110\}\cf0\f1 
\par }
75
Scribble75
TRpgHero.fullheal




Writing



FALSE
13
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue128;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgHero.fullheal\cf0\b0\f1\fs20 
\par 
\par \b\f0 Class
\par \pard\tx420\tab\cf2\b0\strike TRpgHero\cf3\strike0\{linkID=70\}\cf0\b 
\par \pard 
\par Syntax\b0 
\par \pard\tx440\tab\cf4 procedure fullheal;
\par \pard\cf0 
\par \b Description
\par \pard\tx420\b0\tab The fullheal method restores a hero's HP and MP to their maximum values and removes all conditions.\f1 
\par }
76
Scribble76
TRpgHero.takeDamage




Writing



FALSE
17
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue128;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgHero.takeDamage\cf0\b0\f1\fs20 
\par 
\par \b\f0 Class
\par \pard\tx420\tab\cf2\b0\strike TRpgHero\cf3\strike0\{linkID=70\}\cf0\b 
\par \pard 
\par Syntax\b0 
\par \pard\tx440\tab\cf4 function takeDamage(power: word; defense, mDefense, variance: byte): word;
\par \pard\cf0 
\par \b Description
\par \pard\tx420\b0\tab The takeDamage method deals damage to a hero based on \b power\b0  and \b variance\b0 .  \b Defense\b0  and \b mDefense\b0  signify how much the hero is capable of using his \i defense\i0  and \i mind\i0  statistics to defend against this attack.  The return value is how much damage the hero actually took.
\par \tab This method is not yet fully implemented.  It currently simply subtracts a number of hitpoints equal to \b power\b0  from the hero's \i hp\i0  property.
\par 
\par \pard\b Related topics
\par \pard\tx400\b0\tab\cf2\strike TRpgHero Properties\cf3\strike0\{linkID=71\}\cf0 
\par }
77
Scribble77
TRpgHero.setSprite




Writing



FALSE
15
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue128;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgHero.setSprite
\par \cf0\b0\f1\fs20 
\par \b\f0 Class
\par \pard\tx420\tab\cf2\b0\strike TRpgHero\cf3\strike0\{linkID=70\}\cf0\b 
\par \pard 
\par Syntax\b0 
\par \pard\tx440\tab\cf4 procedure setSprite(filename: string; index: byte; translucent: boolean);
\par \pard\cf0 
\par \b Description
\par \pard\tx420\b0\tab This method changes the hero's current sprite (graphics set).  It first checks to see if \b index\b0  falls within the acceptable range (1..8) and if a CharSet file is available with the name given by \b filename\b0 .  If either test fails, nothing happens.  Otherwise, it sets the hero's sprite to the appropriate sprite from that charset.
\par \f1 
\par \pard 
\par }
78
Scribble78
TRpgHero.setPortrait




Writing



FALSE
15
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue128;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgHero.setPortrait\cf0\b0\f1\fs20 
\par 
\par \b\f0 Class
\par \pard\tx420\tab\cf2\b0\strike TRpgHero\cf3\strike0\{linkID=70\}\cf0\b 
\par \pard 
\par Syntax\b0 
\par \pard\tx440\tab\cf4 procedure setPortrait( filename: string; index: byte);
\par \pard\cf0 
\par \b Description
\par \pard\tx420\b0\tab This method changes the hero's current portrait (face graphic).  It first checks to see if \b index\b0  falls within the acceptable range (1..16) and if a FaceSet file is available with the name given by \b filename\b0 .  If either test fails, nothing happens.  Otherwise, it sets the hero's portrait to the appropriate portrait from that faceset.
\par 
\par \pard\f1 
\par }
79
Scribble79
TRpgHero.inParty




Writing



FALSE
16
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue128;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgHero.inParty\cf0\b0\f1\fs20 
\par 
\par \b\f0 Class
\par \pard\tx420\tab\cf2\b0\strike TRpgHero\cf3\strike0\{linkID=70\}\cf0\b 
\par \pard 
\par Syntax\b0 
\par \pard\tx440\tab\cf4 function inParty: boolean;
\par \pard\cf0 
\par \b Description
\par \pard\tx420\b0\tab This method returns \b true\b0  if the hero is in the current party, or \b false\b0  if he isn't.
\par \b 
\par \b0\f1 
\par \pard 
\par }
80
Scribble80
TRpgParty Class




Writing



FALSE
11
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgParty Class\cf0\b0\f1\fs20 
\par 
\par \f0\tab TRpgParty is a class that represents the active party.  One TRpgParty object named \i party\i0  is automatically created during initialization.  Descends from \cf2\strike TRpgCharacter.\cf3\strike0\{linkID=220\}\cf0 
\par 
\par \cf3\f1\{button Properties,\f0 8\f1 1\}\f0   \{button Methods,82\}
\par 
\par \cf2\strike Back to RPG Script Reference\cf3\strike0\{linkID=60\}\cf0\f1 
\par \cf2\strike\f0 Explanation of Object Pascal types\cf3\strike0\{linkID=51\}\cf0\f1 
\par }
81
Scribble81
TRpgParty Properties




Writing



FALSE
42
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgParty Properties\cf0\b0\f1\fs20 
\par 
\par \cf2\strike\f0 Back to TRpgParty\cf3\strike0\{linkID=80\}\cf0 
\par \cf2\strike Back to RPG Script Reference\cf3\strike0\{linkID=60\}\cf0\f1 
\par \cf2\strike\f0 Explanation of Object Pascal types\cf3\strike0\{linkID=51\}
\par 
\par 
\par \{html=<table border="1"><tbody>
\par <tr><td><b>Name</b></td>
\par <td><b>Type</b></td>
\par <td><b>Description</b></td></tr>
\par <tr><td>hero</td>
\par <td>array[1..4] of \}\cf2\strike TRpgHero\cf3\strike0\{linkID=70\}\{html=</td>
\par <td>Array containing the slots for the heroes which make up the party.&nbsp; This is the default property for TRpgParty.</td></tr>
\par <tr><td>money</td>
\par <td>integer</td>
\par <td>The amount of cash the party is currently carrying.</td></tr>
\par <tr><td>inventory</td>
\par <td>\}\cf2\strike TRpgInventory\cf3\strike0\{linkID=140\}\{html=</td>
\par <td>Object containing the party's inventory.</td></tr>
\par <tr><td>levelNotify</td>
\par <td>boolean</td>
\par <td>\}\cf0 Determines whether the \cf2\strike addExp\cf3\strike0\{linkID=132\}\cf0 , \cf2\strike removeExp\cf3\strike0\{linkID=133\}\cf0 , \cf2\strike addLevels\cf3\strike0\{linkID=134\}\cf0  and \cf2\strike removeLevels\cf3\strike0\{linkID=135\}\cf0  routines notify the user when they change a character's level.\cf3\{html=</td></tr>
\par <tr><td>map</td>
\par <td>word</td>
\par <td>The ID number of the current map.</td></tr>
\par <tr><td>x</td>
\par <td>word</td>
\par <td>The x-coordinate of the party's current location on the map.</td></tr>
\par <tr><td>y</td>
\par <td>word</td>
\par <td>The y-coordinate of the party's current location on the map.</td></tr>
\par <tr><td>facing</td>
\par <td>byte</td>
\par <td>A number designating which direction the party is facing.&nbsp; The direction values are: down: 2, left: 4, right: 6, up: 8.</td></tr>
\par <tr><td>translucent</td>
\par <td>boolean</td>
\par <td>Determines whether the party is drawn as partially translucent or completely opaque.</td></tr>
\par </tbody></table>\}\cf0\f1 
\par }
82
Scribble82
TRpgParty Methods




Writing



FALSE
15
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgParty Methods\cf0\b0\f1\fs20 
\par 
\par \pard\tx1440\cf2\strike\f0 Back to TRpgParty\cf3\strike0\{linkID=80\}\cf0 
\par \cf2\strike Back to RPG Script Reference\cf3\strike0\{linkID=60\}\cf0\f1 
\par 
\par \cf2\b\strike\f0 addItem:\cf3\strike0\{linkID=83\}\cf0\b0\tab Adds items to the inventory.
\par \cf2\b\strike removeItem:\cf3\strike0\{linkID=84\}\cf0\b0\tab Removes items from the inventory
\par \cf2\b\strike addExp:\cf3\strike0\{linkID=85\}\cf0\b0\tab Awards EXP to the party members.
\par \cf2\b\strike removeExp:\cf3\strike0\{linkID=86\}\cf0\tab\b0 Removes EXP from the party members.
\par \cf2\b\strike addLevels:\cf3\strike0\{linkID=86\}\cf0\b0\tab Increases the party members' levels.
\par \pard\cf2\b\strike removeLevels:\cf3\strike0\{linkID=88\}\cf0\b0\tab Decreases the party members' levels.
\par \cf2\b\strike takeDamage:\cf3\strike0\{linkID=89\}\cf0\b0\tab Damages all members of the party.\f1 
\par }
83
Scribble83
TRpgParty.addItem




Writing



FALSE
17
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue128;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgParty.addItem\cf0\b0\f1\fs20 
\par 
\par \b\f0 Class
\par \pard\tx420\tab\cf2\b0\strike TRpgParty\cf3\strike0\{linkID=80\}\cf0\b 
\par \pard 
\par Syntax\b0 
\par \pard\tx440\tab\cf4 procedure addItem(const id, number: word);
\par \pard\cf0 
\par \b Description
\par \pard\tx420\b0\tab The addItem method calls the \cf2\i\strike add\cf3\strike0\{linkID=143\}\cf0\i0  method of the party's \cf2\i\strike inventory\cf3\strike0\{linkID=81\}\cf0\i0  property.
\par 
\par \b Related topics\b0 
\par \tab\cf2\strike TRpgParty.removeItem\cf3\strike0\{linkID=84\}\cf0\b 
\par \pard\b0\f1 
\par }
84
Scribble84
TRpgParty.removeItem




Writing



FALSE
16
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue128;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgParty.removeItem\cf0\b0\f1\fs20 
\par 
\par \b\f0 Class
\par \pard\tx420\tab\cf2\b0\strike TRpgParty\cf3\strike0\{linkID=80\}\cf0\b 
\par \pard 
\par Syntax\b0 
\par \pard\tx440\tab\cf4 procedure removeItem(const id, number: word);
\par \pard\cf0 
\par \b Description
\par \pard\tx480\b0\tab The removeItem method attempts to remove a certain amount of an item from the inventory.  It first checks to make sure that \b id\b0  refers to a valid item number from the database, and that the inventory contains at least one of that item.  If not, nothing happens.  Otherwise, it removes \b number\b0  number of item #\b id\b0  to the inventory.  If \b number\b0  is more than the quantity of the item in the inventory, it removes all of that item.
\par 
\par \b Related topics\b0 
\par \tab\cf2\strike TRpgParty.addItem\cf3\strike0\{linkID=83\}\cf0\f1 
\par }
85
Scribble85
TRpgParty.addExp




Writing



FALSE
19
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue128;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgParty.addExp\cf0\b0\f1\fs20 
\par 
\par \b\f0 Class
\par \pard\tx420\tab\cf2\b0\strike TRpgParty\cf3\strike0\{linkID=80\}\cf0\b 
\par \pard 
\par Syntax\b0 
\par \pard\tx440\tab\cf4 procedure addExp(const id: smallint; number: integer);
\par \pard\cf0 
\par \b Description
\par \pard\tx420\b0\tab The addExp method adds EXP to the party.  If \b id\b0  is -1, it adds \b number\b0  exp to every current member of the party.  Otherwise, it adds \b number\b0  EXP to the hero occupying slot #\b id\b0 .  If this is enough to cause the hero to gain a level, his \i level\i0  property is adjusted accordingly.
\par \tab If slot is specified (not -1) and that slot is invalid or empty, it gets redirected to \cf2\strike hero #0\cf3\strike0\{linkID=61\}\cf0 .
\par 
\par \b Related topics\b0 
\par \tab\cf2\strike TRpgParty.removeExp\cf3\strike0\{linkID=84\}
\par \cf0\tab\cf2\strike TRpgParty.addLevels\cf3\strike0\{linkID=87\}\cf0\b 
\par \pard\b0\f1 
\par }
86
Scribble86
TRpgParty.removeExp




Writing



FALSE
18
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue128;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgParty.removeExp\cf0\b0\f1\fs20 
\par 
\par \b\f0 Class
\par \pard\tx420\tab\cf2\b0\strike TRpgParty\cf3\strike0\{linkID=80\}\cf0\b 
\par \pard 
\par Syntax\b0 
\par \pard\tx440\tab\cf4 procedure removeExp(const id: smallint; number: integer);
\par \pard\cf0 
\par \b Description
\par \pard\tx400\tx420\b0\tab The addExp method removes EXP from the party.  If \b id\b0  is -1, it removes \b number\b0  exp from every current member of the party.  If this would reduce their EXP below 0, their EXP stays at 0.  Otherwise, it removes \b number\b0  EXP from the hero occupying slot #\b id\b0 .  If this slot is invalid or empty, it gets redirected to \cf2\strike hero #0\cf3\strike0\{linkID=61\}\cf0 .
\par 
\par \b Related topics\b0 
\par \tab\cf2\strike TRpgParty.addExp\cf3\strike0\{linkID=83\}
\par \cf0\tab\cf2\strike TRpgParty.removeLevels\cf3\strike0\{linkID=88\}\cf0\b 
\par \b0\f1 
\par }
87
Scribble87
TRpgParty.addLevels




Writing



FALSE
20
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue128;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgParty.addLevels\cf0\b0\f1\fs20 
\par 
\par \b\f0 Class
\par \pard\tx420\tab\cf2\b0\strike TRpgParty\cf3\strike0\{linkID=80\}\cf0\b 
\par \pard 
\par Syntax\b0 
\par \pard\tx440\tab\cf4 procedure addLevels(const id: smallint; number: byte);
\par \pard\cf0 
\par \b Description
\par \pard\tx420\b0\tab The addLevels method adds levels to the party.  If \b id\b0  is -1, it adds \b number\b0  exp to every current member of the party.  Otherwise, it adds \b number\b0  levels to the hero occupying slot #\b id\b0 .  If this would raise a character's level above the maximum level, it stays at the maximum level.  The character's EXP is changed to reflect their new level, and any new skills earned that level are awarded.
\par \tab If the specified slot is invalid or empty, it gets redirected to \cf2\strike hero #0\cf3\strike0\{linkID=61\}\cf0 .
\par \tab This method is not yet fully implemented.  Changing levels does not yet adjust a hero's statistics.
\par 
\par \b Related topics
\par \tab\cf2\b0\strike TRpgParty.addExp\cf3\strike0\{linkID=85\}\cf0 
\par \tab\cf2\strike TRpgParty.removeLevels\cf3\strike0\{linkID=88\}\cf0\b 
\par \pard\b0\f1 
\par }
88
Scribble88
TRpgParty.removeLevels




Writing



FALSE
20
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue128;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgParty.removeLevels\cf0\b0\f1\fs20 
\par 
\par \b\f0 Class
\par \pard\tx420\tab\cf2\b0\strike TRpgParty\cf3\strike0\{linkID=80\}\cf0\b 
\par \pard 
\par Syntax\b0 
\par \pard\tx440\tab\cf4 procedure removeLevels(const id: smallint; number: byte);
\par \pard\cf0 
\par \b Description
\par \pard\tx420\b0\tab The removeLevels method removes levels from the party.  If \b id\b0  is -1, it takes \b number\b0  levels from every current member of the party.  Otherwise, it adds \b number\b0  levels to the hero occupying slot #\b id\b0 .  If this would reduce a character's level below 1, it stays at 1.  The character's EXP is changed to reflect their new level, and any skills earned at the level(s) lost are removed.
\par \tab If the specified slot is invalid or empty, it gets redirected to \cf2\strike hero #0\cf3\strike0\{linkID=61\}\cf0 .
\par \tab This method is not yet fully implemented.  Changing levels does not yet adjust a hero's statistics.
\par 
\par \b Related topics
\par \tab\cf2\b0\strike TRpgParty.removeExp\cf3\strike0\{linkID=86\}\cf0 
\par \tab\cf2\strike TRpgParty.addLevels\cf3\strike0\{linkID=87\}\cf0\b 
\par \pard\b0\f1 
\par }
89
Scribble89
TRpgParty.takeDamage




Writing



FALSE
17
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue128;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgParty.takeDamage\cf0\b0\f1\fs20 
\par 
\par \b\f0 Class
\par \pard\tx420\tab\cf2\b0\strike TRpgParty\cf3\strike0\{linkID=80\}\cf0\b 
\par \pard 
\par Syntax\b0 
\par \tab\cf4 function takeDamage(power: word; defense, mDefense, variance: byte): word;
\par \cf0 
\par \b Description
\par \pard\tx420\b0\tab The takeDamage method calls the \cf2\strike takeDamage\cf3\strike0\{linkID=76\}\cf0  method for each hero currently in the party.  It returns the return value of the takeDamage routine from the last hero who took damage.
\par 
\par \b Related topics\b0 
\par \tab\cf2\strike TRpgHero.takeDamage\cf3\strike0\{linkID=76\}\cf0\b 
\par \pard\b0\f1 
\par }
90
Scribble90
TRpgEvent Class




Writing



FALSE
11
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgEvent Class\cf0\b0\f1\fs20 
\par 
\par \f0\tab TRpgEvent is a class that represents an event on the map.  One TRpgEvent object is automatically created per event on the current map.  Descends from \cf2\strike TRpgCharacter.\cf3\strike0\{linkID=220\}\cf0 
\par 
\par \cf3\f1\{button Properties,\f0 9\f1 1\}\f0   \{button Methods,92\}
\par 
\par \cf2\strike Back to RPG Script Reference\cf3\strike0\{linkID=60\}\cf0\f1 
\par \cf2\strike\f0 Explanation of Object Pascal types\cf3\strike0\{linkID=51\}\cf0\f1 
\par }
91
Scribble91
TRpgEvent Properties




Writing



FALSE
29
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgEvent Properties\cf0\b0\f1\fs20 
\par 
\par \cf2\strike\f0 Back to TRpgEvent\cf3\strike0\{linkID=90\}\cf0 
\par \cf2\strike Back to RPG Script Reference\cf3\strike0\{linkID=60\}\cf0\f1 
\par \cf2\strike\f0 Explanation of Object Pascal types\cf3\strike0\{linkID=51\}
\par 
\par 
\par \{html=<table border="1"><tbody>
\par <tr><td><b>Name</b></td>
\par <td><b>Type</b></td>
\par <td><b>Description</b></td></tr>
\par <tr><td>map</td>
\par <td>word</td>
\par <td>Defines which map # the event is on.</td></tr>
\par <tr><td>x</td>
\par <td>word</td>
\par <td>The event's x-coordinate on the tile map.</td></tr>
\par <tr><td>y</td>
\par <td>word</td>
\par <td>The event's y-coordinate on the tile map.</td></tr>
\par <tr><td>facing</td>
\par <td>byte</td>
\par <td style="vertical-align: top;">A number designating which direction the event is facing.&nbsp; If the event is represented by a tile and not a character sprite, it is considered to be facing down by default.&nbsp; The direction values are: down: 2, left: 4, right: 6, up: 8.</td></tr>
\par </tbody></table>\}
\par 
\par \cf0 NOTE: All TRpgEvent properties are currently \cf2\strike read-only\cf3\strike0\{linkID=62\}\cf0 .  This will be changed in the future as more support for events is added.
\par }
92
Scribble92
TRpgEvent Methods




Writing



FALSE
6
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgEvent Methods\cf0\b0\f1\fs20 
\par 
\par \f0\tab\cf2\strike TRpgEvent\cf3\strike0\{linkID=90\}\cf0  does not currently have any methods defined.\f1 
\par }
100
Scribble100
TRpgTimer Class




Writing



FALSE
11
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red128\green0\blue0;\red0\green128\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgTimer Class\cf0\b0\f1\fs20 
\par 
\par \f0\tab TRpgTimer is a class that represents the in-game timer.  One TRpgTimer object named \i timer\i0  is automatically created during initialization.
\par 
\par \cf2\f1\{button Properties,\f0 10\f1 1\}\f0   \{button Methods,102\}
\par 
\par \cf3\strike Back to RPG Script Reference\cf2\strike0\{linkID=60\}\cf0\f1 
\par \cf3\strike\f0 Explanation of Object Pascal types\cf2\strike0\{linkID=51\}\cf0\f1 
\par }
101
Scribble101
TRpgTimer Properties




Writing



FALSE
28
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgTimer Properties\cf0\b0\f1\fs20 
\par 
\par \cf2\strike\f0 Back to TRpgTimer\cf3\strike0\{linkID=100\}\cf0 
\par \cf2\strike Back to RPG Script Reference\cf3\strike0\{linkID=60\}\cf0\f1 
\par \cf2\strike\f0 Explanation of Object Pascal types\cf3\strike0\{linkID=51\}
\par 
\par 
\par \f1\{html=\cf0 <table border="1"><tbody>
\par <tr><td><b>Name</b></td>
\par <td><b>Type</b></td>
\par <td><b>Description</b></td></tr>
\par <tr><td>time</td>
\par <td>integer</td>
\par <td>The number of seconds left on the timer.</td></tr>
\par <tr><td>visible</td>
\par <td>boolean</td>
\par <td>Whether the timer gets shown on the screen or not.</td></tr>
\par <tr><td>active</td>
\par <td>boolean</td>
\par <td>Whether the timer is currently counting down or not.&nbsp;\f0  \f1 This property is\f0  \f1 read-only.&nbsp; It gets changed when the appropriate methods are\f0  \f1 called.</td></tr>
\par <tr><td>inBattle</td>
\par <td>boolean</td>
\par <td style="vertical-align: top;">Determines whether or not the\f0  \f1 timer remains visible and active during\f0  \f1 battles.&nbsp; (This currently does nothing, as battles are not yet\f0  \f1 implemented.)</td></tr>
\par </tbody></table>\f0\}\f1 
\par 
\par }
102
Scribble102
TRpgTimer Methods




Writing



FALSE
13
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgTimer Methods\cf0\b0\f1\fs20 
\par 
\par \pard\tx1440\cf2\strike\f0 Back to TRpgTimer\cf3\strike0\{linkID=100\}\cf0 
\par \cf2\strike Back to RPG Script Reference\cf3\strike0\{linkID=60\}\cf0\f1 
\par 
\par \cf2\b\strike\f0 go:\cf3\strike0\{linkID=103\}\cf0\b0\tab Starts the timer.
\par \cf2\b\strike start:\cf3\strike0\{linkID=104\}\cf0\b0\tab Starts the timer and sets certain settings at the same time.
\par \cf2\b\strike pause:\cf3\strike0\{linkID=105\}\cf0\b0\tab Pauses the timer.
\par \cf2\b\strike reset:\cf3\strike0\{linkID=106\}\cf0\tab\b0 Stops the timer and resets it.
\par \cf2\b\strike 
\par }
103
Scribble103
TRpgTimer.go




Writing



FALSE
16
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue128;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgTimer.go\cf0\b0\f1\fs20 
\par 
\par \pard\tx420\b\f0 Class
\par \tab\cf2\b0\strike TRpgTimer\cf3\strike0\{linkID=100\}\cf0\b 
\par 
\par Syntax\b0 
\par \tab\cf4 procedure go;
\par \cf0 
\par \b Description
\par \b0\tab The go method starts the timer counting down.  It assumes that the \i time\i0  property has already been set.
\par 
\par \b Related topics\b0 
\par \tab\cf2\strike TRpgTimer.start\cf3\strike0\{linkID=104\}\cf0\f1 
\par }
104
Scribble104
TRpgTimer.start




Writing



FALSE
16
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue128;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgTimer.start\cf0\b0\f1\fs20 
\par 
\par \b\f0 Class
\par \pard\tx420\tab\cf2\b0\strike TRpgTimer\cf3\strike0\{linkID=100\}\cf0\b 
\par \pard 
\par \pard\tx420 Syntax\b0 
\par \tab\cf4 procedure start(const visible, inBattle: boolean);
\par \cf0 
\par \b Description
\par \b0\tab The start method sets the timer's \i visible\i0  and \i inBattle\i0  properties to the specified values, then starts the timer counting down.  It assumes that the \i time\i0  property has already been set.
\par 
\par \b Related topics\b0 
\par \tab\cf2\strike TRpgTimer.go\cf3\strike0\{linkID=103\}\cf0\f1 
\par }
105
Scribble105
TRpgTimer.pause




Writing



FALSE
17
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue128;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgTimer.pause\cf0\b0\f1\fs20 
\par 
\par \b\f0 Class
\par \pard\tx420\tab\cf2\b0\strike TRpgTimer\cf3\strike0\{linkID=100\}\cf0\b 
\par \pard 
\par Syntax\b0 
\par \pard\tx440\tab\cf4 procedure pause;
\par \pard\cf0 
\par \b Description
\par \pard\tx400\b0\tab The pause method stops the timer in its countdown, but does not reset it.
\par 
\par \b Related topics\b0 
\par \tab\cf2\strike TRpgTimer.go\cf3\strike0\{linkID=103\}
\par \tab\cf2\strike TRpgTimer.reset\cf3\strike0\{linkID=106\}\cf0\f1 
\par }
106
Scribble106
TRpgTimer.reset




Writing



FALSE
16
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue128;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgTimer.reset\cf0\b0\f1\fs20 
\par 
\par \b\f0 Class
\par \pard\tx420\tab\cf2\b0\strike TRpgTimer\cf3\strike0\{linkID=100\}\cf0\b 
\par \pard 
\par Syntax\b0 
\par \pard\tx440\tab\cf4 procedure reset;
\par \pard\tx400\cf0 
\par \b Description
\par \b0\tab The reset method stops the timer in its countdown, resets the \i time\i0  property to 0, and sets the \i visible\i0  property to false.
\par 
\par \b Related topics\b0 
\par \tab\cf2\strike TRpgTimer.pause\cf3\strike0\{linkID=105\}\cf0\f1 
\par }
110
Scribble110
TSlotList Type




Writing



FALSE
13
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 TSlotList Type\cf0\b0\f1\fs20 
\par 
\par \b\f0 Syntax\b0 
\par \pard\tx440\tab\cf2 type TSlotList = (weapon, shield, armor, helmet, relic, all);
\par \pard\cf0 
\par \b Description
\par \pard\tx420\b0\tab An enumerated type that lists the various slots a hero can equip items in.  The "shield" slot is also used for the second weapon if the hero can dual-wield; the name is simply for reference.  "all" does not refer to a single slot; calling \cf3\strike unequip\cf4\strike0\{linkID=74\}\cf0  with this parameter causes it to unequip all equipped items.
\par 
\par \b Related topics
\par \b0\tab\cf3\strike TRpgHero.unequip\cf4\strike0\{linkID=74\}\cf0\f1 
\par }
111
Scribble111
TMboxLocation Type




Writing



FALSE
14
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 TMboxLocation Type\cf0\b0\f1\fs20 
\par 
\par \b\f0 Syntax\b0 
\par \pard\tx440\tab\cf2 type TMboxLocation = \cf0\f1 (mb_top, mb_middle, mb_bottom)\cf2\f0 ;
\par \pard\cf0 
\par \b Description
\par \pard\tx420\b0\tab An \cf3\strike enumerated type\cf4\strike0\{linkID=51\}\cf0  that lists the positions the message box can occupy on-screen.
\par 
\par \b Related topics
\par \tab\cf3\b0\strike MessageOptions routine\cf4\strike0\{linkID=122\}
\par \cf0\f1 
\par }
112
Scribble112
TBgmTypes Type




Writing



FALSE
13
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 TBgmTypes Type\cf0\b0\f1\fs20 
\par 
\par \b\f0 Syntax\b0 
\par \pard\tx440\cf2\tab type \f1 TBgmTypes = (bgmTitle, bgmBattle, bgmBossBattle, bgmVictory, bgmInn,\f0  \f1 bgmBoat, bgmShip, bgmAirship, bgmGameOver);\f0 
\par \pard\cf0 
\par \b Description
\par \pard\tx420\b0\tab An \cf3\strike enumerated type\cf4\strike0\{linkID=51\}\cf0  that lists the various sytem background music presets.
\par 
\par \b Related topics
\par \pard\b0\tab\cf3\strike SetSytemMusic routine\cf4\strike0\{linkID=136\}\cf0\f1 
\par }
113
Scribble113
TBattleResult Type




Writing



FALSE
13
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 TBattleResult Type\cf0\b0\f1\fs20 
\par 
\par \b\f0 Syntax\b0 
\par \pard\tx440\cf2\tab type \f1 TBattleResult = (br_victory, br_escaped, br_defeated);\f0 
\par \pard\cf0 
\par \b Description
\par \pard\tx420\b0\tab An \cf3\strike enumerated type\cf4\strike0\{linkID=51\}\cf0  that lists the ways a battle can end.
\par 
\par \b Related topics
\par \pard\b0\tab\cf3\strike Battle routine\cf4\strike0\{linkID=137\}\cf0\f1 
\par }
114
Scribble114
TSfxTypes Type




Writing



FALSE
14
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 TSfxTypes Type\cf0\b0\f1\fs20 
\par 
\par \b\f0 Syntax\b0 
\par \pard\tx440\cf2\tab type \f1 TSfxTypes = (sfxCursor, sfxAccept, sfxCancel, sfxBuzzer, sfxBattleStart,\f0  \f1 sfxEscape, sfxEnemyAttack, sfxEnemyDamage, sfxAllyDamage,\f0  \f1 sfxEvade, sfxEnemyDies, sfxItemUsed);\f0 
\par \pard\cf0 
\par \b Description
\par \pard\tx420\b0\tab An \cf3\strike enumerated type\cf4\strike0\{linkID=51\}\cf0  that lists the various sytem sound effect presets.
\par 
\par \b Related topics
\par \pard\b0\tab\cf3\strike SetSytemSound routine\cf4\strike0\{linkID=161\}\cf0\f1 
\par 
\par }
115
Scribble115
TTransitions Type




Writing



FALSE
18
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 TTransitions Type\cf0\b0\f1\fs20 
\par 
\par \b\f0 Syntax\b0 
\par \pard\tx440\cf2\tab type \f1 TTransitions = (trnDefault, trnFadeout, trnBlocks, trnBlockUp, trnBlockDn, trnBlinds, trnStripeHiLo, trnStripeLR, trnOutIn, trnInOut, trnScrollUp, trnScrollDn, trnScrollLeft, trnScrollRight, trnDivHiLow, trnDivLR, trnDivQuarters, trnZoom, trnTwist, trnRipple, trnNone);\f0 
\par \pard\cf0 
\par \b Description
\par \pard\tx420\b0\tab An \cf3\strike enumerated type\cf4\strike0\{linkID=51\}\cf0  that lists the various sytem transitions.
\par 
\par \b Related topics
\par \pard\tx540\b0\tab\cf3\strike eraseScreen routine\cf4\strike0\{linkID=185\}\cf0\f1 
\par \f0\tab\cf3\strike showScreen routine\cf4\strike0\{linkID=186\}\cf0\f1 
\par \f0\tab\cf3\strike setTransition routine\cf4\strike0\{linkID=187\}\cf0\f1 
\par \f0\tab\cf3\strike TTransitionTypes Type\cf4\strike0\{linkID=116\}\cf0\f1 
\par 
\par \pard 
\par }
116
Scribble116
TTransitionTypes Type




Writing



FALSE
15
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 TTransitionTypes Type\cf0\b0\f1\fs20 
\par 
\par \b\f0 Syntax\b0 
\par \pard\tx440\cf2\tab type \f1 TTransitionTypes = (trnMapEnter, trnMapExit, trnBattleStartErase, trnBattleStartShow,\f0  \f1 trnBattleEndErase, trnBattleEndShow);\f0 
\par \pard\cf0 
\par \b Description
\par \pard\tx420\b0\tab An \cf3\strike enumerated type\cf4\strike0\{linkID=51\}\cf0  that lists the various times when sytem transitions are automatically used.
\par 
\par \b Related topics
\par \pard\tx520\tx540\b0\tab\cf3\strike setTransition routine\cf4\strike0\{linkID=187\}\cf0\f1 
\par \f0\tab\cf3\strike TTransitions Type\cf4\strike0\{linkID=115\}\cf0\f1 
\par 
\par }
117
Scribble117
TDirections Type




Writing



FALSE
14
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 TDirections Type\cf0\b0\f1\fs20 
\par 
\par \b\f0 Syntax\b0 
\par \pard\tx440\cf2\tab type \f1 T\f0 Direction\f1 s = (dir_up, dir_right, dir_down, dir_left);\f0 
\par \pard\cf0 
\par \b Description
\par \pard\tx420\b0\tab An \cf3\strike enumerated type\cf4\strike0\{linkID=51\}\cf0  that represents the four cardinal directions.
\par 
\par \b Related topics
\par \pard\tx520\tx540\b0\tab\cf3\strike panScreen routine\cf4\strike0\{linkID=192\}\cf0\f1 
\par \f0 
\par }
118
Scribble118
TWeatherEffects type




Writing



FALSE
14
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 TWeatherEffects type\cf0\b0\f1\fs20 
\par 
\par \b\f0 Syntax\b0 
\par \pard\tx440\cf2\tab type \f1 TWeatherEffects = (we_none, we_rain, we_snow);\f0 
\par \pard\cf0 
\par \b Description
\par \pard\tx420\b0\tab An \cf3\strike enumerated type\cf4\strike0\{linkID=51\}\cf0  that represents the different weather effect patterns.
\par 
\par \b Related topics
\par \pard\tx520\tx540\b0\tab\cf3\strike setWeather routine\cf4\strike0\{linkID=187\}\cf0\f1 
\par \pard 
\par }
119
Scribble119
TImageEffects Type




Writing



FALSE
14
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 TImageEffects Type\cf0\b0\f1\fs20 
\par 
\par \b\f0 Syntax\b0 
\par \pard\tx440\cf2\tab type \f1 T\f0 Image\f1 Effects = (ie_none, ie_rotate, ie_wave);\f0 
\par \pard\cf0 
\par \b Description
\par \pard\tx420\b0\tab An \cf3\strike enumerated type\cf4\strike0\{linkID=51\}\cf0  that represents the different graphical effects that can be applied to a \cf3\strike TRpgImage\cf4\strike0\{linkID=200\}\cf0 .
\par 
\par \b Related topics
\par \pard\tx520\tx540\b0\tab\cf3\strike TRpgImage.applyImageEffect\cf4\strike0\{linkID=204\}\cf0\f1 
\par \pard 
\par }
120
Scribble120
Random routine




Writing



FALSE
15
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;}
\viewkind4\uc1\pard\cf1\b\fs32 Random routine
\par \cf0\b0\f1\fs20 
\par \f0 Generates a random number.
\par 
\par \b Syntax
\par \b0 
\par \pard\tx540\tab\cf2 function random(one, two: integer): integer;
\par 
\par \cf0\b Description
\par 
\par \b0\tab The random routine returns a random integer between the values of parameters \b one\b0  and \b two\b0 , inclusive.  So, \i random(5, 17);\i0  would return any integer value from 5 to 17.
\par \f1 
\par }
121
Scribble121
ShowMessage routine




Writing



FALSE
22
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 ShowMessage \f1 r\f0 outine\cf0\b0\fs20 
\par 
\par \f1 Displays a message in the in-game message box.
\par 
\par \b Syntax
\par \pard\tx540\b0 
\par \tab\cf2 procedure showMessage(const msg: string);
\par 
\par \cf0\b Description
\par 
\par \b0\tab The showMessage routine displays the message box, with the message contained in \b msg\b0  in it.  The script does not continue until the message box is closed.
\par 
\par \b Related Topics\b0 
\par \tab\cf3\strike messageOptions routine\cf4\strike0\{linkID=122\}
\par \cf0\tab\cf3\strike setPortrait routine\cf4\strike0\{linkID=123\}\cf0 
\par \tab\cf3\strike clearPortrait routine\cf4\strike0\{linkID=124\}\cf0 
\par \tab\cf3\strike showChoice routine\cf4\strike0\{linkID=125\}
\par \cf0\tab\cf3\strike inputNumber routine\cf4\strike0\{linkID=126\}\cf0 
\par \f0 
\par }
122
Scribble122
MessageOptions routine




Writing



FALSE
21
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 MessageOptions routine\cf0\b0\f1\fs20 
\par 
\par \f0 Sets display options for the in-game message box.
\par 
\par \b Syntax
\par \pard\tx540\cf2\b0 
\par \cf0\tab\cf2\f1 procedure messageOptions(const transparent: boolean; const position: TMboxLocation; const dontHideHero: boolean; const continueEvents: boolean);\f0 
\par 
\par \cf0\b Description
\par 
\par \b0\tab The messageOptions routine sets the options for the message box.  If \b transparent\b0  is true, then the box will not be drawn on the screen; only the message will.  The \b position\b0  parameter defines which third of the screen the message box displays in.  If \b dontHideHero\b0  is true, the location in \b position\b0  will be temporarily overriden if the message box would cover up the current party.
\par \tab The \b continueEvents\b0  parameter is not yet implemented.  When parallel events are implemented, if a message box opens while \b continueEvents\b0  is set to false, all parallel events currently running will be paused until the box closes.
\par 
\par \b Related Topics\b0 
\par \tab\cf3\strike showMessage routine\cf4\strike0\{linkID=121\}\cf0 
\par \tab\cf3\strike setPortrait routine\cf4\strike0\{linkID=123\}\cf0 
\par \tab\cf3\strike clearPortrait routine\cf4\strike0\{linkID=124\}\cf0 
\par \pard\tab\f1 
\par }
123
Scribble123
SetPortrait routine




Writing



FALSE
21
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 SetPortrait routine\cf0\b0\f1\fs20 
\par 
\par \f0 Sets the portrait to be displayed in the in-game message box.
\par 
\par \b Syntax
\par \pard\tx540\b0 
\par \tab\cf2 procedure setPortrait(const filename: string; const index: byte; const rightside, flipped: boolean);
\par 
\par \cf0\b Description
\par 
\par \b0\tab The setPortrait routine sets the active portrait to be displayed when the message box is opened with \cf3\strike showMessage\cf4\strike0\{linkID=121\}\cf0 .  It first tests to make sure that \b filename\b0  is a valid FaceSet graphic and that \b index\b0  falls within the proper range (1..16).  If either of these tests fails, nothing happens.  Otherwise, the message box is set to display the indicated portrait.  If \b rightside\b0  is true, it will display on the right side of the message box, otherwise it displays on the left.  If \b flipped\b0  is true, the portrait will be flipped horizontally.
\par 
\par \b Related Topics\b0 
\par \tab\cf3\strike showMessage routine\cf4\strike0\{linkID=121\}\cf0 
\par \tab\cf3\strike messageOptions routine\cf4\strike0\{linkID=122\}\cf0 
\par \tab\cf3\strike clearPortrait routine\cf4\strike0\{linkID=124\}\cf0 
\par \tab\f1 
\par \pard 
\par }
124
Scribble124
ClearPortrait routine




Writing



FALSE
19
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 ClearPortrait routine\cf0\b0\f1\fs20 
\par 
\par \f0 Turns off the portrait display feature of the in-game message box.
\par 
\par \b Syntax
\par \pard\tx540\b0 
\par \tab\cf2 procedure clearPortrait;
\par 
\par \cf0\b Description
\par 
\par \b0\tab The clearPortrait routine clears the portrait to be displayed when the message box is opened with \cf3\strike showMessage\cf4\strike0\{linkID=121\}\cf0 .
\par 
\par \b Related Topics\b0 
\par \tab\cf3\strike showMessage routine\cf4\strike0\{linkID=121\}\cf0 
\par \tab\cf3\strike messageOptions routine\cf4\strike0\{linkID=122\}\cf0 
\par \tab\cf3\strike setPortrait routine\cf4\strike0\{linkID=123\}\cf0\f1 
\par }
125
Scribble125
ShowChoice routine




Writing



FALSE
21
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 ShowChoice routine\cf0\b0\f1\fs20 
\par 
\par \f0 Shows a choice in the message box.
\par 
\par \b Syntax
\par \pard\tx540\b0 
\par \tab\cf2 function showChoice(input: string; handler: byte): integer;
\par 
\par \cf0\b Description
\par 
\par \b0\tab The showChoice routine displays the message box and asks the user to select from the options presented in \b input\b0 .  The return value is the number of the option that the player chose.  The value of \b handler\b0  determines how the routine handles the user pressing the Cancel button.  If \b handler\b0  is 0, then Cancel has no effect.  1..4 causes Cancel to return that value.  5 or higher causes Cancel to return a -1.
\par \tab\b NOTE:\b0  This routine, in its current implementation, is designed to work around a few obscure quirks in RPG Maker's choice handler and still always translate properly.  It uses strange characters and syntax, and it is not recommended for script writers to attempt to write their own showChoice calls until after the Project Importer is finished.
\par 
\par \b Related Topics\b0 
\par \tab\cf3\strike showMessage routine\cf4\strike0\{linkID=121\}
\par \cf0\tab\cf3\strike inputNumber routine\cf4\strike0\{linkID=126\}\cf0 
\par \tab\f1 
\par \pard 
\par }
126
Scribble126
InputNumber routine




Writing



FALSE
20
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 InputNumber routine\cf0\b0\f1\fs20 
\par 
\par Uses the message box to input a number from the user.
\par \b\f0 
\par Syntax
\par \pard\tx540\b0 
\par \tab\cf2 function inputNumber(const digits: byte): integer
\par 
\par \cf0\b Description
\par 
\par \b0\tab The inputNumber routine displays the message box and has the user input an integer with a number of digits equal to the \b digits\b0  parameter.  The return value is the number that the user enters.
\par 
\par \b Related Topics\b0 
\par \tab\cf3\strike showMessage routine\cf4\strike0\{linkID=121\}
\par \cf0\tab\cf3\strike showChoice routine\cf4\strike0\{linkID=125\}\cf0 
\par \tab\f1 
\par \pard 
\par }
127
Scribble127
HeldItems routine




Writing



FALSE
20
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 HeldItems routine\cf0\b0\f1\fs20 
\par 
\par \f0 Checks to see how many of a certain item the party has.
\par \b 
\par Syntax
\par \pard\tx540\b0 
\par \tab\cf2 function heldItems(const index: word; const equipped: boolean): word;
\par 
\par \cf0\b Description
\par 
\par \b0\tab The heldItems routine sees how many items of the type \b index\b0  refers to that the party currently has.  If \b equipped\b0  is true, it counts how many are equipped by current party members; otherwise, it returns the number of the item in the inventory.  If \b index\b0  is an invalid number, it returns 0.
\par 
\par \b Related Topics\b0 
\par \tab\cf3\strike addItem routine\cf4\strike0\{linkID=130\}
\par \cf0\tab\cf3\strike removeItem routine\cf4\strike0\{linkID=131\}\cf0 
\par \tab\f1 
\par \pard 
\par }
128
Scribble128
HeroJoin routine




Writing



FALSE
19
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 HeroJoin routine\cf0\b0\f1\fs20 
\par 
\par Adds a hero to the party.
\par 
\par \b\f0 Syntax
\par \pard\tx540\b0 
\par \tab\cf2 procedure heroJoin(const id: byte);
\par 
\par \cf0\b Description
\par 
\par \b0\tab The heroJoin attempts to add hero #\b id\b0  to the party.  It does nothing if the hero is already in the party, the party is full, or \b id\b0  is not a valid hero record number.
\par 
\par \b Related Topics\b0 
\par \tab\cf3\strike heroLeave routine\cf4\strike0\{linkID=129\}\cf0 
\par \tab\f1 
\par \pard 
\par }
129
Scribble129
HeroLeave routine




Writing



FALSE
19
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 HeroLeave routine\cf0\b0\f1\fs20 
\par 
\par \f0 Removes\f1  a hero \f0 from \f1 the party.
\par 
\par \b\f0 Syntax
\par \pard\tx540\b0 
\par \tab\cf2 procedure heroLeave(const id: byte);
\par 
\par \cf0\b Description
\par 
\par \b0\tab The heroJoin attempts to remove hero #\b id\b0  from the party.  It does nothing if the hero is not in the party or if \b id\b0  is not a valid hero record number.
\par 
\par \b Related Topics\b0 
\par \tab\cf3\strike heroLeave routine\cf4\strike0\{linkID=128\}\cf0 
\par \tab\f1 
\par \pard 
\par }
130
Scribble130
AddItem routine




Writing



FALSE
20
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 AddItem routine\cf0\b0\f1\fs20 
\par 
\par Adds items to the inventory.
\par 
\par \b\f0 Syntax
\par \pard\tx540\b0 
\par \tab\cf2 procedure addItem(const id, number: word);
\par 
\par \cf0\b Description
\par 
\par \b0\tab This procedure calls the \cf3\strike party\cf4\strike0\{linkID=60\}\cf0 .\cf3\strike addItem\cf4\strike0\{linkID=83\}\cf0  routine.  It is included for convenience while writing scripts.
\par 
\par \b Related Topics\b0 
\par \tab\cf3\strike heldItems routine\cf4\strike0\{linkID=127\}\cf0 
\par \tab\cf3\strike removeItem routine\cf4\strike0\{linkID=131\}
\par \cf0\tab\cf3\strike TRpgParty.addItem\cf4\strike0\{linkID=83\}
\par \cf0 
\par }
131
Scribble131
RemoveItem routine




Writing



FALSE
20
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 RemoveItem routine\cf0\b0\f1\fs20 
\par 
\par \f0 Removes\f1  items \f0 from \f1 the inventory.
\par 
\par \b\f0 Syntax
\par \pard\tx540\b0 
\par \tab\cf2 procedure removeItem(const id, number: word);
\par 
\par \cf0\b Description
\par 
\par \b0\tab This procedure calls the \cf3\strike party\cf4\strike0\{linkID=60\}\cf0 .\cf3\strike removeItem\cf4\strike0\{linkID=84\}\cf0  routine.  It is included for convenience while writing scripts.
\par 
\par \b Related Topics\b0 
\par \tab\cf3\strike heldItems routine\cf4\strike0\{linkID=127\}\cf0 
\par \tab\cf3\strike removeItem routine\cf4\strike0\{linkID=130\}
\par \cf0\tab\cf3\strike TRpgParty.addItem\cf4\strike0\{linkID=84\}
\par \pard\cf0\f1 
\par }
132
Scribble132
AddExp routine




Writing



FALSE
21
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 AddExp routine\cf0\b0\f1\fs20 
\par 
\par \f0 Adds experience to heroes.\f1 
\par 
\par \b\f0 Syntax
\par \pard\tx540\b0 
\par \tab\cf2 procedure addExp(const id: smallint; number: integer);
\par 
\par \cf0\b Description
\par 
\par \b0\tab The addExp routine adds EXP to heroes.  If \b id\b0  is -1, it calls the \cf3\strike party\cf4\strike0\{linkID=60\}\cf0 .\cf3\strike addExp\cf4\strike0\{linkID=85\}\cf0  routine.  Otherwise, it adds EXP to hero #\b id\b0 .  If \b id\b0  is an invalid number, it gets redirected to \cf3\strike hero #0\cf4\strike0\{linkID=61\}\cf0 .  If a character's EXP value goes above the maximum EXP value, it gets set to the maximum.
\par \tab After adding EXP, if \cf3\i\strike party\cf4\strike0\{linkID=60\}\cf0 .\cf3\strike levelNotify\cf4\strike0\{linkID=82\}\cf0\i0  is true, the routine shows a message if any heroes gained a level.
\par 
\par \b Related Topics\b0 
\par \tab\cf3\strike removeExp routine\cf4\strike0\{linkID=133\}
\par \tab\cf3\strike TRpgParty.addExp\cf4\strike0\{linkID=85\}\cf0 
\par \tab\f1 
\par \pard 
\par }
133
Scribble133
RemoveExp routine




Writing



FALSE
19
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 RemoveExp routine\cf0\b0\f1\fs20 
\par 
\par \f0 Removes experience from heroes.\f1 
\par 
\par \b\f0 Syntax
\par \pard\tx540\b0 
\par \tab\cf2 procedure removeExp(const id: smallint; number: integer);
\par 
\par \cf0\b Description
\par 
\par \b0\tab The removeExp routine removes EXP from heroes.  If \b id\b0  is -1, it calls the \cf3\strike party\cf4\strike0\{linkID=60\}\cf0 .\cf3\strike removeExp\cf4\strike0\{linkID=86\}\cf0  routine.  Otherwise, it removes EXP from hero #\b id\b0 .  If \b id\b0  is an invalid number, it gets redirected to \cf3\strike hero #0\cf4\strike0\{linkID=61\}\cf0 .  If a character's new EXP value drops below 0, it gets set to 0.
\par \tab After subtracting EXP, if \cf3\i\strike party\cf4\strike0\{linkID=60\}\cf0 .\cf3\strike levelNotify\cf4\strike0\{linkID=82\}\cf0\i0  is true, the routine shows a message if any heroes lost a level.
\par 
\par \b Related Topics\b0 
\par \tab\cf3\strike addExp routine\cf4\strike0\{linkID=132\}
\par \tab\cf3\strike TRpgParty.removeExp\cf4\strike0\{linkID=86\}\cf0\b 
\par }
134
Scribble134
AddLevels routine




Writing



FALSE
21
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 AddLevels routine\cf0\b0\f1\fs20 
\par 
\par \f0 Adds levels to heroes.\f1 
\par 
\par \b\f0 Syntax
\par \pard\tx540\b0 
\par \tab\cf2 procedure addLevels(const id: smallint; number: integer);
\par 
\par \cf0\b Description
\par 
\par \b0\tab The addExp routine adds levels to heroes.  If \b id\b0  is -1, it calls the \cf3\strike party\cf4\strike0\{linkID=60\}\cf0 .\cf3\strike addLevels\cf4\strike0\{linkID=87\}\cf0  routine.  Otherwise, it adds EXP to hero #\b id\b0 .  If \b id\b0  is an invalid number, it gets redirected to \cf3\strike hero #0\cf4\strike0\{linkID=61\}\cf0 .  If the new total is greater than the character's maximum level, it gets set to that level.
\par \tab After adding levels, if \cf3\i\strike party\cf4\strike0\{linkID=60\}\cf0 .\cf3\strike levelNotify\cf4\strike0\{linkID=82\}\cf0\i0  is true, the routine shows a message indicating that the hero(es) gained a level.
\par 
\par \b Related Topics\b0 
\par \tab\cf3\strike removeLevels routine\cf4\strike0\{linkID=135\}
\par \tab\cf3\strike TRpgParty.addLevels\cf4\strike0\{linkID=85\}\cf0 
\par \tab\f1 
\par \pard 
\par }
135
Scribble135
RemoveLevels routine




Writing



FALSE
21
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 RemoveLevels routine\cf0\b0\f1\fs20 
\par 
\par \f0 Removes levels from heroes.\f1 
\par 
\par \b\f0 Syntax
\par \pard\tx540\b0 
\par \tab\cf2 procedure addLevels(const id: smallint; number: integer);
\par 
\par \cf0\b Description
\par 
\par \b0\tab The addExp routine adds levels to heroes.  If \b id\b0  is -1, it calls the \cf3\strike party\cf4\strike0\{linkID=60\}\cf0 .\cf3\strike addLevels\cf4\strike0\{linkID=87\}\cf0  routine.  Otherwise, it adds EXP to hero #\b id\b0 .  If \b id\b0  is an invalid number, it gets redirected to \cf3\strike hero #0\cf4\strike0\{linkID=61\}\cf0 .  If the new level is less than 1, it gets set to 1.
\par \tab After adding levels, if \cf3\i\strike party\cf4\strike0\{linkID=60\}\cf0 .\cf3\strike levelNotify\cf4\strike0\{linkID=82\}\cf0\i0  is true, the routine shows a message indicating that the hero(es) lost a level.
\par 
\par \b Related Topics\b0 
\par \tab\cf3\strike addLevels routine\cf4\strike0\{linkID=134\}
\par \tab\cf3\strike TRpgParty.removeLevels\cf4\strike0\{linkID=86\}\cf0 
\par \tab\f1 
\par \pard 
\par }
136
Scribble136
SetSystemMusic routine




Writing



FALSE
18
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 SetSystemMusic routine\cf0\b0\f1\fs20 
\par 
\par \pard\tx520\f0 Changes the system background music for various events.\f1 
\par \pard 
\par \b\f0 Syntax
\par \pard\tx540\b0 
\par \tab\cf2 procedure setSystemMusic(const which: TBgmTypes; const newSong: string);
\par 
\par \cf0\b Description
\par 
\par \b0\tab The setSystemMusic routine changes a system BGM preset.  The system music to be changed is specified by \b which\b0 , and \b newsong\b0  is the filename of the new system music to use.
\par 
\par \b Related Topics\b0 
\par \tab\cf3\strike TBgmTypes type\cf4\strike0\{linkID=112\}\cf0 
\par \f1 
\par }
137
Scribble137
Battle routine




Writing



FALSE
18
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 Battle routine\cf0\b0\f1\fs20 
\par 
\par \pard\tx520\f0 Begins a battle.  (Not yet implemented.)\f1 
\par \pard 
\par \b\f0 Syntax
\par \pard\tx540\b0 
\par \tab\cf2 function battle(which: word; allow_escape, first_strike: boolean): TBattleResult;
\par 
\par \cf0\b Description
\par 
\par \b0\tab The battle routine starts a battle.  The battle functionality is not yet implemented; the current routine simply returns a result of \cf3\strike br_victory\cf4\strike0\{linkID=113\}\cf0  without running a battle.
\par 
\par \b Related Topics\b0 
\par \tab\cf3\strike TBattleResult type\cf4\strike0\{linkID=113\}\cf0 
\par \pard\f1 
\par }
138
Scribble138
Shop routine




Writing



FALSE
21
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 Shop routine\cf0\b0\f1\fs20 
\par 
\par \pard\tx520\f0 Opens the shopping system.\f1 
\par \pard 
\par \b\f0 Syntax
\par \pard\tx540\b0 
\par \tab\cf2 function shop(style: word; messageStyle: byte; store: word): boolean;
\par 
\par \cf0\b Description
\par 
\par \b0\tab The shop routine begins the shopping system.  The \b style\b0  parameter determines which type of shop will be opened: 0 is a buy/sell shop, 1 is a buy-only shop, and 2 is a sell-only shop.  The \b messageStyle\b0  parameter refers to the set of shop messages to be displayed.  The \b store\b0  parameter refers to a store inventory previously set up by the \cf3\strike prepareStore routine\cf4\strike0\{linkID=164\}\cf0 .
\par 
\par \tab If an invalid number is passed to any of these parameters, it will be corrected to a default value: the lowest valid number for each case.  A default \cf3\strike shop # 0\cf4\strike0\{linkID=61\}\cf0  that sells nothing is created automatically during setup.
\par 
\par \tab The routine returns \b true\b0  if anything has been successfully bought or sold, or \b false\b0  otherwise.
\par \b 
\par \pard\tx400\tx420 Related topics\b0 
\par \pard\tx540\tab\cf3\strike PrepareStore routine\cf4\strike0\{linkID=164\}\cf0\b 
\par }
139
Scribble139
Inn routine




Writing



FALSE
15
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 Inn routine\cf0\b0\f1\fs20 
\par 
\par \pard\tx520\f0 Calls the inn.\f1 
\par \pard 
\par \b\f0 Syntax
\par \pard\tx540\b0 
\par \tab\cf2 function inn(messageStyle: byte; cost: integer): boolean;
\par 
\par \cf0\b Description
\par 
\par \b0\tab The inn routine begins the Inn system.  It asks if the party would like to rest at the inn, and presents a choice.  If the party does not have at least \b cost\b0  gold on hand, the "Yes" option is grayed out.  If the player chooses "Yes", all heroes are fully healed via the \cf3\strike fullheal\cf4\strike0\{linkID=75\}\cf0  routine, and the function returns \b true\b0 .  If the player chooses "No" or presses the cancel button, the function returns \b false\b0 .
\par \tab\b NOTE:\b0  Certain MIDI files will not work correctly with this routine.  See \cf3\strike MIDI Lead-in Issue\cf4\strike0\{linkID=63\}\cf0  for details.\f1 
\par }
140
Scribble140
TRpgInventory Class




Writing



FALSE
11
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgInventory Class\cf0\b0\f1\fs20 
\par 
\par \pard\tx540\f0\tab TRpgInventory is a class that represents the party's inventory.  One TRpgInventory object named \i inventory\i0  is automatically created as part of the \cf2\strike TRpgParty\cf3\strike0\{linkID=80\}\cf0  initialization.
\par \pard 
\par \cf3\f1\{button Properties,\f0 14\f1 1\}\f0   \{button Methods,142\}
\par 
\par \cf2\strike Back to RPG Script Reference\cf3\strike0\{linkID=60\}\cf0\f1 
\par \cf2\strike\f0 Explanation of Object Pascal types\cf3\strike0\{linkID=51\}\cf0\f1 
\par }
141
Scribble141
TRpgInventory Properties




Writing



FALSE
12
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgInventory Properties\cf0\b0\f1\fs20 
\par 
\par \cf2\strike\f0 Back to TRpgInventory\cf3\strike0\{linkID=140\}\cf0 
\par \cf2\strike Back to RPG Script Reference\cf3\strike0\{linkID=60\}\cf0\f1 
\par \cf2\strike\f0 Explanation of Object Pascal types\cf3\strike0\{linkID=51\}
\par 
\par 
\par \pard\tx520\tab\cf2\strike TRpgInventory\cf3\strike0\{linkID=140\}\cf0  does not currently have any properties defined.\f1 
\par \pard\cf3\f0 
\par }
142
Scribble142
TRpgInventory Methods




Writing



FALSE
12
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgInventory Methods\cf0\b0\f1\fs20 
\par 
\par \pard\tx1440\cf2\strike\f0 Back to TRpgInventory\cf3\strike0\{linkID=140\}\cf0 
\par \cf2\strike Back to RPG Script Reference\cf3\strike0\{linkID=60\}\cf0\f1 
\par 
\par \cf2\b\strike\f0 add:\cf3\strike0\{linkID=143\}\cf0\b0\tab Adds items to the inventory.
\par \cf2\b\strike contains:\cf3\strike0\{linkID=144\}\cf0\b0\tab Checks to see whether the inventory contains any of a certain item.
\par \cf2\b\strike remove:\cf3\strike0\{linkID=145\}\cf0\tab\b0 Removes items from the inventory.
\par \pard\f1 
\par }
143
Scribble143
TRpgInventory.add




Writing



FALSE
16
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue128;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgInventory.add\cf0\b0\f1\fs20 
\par 
\par \pard\tx420\b\f0 Class
\par \tab\cf2\b0\strike TRpgInventory\cf3\strike0\{linkID=140\}\cf0\b 
\par 
\par Syntax\b0 
\par \tab\cf4 procedure add(const id, number: word);
\par \cf0 
\par \b Description
\par \b0\tab The add method attempts to add a certain amount of an item to the inventory.  It first checks to make sure that \b id\b0  refers to a valid item number from the database.  If not, nothing happens.  Otherwise, it adds \b number\b0  number of item #\b id\b0  to the inventory.  If this makes the current amount of this item greater than 99, the quantity is set back to 99.
\par 
\par \b Related topics\b0 
\par \tab\cf2\strike TRpgInventory.remove\cf3\strike0\{linkID=144\}\cf0\f1 
\par }
144
Scribble144
TRpgInventory.remove




Writing



FALSE
18
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue128;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgInventory.remove\cf0\b0\f1\fs20 
\par 
\par \pard\tx400\tx420\b\f0 Class
\par \tab\cf2\b0\strike TRpgInventory\cf3\strike0\{linkID=140\}\cf0\b 
\par 
\par Syntax\b0 
\par \tab\cf4 procedure remove(const id, number: word)
\par \cf0 
\par \b Description
\par \b0\tab The remove method attempts to remove a certain amount of an item from the inventory.  It first checks to make sure that \b id\b0  refers to a valid item number from the database, and that the inventory contains at least one of that item.  If not, nothing happens.  Otherwise, it removes \b number\b0  number of item #\b id\b0  to the inventory.  If \b number\b0  is more than the quantity of the item in the inventory, it removes all of that item.\f1 
\par \b\f0 
\par Related topics\b0 
\par \tab\cf2\strike TRpgInventory.add\cf3\strike0\{linkID=143\}
\par \cf0\f1 
\par \f0 
\par }
145
Scribble145
TRpgInventory.contains




Writing



FALSE
16
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue128;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgInventory.contains\cf0\b0\f1\fs20 
\par 
\par \pard\tx420\b\f0 Class
\par \tab\cf2\b0\strike TRpgInventory\cf3\strike0\{linkID=140\}\cf0\b 
\par 
\par Syntax\b0 
\par \tab\cf4 f\f1 unction contains(id: word): boolean\f0 ;
\par \cf0 
\par \b Description
\par \pard\tx400\b0\tab The contains method searches through the inventory to determine whether it contains any of the item referred to by \b id\b0 .  If \b id\b0  is not a valid item number from the database, or if the inventory does not contain any of the item, it returns \b false\b0 .  If the inventory does contain at least one of the item, the return value is \b true\b0 .\f1 
\par \pard\tx420\b\f0 
\par Related topics\b0 
\par \tab\cf2\strike TRpgHero.equipped\cf3\strike0\{linkID=150\}\cf0\b 
\par }
150
Scribble150
TRpgHero.equipped




Writing



FALSE
16
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue128;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgHero.equipped
\par \cf0\b0\f1\fs20 
\par \b\f0 Class
\par \pard\tx420\tab\cf2\b0\strike TRpgHero\cf3\strike0\{linkID=70\}\cf0\b 
\par \pard 
\par Syntax\b0 
\par \pard\tx400\tab\cf4\f1 function equipped(id: word): boolean;\f0 
\par \pard\cf0 
\par \b Description
\par \pard\tx420\b0\tab This method returns \b true\b0  if the hero has item #\b id\b0  equipped, or \b false\b0  if he doesn't, or if \b id\b0  is not a valid item number from the database.
\par 
\par \b Related topics\b0 
\par \pard\tx400\tab\cf2\strike TRpgInventory.contains\cf3\strike0\{linkID=145\}\cf0\f1 
\par }
160
Scribble160
ButtonStart routine




Writing



FALSE
17
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;}
\viewkind4\uc1\pard\cf1\b\fs32 ButtonStart routine\cf0\b0\f1\fs20 
\par 
\par \pard\tx520\f0 Checks to see if the current event script was started by pressing the action button.\f1 
\par 
\par \pard\b\f0 Syntax
\par \pard\tx540\b0 
\par \tab\cf2 function buttonStart: boolean;
\par 
\par \cf0\b Description
\par 
\par \b0\tab The buttonStart routine returns \b true\b0  if the current script was started by the user pressing the action button, or \b false\b0  if it was started in some other way.
\par \b 
\par 
\par \pard\tx520\b0\f1 
\par }
161
Scribble161
SetSystemSound routine




Writing



FALSE
18
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 SetSystemSound routine\cf0\b0\f1\fs20 
\par 
\par \f0 Changes the preset system SFX for various events.
\par 
\par \b Syntax
\par \pard\tx540\b0 
\par \tab\cf2 procedure setSystemSound(const which: TSfxTypes; const newSound: string);
\par 
\par \cf0\b Description
\par 
\par \b0\tab The setSystemSound routine changes a system SFX preset.  The system music to be changed is specified by \b which\b0 , and \b newSound\b0  is the filename of the new system sound to use.
\par 
\par \b Related Topics\b0 
\par \tab\cf3\strike TSfxTypes type\cf4\strike0\{linkID=114\}\cf0 
\par \pard\f1 
\par }
162
Scribble162
SetSkin routine




Writing



FALSE
17
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;}
\viewkind4\uc1\pard\cf1\b\fs32 SetSkin routine\cf0\b0\f1\fs20 
\par 
\par \f0 Changes the current system "skin" graphics set.\f1 
\par 
\par \b\f0 Syntax
\par \pard\tx540\b0 
\par \pard\tab\f1 procedure setSkin(const name: string);\cf2\f0 
\par \pard\tx540 
\par \cf0\b Description
\par 
\par \b0\tab The setSkin routine loads a new system "skin" graphic set.  This graphic defines the look of various elements of the game, including menus, cursors and text boxes.
\par \tab NOTE: setSkin will not run while a message box of any type is currently displayed.  For technical reasons, the routine has to unregister the various graphical elements that make up a message box, then register the new ones, and while this is done very quickly, it is not quick enough to prevent the graphics engine from crashing by trying to display an image that's not registered properly.  If setSkin is called while a message box is on-screen, it will wait and run once the box has closed.
\par 
\par \pard\f1 
\par }
163
Scribble163
OpenMenu routine




Writing



FALSE
15
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 OpenMenu routine\cf0\b0\f1\fs20 
\par 
\par \f0 Opens the system menu.\f1 
\par 
\par \b\f0 Syntax
\par \pard\tx540\b0 
\par \tab\f1 procedure \f0 openMenu\f1 ;\cf2\f0 
\par 
\par \cf0\b Description
\par 
\par \b0\tab The openMenu routine opens the system menu.  It will work even if the global \b menuEnabled\b0  flag is set to false.
\par \tab NOTE: Don't change the composition of the current party from the \cf3\strike console\cf4\strike0\{linkID=20\}\cf0  while the menu is open.  This can cause system errors.
\par }
164
Scribble164
PrepareStore routine




Writing



FALSE
21
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 PrepareStore routine\cf0\b0\f1\fs20 
\par 
\par \f0 Sets up a store inventory for the shop system to use.
\par 
\par \b Syntax
\par \pard\tx540\b0 
\par \tab\cf2 function prepareStore(merchandise: string): word;
\par 
\par \cf0\b Description
\par 
\par \b0\tab The PrepareStore function prepares an inventory for the shop system to use in its stores.  The \b merchandise\b0  parameter is a string consisting of several numbers separated by spaces.  Each number is the database ID number of an item to be sold in the shop.  A typical merchandise string would look something like this:
\par 
\par \tab\cf1 5 7 28 13 20 21 12
\par \cf0 
\par \tab Any number that is not a valid item ID is ignored.  The function returns the ID number of the newly-created shop.  It is the designer's responsibility to keep track of this number.  The ID number might be different each time the game runs, but does not change after it has been initially created.
\par \b 
\par \pard\tx400\tx420 Related topics\b0 
\par \pard\tab\cf3\strike Shop routine\cf4\strike0\{linkID=138\}\cf0\f1 
\par }
165
Scribble165
InputText Routine




Writing



FALSE
23
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;}
\viewkind4\uc1\pard\cf1\b\fs32 InputText Routine\cf0\b0\f1\fs20 
\par 
\par \f0 Allows the user to enter a text string.
\par 
\par \b Syntax
\par \pard\tx540\b0 
\par \tab\cf2\f1 function inputText(start: string; heroId: word): string;\f0 
\par 
\par \cf0\b Description
\par 
\par \b0\tab This routine opens the Text Input menu, allowing the user to enter a string of text.  This is commonly used for entering custom hero names, but can serve other purposes as well.
\par 
\par \tab The \b start\b0  parameter contains an optional default string to be displayed.  This can be used to provide the default name for a hero.  The \b heroId\b0  parameter is the ID number of a hero whose name is being entered.  This is used to display the hero's portrait.  If \cf1 0\cf0  is passed to heroId, no portrait will be displayed.  The function returns the string that the user enters.
\par 
\par \b Example\b0 
\par 
\par \tab To set a name for hero #1:
\par 
\par \tab\cf2 hero[1].name := inputText(hero[1].name, 1);\cf0 
\par \b 
\par }
167
Scribble167
MemorizeLocation routine




Writing



FALSE
17
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 MemorizeLocation routine\cf0\b0\f1\fs20 
\par 
\par \f0 Stores the current party's location in three variables. (Currently bugged.)
\par 
\par \b Syntax
\par \pard\tx540\b0 
\par \pard\cf2\tab\f1 procedure memorizeLocation(var map, x, y: integer);\f0 
\par \pard\tx540 
\par \cf0\b Description
\par 
\par \b0\tab This routine stores the location of the current party to three variables, denoting the current map and the party's X and Y coordinates.
\par 
\par \pard\tx400\tx420\b Related topics\b0 
\par \pard\tx540\tab\cf3\strike Teleport routine\cf4\strike0\{linkID=168\}\cf0 
\par }
168
Scribble168
Teleport routine




Writing



FALSE
21
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 Teleport routine\cf0\b0\f1\fs20 
\par 
\par \f0 Teleports the current party to another location. (Partially implemented.)
\par 
\par \b Syntax
\par \pard\tx540\b0 
\par \tab\cf2\f1 procedure teleport(map, x, y: word);\f0 
\par 
\par \cf0\b Description
\par 
\par \b0\tab This routine teleports the current party to a new location, as specified by the map ID # and coordinates passed to the routine.  It does nothing if the location is not on the map.
\par 
\par \tab\b NOTE:\b0  Because multiple maps have not yet been implemented, this routine currently does nothing if the \b map\b0  parameter is not the same as the current map ID#.
\par \tab\b WARNING:\b0  The routine does not check whether or not the destination square is valid terrain to walk on.  It's possible to teleport the party to invalid terrain, and get stuck there.
\par \b 
\par \pard\tx400\tx420 Related topics\b0 
\par \pard\tx540\tab\cf3\strike MemorizeLocation routine\cf4\strike0\{linkID=167\}
\par \tab\cf3\strike TeleportVehicle routine\cf4\strike0\{linkID=169\}\cf0\b 
\par }
169
Scribble169
TeleportVehicle routine




Writing



FALSE
19
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 TeleportVehicle routine\cf0\b0\f1\fs20 
\par 
\par \f0 Teleports a specified vehicle to another location.
\par 
\par \b Syntax
\par \pard\tx540\b0 
\par \tab\cf2\f1 procedure teleportVehicle(which: TRpgVehicle; map, x, y: integer);\f0 
\par 
\par \cf0\b Description
\par 
\par \b0\tab This routine teleports the specified vehicle to a new location, as specified by the map ID # and coordinates passed to the routine.  It does nothing if the location is not within the map's boundaries.  It also does nothing if the vehicle is currently carrying the active party and the destination is on a different map; the \cf3\strike teleport\cf4\strike0\{linkID=168\}\cf0  routine should be used in this case.
\par 
\par \tab\b WARNING:\b0  The routine does not check whether or not the destination square is valid terrain for the vehicle.  It's possible to teleport the vehicle to invalid terrain, which could cause movement problems.
\par \b 
\par \pard\tx400\tx420 Related topics\b0 
\par \pard\tab\cf3\strike teleport routine\cf4\strike0\{linkID=168\}\cf0\f1 
\par }
170
Scribble170
TRpgVehicle Class




Writing



FALSE
11
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgVehicle Class\cf0\b0\f1\fs20 
\par 
\par \pard\tx540\f0\tab TRpgVehicle is a class that represents the in-game vehicles.  Three TRpgVehicle objects are automatically created during initialization: a boat, a ship and an airship.  Descends from \cf2\strike TRpgCharacter.\cf3\strike0\{linkID=220\}\cf0 
\par \pard 
\par \cf3\f1\{button Properties,\f0 17\f1 1\}\f0   \{button Methods,172\}
\par 
\par \cf2\strike Back to RPG Script Reference\cf3\strike0\{linkID=60\}\cf0\f1 
\par \cf2\strike\f0 Explanation of Object Pascal types\cf3\strike0\{linkID=51\}\cf0\f1 
\par }
171
Scribble171
TRpgVehicle Properties




Writing



FALSE
30
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgVehicle Properties\cf0\b0\f1\fs20 
\par 
\par \cf2\strike\f0 Back to TRpgVehicle\cf3\strike0\{linkID=170\}\cf0 
\par \cf2\strike Back to RPG Script Reference\cf3\strike0\{linkID=60\}\cf0\f1 
\par \cf2\strike\f0 Explanation of Object Pascal types\cf3\strike0\{linkID=51\}
\par 
\par 
\par \f1\{html=\cf0 <table border="1"><tbody>
\par <tr><td><b>Name</b></td>
\par <td><b>Type</b></td>
\par <td><b>Description</b></td></tr>
\par \cf3\f0 <tr><td>map</td>
\par <td>smallint</td>
\par <td>The ID number of the map the vehicle's on.</td></tr>
\par <tr><td>x</td>
\par <td>word</td>
\par <td>The x-coordinate of the vehicle's current location on the map.</td></tr>
\par <tr><td>y</td>
\par <td>word</td>
\par <td>The y-coordinate of the vehicle's current location on the map.</td></tr>
\par <tr><td>facing</td>
\par <td>byte</td>
\par <td>A number designating which direction the vehicle is facing.&nbsp; The direction values are: down: 2, left: 4, right: 6, up: 8.</td></tr>
\par <tr><td>inUse</td>
\par <td>boolean</td>
\par <td>Shows whether the vehicle is currently active.</td></tr>
\par \cf0\f1 </tbody></table>\f0\}
\par }
172
Scribble172
TRpgVehicle Methods




Writing



FALSE
9
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgVehicle Methods\cf0\b0\f1\fs20 
\par 
\par \pard\tx1440\cf2\strike\f0 Back to TRpgVehicle\cf3\strike0\{linkID=170\}\cf0 
\par \cf2\strike Back to RPG Script Reference\cf3\strike0\{linkID=60\}\cf0\f1 
\par 
\par \cf2\b\strike\f0 setSprite:\cf3\strike0\{linkID=173\}\cf0\b0\tab Changes the vehicle's current graphic.\f1 
\par }
173
Scribble173
TRpgVehicle.setSprite




Writing



FALSE
14
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue128;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgVehicle.setSprite\cf0\b0\f1\fs20 
\par 
\par \pard\tx420\b\f0 Class
\par \tab\cf2\b0\strike TRpgVehicle\cf3\strike0\{linkID=170\}\cf0\b 
\par 
\par Syntax\b0 
\par \tab\cf4 procedure setSprite(filename: string; index: byte);
\par \cf0 
\par \b Description
\par \b0\tab This method changes the vehicle's current sprite (graphics set).  It first checks to see if \b index\b0  falls within the acceptable range (1..8) and if a CharSet file is available with the name given by \b filename\b0 .  If either test fails, nothing happens.  Otherwise, it sets the vehicle's sprite to the appropriate sprite from that charset.
\par \pard\f1 
\par }
180
Scribble180
TeleportEvent routine




Writing



FALSE
18
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 TeleportEvent routine\cf0\b0\f1\fs20 
\par 
\par \pard\tx520\f0 Teleports a specified event to another location on the map.
\par \pard 
\par \b Syntax
\par \pard\tx540\b0 
\par \tab\cf2\f1 procedure teleportEvent(which: TRpgEvent; x, y: integer);\f0 
\par 
\par \cf0\b Description
\par 
\par \b0\tab This routine teleports the event specified by \b which\b0  to a new location, specified by the coordinates passed to the routine.  Events can only be teleported within the current map.  If the location is not within the boundaries of the map, it does nothing.
\par 
\par \pard\tx400\tx420\b Related topics\b0 
\par \pard\tx540\tab\cf3\strike Teleport routine\cf4\strike0\{linkID=168\}
\par \pard\tx520\tab\cf3\strike SwapEvents routine\cf4\strike0\{linkID=181\}\cf0\f1 
\par }
181
Scribble181
SwapEvents routine




Writing



FALSE
18
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 SwapEvents routine\cf0\b0\f1\fs20 
\par 
\par \f0 Exchanges the position of two events on the map.
\par 
\par \b Syntax
\par \pard\tx540\b0 
\par \tab\cf2\f1 procedure swapEvents(first, second: TR\f0 p\f1 gEvent);\f0 
\par 
\par \cf0\b Description
\par 
\par \b0\tab This routine swaps the position of the events passed to \b first\b0  and \b second\b0 .
\par 
\par \pard\tx400\tx420\b Related topics\b0 
\par \pard\tx540\tab\cf3\strike Teleport routine\cf4\strike0\{linkID=168\}\cf0\f1 
\par \pard 
\par }
182
Scribble182
RideVehicle routine




Writing



FALSE
19
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 RideVehicle routine\cf0\b0\f1\fs20 
\par 
\par \pard\tx520\f0 Causes the party to board/leave a vehicle.
\par \pard\b 
\par Syntax
\par \pard\tx540\b0 
\par \tab\cf2\f1 procedure \f0 rideVehicle;
\par 
\par \cf0\b Description
\par 
\par \b0\tab This routine causes the party to ride a vehicle in either the current square or the square immediately in front of the party, or to attempt to leave the vehicle if the party is already aboard a vehicle.  If no vehicle is available, the routine does nothing.
\par 
\par \pard\tx400\tx420\b Related topics\b0 
\par \pard\tx540\tab\cf3\strike Teleport routine\cf4\strike0\{linkID=168\}\cf0\f1 
\par \pard 
\par 
\par }
183
Scribble183
GetTerrainID routine




Writing



FALSE
18
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 GetTerrainID routine\cf0\b0\f1\fs20 
\par 
\par \f0 Returns the ID # of the map terrain at a certain square.
\par 
\par \b Syntax
\par \pard\tx540\b0 
\par \tab\cf2\f1 function getTerrainID(x, y: word): word;\f0 
\par 
\par \cf0\b Description
\par 
\par \b0\tab This function is used to find the terrain type from the database of the lower tile on the map at grid position \b x,y\b0 .  If \b x,y\b0  is not a valid location, the function returns a value of 0.
\par 
\par \pard\tx400\tx420\b Related topics\b0 
\par \pard\tx540\tab\cf3\strike getEventID routine\cf4\strike0\{linkID=184\}\cf0\f1 
\par \pard 
\par }
184
Scribble184
GetEventID routine




Writing



FALSE
18
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 GetEventID routine\cf0\b0\f1\fs20 
\par 
\par \pard\tx520\f0 Returns the ID # of the event at a certain square.\f1 
\par \pard\f0 
\par \b Syntax
\par \pard\tx540\b0 
\par \tab\cf2\f1 function get\f0 Event\f1 ID(x, y: word): word;\f0 
\par 
\par \cf0\b Description
\par 
\par \b0\tab This function is used to find the ID # of the event on the map at grid position \b x,y\b0 .  If \b x,y\b0  is not a valid location, orif there is no event at that square, the function returns a value of 0.
\par 
\par \pard\tx400\tx420\b Related topics\b0 
\par \pard\tx540\tab\cf3\strike getTerrainID routine\cf4\strike0\{linkID=183\}\cf0\f1 
\par \pard 
\par }
185
Scribble185
EraseScreen routine




Writing



FALSE
19
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 EraseScreen routine\cf0\b0\f1\fs20 
\par 
\par \f0 Clears the screen, optionally using a system transition.
\par \b 
\par Syntax
\par \pard\tx540\b0 
\par \tab\cf2\f1 procedure eraseScreen(whichTransition: TTransitions);\f0 
\par 
\par \cf0\b Description
\par 
\par \b0\tab This routine erases the screen, using the method specified by \b whichTransition\b0 .  If \cf2 trnDefault\cf0  is passed to \b whichTransition\b0 , the default map exit transition is used.  The routine does nothing if the screen is already blank.
\par 
\par \pard\tx400\tx420\b Related topics\b0 
\par \pard\tx540\tab\cf3\strike showScreen routine\cf4\strike0\{linkID=186\}\cf0\f1 
\par \f0\tab\cf3\strike setTransition routine\cf4\strike0\{linkID=187\}\cf0\f1 
\par \f0\tab\cf3\strike TTransitions Type\cf4\strike0\{linkID=115\}\cf0\f1 
\par }
186
Scribble186
ShowScreen Routine




Writing



FALSE
19
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 ShowScreen Routine\cf0\b0\f1\fs20 
\par 
\par \f0 Restores the screen after an erase, optionally using a system transition.
\par \b 
\par Syntax
\par \pard\tx540\b0 
\par \tab\cf2\f1 procedure \f0 show\f1 Screen(whichTransition: TTransitions);\f0 
\par 
\par \cf0\b Description
\par 
\par \b0\tab This routine restores the screen from the blank state set by the \cf3\strike eraseScreen\cf4\strike0\{linkID=186\}\cf0  routine, using the method specified by \b whichTransition\b0 .  If \cf2 trnDefault\cf0  is passed to \b whichTransition\b0 , the default map entrance transition is used.  The routine does nothing if the screen is not already blank.
\par 
\par \pard\tx400\tx420\b Related topics\b0 
\par \pard\tx540\tab\cf3\strike eraseScreen routine\cf4\strike0\{linkID=185\}\cf0\f1 
\par \f0\tab\cf3\strike setTransition routine\cf4\strike0\{linkID=187\}\cf0\f1 
\par \f0\tab\cf3\strike TTransitions Type\cf4\strike0\{linkID=115\}\cf0\f1 
\par }
187
Scribble187
SetTransition routine




Writing



FALSE
21
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green0\blue0;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 SetTransition routine\cf0\b0\f1\fs20 
\par 
\par \f0 Changes the default transition settings.
\par \b 
\par Syntax
\par \pard\tx540\b0 
\par \tab\cf2\f1 procedure setTransition(const which: TTransitionTypes; const newTransition: TTransitions);\f0 
\par 
\par \cf0\b Description
\par 
\par \b0\tab This routine changes the default system transition specified by \b which\b0  to the value passed to \b newTransition\b0 .  It does nothing if \cf2 trnDefault\cf3  is passed.\cf0 
\par 
\par \pard\tx400\tx420\b Related topics\b0 
\par \pard\tx540\tab\cf4\strike eraseScreen routine\cf5\strike0\{linkID=185\}\cf0\f1 
\par \f0\tab\cf4\strike showScreen routine\cf5\strike0\{linkID=186\}\cf0\f1 
\par \f0\tab\cf4\strike TTransitions Type\cf5\strike0\{linkID=115\}\cf0\f1 
\par \f0\tab\cf4\strike TTransitionTypes Type\cf5\strike0\{linkID=116\}\cf0\f1 
\par 
\par }
188
Scribble188
SetScreenTone routine




Writing



FALSE
22
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 SetScreenTone routine\cf0\b0\f1\fs20 
\par 
\par \f0 Changes the overall color of the screen.
\par 
\par \b Syntax
\par \pard\tx540\b0 
\par \tab\cf2\f1 procedure setScreenTone(r, g, b, sat: byte; duration: cardinal; wait: boolean);\f0 
\par 
\par \cf0\b Description
\par 
\par \b0\tab This routine shades the entire map.  The first four parameters represent percentages, and can be anywhere from 0-200.  (Any value larger than 200 is treated as a 200).  The \b r\b0 , \b g\b0 , and \b b\b0  parameters refer to the red, green and blue components of the shading.  The \b sat\b0  parameter refers to the overall color saturation of the map.  The transition will take place gradually, over a period of \b duration\b0  miliseconds.  If \cf2 true\cf0  is passed to the \b wait\b0  parameter, the script's execution will pause until the transition is finished.
\par \tab This routine only shades the map and the characters and events on it.  Menus and message boxes aren't shaded.
\par \tab\b NOTE:\b0   The Fade Out screen transition, as well as other script functions that may make use of fade-outs (such as the \cf3\strike Inn routine\cf4\strike0\{linkID=139\}\cf0 ) will fade from the current screen tone to black.  The previous screen tone will be lost, and it is the designer's responsibility to restore it when the screen returns to normal.\tab 
\par \tab\b NOTE:\b0  This routine is not yet fully implemented.  The \b sat\b0  parameter currently does nothing, due to technical limitations.  Saturation changes will be implemented during the planned graphics engine rewrite.
\par 
\par \pard\tx400\tx420\b Related topics\b0 
\par \pard\tx540\tab\cf3\strike flashScreen routine\cf4\strike0\{linkID=188\}\cf0\f1 
\par 
\par \pard 
\par }
189
Scribble189
FlashScreen routine




Writing



FALSE
21
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 FlashScreen routine\cf0\b0\f1\fs20 
\par 
\par \pard\tx520\f0 Flashes the screen a certain color.
\par 
\par \pard\b Syntax
\par \pard\tx540\b0 
\par \tab\cf2\f1 procedure flashScreen(r, g, b, power: byte; duration: cardinal; wait: boolean);\f0 
\par 
\par \cf0\b Description
\par 
\par \b0\tab This routine causes a flash to appear, briefly shading the entire map, then fading.  The first four parameters represent color values, and can be anywhere from 0-255.  The \b r\b0 , \b g\b0 , and \b b\b0  parameters refer to the red, green and blue components of the flash.  The \b power\b0  parameter refers to the initial opacity of the flash.  The flash color will appear on-screen immediately, then gradually fade out (become more transparent) over a period of \b duration\b0  miliseconds.  If \cf2 true\cf0  is passed to the \b wait\b0  parameter, the script's execution will pause until the flash has faded completely.
\par \tab This routine only flashes the map and the characters and events on it.  Menus and message boxes aren't included.
\par \tab\b NOTE:\b0   The routine uses different code than the \cf3\strike setScreenTone\cf4\strike0\{linkID=189\}\cf0  routine uses.  A screen tone and a flash can be on-screen at the same time.  If so, the flash is drawn on top of the already-shaded screen.
\par \pard\tx400\tx420 
\par \b Related topics\b0 
\par \pard\tx540\tab\cf3\strike setScreenTone routine\cf4\strike0\{linkID=188\}
\par \cf0\tab\cf3\strike TRpgCharacter.flash\cf4\strike0\{linkID=223\}\cf0\f1 
\par \pard 
\par }
190
Scribble190
LockScreen routine




Writing



FALSE
19
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 LockScreen routine\cf0\b0\f1\fs20 
\par 
\par \pard\tx520\f0 Locks the current screen position.
\par 
\par \pard\b Syntax
\par \pard\tx540\b0 
\par \tab\cf2\f1 procedure \f0 lockScreen\f1 ;\f0 
\par 
\par \cf0\b Description
\par 
\par \b0\tab This routine causes the screen to stop scrolling to follow the party's movements.  Teleporting will recenter the screen on the new location, but it will remain locked in place in the new position until \cf3\strike unlockScreen\cf4\strike0\{linkID=191\}\cf0  is called.  The screen can still be scrolled with the \cf3\strike panScreen\cf4\strike0\{linkID=192\}\cf0  routine while it is locked.
\par \pard\tx400\tx420 
\par \b Related topics\b0 
\par \pard\tx540\tab\cf3\strike unlockScreen routine\cf4\strike0\{linkID=191\}\cf0\f1 
\par \f0\tab\cf3\strike panScreen routine\cf4\strike0\{linkID=192\}\cf0\f1 
\par \pard 
\par }
191
Scribble191
UnlockScreen routine




Writing



FALSE
19
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 UnlockScreen routine\cf0\b0\f1\fs20 
\par 
\par \pard\tx520\f0 Unlocks the current screen position.
\par 
\par \pard\b Syntax
\par \pard\tx540\b0 
\par \tab\cf2\f1 procedure \f0 unlockScreen\f1 ;\f0 
\par 
\par \cf0\b Description
\par 
\par \b0\tab This routine causes the screen to go back to following the party as it moves, if it was previously locked by \cf3\strike lockScreen\cf4\strike0\{linkID=190\}\cf0 .
\par \pard\tx400\tx420 
\par \b Related topics\b0 
\par \pard\tx540\tab\cf3\strike lockScreen routine\cf4\strike0\{linkID=190\}\cf0\f1 
\par \f0\tab\cf3\strike panScreen routine\cf4\strike0\{linkID=192\}\cf0\f1 
\par \pard 
\par }
192
Scribble192
PanScreen routine




Writing



FALSE
20
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 PanScreen routine\cf0\b0\f1\fs20 
\par 
\par \f0 Pans the screen from the current camera position.
\par 
\par \b Syntax
\par \pard\tx540\b0 
\par \tab\cf2\f1 procedure panScreen(direction: TDirections; distance: word; speed: byte; wait: boolean);\f0 
\par 
\par \cf0\b Description
\par 
\par \b0\tab This routine causes the screen to move away from the current position, in the direction specified by \b direction\b0 , by \b distance\b0  number of squares.  The \b speed\b0  parameter indicates the speed at which the screen scrolls, from 1 (slowest) to 6 (fastest).  Passing 0 or any other invalid value causes the scroll speed to remain unchanged.  If \cf2 true\cf0  is passed to the \b wait\b0  parameter, the script's execution will pause until the pan is complete.
\par \pard\tx400\tx420 
\par \b Related topics\b0 
\par \pard\tx540\tab\cf3\strike TDirections Type\cf4\strike0\{linkID=117\}\cf0\f1 
\par \f0\tab\cf3\strike panScreenTo routine\cf4\strike0\{linkID=193\}\cf0\f1 
\par \f0\tab\cf3\strike returnScreen routine\cf4\strike0\{linkID=194\}\cf0\f1 
\par \pard 
\par }
193
Scribble193
PanScreenTo routine




Writing



FALSE
19
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 PanScreenTo routine\cf0\b0\f1\fs20 
\par 
\par \f0 Pans the screen to a certain position.
\par 
\par \b Syntax
\par \pard\tx540\b0 
\par \tab\cf2\f1 procedure panScreenTo(x, y: word; speed: byte; wait: boolean);\f0 
\par 
\par \cf0\b Description
\par 
\par \b0\tab This routine causes the screen to move to recenter itself around the point specified by the coordinates \b x\b0  and \b y\b0 .  The \b speed\b0  parameter indicates the speed at which the screen scrolls, from 1 (slowest) to 6 (fastest).  Passing 0 or any other invalid value causes the scroll speed to remain unchanged.  If \cf2 true\cf0  is passed to the \b wait\b0  parameter, the script's execution will pause until the pan is complete.
\par \pard\tx400\tx420 
\par \b Related topics\b0 
\par \pard\tx540\tab\cf3\strike panScreen routine\cf4\strike0\{linkID=192\}\cf0\f1 
\par \f0\tab\cf3\strike returnScreen routine\cf4\strike0\{linkID=194\}\cf0\f1 
\par \pard 
\par }
194
Scribble194
ReturnScreen routine




Writing



FALSE
20
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 ReturnScreen routine\cf0\b0\f1\fs20 
\par 
\par \f0 Causes the screen to return to the current party.
\par 
\par \b Syntax
\par \pard\tx540\b0 
\par \tab\cf2\f1 procedure returnScreen(speed: byte; wait: boolean);\f0 
\par 
\par \cf0\b Description
\par 
\par \b0\tab This routine causes the screen to move to recenter itself around the current party after having been moved away from it with the \cf3\strike panScreen\cf4\strike0\{linkID=192\}\cf0  or \cf3\strike panScreenTo\cf4\strike0\{linkID=193\}\cf0  routines.  The \b speed\b0  parameter indicates the speed at which the screen scrolls, from 1 (slowest) to 6 (fastest).  Passing 0 or any other invalid value causes the scroll speed to remain unchanged.  If \cf2 true\cf0  is passed to the \b wait\b0  parameter, the script's execution will pause until the pan is complete.
\par \pard\tx400\tx420 
\par \b Related topics\b0 
\par \pard\tx540\tab\cf3\strike panScreen routine\cf4\strike0\{linkID=192\}\cf0\f1 
\par \f0\tab\cf3\strike panScreenTo routine\cf4\strike0\{linkID=193\}\cf0\f1 
\par \pard 
\par 
\par }
195
Scribble195
SetWeather routine




Writing



FALSE
20
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 SetWeather routine\cf0\b0\f1\fs20 
\par 
\par \pard\tx520\f0 Sets the current weather effect.
\par \pard 
\par \b Syntax
\par \pard\tx540\b0 
\par \tab\cf2\f1 procedure setWeather(effect: TWeatherEffects; severity: byte);\f0 
\par 
\par \cf0\b Description
\par 
\par \b0\tab This routine sets the state of the weather effect system.  The \b effect\b0  parameter specifies \cf3\strike which effect to set\cf4\strike0\{linkID=118\}\cf0 , and \b severity\b0  describes how strong the weather should be, from 0 (none) to 10 (strong).  Any value higher than 10 is treated as a 10.  If \cf2 we_off\cf0  is passed to \b effect\b0 , then the weather is turned off and \b severity\b0  has no effect.
\par \tab\b NOTE:\b0  Any weather effect will continue to display on the map, independently of any other changes.  This includes teleport events that change the scene to a different map.
\par \pard\tx400\tx420 
\par \b Related topics\b0 
\par \pard\tx540\tab\cf3\strike TWeatherEffects Type\cf4\strike0\{linkID=118\}\cf0\f1 
\par \f0\tab\cf3\strike increaseWeather routine\cf4\strike0\{linkID=196\}
\par \cf0\tab\cf3\strike decreaseWeather routine\cf4\strike0\{linkID=197\}\cf0\f1 
\par }
196
Scribble196
IncreaseWeather routine




Writing



FALSE
19
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 IncreaseWeather routine\cf0\b0\f1\fs20 
\par 
\par \pard\tx520\f0 Increases the severity of the current weather effect.
\par \pard 
\par \b Syntax
\par \pard\tx540\b0 
\par \tab\cf2\f1 procedure \f0 increase\f1 Weather;\f0 
\par 
\par \cf0\b Description
\par 
\par \b0\tab This routine increases the strength of the active weather effect by 1, up to a maximum value of 10.  It has no effect if the weather is currently turned off.
\par \pard\tx400\tx420 
\par \b Related topics\b0 
\par \pard\tx540\tab\cf3\strike setWeather routine\cf4\strike0\{linkID=195\}
\par \cf0\tab\cf3\strike decreaseWeather routine\cf4\strike0\{linkID=197\}\cf0\f1 
\par \pard 
\par }
197
Scribble197
DecreaseWeather routine




Writing



FALSE
18
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 DecreaseWeather routine\cf0\b0\f1\fs20 
\par 
\par \pard\tx520\f0 Decreases the severity of the current weather effect.
\par \pard 
\par \b Syntax
\par \pard\tx540\b0 
\par \tab\cf2\f1 procedure \f0 decrease\f1 Weather;\f0 
\par 
\par \cf0\b Description
\par 
\par \b0\tab This routine decreases the strength of the active weather effect by 1, down to a minimum value of 0.  It has no effect if the weather is currently turned off.
\par \pard\tx400\tx420 
\par \b Related topics\b0 
\par \pard\tx540\tab\cf3\strike setWeather routine\cf4\strike0\{linkID=195\}
\par \cf0\tab\cf3\strike increaseWeather routine\cf4\strike0\{linkID=196\}\cf0\f1 
\par }
198
Scribble198
Wait routine




Writing



FALSE
15
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;}
\viewkind4\uc1\pard\cf1\b\fs32 Wait routine\cf0\b0\f1\fs20 
\par 
\par \pard\tx520\f0 Pauses the script execution.
\par \pard 
\par \b Syntax
\par \pard\tx540\b0 
\par \tab\cf2\f1 procedure wait(time: cardinal);\f0 
\par 
\par \cf0\b Description
\par 
\par \b0\tab This routine causes the script's execution to pause for \b time\b0  miliseconds.
\par 
\par }
199
Scribble199
NewImage Routine




Writing



FALSE
20
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 NewImage Routine\cf0\b0\f1\fs20 
\par 
\par \f0 Sets up a new on-screen image.
\par 
\par \b Syntax
\par \pard\tx540\b0 
\par \tab\cf2\f1 function newImage(name: string; x, y: integer; zoom, transparency: word; pinned, mask: boolean): TRpgImage;\f0 
\par 
\par \cf0\b Description
\par 
\par \b0\tab This routine creates a new on-screen image, which is loaded from the file named in the \b name\b0  parameter.  No path or file extension is necessary; the engine will automatically search in the appropriate folders for on-screen images, and check all valid image extensions.
\par \tab The image is centered around the point (\b x\b0 , \b y\b0 ) on the screen.  The \b zoom\b0  parameter denotes what percentage of the image's original size it will be displayed as on-screen, and \b transparency\b0  is also a percent value, from 0 (completely opaque) to 100 (completely transparent and invisible.)  If \cf2 true\cf0  is passed to \b pinned\b0 , the image will be "pinned to the camera" and always remain in the same position on-screen.  Otherwise, it will be located on the map, and move on-screen when the map scrolls.  The \b mask\b0  parameter specifies whether or not to use a "mask color" for image transparency.
\par \tab If \b name\b0  is not a valid filename, the image created will be invisible, with a size of 0x0.
\par \pard\tx400\tx420 
\par \b Related topics\b0 
\par \pard\tx540\tab\cf3\strike TRpgImage Class\cf4\strike0\{linkID=200\}\cf0\f1 
\par \pard 
\par }
200
Scribble200
TRpgImage Class




Writing



FALSE
11
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red128\green0\blue0;\red0\green128\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgImage Class\cf0\b0\f1\fs20 
\par 
\par \pard\tx540\f0\tab TRpgImage is a class that represents an on-screen image.
\par \pard 
\par \cf2\f1\{button Properties,\f0 20\f1 1\}\f0   \{button Methods,202\}
\par 
\par \cf3\strike Back to RPG Script Reference\cf2\strike0\{linkID=60\}\cf0\f1 
\par \cf3\strike\f0 Explanation of Object Pascal types\cf2\strike0\{linkID=51\}\cf0\f1 
\par }
201
Scribble201
TRpgImage Properties




Writing



FALSE
24
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgImage Properties\cf0\b0\f1\fs20 
\par 
\par \cf2\strike\f0 Back to TRpgImage\cf3\strike0\{linkID=200\}\cf0 
\par \cf2\strike Back to RPG Script Reference\cf3\strike0\{linkID=60\}\cf0\f1 
\par \cf2\strike\f0 Explanation of Object Pascal types\cf3\strike0\{linkID=51\}\cf0 
\par 
\par 
\par \{html=<table border="1">
\par <tbody>
\par <tr>
\par <td><b>Name</b></td>
\par <td><b>Type</b></td>
\par <td><b>Description</b></td></tr>
\par <tr><td>zoom</td>
\par <td>word</td>
\par <td>Percent value describing how large the image appears on-screen</td></tr>
\par <tr><td>timer</td>
\par <td>cardinal</td>
\par <td>Length of time that image transitions will take.</td></tr>
\par </tbody></table>\}\f1 
\par 
\par }
202
Scribble202
TRpgImage Methods




Writing



FALSE
12
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgImage Methods\cf0\b0\f1\fs20 
\par 
\par \pard\tx1440\cf2\strike\f0 Back to TRpgImage\cf3\strike0\{linkID=200\}\cf0 
\par \cf2\strike Back to RPG Script Reference\cf3\strike0\{linkID=60\}\cf0\f1 
\par 
\par \pard\tx2900\cf2\b\strike\f0 applyImageColors:\cf3\strike0\{linkID=203\}\cf0\b0\tab Changes the shading of the image.
\par \cf2\b\strike applyImageEffect:\cf3\strike0\{linkID=204\}\cf0\b0\tab Applies a graphical effect to the image.
\par \cf2\b\strike moveTo:\cf3\strike0\{linkID=205\}\cf0\b0\tab Causes the image to move, resize itself, or change transparency.
\par \pard\tx1440\tx3120\f1 
\par }
203
Scribble203
TRpgImage.applyImageColors




Writing



FALSE
16
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue128;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgImage.applyImageColors\cf0\b0\f1\fs20 
\par 
\par \pard\tx420\b\f0 Class
\par \tab\cf2\b0\strike TRpgImage\cf3\strike0\{linkID=200\}\cf0\b 
\par 
\par Syntax\b0 
\par \tab\cf4 procedure applyImageColors(r, g, b, sat: byte);
\par \cf0 
\par \b Description
\par \b0\tab This method changes the image's shading.  The \b r\b0 , \b g\b0 , and \b b\b0  parameters affect red, green and blue color balance, and accept percentage values from 0-200.  Any number higher than 200 will be treated as a 200.  The \b sat\b0  parameter affects color saturation of the overall image, from 0 (completely gray) to 200 (exaggeratedly vibrant colors).
\par \tab The values entered in this routine will be gradually changed over the period of time set in the object's \b timer\b0  property.  If \b timer\b0  is currently at 0, or has not yet been set, the changes will take place immediately.
\par \tab\b NOTE:\b0  This routine is not yet fully implemented.  The \b sat\b0  parameter currently does nothing, due to technical limitations.  Saturation changes will be implemented during the planned graphics engine rewrite.
\par \f1 
\par }
204
Scribble204
TRpgImage.applyImageEffect




Writing



FALSE
20
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue128;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgImage.applyImageEffect\cf0\b0\f1\fs20 
\par 
\par \pard\tx420\b\f0 Class
\par \tab\cf2\b0\strike TRpgImage\cf3\strike0\{linkID=200\}\cf0\b 
\par 
\par Syntax\b0 
\par \tab\cf4 procedure applyImageEffect(which: TImageEffects; power: byte);
\par \cf0 
\par \b Description
\par \b0\tab This method applies a graphical effect to the image, specified by the \b which\b0  parameter.  The \b power\b0  parameter gives the magnitude of the effect, from 0 to 10.  Any value higher than 10 will be treated as a 10.
\par \tab The \cf4 ie_rotate\cf0  effect will cause the image to spin continually on-screen, while passing \cf4 ie_wave\cf0  to \b which\b0  will cause the image to ripple.
\par \tab The values entered in this routine will be gradually changed over the period of time set in the object's \b timer\b0  property.  If \b timer\b0  is currently at 0, or has not yet been set, the changes will take place immediately.  Passing \cf4 ie_none\cf0  to \b which\b0  will deactivate all graphical effects immediately.  To deactivate an effect gradually, pass that effect with a \b power\b0  value of 0.
\par \tab\b NOTE:\b0   It is possible to set both image effects at the same time.  Due to technical limitations, only one can be displayed at the present time.  This will be changed in the future.  But for now, the ripple effect will take priority over the rotate effect.
\par \pard\b 
\par Related topics
\par \pard\tx400\b0\tab\cf2\strike TImageEffects Type\cf3\strike0\{linkID=119\}
\par \pard\tx420\cf0\f1 
\par }
205
Scribble205
TRpgImage.moveTo




Writing



FALSE
14
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue128;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgImage.moveTo\cf0\b0\f1\fs20 
\par 
\par \pard\tx420\b\f0 Class
\par \tab\cf2\b0\strike TRpgImage\cf3\strike0\{linkID=200\}\cf0\b 
\par 
\par Syntax\b0 
\par \tab\cf4 procedure moveTo(x, y: integer; zoom, opacity: word);
\par \cf0 
\par \b Description
\par \b0\tab This method causes the image to move on-screen to the destination specified by (\b x\b0 , \b y\b0 ).  It will also change the image's \b zoom\b0  property and \b opacity\b0  with the appropriate parameters.
\par \tab The values entered in this routine will be gradually changed over the period of time set in the object's \b timer\b0  property.  If \b timer\b0  is currently at 0, or has not yet been set, the changes will take place immediately.\f1 
\par }
206
Scribble206
TRpgImage.erase




Writing



FALSE
14
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue128;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgImage.erase\cf0\b0\f1\fs20 
\par 
\par \pard\tx420\b\f0 Class
\par \tab\cf2\b0\strike TRpgImage\cf3\strike0\{linkID=200\}\cf0\b 
\par 
\par Syntax\b0 
\par \tab\cf4 procedure erase;
\par \cf0 
\par \b Description
\par \b0\tab This method erases an on-screen image.  After calling it, the image's variable will no longer be valid.
\par \tab\b NOTE:\b0   Calling \b erase\b0  on \cf2\i\strike image[0]\cf3\strike0\{linkID=61\}\cf0\i0  has no effect, to avoid potential memory corruption.
\par }
210
Scribble210
SetBGImage routine




Writing



FALSE
16
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;}
\viewkind4\uc1\pard\cf1\b\fs32 SetBGImage routine\cf0\b0\f1\fs20 
\par 
\par \pard\tx520\f0 Changes the current background image.
\par \pard 
\par \b Syntax
\par \pard\tx540\b0 
\par \tab\cf2\f1 procedure setBGImage(name: string; scrollX, scrollY: shortint; autoX, autoY: boolean);\f0 
\par 
\par \cf0\b Description
\par 
\par \b0\tab This routine sets a new background image, which is loaded from the file named in the \b name\b0  parameter.  No path or file extension is necessary; the engine will automatically search in the appropriate folders for background images, and check all valid image extensions.
\par \tab If the \b scrollX\b0  and \b scrollY\b0  parameters are not 0, the background image will continually be in motion.  A positive \b scrollX\b0  causes it to scroll to the right; negative \b scrollX\b0  makes it scroll to the left.  Likewise, positive \b scrollY\b0  causes downward movement, and negative \b scrollY\b0  will make the image scroll upwards.
\par \tab If \cf2 true\cf0  is passed to the \b autoX\b0  or \b autoY\b0  parameters, their scrolling in the corresponding direction will automatically be 1/2 of the camera's movement distance.  This overrides the values of \b scrollX\b0  and \b scrollY\b0 .  Auto-scrolling can be used to create a "parallax" effect.\f1 
\par }
211
Scribble211
PlayMusic routine




Writing



FALSE
20
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 PlayMusic routine\cf0\b0\f1\fs20 
\par 
\par \pard\tx520\f0 Changes the background music.
\par \pard\b 
\par Syntax
\par \pard\tx540\b0 
\par \tab\cf2\f1 procedure playMusic(name: string; time, volume, tempo, balance: word);\f0 
\par 
\par \cf0\b Description
\par 
\par \b0\tab This routine changes the current background music to the song specified by \b name\b0 .  No path or file extension is necessary; the engine will automatically search in the appropriate folders for background music, and check all valid music extensions.  If the \b time\b0  parameter is greater than 0, the song will fade in over that many milliseconds.  The \b volume\b0  parameter tells what percentage of the maximum volume the song will play at.
\par \tab\b NOTE:\b0   The \b tempo\b0  and \b balance\b0  parameters are not currently implemented, due to technical limitations.
\par \tab\b WARNING:\b0   Playback of certain MIDI files will be delayed by the \cf3\strike MIDI lead-in issue.\cf4\strike0\{linkID=63\}
\par 
\par \cf0\b Related Topics\b0 
\par \tab\cf3\strike fadeOutMusic routine\cf4\strike0\{linkID=212\}
\par \tab\cf3\strike memorizeBgm routine\cf4\strike0\{linkID=214\}\cf0\f1 
\par }
212
Scribble212
FadeOutMusic routine




Writing



FALSE
18
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red128\green0\blue0;\red0\green128\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 FadeOutMusic routine\cf0\b0\f1\fs20 
\par 
\par \pard\tx520\f0 Causes the background music to gradually fade out.
\par \pard\b 
\par Syntax
\par \pard\tx540\b0 
\par \tab\cf2\f1 procedure fadeOutMusic(time: word);\f0 
\par 
\par \cf0\b Description
\par 
\par \b0\tab This routine causes the current background music to fade to silence, over a period of \b time\b0  miliseconds.  Passing 0 to \b time\b0  will immediately stop the background music.
\par \cf3 
\par \cf0\b Related Topics\b0 
\par \tab\cf4\strike playMusic routine\cf3\strike0\{linkID=211\}\cf0\f1 
\par \pard 
\par }
213
Scribble213
ShowBattleAnim routine




Writing



FALSE
16
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 ShowBattleAnim routine\cf0\b0\f1\fs20 
\par 
\par \f0 Displays a battle animation on-screen.
\par 
\par \b Syntax
\par \pard\tx540\b0 
\par \tab\cf2\f1 procedure showBattleAnim(which: word; target: TRpgCharacter; wait, fullscreen: boolean);\f0 
\par 
\par \cf0\b Description
\par 
\par \b0\tab This routine plays a battle animation from the database.  The \b which\b0  parameter indicates the ID number of the animation to play, and \b target\b0  specifies the target of the animation.  If \cf2 true\cf0  is passed to \b wait\b0 , the script's execution will pause until the animation is finished, and if \b fullscreen\b0  is \cf2 true\cf0 , the animation will center itself at the center of the screen instead of on the target character, although a valid \b target\b0  parameter is still necessary for proper script execution.
\par \tab If \b which\b0  is not a valid animation ID, \b target\b0  is not a valid character, or the graphics filename listed in the database for the animation does not refer to a valid image file, no animation will play.\cf3 
\par \pard\cf0\f1 
\par }
214
Scribble214
MemorizeBgm routine




Writing



FALSE
18
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red128\green0\blue0;\red0\green128\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 MemorizeBgm routine\cf0\b0\f1\fs20 
\par 
\par \pard\tx520\f0 Causes the system to remember the current background music.
\par \pard\b 
\par Syntax
\par \pard\tx540\b0 
\par \tab\cf2\f1 procedure memorizeBgm;\f0 
\par 
\par \cf0\b Description
\par 
\par \b0\tab This routine stores the current BGM in the system's memory.  This makes it easy to change to another song temporarily and then change back, without having to keep track of the current song.
\par \cf3 
\par \cf0\b Related Topics\b0 
\par \tab\cf4\strike playMusic routine\cf3\strike0\{linkID=211\}
\par \tab\cf4\strike playMemorizedBgm routine\cf3\strike0\{linkID=215\}\cf1\b\f1\fs32 
\par }
215
Scribble215
PlayMemorizedBgm routine




Writing



FALSE
18
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 PlayMemorizedBgm routine\cf0\b0\f1\fs20 
\par 
\par \pard\tx520\f0 Causes the system to play previously memorized background music.
\par \pard\b 
\par Syntax
\par \pard\tx540\b0 
\par \tab\cf2\f1 procedure playMemorizedBgm;\f0 
\par 
\par \cf0\b Description
\par 
\par \b0\tab This routine starts playing the BGM that was memorized by the most recent call to the \cf3\strike memorizeBgm routine\cf4\strike0\{linkID=214\}\cf0 .  If \cf2 memorizeBgm\cf0  has not been called yet, it does nothing.
\par \cf4 
\par \cf0\b Related Topics\b0 
\par \tab\cf3\strike playMusic routine\cf4\strike0\{linkID=211\}
\par \tab\cf3\strike memorizeBgm routine\cf4\strike0\{linkID=214\}\cf0\f1 
\par }
216
Scribble216
PlaySound routine




Writing



FALSE
17
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;}
\viewkind4\uc1\pard\cf1\b\fs32 PlaySound routine\cf0\b0\f1\fs20 
\par 
\par \pard\tx520\f0 Plays a sound effect.
\par \pard\b 
\par Syntax
\par \pard\tx540\b0 
\par \tab\cf2\f1 procedure playSound(name: string; volume, tempo, balance: word);\f0 
\par 
\par \cf0\b Description
\par 
\par \b0\tab This routine plays the sound effect specified by \b name\b0 .  No path or file extension is necessary; the engine will automatically search in the appropriate folders for sound effects, and check all valid sound extensions.  The \b volume\b0  parameter tells what percentage of the maximum volume the song will play at, and the \b balance\b0  parameter defines left/right balance, from 0 (full left) to 100 (full right).
\par \tab If \b name\b0  does not refer to a valid sound effect, no sound is played.
\par \tab\b NOTE:\b0   The \b tempo\b0  parameter is not currently implemented, due to technical limitations.
\par \b 
\par }
217
Scribble217
WaitUntilMoved routine




Writing



FALSE
21
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 WaitUntilMoved routine\cf0\b0\f1\fs20 
\par 
\par \pard\tx520\f0 Pauses the script until all characters have finished their move scripts.
\par \pard\b 
\par Syntax
\par \pard\tx540\b0 
\par \tab\cf2\f1 procedure waitUntilMoved;\f0 
\par 
\par \cf0\b Description
\par 
\par \b0\tab This routine pauses the current script until all characters on the map with assigned move scripts have completed the move script at least once.  Characters that move by their default movement parameters are not included.
\par \tab\b NOTE:\b0  If another script calls \cf3\strike stopMoveScripts\cf4\strike0\{linkID=217\}\cf0  while waitUntilMoved is waiting, the waiting will immediately end, as all assigned move scripts will be canceled.
\par \tab\b WARNING:\b0   If any character is running a move script with the \b ignore\b0  parameter set to \cf2 false\cf0 , it is possible for the waitUntilMoved routine to deadlock a script indefinitely if the character runs into an obstacle that does not move.
\par \pard\f1 
\par \b\f0 Related topics
\par \pard\tx400\b0\tab\cf3\strike stopMoveScripts routine\cf4\strike0\{linkID=218\}\cf0 
\par \tab\cf3\strike TRpgCharacter.move\cf4\strike0\{linkID=224\}\cf0\f1 
\par \pard 
\par }
218
Scribble218
StopMoveScripts routine




Writing



FALSE
19
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 StopMoveScripts routine\cf0\b0\f1\fs20 
\par 
\par \pard\tx520\f0 Cancels all active move scripts.
\par \pard\b 
\par Syntax
\par \pard\tx540\b0 
\par \tab\cf2\f1 procedure \f0 stopMoveScripts\f1 ;\f0 
\par 
\par \cf0\b Description
\par 
\par \b0\tab This routine stops all assigned move scripts for all characters on the current map.  It does not affect the characters' default movement, though.
\par \pard\f1 
\par \b\f0 Related topics
\par \pard\tx400\b0\tab\cf3\strike waitUntilMoved routine\cf4\strike0\{linkID=217\}\cf0 
\par \tab\cf3\strike TRpgCharacter.move\cf4\strike0\{linkID=224\}\cf0\f1 
\par \pard 
\par }
219
Scribble219
PrepareRoute routine




Writing



FALSE
19
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 PrepareRoute routine\cf0\b0\f1\fs20 
\par 
\par \pard\tx520\f0 Sets up a new move script for characters to use.
\par \pard\b 
\par Syntax
\par \pard\tx540\b0 
\par \tab\cf2\f1 function prepareRoute(route: string; loop: boolean): word;\f0 
\par 
\par \cf0\b Description
\par 
\par \b0\tab The prepareRoute routine creates a new move script, according to the parameters passed to \b route\b0 .  If \cf2 true\cf0  is passed to \b loop\b0 , the script will loop continually; if \b loop\b0  is \cf2 false\cf0 , it will only run one time.  The routine returns an ID number for the new move script, which can be passed to \cf3\strike TRpgCharacter.move\cf4\strike0\{linkID=224\}\cf0 .  If the route entered is identical to an existing move script, it returns that route's ID number instead of creating a duplicate.
\par \tab The \b route\b0  string is made up of numbers separated by spaces.  Each number represents either a movement command or, in a few specific cases, extra data for the previous movement command.  The format is currently based on RPG Maker's somewhat convoluted system, and it is not recommended to attempt to write route strings manually.  The system will be simplified in a later version.
\par \pard\f1 
\par \b\f0 Related topics
\par \pard\tx400\b0\tab\cf3\strike stopMoveScripts routine\cf4\strike0\{linkID=217\}\cf0 
\par \tab\cf3\strike TRpgCharacter.move\cf4\strike0\{linkID=224\}\cf0\f1 
\par }
220
Scribble220
TRpgCharacter Class




Writing



FALSE
11
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgCharacter Class\cf0\b0\f1\fs20 
\par 
\par \pard\tx540\f0\tab TRpgCharacter is a base class that represents any on-screen character or event.  The \cf2\strike TRpgParty\cf3\strike0\{linkID=80\}\cf0 , \cf2\strike TRpgEvent\cf3\strike0\{linkID=90\}\cf0  and \cf2\strike TRpgVehicle\cf3\strike0\{linkID=170\}\cf0  classes inherit from TRpgCharacter.
\par \pard 
\par \cf3\f1\{button Properties,\f0 22\f1 1\}\f0   \{button Methods,222\}
\par 
\par \cf2\strike Back to RPG Script Reference\cf3\strike0\{linkID=60\}\cf0\f1 
\par \cf2\strike\f0 Explanation of Object Pascal types\cf3\strike0\{linkID=51\}\cf0\f1 
\par }
221
Scribble221
TRpgCharacter Properties




Writing



FALSE
27
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgCharacter Properties\cf0\b0\f1\fs20 
\par 
\par \cf2\strike\f0 Back to TRpgCharacter\cf3\strike0\{linkID=220\}\cf0 
\par \cf2\strike Back to RPG Script Reference\cf3\strike0\{linkID=60\}\cf0\f1 
\par \cf2\strike\f0 Explanation of Object Pascal types\cf3\strike0\{linkID=51\}\cf0 
\par 
\par 
\par \{html=<table border="1">
\par <tbody>
\par <tr>
\par <td><b>Name</b></td>
\par <td><b>Type</b></td>
\par <td><b>Description</b></td></tr>
\par <tr><td>screenX</td>
\par <td>integer</td>
\par <td>Determines the X coordinate of the character's position relative to the current screen position.</td></tr>
\par <tr><td>screenY</td>
\par <td>integer</td>
\par <td>Determines the Y coordinate of the character's position relative to the current screen position.</td></tr>
\par <tr><td>translucency</td>
\par <td>byte</td>
\par <td>Value from 0 (fully opaque) to 7 (almost completely translucent)</td></tr>
\par </tbody></table>\}\f1 
\par 
\par }
222
Scribble222
TRpgCharacter Methods




Writing



FALSE
10
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgCharacter Methods\cf0\b0\f1\fs20 
\par 
\par \pard\tx1440\cf2\strike\f0 Back to TRpgImage\cf3\strike0\{linkID=200\}\cf0 
\par \cf2\strike Back to RPG Script Reference\cf3\strike0\{linkID=60\}\cf0\f1 
\par 
\par \pard\tx2900\cf2\b\strike\f0 flash:\cf3\strike0\{linkID=223\}\cf0\b0\tab Causes the character to flash a certain color.
\par \cf2\b\strike move:\cf3\strike0\{linkID=224\}\cf0\b0\tab Causes the character to move onscreen.\cf2\b\strike 
\par }
223
Scribble223
TRpgCharacter.flash




Writing



FALSE
16
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue128;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgCharacter.flash\cf0\b0\f1\fs20 
\par 
\par \pard\tx420\b\f0 Class
\par \tab\cf2\b0\strike TRpgCharacter\cf3\strike0\{linkID=220\}\cf0\b 
\par 
\par Syntax\b0 
\par \tab\cf4 procedure flash(r, g, b, power: byte; time: cardinal; wait: boolean);
\par \cf0 
\par \b Description
\par \b0\tab This method causes the character to flash a certain color.  The \b r\b0 , \b g\b0 , and \b b\b0  parameters determine the color, and \b power\b0  is the opacity of the flash.  These parameters accept any value from 0 (none) to 255 (full).  The flash appears instantly, then gradually fades out over a period of \b time\b0  miliseconds.
\par 
\par \pard\b Related topics
\par \pard\tx400\b0\tab\cf2\strike FlashScreen routine\cf3\strike0\{linkID=189\}\cf0 
\par }
224
Scribble224
TRpgCharacter.move




Writing



FALSE
18
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue128;}
\viewkind4\uc1\pard\cf1\b\fs32 TRpgCharacter.move\cf0\b0\f1\fs20 
\par 
\par \pard\tx420\b\f0 Class
\par \tab\cf2\b0\strike TRpgCharacter\cf3\strike0\{linkID=220\}\cf0\b 
\par 
\par Syntax\b0 
\par \tab\cf4 procedure move(frequency: byte; skip: boolean; route: word);
\par \cf0 
\par \b Description
\par \b0\tab This method causes the character to move on-screen, or perform other actions according to the movement script designated by the \b route\b0  parameter.  The \b frequency\b0  parameter can be any number from 1 (long delay between steps) to 8 (no delay between steps).  If \cf4 true\cf0  is passed to the \b skip\b0  parameter, the movement script will skip any moves that are blocked or invalid and continue to the next step.  If \b skip\b0  is \cf4 false\cf0 , the character will wait until it is able to perform the movement before continuing.
\par 
\par \pard\b Related topics
\par \pard\tx400\b0\tab\cf2\strike waitUntilMoved routine\cf3\strike0\{linkID=217\}
\par \cf0\tab\cf2\strike stopMoveScripts routine\cf3\strike0\{linkID=217\}\cf0 
\par \pard\f1 
\par }
230
Scribble230
KeyScan routine




Writing



FALSE
40
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;}
\viewkind4\uc1\pard\cf1\b\fs32 KeyScan routine\cf0\b0\f1\fs20 
\par 
\par \pard\tx520\f0 Checks to see if any commands are being entered from the keyboard.
\par \pard\f1 
\par \b\f0 Syntax
\par \pard\tx540\b0 
\par \tab\cf2\f1 function keyScan(mask: word; wait: boolean): byte;\f0 
\par 
\par \cf0\b Description
\par 
\par \b0\tab The keyScan routine checks to see if any command keys are being pressed.  This includes arrow keys as well as action buttons.  The \b mask\b0  parameter determines which keys are checked for, according to the predefined key constants.  If \cf2 true\cf0  is passed to \b wait\b0 , then the script will pause until the player presses a key, but if \b wait\b0  is \cf2 false\cf0 , it will scan the keyboard once and return a value based on whatever key is currently pressed.
\par 
\par \b Mask key constants:
\par \tab NAME\tab\tab\tab\tab\tab VALUE\b0 
\par \tab KS_DOWN: down arrow\tab\tab\tab 1
\par \tab KS_UP: up arrow\tab\tab\tab\tab 2
\par \tab KS_LEFT: left arrow\tab\tab\tab 4
\par \tab KS_RIGHT: right arrow\tab\tab\tab 8
\par \tab KS_DIRS: \i any\i0  arrow key\tab\tab\tab 0xF
\par \tab KS_ACTION: action button (enter)\tab\tab 0x10
\par \tab KS_CANCEL: cancel button (escape)\tab 0x20
\par \tab KS_ALL: all input keys.  \tab\tab\tab 0xFFFF
\par 
\par \tab These names represent hexidecimal numbers, and can be combined with the word \b or\b0 .  For example, to check for either the action button or the cancel button, but not the arrow keys, you would pass "\cf2 KS_ACTION or KS_CANCEL\cf0 " to \b mask\b0 .  The KS_DIRS and KS_ALL variables include several numbers already combined, to simplify things.
\par 
\par \b Return values:
\par \b0\tab 0: No key pressed.
\par \tab 1: Down arrow
\par \tab 2: Left arrow
\par \tab 3: Right arrow
\par \tab 4: Up arrow
\par \tab 5: Action button
\par \tab 6: Cancel button
\par 
\par \tab\b NOTE:\b0   If \cf2 0\cf0  is passed to \b mask\b0 , there will be nothing to scan for, and the routine will immediately return a value of 0, even if \b wait\b0  is \cf2 true\cf0 .  This is to avoid deadlocking the script in an infinite loop.
\par \tab\b NOTE:\b0   KS_ALL does not check for <SPACE>, which opens the console window instead of sending input to the game itself.
\par \tab\b NOTE:\b0   This routine automatically delays 10 miliseconds (1/100 of a second) before returning when \b wait\b0  is \cf2 false\cf0 .  This is necessary to avoid a timing issue that could stall the program if keyScan is used in a loop.  This is a very short delay and should not cause any problems, but any scripts that depend on very precise timing may need to take this delay into account.\b 
\par }
231
Scribble231
DeleteCharacter routine




Writing



FALSE
14
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;}
\viewkind4\uc1\pard\cf1\b\fs32 DeleteCharacter routine\cf0\b0\f1\fs20 
\par 
\par \pard\tx520\f0 Removes the character the current script is attached to from the map entirely.
\par \pard\f1 
\par \b\f0 Syntax
\par \pard\tx540\b0 
\par \tab\cf2\f1 procedure deleteCharacter(permanant: boolean);\f0 
\par 
\par \cf0\b Description
\par 
\par \b0\tab The deleteCharacter routine deletes the character running the current script from the map, preventing it from running any further scripts.  If a script is in progress, it will be cut short.  If \cf2 true\cf0  is passed to \b permanant\b0 , the character will be removed for the rest of the game; otherwise, it will load again if the party leaves the map and comes back to it.
\par }
232
Scribble232
CallGlobalEvent routine




Writing



FALSE
18
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 CallGlobalEvent routine\cf0\b0\f1\fs20 
\par 
\par \f0 Causes a global event to run.
\par 
\par \b Syntax
\par \pard\tx540\b0 
\par \tab\cf2\f1 procedure callGlobalEvent(id: word);\f0 
\par 
\par \cf0\b Description
\par 
\par \pard\b0\tab The callGlobalEvent routine invokes event # \b id\b0  from the global event set.  This routine also ends the current script. If \b id\b0  is not a valid event number, no event is called, but the current script still ends.
\par 
\par \pard\tx540\b Related Topics\b0 
\par \tab\cf3\strike callEvent routine\cf4\strike0\{linkID=233\}\cf0\f1 
\par \pard 
\par }
233
Scribble233
CallEvent routine




Writing



FALSE
19
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue128;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 CallEvent routine\cf0\b0\f1\fs20 
\par 
\par \f0 Causes an event from the current map to run.
\par 
\par \b Syntax
\par \pard\tx540\b0 
\par \tab\cf2\f1 procedure callEvent(event, page: word);\f0 
\par 
\par \cf0\b Description
\par 
\par \pard\b0\tab The callEvent routine invokes the event attatched to the page specified by \b page\b0 , of the character # \b event\b0  from the current map.  This routine also ends the current script. If either \b event\b0  or \b page\b0  is not a valid ID number, no event is called, but the current script still ends.
\par 
\par \pard\tx540\b Related Topics\b0 
\par \tab\cf3\strike callGlobalEvent routine\cf4\strike0\{linkID=232\}\cf0\f1 
\par \pard 
\par 
\par }
0
0
0
159
1 About TURBU=Scribble10
1 Console
2 Console help=Scribble20
2 Script input line=Scribble25
2 Console Data Grids=Scribble30
1 Using RPG Script
2 RPG Script=Scribble40
2 Object Pascal basics=Scribble50
2 Explanation of Object Pascal types=Scribble51
2 Inheritance=Scribble53
2 Default Property=Scribble52
2 Array Element Zero=Scribble61
2 Read-only Properties=Scribble62
1 Technical Issues
2 MIDI Lead-in Issue=Scribble63
2 XYZ Transparency Issue=Scribble64
1 RPG Script reference
2 RPG Script reference=Scribble60
2 TRpgHero
3 TRpgHero Class=Scribble70
3 TRpgHero Properties=Scribble71
3 TRpgHero Methods=Scribble72
3 TRpgHero.equip=Scribble73
3 TRpgHero.unequip=Scribble74
3 TRpgHero.fullheal=Scribble75
3 TRpgHero.takeDamage=Scribble76
3 TRpgHero.setSprite=Scribble77
3 TRpgHero.setPortrait=Scribble78
3 TRpgHero.inParty=Scribble79
3 TRpgHero.equipped=Scribble150
2 TRpgCharacter Class
3 TRpgCharacter Class=Scribble220
3 TRpgCharacter Properties=Scribble221
3 TRpgCharacter Methods=Scribble222
3 TRpgCharacter.flash=Scribble223
3 TRpgCharacter.move=Scribble224
2 TRpgParty
3 TRpgParty Class=Scribble80
3 TRpgParty Properties=Scribble81
3 TRpgParty Methods=Scribble82
3 TRpgParty.addItem=Scribble83
3 TRpgParty.removeItem=Scribble84
3 TRpgParty.addExp=Scribble85
3 TRpgParty.removeExp=Scribble86
3 TRpgParty.addLevels=Scribble87
3 TRpgParty.removeLevels=Scribble88
3 TRpgParty.takeDamage=Scribble89
2 TRpgEvent
3 TRpgEvent Class=Scribble90
3 TRpgEvent Properties=Scribble91
3 TRpgEvent Methods=Scribble92
2 TRpgTimer
3 TRpgTimer Class=Scribble100
3 TRpgTimer Properties=Scribble101
3 TRpgTimer Methods=Scribble102
3 TRpgTimer.go=Scribble103
3 TRpgTimer.start=Scribble104
3 TRpgTimer.pause=Scribble105
3 TRpgTimer.reset=Scribble106
2 TRpgInventory
3 TRpgInventory Class=Scribble140
3 TRpgInventory Properties=Scribble141
3 TRpgInventory Methods=Scribble142
3 TRpgInventory.add=Scribble143
3 TRpgInventory.remove=Scribble144
3 TRpgInventory.contains=Scribble145
2 TRpgVehicle
3 TRpgVehicle Class=Scribble170
3 TRpgVehicle Properties=Scribble171
3 TRpgVehicle Methods=Scribble172
3 TRpgVehicle.setSprite=Scribble173
2 TRpgImage Class
3 TRpgImage Class=Scribble200
3 TRpgImage Properties=Scribble201
3 TRpgImage Methods=Scribble202
3 TRpgImage.applyImageColors=Scribble203
3 TRpgImage.applyImageEffect=Scribble204
3 TRpgImage.moveTo=Scribble205
3 TRpgImage.erase=Scribble206
2 Enumerated types
3 TSlotList Type=Scribble110
3 TMboxLocation Type=Scribble111
3 TBgmTypes Type=Scribble112
3 TSfxTypes Type=Scribble114
3 TBattleResult Type=Scribble113
3 TTransitions Type=Scribble115
3 TTransitionTypes Type=Scribble116
3 TDirections Type=Scribble117
3 TWeatherEffects type=Scribble118
3 TImageEffects Type=Scribble119
2 Global RPG Script routines
3 Random routine=Scribble120
3 ButtonStart routine=Scribble160
3 Wait routine=Scribble198
3 DeleteCharacter routine=Scribble231
3 Message Box routines
4 ShowMessage routine=Scribble121
4 MessageOptions routine=Scribble122
4 SetPortrait routine=Scribble123
4 ClearPortrait routine=Scribble124
4 ShowChoice routine=Scribble125
4 InputNumber routine=Scribble126
4 Inn routine=Scribble139
3 Party Management routines
4 HeldItems routine=Scribble127
4 HeroJoin routine=Scribble128
4 HeroLeave routine=Scribble129
4 AddItem routine=Scribble130
4 RemoveItem routine=Scribble131
4 AddExp routine=Scribble132
4 RemoveExp routine=Scribble133
4 AddLevels routine=Scribble134
4 RemoveLevels routine=Scribble135
3 System Settings routines
4 SetSystemMusic routine=Scribble136
4 SetSystemSound routine=Scribble161
4 SetSkin routine=Scribble162
4 SetTransition routine=Scribble187
4 PlayMusic routine=Scribble211
4 FadeOutMusic routine=Scribble212
4 MemorizeBgm routine=Scribble214
4 PlayMemorizedBgm routine=Scribble215
4 PlaySound routine=Scribble216
4 KeyScan routine=Scribble230
4 CallGlobalEvent routine=Scribble232
4 CallEvent routine=Scribble233
3 Menu routines
4 OpenMenu routine=Scribble163
4 PrepareStore routine=Scribble164
4 Shop routine=Scribble138
4 InputText Routine=Scribble165
4 Battle routine=Scribble137
3 Map Control routines
4 MemorizeLocation routine=Scribble167
4 Teleport routine=Scribble168
4 TeleportVehicle routine=Scribble169
4 TeleportEvent routine=Scribble180
4 SwapEvents routine=Scribble181
4 RideVehicle routine=Scribble182
4 GetTerrainID routine=Scribble183
4 GetEventID routine=Scribble184
4 EraseScreen routine=Scribble185
4 ShowScreen Routine=Scribble186
4 SetScreenTone routine=Scribble188
4 FlashScreen routine=Scribble189
4 LockScreen routine=Scribble190
4 UnlockScreen routine=Scribble191
4 PanScreen routine=Scribble192
4 PanScreenTo routine=Scribble193
4 ReturnScreen routine=Scribble194
4 SetWeather routine=Scribble195
4 IncreaseWeather routine=Scribble196
4 DecreaseWeather routine=Scribble197
4 NewImage Routine=Scribble199
4 SetBGImage routine=Scribble210
4 ShowBattleAnim routine=Scribble213
4 PrepareRoute routine=Scribble219
4 WaitUntilMoved routine=Scribble217
4 StopMoveScripts routine=Scribble218
6
*InternetLink
16711680
Courier New
0
10
1
....
0
0
0
0
0
0
*ParagraphTitle
-16777208
Arial
0
11
1
B...
0
0
0
0
0
0
*PopupLink
-16777208
Arial
0
8
1
....
0
0
0
0
0
0
*PopupTopicTitle
16711680
Arial
0
10
1
B...
0
0
0
0
0
0
*TopicText
-16777208
Arial
0
10
1
....
0
0
0
0
0
0
*TopicTitle
16711680
Arial
0
16
1
B...
0
0
0
0
0
0
